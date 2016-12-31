{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE PatternGuards #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE DuplicateRecordFields #-}
module JoScript.Text.LexerPass (runLexerPass) where

import Prelude (Bool, Int, otherwise, (.), (+), (-), (||), fromIntegral)
import qualified Prelude as Std

import Data.Eq ((==))
import Data.Ord ((>=))
import Data.Char (Char, isDigit)
import qualified Data.Char as Char
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Text (Text)
import Data.Word (Word64)
import qualified Data.Foldable as F
import qualified Data.Text as T
import qualified Data.Conduit as C

import Control.Lens ((%=), (.=), use)
import Control.Monad ((>>=), (>>), Monad, unless)
import Control.Applicative (pure, (<*))
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State as S

import JoScript.Data.Error ( Error(..)
                           , Repr(LexerError)
                           , LexerErrorT(..)
                           , known
                           )
import JoScript.Data.Position (Position)
import JoScript.Data.BlockPass (BlockPass(..), BpRepr(..))
import JoScript.Data.LexerPass (LexerPass(..), LpRepr(..), LpNumber(..))
import qualified JoScript.Data.BlockPass as Bp
import qualified JoScript.Data.LexerPass as Lp
import qualified JoScript.Data.Error as Error
import qualified JoScript.Data.Position as Position
import qualified JoScript.Util.Text as T
import JoScript.Util.Conduit (ConduitE, ResultConduit)

type ContCallback = Text -> Int -> ItCont

data ItCont
  = ItJump Int ContCallback
  | ItFail Error.LexerErrorT
  | ItEmit Int (Text -> Lp.LpRepr)
  | ItNext

data Payload = P { line  :: Text
                 , lexer :: ContCallback
                 }

data Branch
  = Read
  | ReadMore Payload Word64
  | ContToken Payload Word64
  | StartToken Text

data State = S { position :: Position }

--------------------------------------------------------------
--                          Lens                            --
--------------------------------------------------------------

-- line' :: Std.Functor f => (Text -> f Text) -> Payload -> f Payload
-- line' f (P ln lx) = Std.fmap (\ln' -> P ln' lx) (f ln)

--position' :: Std.Functor f => (Position -> f Position) -> Payload -> f Payload
--position' f (P ln p lx) = Std.fmap (\p' -> P ln p' lx) (f p)

-- lexer' :: Std.Functor f => (ContCallback -> f ContCallback) -> Payload -> f Payload
-- lexer' f (P ln lx) = Std.fmap (\lx' -> P ln lx') (f lx)

sPosition :: Std.Functor f => (Position -> f Position) -> State -> f State
sPosition f (S p) = Std.fmap (\p' -> S  p') (f p)

--------------------------------------------------------------
--                      Entry point                         --
--------------------------------------------------------------

type LexerConduit = ResultConduit BlockPass LexerPass
type Lexer m = E.ExceptT Error (S.StateT State (LexerConduit m))

runLexerPass :: Monad m => LexerConduit m ()
runLexerPass =
  let s = E.runExceptT (loop Read)
      i = S { position = Position.init }
   in S.evalStateT s i >>= \case
      Right _____ -> pure ()
      Left except -> C.yield (Left except)

--------------------------------------------------------------
--                      Event loop                          --
--------------------------------------------------------------

loop :: Monad m => Branch -> Lexer m ()

-- this called every time we're looping without any undetermined state
loop Read = readToken >>= \case
  BpEnd    -> yieldToken LpEnd
  BpDedent -> yieldToken LpDedent >> loop Read
  BpIndent -> yieldToken LpIndent >> loop Read
  BpLine l -> loop (StartToken l)

-- will be invoked when `ContToken' needs more text, and
-- an error will occur with the awaited token is anything
-- other than a new line
loop (ReadMore payload offset) = readToken >>= \case
  BpEnd    -> throwFromPosition (UnexpectedToken BpEnd)
  BpDedent -> throwFromPosition (UnexpectedToken BpDedent)
  BpIndent -> throwFromPosition (UnexpectedToken BpIndent)
  BpLine l ->
    let nl = T.append (line payload) l
     in loop (ContToken (payload { line = nl }) offset)

-- This is branch that is called at the start of every token,
-- as well was spawning error messages on any illegal tokens.
loop (StartToken line) = withLine line where
  withLine (T.uncons -> Nothing) = loop Read
  withLine (T.uncons -> Just (head, tail)) = withHead head where

    continueToken lexer = loop (ContToken payload 0)
      where payload = P { line, lexer }

    emit token = do
      yieldToken token
      sPosition %= Position.update head
      loop (StartToken tail)

    withHead '\n' = emit LpNewline
    withHead '\'' = emit LpQuote
    withHead '.'  = emit LpDotOperator
    withHead '*'  = emit LpRestOperator
    withHead '@'  = emit LpDecoratorPrefix
    withHead '['  = emit LpBraceL
    withHead ']'  = emit LpBraceR
    withHead '|'  = emit LpPipe
    withHead '='  = emit LpAssign
    withHead ':'  = emit LpColon
    withHead ' '  = continueToken lexWhitespace
    withHead '#'  = continueToken lexComment
    withHead '"'  = continueToken lexString
    withHead c
      | isIdentiferHead c = continueToken lexIdentifier
      | isDigit c         = continueToken lexUInt
      | otherwise         = throwFromPosition (UnknownTokenStart c)

-- traverse over non trival multicharacter tokens
loop (ContToken payload i) = iter i where
  line' :: Text
  line' = (line payload)

  length :: Word64
  length = fromIntegral (T.length line')

  iter :: Monad m => Word64 -> Lexer m ()
  iter offset
    | offset >= length = loop (ReadMore payload offset)
    | length == 0 = loop Read
    | otherwise   = case lexer payload line' (fromIntegral offset) of
        ItNext       -> iter (offset + 1)
        ItJump i fn -> loop (ContToken (payload { lexer = fn }) (fromIntegral i))
        ItEmit i fn  -> do
          let taken  = T.take i line'
          let remain = T.drop i line'
          yieldToken (fn taken)
          sPosition %= Position.moveOver taken
          loop (StartToken remain)
        ItFail error -> do
          position <- use sPosition
          let taken = T.take (fromIntegral offset) line'
          let shift = Position.moveOver taken position
          E.throwE (known (LexerError error) shift)


--------------------------------------------------------------
--                        lex trees                         --
--------------------------------------------------------------

lexIdentifier :: Text -> Int -> ItCont
lexIdentifier t i
  | isIdentiferTerminator head = ItEmit i LpIdentifier
  | isIdentiferCharacter  head = ItNext
  where head = T.index t i

lexWhitespace :: Text -> Int -> ItCont
lexWhitespace t i
  | ' ' <- T.index t i = ItNext
  | otherwise          = ItEmit i (LpSpace . fromIntegral . T.length)

lexComment :: Text -> Int -> ItCont
lexComment t i
  | '\n' <- T.index t i = ItEmit i (LpComment . T.drop 1)
  | char <- T.index t i = ItNext


lexString :: Text -> Int -> ItCont
lexString t i
  | '"' <- T.index t i = if i == 0
      then ItNext
      else ItEmit (i + 1) (LpString . T.drop 1)
  | otherwise          = ItNext

lexUInt :: Text -> Int -> ItCont
lexUInt t i
  | Char.isDigit head    = ItNext
  | head == '.'          = ItJump (i + 1) lexUFloat
  | isNumTerminator head = ItEmit i (LpNumberLit . LpInteger . T.readInt)
  | otherwise            = ItFail (InvalidIntSuffix head)
  where head = T.index t i

lexUFloat :: Text -> Int -> ItCont
lexUFloat t i
  | Char.isDigit head    = ItNext
  | isNumTerminator head = ItEmit i (LpNumberLit . LpFloat . T.readFloat)
  | head == '.'          = ItFail DuplicateDecimial
  | otherwise            = ItFail (InvalidIntSuffix head)
  where head = T.index t i

--------------------------------------------------------------
--                        Util func                         --
--------------------------------------------------------------

isNumTerminator :: Char -> Bool
isNumTerminator = isIdentiferTerminator

isIdentiferTerminator :: Char -> Bool
isIdentiferTerminator c = F.elem c " :\t\n,.(){}[]|"

isIdentiferHead :: Char -> Bool
isIdentiferHead ch = Std.not (isDigit || isTerminator || containsIllegal)
  where containsIllegal = F.elem ch "#@"
        isTerminator    = isIdentiferTerminator ch
        isDigit         = Char.isDigit ch

isIdentiferCharacter :: Char -> Bool
isIdentiferCharacter = Std.not . isIdentiferTerminator

throwFromPosition :: Monad m => LexerErrorT -> Lexer m ()
throwFromPosition error' = do
  position <- use sPosition
  E.throwE (known (LexerError error') position)

readUpdate :: Monad m => Lexer m (Position, BpRepr)
readUpdate = liftConduit C.await >>= \case
  Nothing -> do
    position <- use sPosition
    E.throwE (known (LexerError UnexpectedEnd) position)

  Just (Right item) -> do
    let (Bp repr p) = item
    sPosition .= p
    pure (p, addNewline repr) where
      addNewline (BpLine line) = BpLine (T.snoc line '\n')
      addNewline otherResult   = otherResult

  Just (Left except) -> E.throwE except

readToken :: Monad m => Lexer m BpRepr
readToken = readUpdate >>= \(_, t) -> pure t

yieldElem :: Monad m => Position -> LpRepr -> Lexer m ()
yieldElem p r = liftConduit (C.yield (Right (Lp r p)))

yieldToken :: Monad m => LpRepr -> Lexer m ()
yieldToken token = do
  position <- use sPosition
  yieldElem position token


--------------------------------------------------------------
--                        Wrap func                         --
--------------------------------------------------------------

{- whenever the monad stack is modified we'll only need
 - to update the lifting of conduit function calls here -}
liftConduit m = lift (lift m)

