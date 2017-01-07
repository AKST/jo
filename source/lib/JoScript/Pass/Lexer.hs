module JoScript.Pass.Lexer (runLexerPass) where

import Protolude hiding (State)
import Data.Char (isDigit)

import qualified Data.Text as T
import qualified Data.Conduit as C

import Control.Lens ((%=), (.=), use)

import JoScript.Data.Error (Error(..), Repr(LexerError), LexerErrorT(..), known)
import JoScript.Data.Position (Position)
import JoScript.Data.Block (BlockPass(..), BpRepr(..))
import JoScript.Data.Lexer (LexerPass(..), LpRepr(..))
import qualified JoScript.Data.Lexer as Lp
import qualified JoScript.Data.Error as Error
import qualified JoScript.Data.Position as Position
import qualified JoScript.Util.Text as T
import JoScript.Util.Conduit (ResultConduit)

type TokenBranchCallback = Text -> Int -> TokenBranchStep

data TokenBranchStep
  = JumpStep Int TokenBranchCallback
  | FailStep Error.LexerErrorT
  | EmitStep Int (Text -> Lp.LpRepr)
  | NextStep

data Payload = P { line  :: Text
                 , lexer :: TokenBranchCallback
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

position' :: Functor f => (Position -> f Position) -> State -> f State
position' f (S p) = fmap (\p' -> S  p') (f p)

--------------------------------------------------------------
--                      Entry point                         --
--------------------------------------------------------------

type LexerConduit m = ResultConduit BlockPass LexerPass m
newtype Lexer m a   = Lexer { run :: ExceptT Error (StateT State (LexerConduit m)) a }
  deriving (Functor, Applicative, Monad, MonadError Error, MonadState State)

runLexerPass :: Monad m => LexerConduit m ()
runLexerPass =
  let s = runExceptT (run (loop Read))
      i = S { position = Position.init }
   in evalStateT s i >>= \case
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
  BpEnd    -> throwFromPosition (LUnexpectedToken BpEnd)
  BpDedent -> throwFromPosition (LUnexpectedToken BpDedent)
  BpIndent -> throwFromPosition (LUnexpectedToken BpIndent)
  BpLine l ->
    let nl = T.append (line payload) l
     in loop (ContToken (payload { line = nl }) offset)

-- This is branch that is called at the start of every token,
-- as well was spawning error messages on any illegal tokens.
loop (StartToken line) = withLine line where

  withLine (T.uncons -> Just (h, t)) = withHead h where

    continueToken lexer = loop (ContToken payload 0)
      where payload = P { line, lexer }

    emit token = do
      yieldToken token
      position' %= Position.update h
      loop (StartToken t)

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
      | otherwise         = throwFromPosition (LUnknownTokenStart c)

  withLine _ = loop Read

-- traverse over non trival multicharacter tokens
loop (ContToken payload startIndex) = iter startIndex where
  line' :: Text
  line' = (line payload)

  length' :: Word64
  length' = fromIntegral (T.length line')

  iter :: Monad m => Word64 -> Lexer m ()
  iter offset
    | length' == 0 = loop Read
    | offset >= length' = loop (ReadMore payload offset)
    | otherwise = case lexer payload line' (fromIntegral offset) of
        NextStep       -> iter (offset + 1)
        JumpStep i fn -> loop (ContToken (payload { lexer = fn }) (fromIntegral i))
        EmitStep i fn  -> do
          let taken  = T.take i line'
          let remain = T.drop i line'
          yieldToken (fn taken)
          position' %= Position.moveOver taken
          loop (StartToken remain)
        FailStep err -> do
          let taken = T.take (fromIntegral offset) line'
          position' %= Position.moveOver taken
          throwFromPosition err


--------------------------------------------------------------
--                        lex trees                         --
--------------------------------------------------------------

lexIdentifier :: Text -> Int -> TokenBranchStep
lexIdentifier t i
  | isIdentiferTerminator h = EmitStep i LpIdentifier
  | isIdentiferCharacter  h = NextStep
  | otherwise               = FailStep (LUnexpectedCharacter h)
  where h = T.index t i

lexWhitespace :: Text -> Int -> TokenBranchStep
lexWhitespace t i
  | ' ' <- T.index t i = NextStep
  | otherwise          = EmitStep i (LpSpace . fromIntegral . T.length)

lexComment :: Text -> Int -> TokenBranchStep
lexComment t i
  | '\n' <- T.index t i = EmitStep i (LpComment . T.drop 1)
  | ____ <- T.index t i = NextStep


lexString :: Text -> Int -> TokenBranchStep
lexString t i
  | '"' <- T.index t i = if i == 0
      then NextStep
      else EmitStep (i + 1) (LpString . T.tail . T.init)
  | otherwise          = NextStep

lexUInt :: Text -> Int -> TokenBranchStep
lexUInt t i
  | isDigit h         = NextStep
  | h == '.'          = JumpStep (i + 1) lexUFloat
  | isNumTerminator h = EmitStep i (LpInteger . T.readInt)
  | otherwise         = FailStep (LInvalidIntSuffix h)
  where h = T.index t i

lexUFloat :: Text -> Int -> TokenBranchStep
lexUFloat t i
  | isDigit h         = NextStep
  | isNumTerminator h = EmitStep i (LpFloat . T.readFloat)
  | h == '.'          = FailStep LDuplicateDecimial
  | otherwise         = FailStep (LInvalidIntSuffix h)
  where h = T.index t i

--------------------------------------------------------------
--                        Util func                         --
--------------------------------------------------------------

isNumTerminator :: Char -> Bool
isNumTerminator = isIdentiferTerminator

isIdentiferTerminator :: Char -> Bool
isIdentiferTerminator c = elem c " :\t\n,.(){}[]|"

isIdentiferHead :: Char -> Bool
isIdentiferHead ch = not (isDigit' || isTerminator || containsIllegal)
  where containsIllegal = elem ch "#@"
        isTerminator    = isIdentiferTerminator ch
        isDigit'        = isDigit ch

isIdentiferCharacter :: Char -> Bool
isIdentiferCharacter = not . isIdentiferTerminator

throwFromPosition :: Monad m => LexerErrorT -> Lexer m ()
throwFromPosition error' = do
  position <- use position'
  throwError (known (LexerError error') position)

readUpdate :: Monad m => Lexer m (Position, BpRepr)
readUpdate = liftConduit C.await >>= \case
  Nothing -> do
    position <- use position'
    throwError (known (LexerError LUnexpectedEnd) position)

  Just (Right item) -> do
    let (Bp repr p) = item
    position' .= p
    pure (p, addNewline repr) where
      addNewline (BpLine line) = BpLine (T.snoc line '\n')
      addNewline otherResult   = otherResult

  Just (Left except) -> throwError except

readToken :: Monad m => Lexer m BpRepr
readToken = readUpdate >>= \(_, t) -> pure t

yieldElem :: Monad m => Position -> LpRepr -> Lexer m ()
yieldElem p r = liftConduit (C.yield (Right (Lp r p)))

yieldToken :: Monad m => LpRepr -> Lexer m ()
yieldToken token = do
  position <- use position'
  yieldElem position token


--------------------------------------------------------------
--                        Wrap func                         --
--------------------------------------------------------------

{- whenever the monad stack is modified we'll only need
 - to update the lifting of conduit function calls here -}
liftConduit :: Monad m => LexerConduit m a -> Lexer m a
liftConduit m = Lexer (lift (lift m))

