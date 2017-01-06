{-# LANGUAGE OverloadedStrings #-}
module JoScript.Pass.Parse (runParsePass) where

import Prelude (undefined)
import Protolude hiding (State, undefined, try)

import Data.Sequence ((|>))
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Conduit as C

import Text.Parser.Combinators (choice)

import Control.Lens ((.=), (%=), use, view)

import JoScript.Util.Conduit (ResultSink, Result)
import JoScript.Data.Config (FileBuildConfig(..), filename')
import JoScript.Data.Error (Error(..), known, Repr(ParseError), ParseErrorT(..), newestError)
import JoScript.Data.Syntax ( SynModule(..)
                            , SynExpr(..)
                            , SynExprRepr(..)
                            , SynParamsApp(..)
                            , Ref(..)
                            , SynId(SynId)
                            )
import JoScript.Data.Position (Position)
import JoScript.Data.Lexer (LexerPass(..), LpRepr(..))
import qualified JoScript.Data.Lexer as Lexer
import qualified JoScript.Data.Position as Position


data Recovery
  = RecEnd
  | RecAdd State [LexerPass]
  deriving (Show, Eq)

data Label = Label { position :: Position, description :: Text }
  deriving (Show, Eq)

data State = S { recovery :: Recovery, labels :: Labels, position :: Position }
  deriving (Show, Eq)

type Labels = [Label]

--------------------------------------------------------------
--                          Lens                            --
--------------------------------------------------------------

recovery' :: Functor f => (Recovery -> f Recovery) -> State -> f State
recovery' f (S r l p) = fmap (\r' -> S r' l p) (f r)

labels' :: Functor f => (Labels -> f Labels) -> State -> f State
labels' f (S r l p) = fmap (\l' -> S r l' p) (f l)

position' :: Functor f => (Position -> f Position) -> State -> f State
position' f (S r l p) = fmap (\p' -> S r l p') (f p)

--------------------------------------------------------------
--                      Entry point                         --
--------------------------------------------------------------

type ParserConduit m = ResultSink LexerPass m

newtype Parser m a = Parser { run :: ExceptT Error (StateT State (ParserConduit m)) a }
  deriving ( Functor
           , Applicative
           , Monad
           , MonadError Error
           , MonadState State
           , MonadReader FileBuildConfig
           )

runParsePass :: Monad m => ParserConduit m (Result SynModule)
runParsePass =
  let s = runExceptT (run root)
      i = S { recovery = RecEnd
            , labels   = mempty
            , position = Position.init }
   in evalStateT s i >>= \case
      Right mod -> pure (Right mod)
      Left except -> pure (Left except)

--------------------------------------------------------------
--                          Parser                          --
--------------------------------------------------------------

root :: Monad m => Parser m SynModule
root = SynModule <$> block <*> view filename'

block :: Monad m => Parser m (Seq SynExpr)
block = iter mempty where
  iter acc = newItem acc <|> emptyLine <|> end where
    emptyLine = newline >> iter acc
    end       = consumeIf Lexer.isEnd >> pure acc

  newItem acc = do
    item <- statement <* newline
    iter (acc |> item)

statement :: Monad m => Parser m SynExpr
statement = comment
        <|> unwrappedInvoke

unwrappedInvoke :: Monad m => Parser m SynExpr
unwrappedInvoke = SynInvokation <$> func <*> args >>= pureExpr where
  func = expression <* some space

  args :: Monad m => Parser m SynParamsApp
  args = SynParamsApp <$> positional <*> rest <*> keywords where

    positional = expression `sepBy1` some space

    rest = pure Nothing

    keywords :: Monad m => Parser m (Map SynId SynExpr)
    keywords = (Map.fromList . toList) <$> pairs
      where pairs   = pair `sepBy` some space
            pair    = (,) <$> (keyword <* some space) <*> expression
            keyword = synIdentifier <* consumeIf Lexer.isColon

propableExpressions :: Monad m => [Parser m SynExpr]
propableExpressions = [contextual, string, symbol, identifier]

quoteableExpressions :: Monad m => [Parser m SynExpr]
quoteableExpressions = property : propableExpressions

expression :: Monad m => Parser m SynExpr
expression = choice (quote : quoteableExpressions)

comment :: Monad m => Parser m SynExpr
comment = consume >>= \case
  LpComment com -> pureExpr (SynComment com)
  token         -> throwFromHere (PUnexpectedToken token)

property :: Monad m => Parser m SynExpr
property = (conn <$> expr <*> prop) >>= pureExpr where
  expr = choice propableExpressions
  conn e p = SynReference (RefProperty e p)
  prop = consumeIf Lexer.isDotOperator *> synIdentifier

quote :: Monad m => Parser m SynExpr
quote = tick *> (SynQuote <$> expr) >>= pureExpr where
  tick = consumeIf Lexer.isQuote
  expr = choice quoteableExpressions

string :: Monad m => Parser m SynExpr
string = (SynStringLit <$> stringToken) >>= pureExpr where
  stringToken = consume >>= \case
    LpString string -> pure string
    token           -> throwFromHere (PUnexpectedToken token)

symbol :: Monad m => Parser m SynExpr
symbol = colon *> (SynSymbol <$> synIdentifier) >>= pureExpr where
  colon = consumeIf Lexer.isColon

contextual :: Monad m => Parser m SynExpr
contextual = dot *> (SynContextual <$> synIdentifier) >>= pureExpr where
  dot = consumeIf Lexer.isDotOperator

identifier :: Monad m => Parser m SynExpr
identifier = (SynReference . RefIdentity <$> synIdentifier) >>= pureExpr

newline :: Monad m => Parser m ()
newline = void (consumeIf Lexer.isNewline)

space :: Monad m => Parser m ()
space = void (consumeIf Lexer.isSpace)

synIdentifier :: Monad m => Parser m SynId
synIdentifier = consume >>= \case
  LpIdentifier text -> pure (SynId text)
  unexpected        -> throwFromHere (PUnexpectedToken unexpected)

--------------------------------------------------------------
--                   Parse Combinators                      --
--------------------------------------------------------------

sepBy :: Parser m a -> Parser m b -> Parser m (Seq a)
sepBy elem sep = (elem `sepBy1` sep) <|> pure mempty


sepBy1 :: Parser m a -> Parser m b -> Parser m (Seq a)
sepBy1 elem sep = elem >>= \e -> iter (mempty |> e) where
  iter acc = try (sep *> elem) >>= \case
    Just item -> iter (acc |> item)
    Nothing   -> pure acc

try :: Parser m a -> Parser m (Maybe a)
try p = (Just <$> p) <|> pure Nothing

failIf :: Parser m LpRepr -> Parser m ()
failIf m = try m >>= \case
  Just t  -> throwFromHere (PUnexpectedToken t)
  Nothing -> pure ()

--------------------------------------------------------------
--                        Util func                         --
--------------------------------------------------------------

pureExpr :: SynExprRepr -> Parser m SynExpr
pureExpr expr = do
  position <- use position'
  pure (SynExpr { expr, position })

throwFromHere :: ParseErrorT -> Parser m a
throwFromHere err = do
  pos <- use position'
  throwKnown err pos

throwKnown :: ParseErrorT -> Position -> Parser m a
throwKnown err pos =
  throwError (known (ParseError err) pos)

consume :: Monad m => Parser m LpRepr
consume = do
  t@(Lp repr _) <- rawAwait
  record t
  pure repr

consumeIf :: Monad m => (LpRepr -> Bool) -> Parser m LpRepr
consumeIf predicate = consume >>= \repr ->
  if predicate repr
    then pure repr
    else throwFromHere (PUnexpectedToken repr)

rawAwait :: Monad m => Parser m LexerPass
rawAwait = liftConduit C.await >>= \case
  Nothing -> do
    pos <- use position'
    throwError (known (ParseError PUnexpectedEnd) pos)
  Just (Left error) ->
    throwError error
  Just (Right item@(Lp _ position)) -> do
    position' .= position
    pure item

--------------------------------------------------------------
--                     Parse Recovery                       --
--------------------------------------------------------------

record :: LexerPass -> Parser m ()
record pass = use recovery' >>= \case
  RecEnd        -> pure ()
  RecAdd s prev -> recovery' .= RecAdd s (pass:prev)

{- # activateRecovery
 -
 - Any calls to await will also be recorded so if a recoverable
 - error takes place any fetched items during parse can be
 - recovered -}
activateRecovery :: Parser m ()
activateRecovery = do
  state     <- get
  recovery' .= RecAdd state mempty

{- # forgetRecovery
 -
 - Discard the cache of any awaited items since the previous
 - call to `activateRecovery` -}
forgetRecovery :: Parser m ()
forgetRecovery = use recovery' >>= \case
  RecEnd      -> pure ()
  RecAdd st _ -> recovery' .= (recovery st)

{- # applyRecovery
 -
 - Restores all cached lexicons to the previous state -}
applyRecovery :: Parser m ()
applyRecovery = use recovery' >>= \case
  RecEnd       -> pure ()
  RecAdd st sv -> put st *> liftConduit (forM_ sv (C.leftover . Right))

instance Alternative (Parser m) where
  empty = do
    pos <- use position'
    throwError (known (ParseError PIncompleteAlt) pos)

  left <|> right = do
    activateRecovery
    catchError (left <* forgetRecovery) $ \first ->
      catchError (applyRecovery *> right) $ \second ->
        throwError (newestError first second)

instance MonadPlus (Parser m)

--------------------------------------------------------------
--                        Wrap func                         --
--------------------------------------------------------------

{- whenever the monad stack is modified we'll only need
 - to update the lifting of conduit function calls here -}
liftConduit :: ParserConduit m a -> Parser m a
liftConduit m = Parser (lift (lift m))

