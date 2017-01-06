{-# LANGUAGE OverloadedStrings #-}
module JoScript.Pass.Parse (runParsePass) where

import Prelude (undefined)
import Protolude hiding (State, undefined, try)

import Data.Sequence ((|>), ViewL(..), viewl)
import qualified Data.List as List
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Conduit as C

import Text.Parser.Combinators (choice)

import Control.Lens ((.=), (%=), use, view)

import JoScript.Util.Conduit (ResultSink, Result)
import JoScript.Data.Config (FileBuildConfig(..), filename')
import JoScript.Data.Error ( Error(..)
                           , known
                           , Location(..)
                           , Repr(ParseError)
                           , ParseErrorT(..)
                           , newestError
                           , Label(..)
                           )
import JoScript.Data.Syntax ( SynModule(SynModule)
                            , SynExpr(..)
                            , SynNumLitT(..)
                            , SynExprRepr(..)
                            , SynParamsApp(..)
                            , Ref(..)
                            , SynId(SynId)
                            )
import JoScript.Data.Position (Position)
import JoScript.Data.Lexer ( LexerPass(..)
                           , LpRepr(..)
                           , LpNumber(..)
                           , LpReprKind(..)
                           , reprKind)
import qualified JoScript.Data.Lexer as Lexer
import qualified JoScript.Data.Position as Position


data Recovery
  = RecEnd
  | RecAdd State [LexerPass]
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
  where block = statements <* consumeKind LpKindEnd

statements :: Monad m => Parser m (Seq SynExpr)
statements = (statement `sepEndBy1` some sep) <?> "block"
  where sep = try spaces >> newline

statement :: Monad m => Parser m SynExpr
statement = parser <?> "statement" where
  parser = comment <|> unwrappedInvoke

unwrappedInvoke :: Monad m => Parser m SynExpr
unwrappedInvoke = (SynInvokation <$> expression <*> args >>= pureExpr) <?> "function call" where
  args :: Monad m => Parser m SynParamsApp
  args = SynParamsApp <$> try' mempty (spaces *> positional)
                      <*> try         (spaces *> rest)
                      <*> try' mempty (spaces *> keywords) where

    positional = (expression `sepBy1` some space) <?> "positional arguments"

    rest = consumeKind LpKindRestOperator *> expression

    keywords :: Monad m => Parser m (Map SynId SynExpr)
    keywords = (Map.fromList . toList <$> pairs) <?> "keyword arguments"
      where pairs   = pair `sepBy1` spaces
            pair    = (,) <$> keyword <*> (expression <?> "keyword value")
            keyword = (name <* colon <* spaces) <?> "keyword" where
              name  = synIdentifier <?> "keyword name"
              colon = consumeKind LpKindColon <?> "suffix"

quoteableExpressions :: Monad m => [Parser m SynExpr]
quoteableExpressions = [nestedCall, contextual, string, symbol, reference, number]

expression :: Monad m => Parser m SynExpr
expression = choice (quote : quoteableExpressions) <?> "expression"

nestedCall :: Monad m => Parser m SynExpr
nestedCall = (consumeKind LpKindBraceL) *> unwrappedInvoke <* (consumeKind LpKindBraceR)

comment :: Monad m => Parser m SynExpr
comment = consume >>= \case
  LpComment com -> pureExpr (SynComment com)
  token         -> throwFromHere (PUnexpectedToken token)

quote :: Monad m => Parser m SynExpr
quote = (tick *> (SynQuote <$> expr) >>= pureExpr) <?> "quote" where
  tick = consumeKind LpKindQuote
  expr = choice quoteableExpressions

string :: Monad m => Parser m SynExpr
string = (SynStringLit <$> stringToken >>= pureExpr) <?> "string" where
  stringToken = consume >>= \case
    LpString string -> pure string
    token           -> throwFromHere (PUnexpectedToken token)

number :: Monad m => Parser m SynExpr
number = (consume >>= fn) <?> "number" where
  fn (LpNumberLit (LpInteger i)) = pureExpr (SynNumLit (SynIntLit i))
  fn (LpNumberLit (LpFloat   f)) = pureExpr (SynNumLit (SynFltLit f))
  fn t                           = throwFromHere (PUnexpectedToken t)

symbol :: Monad m => Parser m SynExpr
symbol = (colon *> (SynSymbol <$> synIdentifier) >>= pureExpr) <?> "symbol" where
  colon  = consumeKind LpKindColon

contextual :: Monad m => Parser m SynExpr
contextual = (dot *> (SynContextual <$> synIdentifier) >>= pureExpr) <?> "contextual" where
  dot = consumeKind LpKindDotOperator

reference :: Monad m => Parser m SynExpr
reference = ref mempty <?> "identifier" where
  reduceRef :: Monad m => Seq (Position, SynId) -> Parser m SynExpr
  reduceRef (viewl ->         EmptyL) = throwFromHere PImpossible
  reduceRef (viewl -> (p, x) :< rest) = iter rest (SynExpr (SynReference (RefIdentity x)) p) where
    iter :: Monad m => Seq (Position, SynId) -> SynExpr -> Parser m SynExpr
    iter (viewl ->         EmptyL) acc = pure acc
    iter (viewl -> (p, x) :< rest) acc = iter rest (SynExpr (SynReference (RefProperty acc x)) p)

  ref :: Monad m => Seq (Position, SynId) -> Parser m SynExpr
  ref acc = do
    item <- (,) <$> use position' <*> synIdentifier
    lookAhead consume >>= \case
      LpDotOperator -> consume >> ref (acc |> item)
      LpColon       -> throwFromHere (PUnexpectedToken LpColon)
      _______       -> reduceRef (acc |> item)

newline :: Monad m => Parser m ()
newline = void (consumeKind LpKindNewline) <?> "newline"

endOfLine :: Monad m => Parser m ()
endOfLine = newline <|> void (lookAhead (consumeKind LpKindEnd))

space :: Monad m => Parser m ()
space = void (consumeKind LpKindSpace) <?> "space"

spaces :: Monad m => Parser m ()
spaces = void (some (consumeKind LpKindSpace)) <?> "spaces"

synIdentifier :: Monad m => Parser m SynId
synIdentifier = consume >>= \case
  LpIdentifier text -> pure (SynId text)
  unexpected        -> throwFromHere (PUnexpectedToken unexpected)

--------------------------------------------------------------
--                   Parse Combinators                      --
--------------------------------------------------------------

sepBy1 :: Parser m a -> Parser m b -> Parser m (Seq a)
sepBy1 elem sep = elem >>= \e -> iter (mempty |> e) where
  iter acc = try (sep *> elem) >>= \case
    Just item -> iter (acc |> item)
    Nothing   -> pure acc

sepEndBy1 :: Parser m a -> Parser m b -> Parser m (Seq a)
sepEndBy1 elem sep = (sepBy1 elem sep) <* sep

lookAhead :: Parser m a -> Parser m a
lookAhead p = startRecovery *> p <* restoreRecovery

try :: Parser m a -> Parser m (Maybe a)
try p = do
  startRecovery
  catchError (Just <$> p <* forgetRecovery) $ \e ->
    restoreRecovery >> pure Nothing

try' :: a -> Parser m a -> Parser m a
try' a p = fromMaybe a <$> try p

failKind :: Monad m => LpReprKind -> Parser m ()
failKind k = try (consumeKind k) >>= \case
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
  lbs <- use labels'
  throwError (Error (ParseError err lbs) (Known pos))

consume :: Monad m => Parser m LpRepr
consume = do
  t@(Lp repr _) <- rawAwait
  record t
  pure repr

consumeKind :: Monad m => LpReprKind -> Parser m LpRepr
consumeKind kind = consume >>= \repr ->
  if kind == reprKind repr
    then pure repr
    else throwFromHere (PExpectedToken kind repr)

rawAwait :: Monad m => Parser m LexerPass
rawAwait = liftConduit C.await >>= \case
  Nothing ->
    throwFromHere PUnexpectedEnd
  Just (Left error) ->
    throwError error
  Just (Right item@(Lp _ position)) -> do
    position' .= position
    pure item

--------------------------------------------------------------
--                   Label Specification                    --
--------------------------------------------------------------

infixr 0 <?>

(<?>) :: Parser m a -> Text -> Parser m a
parser <?> description = push *> parser <* pop where
  push = do
    position <- use position'
    labels' %= ((:) Label { position, description })
  pop = do
    labels' %= List.tail

--------------------------------------------------------------
--                     Parse Recovery                       --
--------------------------------------------------------------

record :: LexerPass -> Parser m ()
record pass = use recovery' >>= \case
  RecEnd        -> pure ()
  RecAdd s prev -> recovery' .= RecAdd s (pass:prev)

{- # startRecovery
 -
 - Any calls to await will also be recorded so if a recoverable
 - error takes place any fetched items during parse can be
 - recovered -}
startRecovery :: Parser m ()
startRecovery = do
  state     <- get
  recovery' .= RecAdd state mempty

{- # forgetRecovery
 -
 - Discard the cache of any awaited items since the previous
 - call to `startRecovery` -}
forgetRecovery :: Parser m ()
forgetRecovery = use recovery' >>= \case
  RecEnd      -> pure ()
  RecAdd st _ -> recovery' .= (recovery st)

{- # restoreRecovery
 -
 - Restores all cached lexicons to the previous state -}
restoreRecovery :: Parser m ()
restoreRecovery = use recovery' >>= \case
  RecEnd       -> pure ()
  RecAdd st sv -> put st *> liftConduit (forM_ sv (C.leftover . Right))

instance Alternative (Parser m) where
  empty = do
    pos <- use position'
    lbs <- use labels'
    throwError (Error (ParseError PIncompleteAlt lbs) (Known pos))

  left <|> right = do
    startRecovery
    catchError (left <* forgetRecovery) $ \first ->
      catchError (restoreRecovery *> right) $ \second ->
        throwError (newestError first second)

instance MonadPlus (Parser m)

--------------------------------------------------------------
--                        Wrap func                         --
--------------------------------------------------------------

{- whenever the monad stack is modified we'll only need
 - to update the lifting of conduit function calls here -}
liftConduit :: ParserConduit m a -> Parser m a
liftConduit m = Parser (lift (lift m))

