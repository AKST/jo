module JoScript.Pass.Parse (runParsePass) where

import Prelude (error)
import Protolude hiding (State, error)

import qualified Data.Conduit as C

import Control.Lens ((.=), (%=), use)

import JoScript.Util.Conduit (ResultSink, Result)
import JoScript.Data.Error (Error, known, Repr(ParseError), ParseErrorT(..))
import JoScript.Data.Syntax (SynModule)
import JoScript.Data.Position (Position)
import JoScript.Data.Lexer (LexerPass(..), LpRepr(..))
import qualified JoScript.Data.Position as Position


data Recovery
  = RecEnd
  | RecAdd State [LexerPass]

data Label = Label { position :: Position, description :: Text }
type Labels = [Label]

data State = S { recovery :: Recovery, labels :: Labels, position :: Position }

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

newtype Parser m a   = Parser { run :: ExceptT Error (StateT State (ParserConduit m)) a }
  deriving (Functor, Applicative, Monad, MonadError Error, MonadState State)

runParsePass :: Monad m => ParserConduit m (Result SynModule)
runParsePass =
  let s = runExceptT (run root)
      i = S { recovery = RecEnd
            , labels   = mempty
            , position = Position.init }
   in evalStateT s i >>= \case
      Right _____ -> error "not yet complete"
      Left except -> pure (Left except)

--------------------------------------------------------------
--                      Event loop                          --
--------------------------------------------------------------

root :: Monad m => Parser m SynModule
root = undefined

--------------------------------------------------------------
--                        Util func                         --
--------------------------------------------------------------


--------------------------------------------------------------
--                     Parse Recovery                       --
--------------------------------------------------------------

{- # activateRecovery
 -
 - Any calls to await will also be recorded so if a recoverable
 - error takes place any fetched items during parse can be
 - recovered -}
activateRecovery :: Monad m => Parser m ()
activateRecovery = do
  state     <- get
  recovery' .= RecAdd state mempty

{- # forgetRecovery
 -
 - Discard the cache of any awaited items since the previous
 - call to `activateRecovery` -}
forgetRecovery :: Monad m => Parser m ()
forgetRecovery = use recovery' >>= \case
  RecEnd      -> pure ()
  RecAdd st _ -> recovery' .= (recovery st)

{- # applyRecovery
 -
 - Restores all cached lexicons to the previous state -}
applyRecovery :: Monad m => Parser m ()
applyRecovery = use recovery' >>= \case
  RecEnd       -> pure ()
  RecAdd st sv -> put st *> liftConduit (forM_ sv (C.leftover . Right))

instance Monad m => Alternative (Parser m) where
  empty = do
    pos <- use position'
    throwError (known (ParseError PIncompleteAlt) pos)

  left <|> right = do
    activateRecovery
    catchError (left <* forgetRecovery)
      (\_ -> applyRecovery *> right)

--------------------------------------------------------------
--                        Wrap func                         --
--------------------------------------------------------------

{- whenever the monad stack is modified we'll only need
 - to update the lifting of conduit function calls here -}
liftConduit :: Monad m => ParserConduit m a -> Parser m a
liftConduit m = Parser (lift (lift m))

