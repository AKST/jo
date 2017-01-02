module JoScript.Pass.Parse (runParsePass) where

import Prelude (error)
import Protolude hiding (State, error)

import qualified Data.Conduit as C

import Control.Lens ((.=), use)

import JoScript.Util.Conduit (ResultSink, Result)
import JoScript.Data.Error (Error, known, Repr(ParseError), ParseErrorT(..))
import JoScript.Data.Syntax (SynModule)
import JoScript.Data.Position (Position)
import JoScript.Data.Lexer (LexerPass(..), LpRepr(..))
import qualified JoScript.Data.Position as Position


data Branch = BrInit | BrFrom LpRepr
data State = S { position :: Position }

--------------------------------------------------------------
--                          Lens                            --
--------------------------------------------------------------

position' :: Functor f => (Position -> f Position) -> State -> f State
position' f (S p) = fmap (\p' -> S  p') (f p)

--------------------------------------------------------------
--                      Entry point                         --
--------------------------------------------------------------

type ParserConduit m = ResultSink LexerPass m

newtype Parser m a   = Parser { run :: ExceptT Error (StateT State (ParserConduit m)) a }
  deriving (Functor, Applicative, Monad, MonadError Error, MonadState State)

runParsePass :: Monad m => ParserConduit m (Result SynModule)
runParsePass =
  let s = runExceptT (run (loop BrInit))
      i = S { position = Position.init }
   in evalStateT s i >>= \case
      Right _____ -> error "not yet complete"
      Left except -> pure (Left except)

--------------------------------------------------------------
--                      Event loop                          --
--------------------------------------------------------------

loop :: Monad m => Branch -> Parser m ()
loop BrInit = readUpdate >>= \_ -> pure ()
loop (BrFrom _) = pure ()

--------------------------------------------------------------
--                        Util func                         --
--------------------------------------------------------------

readUpdate :: Monad m => Parser m (Position, LpRepr)
readUpdate = liftConduit C.await >>= \case
  Nothing -> do
    position <- use position'
    throwError (known (ParseError PUnexpectedEnd) position)

  Just (Right (Lp repr p)) -> do
    position' .= p
    pure (p, repr)

  Just (Left except) -> throwError except

--------------------------------------------------------------
--                        Wrap func                         --
--------------------------------------------------------------

{- whenever the monad stack is modified we'll only need
 - to update the lifting of conduit function calls here -}
liftConduit :: Monad m => ParserConduit m a -> Parser m a
liftConduit m = Parser (lift (lift m))

