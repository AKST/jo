module JoScript.Pass.Parse (runParsePass) where

import Protolude hiding (State)

import qualified Data.Conduit as C

import Control.Lens ((%=), (.=), use)

import JoScript.Util.Conduit (ConduitE, ResultSink, Result)
import JoScript.Data.Error (Error)
import JoScript.Data.Syntax (SynModule)
import JoScript.Data.Position (Position)
import JoScript.Data.Lexer (LexerPass(..), LpRepr(..), LpNumber(..))
import qualified JoScript.Data.Lexer as Lp
import qualified JoScript.Data.Error as Error
import qualified JoScript.Data.Position as Position


data Branch = Init
data State = S { position :: Position }

--------------------------------------------------------------
--                      Entry point                         --
--------------------------------------------------------------

type ParserConduit m = ResultSink LexerPass m
type Parser m = ExceptT Error (StateT State (ParserConduit m))

runParsePass :: Monad m => ParserConduit m (Result SynModule)
runParsePass =
  let s = runExceptT (loop Init)
      i = S { position = Position.init }
   in evalStateT s i >>= \case
      Left except -> pure (Left except)

--------------------------------------------------------------
--                      Event loop                          --
--------------------------------------------------------------

loop :: Monad m => Branch -> Parser m ()
loop Init = pure ()

