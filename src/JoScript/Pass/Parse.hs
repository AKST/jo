{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DuplicateRecordFields #-}
module JoScript.Pass.Parse (runParsePass) where

import Prelude (otherwise, (.))
import qualified Prelude as Std

import Data.Eq ((==))
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Conduit as C

import Control.Lens ((%=), (.=), use)
import Control.Monad ((>>=), (>>), Monad, unless)
import Control.Applicative (pure, (<*))
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State as S

import JoScript.Data.Error (Error)
import JoScript.Data.Syntax (SynModule)
import JoScript.Data.Position (Position)
import JoScript.Data.LexerPass (LexerPass(..), LpRepr(..), LpNumber(..))
import qualified JoScript.Data.LexerPass as Lp
import qualified JoScript.Data.Error as Error
import qualified JoScript.Data.Position as Position
import JoScript.Util.Conduit (ConduitE, ResultSink, Result)

data Branch = Init
data State = S { position :: Position }

--------------------------------------------------------------
--                      Entry point                         --
--------------------------------------------------------------

type ParserConduit m = ResultSink LexerPass m
type Parser m = E.ExceptT Error (S.StateT State (ParserConduit m))

runParsePass :: Monad m => ParserConduit m (Result SynModule)
runParsePass =
  let s = E.runExceptT (loop Init)
      i = S { position = Position.init }
   in S.evalStateT s i >>= \case
      Left except -> pure (Left except)

--------------------------------------------------------------
--                      Event loop                          --
--------------------------------------------------------------

loop :: Monad m => Branch -> Parser m ()
loop Init = pure ()

