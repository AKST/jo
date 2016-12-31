{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PartialTypeSignatures #-}
module JoScript.Job.Build (buildFiles) where

import qualified Conduit as StdCon

import Data.Void
import Data.Bool
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.Conduit ((=$=), (.|))
import qualified Data.Conduit as C
import qualified Data.Foldable as F
import qualified Data.Aeson as A

import Control.Applicative (pure)
import Control.Monad (forM_, (>>=))
import Control.Monad.IO.Class (MonadIO)
import Control.Lens (view)

import System.IO (IO, FilePath, print)
import System.Exit (die)
import System.Directory (doesFileExist)

import JoScript.Data.Config (DebugMode(..), DebugKind(..), debugModeText, FileBuildM(..))
import JoScript.Data.Syntax (SynModule)
import JoScript.Pass.Block (runBlockPass)
import JoScript.Pass.Lexer (runLexerPass)
import JoScript.Pass.Parse (runParsePass)
import JoScript.Util.Conduit (characterStream, printJsonCon, Result)
import qualified JoScript.Data.Config as Con

buildFiles :: Con.BuildConfig -> IO ()
buildFiles b@(Con.BuildC debug files) = forM_ files (handler debug) where

  handler Nothing                 f = pure ()
  handler (Just m@(Debug mode _)) f = withFile mode where
      withFile DebugTextBlock = C.runConduitRes (withBlockPass .| printJsonCon m)
      withFile DebugTextLexer = C.runConduitRes (withLexerPass .| printJsonCon m)
      withFile DebugTextParse = C.runConduitRes (withParsePass) >>= printJsonSyntax f m

      withBlockPass = characterStream f .| runBlockPass
      withLexerPass = withBlockPass .| runLexerPass
      withParsePass = withLexerPass .| runParsePass

checkFilesExist :: [FilePath] -> IO ()
checkFilesExist files = result where
  result = fold >>= \case
    Nothing -> pure ()
    Just f  -> die "not all mains exist"

  fold = F.foldlM reducer Nothing files

  reducer :: Maybe FilePath -> FilePath -> IO (Maybe FilePath)
  reducer (Just f) ___ = pure (Just f)
  reducer Nothing file = do
    s <- doesFileExist file
    pure (if s then Nothing else Just file)

printJsonSyntax :: FilePath -> DebugMode -> Result SynModule -> IO ()
printJsonSyntax f (Debug _ p) = \case
  Left error -> pure ()
  Right expr -> pure ()
