{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module JoScript.Job.Build (build) where

import qualified Conduit as StdCon

import Data.Void
import Data.Bool
import Data.Maybe (Maybe(..))
import Data.Conduit ((=$=), (.|))
import qualified Data.Conduit as C
import qualified Data.Foldable as F
import qualified Data.Aeson as A

import Control.Applicative (pure)
import Control.Monad (forM_, (>>=))

import System.IO (IO, FilePath, print)
import System.Exit (die)
import System.Directory (doesFileExist)

import JoScript.Data.Config (DebugMode(..), DebugKind(..), debugModeText)
import JoScript.Text.BlockPass (runBlockPass)
import JoScript.Text.LexerPass (runLexerPass)
import JoScript.Text.ParsePass (runParsePass)
import JoScript.Util.Conduit (characterStream, printDebug)

build :: Maybe DebugMode -> [FilePath] -> IO ()
build debug files = forM_ files (handler debug) where
  handler :: Maybe DebugMode -> FilePath -> IO ()
  handler Nothing __    = pure ()
  handler (Just m@(Debug mode _)) f = C.runConduitRes (withFile mode) where
    withFile DebugTextBlock = withBlockPass f .| printDebug m
    withFile DebugTextLexer = withLexerPass f .| printDebug m
    withFile DebugTextParse = withParsePass f .| printDebug m

    withBlockPass file = characterStream file .| runBlockPass
    withLexerPass file = withBlockPass file   .| runLexerPass
    withParsePass file = withLexerPass file   .| runParsePass

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


