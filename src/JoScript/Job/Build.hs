{-# LANGUAGE LambdaCase #-}
module JoScript.Job.Build (build) where

import qualified Conduit as StdCon

import Data.Void
import Data.Bool
import Data.Maybe (Maybe(..))
import Data.Conduit ((=$=), (.|))
import qualified Data.Conduit as C
import qualified Data.Foldable as F

import Control.Applicative (pure)
import Control.Monad (forM_, (>>=))

import System.IO (IO, FilePath, print)
import System.Exit (die)
import System.Directory (doesFileExist)

import JoScript.Data.Config (DebugMode(..))
import JoScript.Text.BlockPass (runBlockPass)
import JoScript.Util.Conduit (characterStream, printAsJSON)

build :: Maybe DebugMode -> [FilePath] -> IO ()
build debug files = forM_ files (handler debug) where
  handler :: Maybe DebugMode -> FilePath -> IO ()
  handler Nothing __    = pure ()
  handler (Just mode) f = C.runConduitRes (withFile mode) where
    withFile DebugTextBlock = withBlockPass f .| printAsJSON

  withBlockPass file = characterStream file .| runBlockPass

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


