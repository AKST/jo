module JoScript.Job.Build (build) where

import Data.Bool
import Data.Maybe (Maybe(..))
import qualified Data.Foldable as F

import Control.Applicative (pure)
import Control.Monad (forM_, (>>=))

import System.IO (IO, FilePath, print)
import System.Directory (doesFileExist)

import JoScript.Data.Config (DebugMode(..))

build :: Maybe DebugMode -> [FilePath] -> IO ()
build debug files = do
  confirmation <- allFilesExist files
  print (if confirmation then "yay" else "boo")

allFilesExist files = F.foldlM f True files where
  f True file = doesFileExist file
  f False ___ = pure False

