{-# LANGUAGE OverloadedStrings #-}
module JoScript.Data.Config where

import Prelude (Show)

import Data.Maybe (Maybe)
import Data.Text (Text)

import System.IO (FilePath)

data Job
  = JobBuild (Maybe DebugMode) [FilePath]
  deriving (Show)

data DebugMode
  = DebugTextBlock
  | DebugTextLexer
  | DebugTextParse
  deriving (Show)

debugModeText :: DebugMode -> Text
debugModeText DebugTextBlock = "debug:block"
debugModeText DebugTextLexer = "debug:lexer"
debugModeText DebugTextParse = "debug:parse"

versionStr :: Text
versionStr = "0.0.1"


