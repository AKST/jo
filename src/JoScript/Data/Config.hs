{-# LANGUAGE OverloadedStrings #-}
module JoScript.Data.Config where

import Protolude hiding (get)

import Control.Monad.Trans (MonadTrans)

data BuildConfig = BuildC { debug :: Maybe DebugMode, files :: [FilePath] }
  deriving (Show)

data FileBuildConfig = FileBC { filename :: FilePath, build :: BuildConfig }
  deriving (Show)

data Job = JobBuild BuildConfig
  deriving (Show)

data DebugMode = Debug { mode :: DebugKind, pretty :: Bool }
  deriving (Show)

data DebugKind
  = DebugTextBlock
  | DebugTextLexer
  | DebugTextParse
  deriving (Show)

newtype FileBuildM m a = FileBuildM { get :: ReaderT FileBuildConfig m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader FileBuildConfig
    , MonadTrans
    , MonadIO
    )

runFileBuildM r m = runReaderT r (get m)

--------------------------------------------------------------
--                          Lens                            --
--------------------------------------------------------------

debug' fn (BuildC d f) = fmap (\d' -> BuildC d' f) (fn d)

files' fn (BuildC d f) = fmap (\f' -> BuildC d f') (fn f)

build' fn (FileBC f b) = fmap (\b' -> FileBC f b') (fn b)

filename' fn (FileBC f b) = fmap (\f' -> FileBC f' b) (fn f)

--------------------------------------------------------------
--                        Util func                         --
--------------------------------------------------------------

debugModeText :: DebugKind -> Text
debugModeText DebugTextBlock = "debug:block"
debugModeText DebugTextLexer = "debug:lexer"
debugModeText DebugTextParse = "debug:parse"

versionStr :: Text
versionStr = "0.0.1"

