{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module JoScript.Data.Config where

import Protolude hiding (get)
import Conduit (MonadResource, MonadThrow, MonadBase)

import Control.Monad.Trans (MonadTrans)
import Control.Monad.Trans.Control ( MonadBaseControl(..)
                                   , MonadTransControl(..)
                                   , ComposeSt
                                   , defaultLiftBaseWith
                                   , defaultRestoreM
                                   , defaultRestoreT
                                   , defaultLiftWith
                                   )

data BuildConfig = BuildC { debug :: Maybe DebugMode, files :: [FilePath] }
  deriving (Show)

data FileBuildConfig = FileBC { filename :: FilePath }
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

type FileBuildBase m = (MonadIO m, MonadThrow m, MonadBaseControl IO m, MonadResource m)

newtype FileBuildM m a = FileBuildM { get :: ReaderT FileBuildConfig m a }
  deriving
    ( Functor
    , Applicative
    , Monad
    , MonadReader FileBuildConfig
    , MonadTrans
    , MonadIO
    )

runFileBuildM :: FileBuildConfig -> FileBuildM m a -> m a
runFileBuildM r m = runReaderT (get m) r

deriving instance MonadBase IO m => MonadBase IO (FileBuildM m)
deriving instance MonadThrow m => MonadThrow (FileBuildM m)
deriving instance MonadResource m => MonadResource (FileBuildM m)

instance MonadTransControl FileBuildM where
  type StT FileBuildM a = StT (ReaderT FileBuildConfig) a
  liftWith = defaultLiftWith FileBuildM get
  restoreT = defaultRestoreT FileBuildM

instance MonadBaseControl IO m => MonadBaseControl IO (FileBuildM m) where
  type StM (FileBuildM m) a = ComposeSt FileBuildM m a
  liftBaseWith = defaultLiftBaseWith
  restoreM     = defaultRestoreM

--------------------------------------------------------------
--                          Lens                            --
--------------------------------------------------------------

debug' :: Functor f => (Maybe DebugMode -> f (Maybe DebugMode)) -> BuildConfig -> f BuildConfig
debug' fn (BuildC d f) = fmap (\d' -> BuildC d' f) (fn d)

files' :: Functor f => ([FilePath] -> f [FilePath]) -> BuildConfig -> f BuildConfig
files' fn (BuildC d f) = fmap (\f' -> BuildC d f') (fn f)

filename' :: Functor f => (FilePath -> f FilePath) -> FileBuildConfig -> f FileBuildConfig
filename' fn (FileBC f) = fmap (\f' -> FileBC f') (fn f)

--------------------------------------------------------------
--                        Util func                         --
--------------------------------------------------------------

debugModeText :: DebugKind -> Text
debugModeText DebugTextBlock = "debug:block"
debugModeText DebugTextLexer = "debug:lexer"
debugModeText DebugTextParse = "debug:parse"

versionStr :: Text
versionStr = "0.0.1"

