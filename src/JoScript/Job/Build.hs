{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module JoScript.Job.Build (buildFiles) where

import Protolude

import Conduit (MonadResource, MonadBaseControl)
import Data.Conduit ((.|))
import qualified Data.Conduit as C

import JoScript.Data.Config ( DebugMode(..)
                            , DebugKind(..)
                            , FileBuildM(..)
                            , FileBuildBase
                            , FileBuildConfig(..)
                            , runFileBuildM)

import JoScript.Pass.Block (runBlockPass)
import JoScript.Pass.Lexer (runLexerPass)
import JoScript.Pass.Parse (runParsePass)
import JoScript.Pass.Debug (runDebugPass)
import JoScript.Util.Conduit (characterStream, ResultConduit)
import JoScript.Util.Debug (consumeBlockPass, consumeLexerPass, consumeSyntax, printPass, FileDebug)
import qualified JoScript.Data.Config as Con

buildFiles :: (MonadResource m, MonadBaseControl IO m) => Con.BuildConfig -> m ()
buildFiles (Con.BuildC debug files) = impl where

  impl = forM_ files $ \file ->
    runFileBuildM (FileBC file) (handler debug)

  handler Nothing               = pure ()
  handler (Just (Debug mode p)) = do
    result <- C.runConduitRes (characterStream .| runDebugPass mode)
    printPass p result

