{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module JoScript.Job.Build (buildFiles) where

import Protolude

import Data.Conduit ((.|))
import qualified Data.Conduit as C
import qualified Data.Aeson as A

import System.Directory (doesFileExist)

import JoScript.Data.Config (DebugMode(..), DebugKind(..), debugModeText, FileBuildM(..))
import JoScript.Data.Syntax (SynModule)
import JoScript.Pass.Block (runBlockPass)
import JoScript.Pass.Lexer (runLexerPass)
import JoScript.Pass.Parse (runParsePass)
import JoScript.Util.Conduit (characterStream, printJsonCon, Result)
import JoScript.Util.Debug (consumeBlockPass, consumeLexerPass, consumeSyntax, printPass)
import qualified JoScript.Data.Config as Con

buildFiles :: Con.BuildConfig -> IO ()
buildFiles b@(Con.BuildC debug files) = impl where

  impl = do
    forM_ files (handler debug)

  handler Nothing                 f = pure ()
  handler (Just m@(Debug mode p)) f = pass mode >>= printPass p where
      pass DebugTextBlock = C.runConduitRes (withBlockPass .| consumeBlockPass f)
      pass DebugTextLexer = C.runConduitRes (withLexerPass .| consumeLexerPass f)
      pass DebugTextParse = C.runConduitRes withParsePass >>= consumeSyntax f

      withBlockPass = characterStream f .| runBlockPass
      withLexerPass = withBlockPass .| runLexerPass
      withParsePass = withLexerPass .| runParsePass

