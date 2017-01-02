{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module JoScript.Job.Build (buildFiles) where

import Protolude

import Data.Conduit ((.|))
import qualified Data.Conduit as C

import JoScript.Data.Config (DebugMode(..), DebugKind(..))
import JoScript.Pass.Block (runBlockPass)
import JoScript.Pass.Lexer (runLexerPass)
import JoScript.Pass.Parse (runParsePass)
import JoScript.Util.Conduit (characterStream)
import JoScript.Util.Debug (consumeBlockPass, consumeLexerPass, consumeSyntax, printPass)
import qualified JoScript.Data.Config as Con

buildFiles :: Con.BuildConfig -> IO ()
buildFiles (Con.BuildC debug files) = impl where

  impl = do
    forM_ files (handler debug)

  handler Nothing               _ = pure ()
  handler (Just (Debug mode p)) f = debugPass mode >>= printPass p where
      debugPass DebugTextBlock = C.runConduitRes (withBlockPass .| consumeBlockPass f)
      debugPass DebugTextLexer = C.runConduitRes (withLexerPass .| consumeLexerPass f)
      debugPass DebugTextParse = C.runConduitRes withParsePass >>= consumeSyntax f

      withBlockPass = characterStream f .| runBlockPass
      withLexerPass = withBlockPass .| runLexerPass
      withParsePass = withLexerPass .| runParsePass

