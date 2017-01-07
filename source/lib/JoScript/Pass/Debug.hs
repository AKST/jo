module JoScript.Pass.Debug (runDebugPass) where

import Protolude
import Data.Conduit ((.|))

import JoScript.Data.Config ( DebugMode(..)
                            , DebugKind(..)
                            , FileBuildM(..)
                            , FileBuildBase
                            , FileBuildConfig(..)
                            , runFileBuildM)

import JoScript.Data.Debug (FileDebug)
import JoScript.Data.Block (BlockPass)
import JoScript.Data.Lexer (LexerPass)
import JoScript.Data.Syntax (SynModule)

import JoScript.Pass.Block (runBlockPass)
import JoScript.Pass.Lexer (runLexerPass)
import JoScript.Pass.Parse (runParsePass)
import JoScript.Util.Conduit (characterStream, ResultSink, ResultConduit, Result)
import JoScript.Util.Debug (consumeBlockPass, consumeLexerPass, consumeSyntax)

runDebugPass :: Monad m => DebugKind -> ResultSink Char m FileDebug
runDebugPass DebugTextBlock = withBlockPass .| consumeBlockPass
runDebugPass DebugTextLexer = withLexerPass .| consumeLexerPass
runDebugPass DebugTextParse = withParsePass >>= lift . lift . consumeSyntax

withParsePass :: Monad m => ResultSink Char m (Result SynModule)
withParsePass = withLexerPass .| runParsePass

withLexerPass :: Monad m => ResultConduit Char LexerPass m ()
withLexerPass = withBlockPass .| runLexerPass

withBlockPass :: Monad m => ResultConduit Char BlockPass m ()
withBlockPass = runBlockPass

