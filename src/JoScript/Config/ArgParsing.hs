{-# LANGUAGE LambdaCase #-}
module JoScript.Config.ArgParsing (parseArgs, readJob) where

import Prelude (String)

import qualified Options.Applicative.Types as A
import qualified Options.Applicative.Extra as A
import qualified Options.Applicative.Builder as A

import Data.Maybe (Maybe(..))
import Data.Monoid (mempty, (<>))

import Control.Monad ((>>=))
import Control.Applicative ((<$>), (<*>), pure, some)

import System.IO (IO)

import JoScript.Data.Config (Job(..), DebugMode(..), versionStr)

readJob :: IO Job
readJob = A.execParser parseArgs

parseArgs :: A.ParserInfo Job
parseArgs = A.info (A.subparser buildParser) meta where
  meta = A.header "Jo Compiler"
      <> A.progDesc "Jo is intended to be a Typed, Functional, Sane compiler"
      <> A.failureCode 1

  buildParser = A.command "build" info where
    info = A.info parseBuildMode (A.fullDesc <> header <> failure) where
      failure = A.failureCode 1
      header  = A.header "Builds a list of supplied main files"

parseBuildMode :: A.Parser Job
parseBuildMode = JobBuild <$> debug <*> files where
  files = some (A.argument A.str meta) where
    meta = A.help ("A list of main files") <> A.metavar "MAINS"

  debug = A.option reader meta where

    reader = A.str >>= \case
      "text:block" -> pure (Just DebugTextBlock)
      "text:lexer" -> pure (Just DebugTextLexer)
      "text:parse" -> pure (Just DebugTextParse)
      ____________ -> A.readerError "invalid debug mode"

    meta = A.short 'd' <> A.long "debug" <> help <> def <> var where
      var  = A.metavar "DEBUG_MODE"
      def  = A.value Nothing
      help = A.help ( "optional flag for debugging, possible options"
                   <> "include: 'text:block', 'text:lexer', 'text:parse'")

