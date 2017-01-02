{-# LANGUAGE QuasiQuotes #-}
module JoScript.Config.ArgParsing (parseArgs, readJob) where

import Protolude hiding ((<>))

import Text.PrettyPrint.ANSI.Leijen (Doc)
import qualified Text.PrettyPrint.ANSI.Leijen as Le

import qualified Options.Applicative.Types as A
import qualified Options.Applicative.Extra as A
import qualified Options.Applicative.Builder as A

import Data.Monoid ((<>))

import JoScript.Util.Strings (multiline, toDocument)
import JoScript.Data.Config (Job(..), DebugMode(..), DebugKind(..), versionStr, BuildConfig(..))

readJob :: IO Job
readJob = A.execParser parseArgs

parseArgs :: A.ParserInfo Job
parseArgs = A.info (A.subparser buildParser) meta where
  meta = A.headerDoc (Just mainHeader)
      <> A.fullDesc
      <> A.progDescDoc (Just mainDescription)
      <> A.failureCode 1

  buildParser = A.command "build" info where
    info = A.info parseBuildMode (A.fullDesc <> header <> failure) where
      failure = A.failureCode 1
      header  = A.header "Builds a list of supplied main files"

parseBuildMode :: A.Parser Job
parseBuildMode = (\d f -> JobBuild (BuildC d f)) <$> debug <*> files where
  files = some (A.argument A.str meta) where
    meta = A.help ("A list of main files") <> A.metavar "MAINS"

  debug = (\m p -> Just (Debug m p)) <$> debugMode <*> pretty
      <|> pure Nothing

  pretty = A.flag False True meta where
    meta = A.long "pretty"

  debugMode = A.option reader meta where

    reader = A.str >>= \case
      "text:block"        -> pure (DebugTextBlock)
      "text:lexer"        -> pure (DebugTextLexer)
      "text:parse"        -> pure (DebugTextParse)
      ____________ -> A.readerError "invalid debug mode"

    meta = A.short 'd' <> A.long "debug" <> help <> var where
      var  = A.metavar "DEBUG_MODE"
      help = A.help ( "optional flag for debugging, possible options"
                   <> "include: 'text:block', 'text:lexer', 'text:parse'")

--------------------------------------------------------------
--                          TEXT                            --
--------------------------------------------------------------

mainHeader :: Doc
mainHeader = Le.text "Jo, aka Jolang Joscript" <> Le.linebreak
  <> Le.nest 2 (Le.text "A safe yet practical functional lanaguage")

mainDescription :: Doc
mainDescription = toDocument description where
  description = [multiline|
    Jo is a safe yet practical functional language, focusing on
    simplistity while not compromising on expressive power. Top
    level commands:

      - `build' [...main files]

    |]


