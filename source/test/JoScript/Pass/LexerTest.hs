{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module JoScript.Pass.LexerTest (tests) where

import Protolude

import Data.Conduit
import Data.Conduit.List (sourceList)
import qualified Data.Sequence as Seq

import Text.Show.Pretty (ppShow)
import Test.HUnit

import JoScript.Data.Block
import JoScript.Data.Debug
import JoScript.Data.Lexer
import JoScript.Util.Debug (consumeLexerPass)
import JoScript.Data.Config (FileBuildConfig(FileBC), runFileBuildM)
import JoScript.Pass.Lexer (runLexerPass)
import qualified JoScript.Data.Position as Position

tests :: [Test]
tests =
  [ TestLabel "JoScript.Pass.Lexer (integers)"        lexerUnsignedInteger
  , TestLabel "JoScript.Pass.Lexer (floats)"          lexerUnsignedFloat
  , TestLabel "JoScript.Pass.Lexer (signed integers)" lexerSignedInteger
  , TestLabel "JoScript.Pass.Lexer (signed floats)"   lexerSignedFloat
  ]

--------------------------------------------------------------
--                        Numbers                           --
--------------------------------------------------------------

lexerUnsignedInteger = TestCase $ do
  tokens <- getLexerReprs [ BpLine "123" ]
  assertEqual "line of int should equal" tokens [ LpInteger 123 ]

lexerUnsignedFloat = TestCase $ do
  tokens <- getLexerReprs [ BpLine "69.11" ]
  assertEqual "line of int should equal" tokens [ LpFloat 69.11 ]

lexerSignedInteger = TestCase $ do
  tokens <- getLexerReprs [ BpLine "-123" ]
  assertEqual "line of int should equal" tokens [ LpInteger (-123) ]

lexerSignedFloat = TestCase $ do
  tokens <- getLexerReprs [ BpLine "-69.11" ]
  assertEqual "line of int should equal" tokens [ LpFloat (-69.11) ]

--------------------------------------------------------------
--                         Utility                          --
--------------------------------------------------------------

getLexerReprs :: [BpRepr] -> IO (Seq LpRepr)
getLexerReprs tokens = extract <$> (runParse tokens >>= withSuccess) where

  -- drops the trailing new line for each result
  extract = fmap (\(Lp repr _) -> repr)
          . (\items -> Seq.take ((Seq.length items) - 2) items)

  withSuccess :: FileDebug -> IO (Seq LexerPass)
  withSuccess (FileDebug { output = PDebugLexer p, error = Nothing }) = pure p
  withSuccess (FileDebug {                         error = Just er }) = assertFailure (ppShow er) >> undefined
  withSuccess (FileDebug { output = _____________, error = Nothing }) = assertFailure message >> undefined
    where message = "unexpected pass debug target"

  runParse :: [BpRepr] -> IO FileDebug
  runParse ts = runFileBuildM config (runConduitRes conduit) where
    conduit = source .| runLexerPass .| consumeLexerPass
    config = FileBC "test"
    source = sourceList (fmap (\x -> Right (Bp x Position.init)) (ts <> [BpEnd]))



