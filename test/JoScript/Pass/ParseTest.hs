{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module JoScript.Pass.ParseTest (tests) where

import Protolude

import Data.Conduit
import Data.Conduit.List (sourceList)

import Text.Show.Pretty (ppShow)
import Test.HUnit

import JoScript.Data.Lexer
import JoScript.Data.Syntax
import JoScript.Data.Error
import JoScript.Data.Config (FileBuildConfig(FileBC), runFileBuildM)
import qualified JoScript.Data.Position as Position
import JoScript.Pass.Parse (runParsePass)

tests :: [Test]
tests =
  [ TestLabel "JoScript.Pass.Parse (integers)"         parseInteger
  , TestLabel "JoScript.Pass.Parse (floats)"           parseFloat
  , TestLabel "JoScript.Pass.Parse (identifier quote)" parseIdentifierQuote
  , TestLabel "JoScript.Pass.Parse (symbol)"           parseSymbol
  ]

--------------------------------------------------------------
--                        Symbols                           --
--------------------------------------------------------------

parseSymbol = TestCase $ do
  mod <- getModule
    [ LpColon
    , LpIdentifier "abc"
    , LpEnd ]
  assertEqual "module result" (statements mod)
    [ invokeExpr (SynSymbol (SynId "abc")) ]

--------------------------------------------------------------
--                         Quotes                           --
--------------------------------------------------------------

parseIdentifierQuote = TestCase $ do
  mod <- getModule
    [ LpQuote
    , LpIdentifier "abc"
    , LpEnd ]
  assertEqual "module result" (statements mod)
    [ invokeExpr (SynQuote (idExpr "abc")) ]

--------------------------------------------------------------
--                         Numbers                          --
--------------------------------------------------------------

parseInteger = TestCase $ do
  mod <- getModule
    [ LpNumberLit (LpInteger 20)
    , LpEnd ]
  assertEqual "module result" (statements mod)
    [ invokeExpr (SynNumLit (SynIntLit 20)) ]

parseFloat = TestCase $ do
  mod <- getModule
    [ LpNumberLit (LpFloat 420.6911)
    , LpEnd ]
  assertEqual "module result" (statements mod)
    [ invokeExpr (SynNumLit (SynFltLit 420.6911)) ]

--------------------------------------------------------------
--                     expression dsl                       --
--------------------------------------------------------------

invokeExpr e = expr' (SynInvokation (expr' e) defApply)

defApply = SynParamsApp mempty Nothing mempty

idExpr = expr' . SynReference . RefIdentity . SynId

expr' r = SynExpr r Position.init

--------------------------------------------------------------
--                         Utility                          --
--------------------------------------------------------------

getModule :: [LpRepr] -> IO SynModule
getModule tokens = runParse tokens >>= withSuccess where

  withSuccess :: Show b => Either b a -> IO a
  withSuccess (Left  b) = assertFailure (ppShow b) >> undefined
  withSuccess (Right a) = pure a

  runParse :: [LpRepr] -> IO (Either Error SynModule)
  runParse ts = runFileBuildM config (runConduitRes conduit) where
    conduit = source .| runParsePass
    config = FileBC "test"
    source = sourceList (fmap (\x -> Right (Lp x Position.init)) ts)

