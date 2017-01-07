{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedLists #-}
module JoScript.Pass.ParseTest (tests) where

import Protolude

import Data.Conduit
import Data.Conduit.List (sourceList)
import qualified Data.Map.Strict as Map

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
  [ TestLabel "JoScript.Pass.Parse (integers)"                parseInteger
  , TestLabel "JoScript.Pass.Parse (floats)"                  parseFloat
  , TestLabel "JoScript.Pass.Parse (identifier quote)"        parseIdentifierQuote
  , TestLabel "JoScript.Pass.Parse (symbol)"                  parseSymbol
  , TestLabel "JoScript.Pass.Parse (application pos)"         parseAppPos
  , TestLabel "JoScript.Pass.Parse (application pos rest)"    parseAppPosRest
  , TestLabel "JoScript.Pass.Parse (application pos kw)"      parseAppPosKeywords
  , TestLabel "JoScript.Pass.Parse (application pos rest kw)" parseAppPosRestKeywords
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
--                   function application                   --
--------------------------------------------------------------

-- add 2 2
parseAppPos = TestCase $ do
  mod <- getModule
    [ LpIdentifier "add"
    , LpSpace 1
    , LpInteger 2
    , LpSpace 1
    , LpInteger 2
    , LpEnd ]
  assertEqual "module result" (statements mod)
    [ invoke (idExpr "add") [SynIntLit 2, SynIntLit 2] Nothing mempty ]

-- add 2 *numbers
parseAppPosRest = TestCase $ do
  mod <- getModule
    [ LpIdentifier "add"
    , LpSpace 1
    , LpInteger 2
    , LpSpace 1
    , LpRestOperator
    , LpIdentifier "numbers"
    , LpEnd ]
  assertEqual "module result" (statements mod)
    [ invoke (idExpr "add") [SynIntLit 2] (Just (idExpr "numbers")) mempty ]

-- add 2 overflow: true
parseAppPosKeywords = TestCase $ do
  mod <- getModule
    [ LpIdentifier "add"
    , LpSpace 1
    , LpInteger 2
    , LpSpace 1
    , LpIdentifier "overflow"
    , LpColon
    , LpSpace 1
    , LpIdentifier "true"
    , LpEnd ]
  assertEqual "module result" (statements mod)
    [ invoke (idExpr "add") [SynIntLit 2] Nothing [("overflow", idExpr "true")] ]

-- add 2 *numbers overflow: true
parseAppPosRestKeywords = TestCase $ do
  mod <- getModule
    [ LpIdentifier "add"
    , LpSpace 1
    , LpInteger 2
    , LpSpace 1
    , LpRestOperator
    , LpIdentifier "numbers"
    , LpSpace 1
    , LpIdentifier "overflow"
    , LpColon
    , LpSpace 1
    , LpIdentifier "true"
    , LpEnd ]
  assertEqual "module result" (statements mod)
    [ invoke (idExpr "add") [SynIntLit 2] (Just (idExpr "numbers")) [("overflow", idExpr "true")] ]

--------------------------------------------------------------
--                         Numbers                          --
--------------------------------------------------------------

parseInteger = TestCase $ do
  mod <- getModule
    [ LpInteger 20
    , LpEnd ]
  assertEqual "module result" (statements mod)
    [ invokeExpr (SynIntLit 20) ]

parseFloat = TestCase $ do
  mod <- getModule
    [ LpFloat 420.6911
    , LpEnd ]
  assertEqual "module result" (statements mod)
    [ invokeExpr (SynFltLit 420.6911) ]

--------------------------------------------------------------
--                     expression dsl                       --
--------------------------------------------------------------

invoke :: SynExpr -> Seq SynExprRepr -> Maybe SynExpr -> [(Text, SynExpr)] -> SynExpr
invoke fn args rest keywords = expr' (SynInvokation fn apply)
  where toExpr (label, ex) = (SynId label, ex)
        apply = SynParamsApp (fmap expr' args)
                             rest
                             (Map.fromList (fmap toExpr keywords))


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

