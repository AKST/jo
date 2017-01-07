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
  [ TestLabel "JoScript.Pass.Parse (numbers)" parseInteger
  ]

--------------------------------------------------------------
--                         Numbers                          --
--------------------------------------------------------------

parseInteger = TestCase $ do
  let tokens = [ LpNumberLit (LpInteger 20)
               , LpEnd
               ]
  mod <- runParse tokens >>= withSuccess
  assertEqual "module result" (exprs mod)
    [ invokeExpr (SynNumLit (SynIntLit 20)) ]

parseFloat = TestCase $ do
  let tokens = [ LpNumberLit (LpFloat 420.6911)
               , LpEnd
               ]
  mod <- runParse tokens >>= withSuccess
  assertEqual "module result" (exprs mod)
    [ invokeExpr (SynNumLit (SynFltLit 420.6911)) ]

--------------------------------------------------------------
--                         Utility                          --
--------------------------------------------------------------

invokeExpr e = ex (SynInvokation (ex e) defApply)

defApply = SynParamsApp mempty Nothing mempty

ex r = SynExpr r Position.init

withSuccess :: Show b => Either b a -> IO a
withSuccess (Left  b) = assertFailure (ppShow b) >> undefined
withSuccess (Right a) = pure a

runParse :: [LpRepr] -> IO (Either Error SynModule)
runParse ts = runFileBuildM config (runConduitRes conduit) where
  conduit = source .| runParsePass
  config = FileBC "test"
  source = sourceList (fmap (\x -> Right (Lp x Position.init)) ts)

exprs :: SynModule -> Seq SynExpr
exprs (SynModule { statements }) = statements
