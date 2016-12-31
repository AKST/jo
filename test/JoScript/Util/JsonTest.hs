{-# LANGUAGE OverloadedStrings #-}
module JoScript.Util.JsonTest (tests) where

import Prelude (flip, String, ($), Int)

import Test.HUnit
import JoScript.Util.Json
import Data.Aeson hiding (withObject)

tests = [ TestLabel "JoScript.Util.JsonTest.withObject" withObjectUniques
        , TestLabel "JoScript.Util.JsonTest.withObject" withObjectDups
        ]

withObjectUniques = TestCase $ do
  let (a, b) = (1 :: Int, 2 :: Int)
  let expected = object ["a" .= a, "b" .= b]
  let resulted = withObject ["a" .= a] (object ["b" .= b])
  assertEqual "withObject with no duplicated" expected resulted

withObjectDups = TestCase $ do
  let (a, b) = (1 :: Int, 2 :: Int)
  let expected = object ["a" .= b]
  let resulted = withObject ["a" .= b] (object ["a" .= a])
  assertEqual "withObject's left should replace values on the right" expected resulted

