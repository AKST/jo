{-# LANGUAGE OverloadedStrings #-}
module JoScript.Util.StringsTest (tests) where

import Prelude (flip, String, ($))

import Test.HUnit
import JoScript.Util.Strings

tests =
  [ TestLabel "JoScript.Strings.trim" tryTrim
  , TestLabel "JoScript.Strings.trim" trimWithDoubleNL
  ]

tryTrim = TestCase $ do
  let expected = "abc def"
  let resulted = trim "\n  abc\n  def"
  assertEqual "skips indent" expected resulted

trimWithDoubleNL = TestCase $ do
  let expected = "abc \n\ndef"
  let resulted = trim "\n  abc\n\n  def"
  assertEqual "skips indent" expected resulted

testWithStateEmpty = TestCase $ do
  let expected = "abc \n\n"
  let resulted = trim "\n  abc\n\n  "
  assertEqual "test impossible state" expected resulted
