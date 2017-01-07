{-# LANGUAGE OverloadedStrings #-}
module JoScript.Util.TextTest (tests) where

import Prelude (flip, String, ($))

import Data.Word
import Test.HUnit
import Control.Applicative (pure, (<$>), (<*>))
import JoScript.Util.Text

tests =
  [ TestLabel "JoScript.Util.Text.foldlM" tryFoldM
  , TestLabel "JoScript.Util.Text.readInt" tryReadInt
  , TestLabel "JoScript.Util.Text.readFloat" tryReadFloat
  ]

tryFoldM = TestCase $ do
  let expected = "olleh" :: String
  let mFold a b = pure (b : a)
  resulted <- foldlM mFold "" "hello"

  -- $ foldl (flip (:)) [] "hello"
  -- > "olleh"
  assertEqual "reverses the string" expected resulted

tryReadInt = TestCase $ do
  assertEqual "read int works" (readInt "123") 123

tryReadFloat = TestCase $ do
  assertEqual "read int works" (readFloat "123.234") 123.234

