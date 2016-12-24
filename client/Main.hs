module Main where

import Control.Monad ((>>=))

import System.IO (IO, print)

import JoScript.Config.ArgParsing (readJob)

main :: IO ()
main = readJob >>= print
