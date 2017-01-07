module Main where

import Protolude

import JoScript.Config.ArgParsing (readJob)
import JoScript.Job (runJob)

main :: IO ()
main = readJob >>= runJob
