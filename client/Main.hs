module Main where

import Control.Monad ((>>=))

import System.IO (IO, print)

import JoScript.Config.ArgParsing (readJob)
import JoScript.Job (runJob)

main :: IO ()
main = readJob >>= runJob
