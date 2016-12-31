module JoScript.Job (runJob) where

import System.IO (IO)

import JoScript.Job.Build (buildFiles)
import JoScript.Data.Config

runJob :: Job -> IO ()
runJob (JobBuild config) = buildFiles config
