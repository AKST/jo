module JoScript.Job (runJob) where

import System.IO (IO)

import JoScript.Job.Build (build)
import JoScript.Data.Config

runJob :: Job -> IO ()
runJob (JobBuild debug files) = build debug files
