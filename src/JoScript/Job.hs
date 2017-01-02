module JoScript.Job (runJob) where

import Protolude

import JoScript.Job.Build (buildFiles)
import JoScript.Data.Config (Job(..))

runJob :: Job -> IO ()
runJob (JobBuild config) = buildFiles config
