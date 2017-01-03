module JoScript.Job (runJob) where

import Protolude

import Control.Monad.Trans.Resource (runResourceT)

import JoScript.Job.Build (buildFiles)
import JoScript.Data.Config (Job(..))

runJob :: Job -> IO ()
runJob (JobBuild config) = runResourceT (buildFiles config)

