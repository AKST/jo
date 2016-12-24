module JoScript.Util.Json where

import qualified Data.Aeson as A
import qualified Data.HashMap.Strict as HM

import Control.Applicative ((<*>), (<$>), pure)
import qualified Data.Aeson.Types as A

withObject :: [A.Pair] -> A.Value -> A.Value
withObject updates (A.Object map) = A.Object newMap where
  newMap = HM.union map (HM.fromList updates)

