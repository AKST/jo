module JoScript.Util.Json where

import Protolude

import qualified Data.Aeson as A
import qualified Data.Aeson.Types as A
import qualified Data.HashMap.Strict as HM

withObject :: [A.Pair] -> A.Value -> A.Value
withObject updates (A.Object map) = A.Object newMap where
  newMap = HM.union (HM.fromList updates) map

