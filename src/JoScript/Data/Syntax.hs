module JoScript.Data.Syntax where

import Prelude (Show, Int, Float)

import JoScript.Data.Position (Position)
import Data.Word
import Data.Text (Text)
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as A

data Syntax

instance A.ToJSON Syntax where
  toJSON _ = A.object []
