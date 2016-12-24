{-# LANGUAGE OverloadedStrings #-}
module JoScript.Data.Error where

import Prelude (Show)

import JoScript.Data.Position (Position)

import Control.Monad ((>>=))
import Control.Applicative ((<*>), (<$>), pure)
import qualified Data.Aeson.Types as A
import qualified Data.Aeson as A
import Data.Aeson ((.=), (.:))
import Data.Monoid ((<>), mempty)
import Data.Text (Text)

import JoScript.Util.Json (withObject)

data Error = Error Repr Location
  deriving Show

data Location = Known Position
  deriving Show

data Repr = IndentError IndentErrorT
  deriving Show

data IndentErrorT
  = ShallowDedent
  deriving Show

known k p = Error k (Known p)

instance A.ToJSON Error where
  toJSON (Error repr loc) = A.object [ "location" .= loc, "repr" .= repr]

instance A.ToJSON Location where
  toJSON (Known p) = withObject ["type" .= ("known" :: Text)] (A.toJSON p)

instance A.ToJSON Repr where
  toJSON (IndentError err) = withObject ["type" .= ("text:block" :: Text)] (A.toJSON err)

instance A.ToJSON IndentErrorT where
  toJSON ShallowDedent = A.object ["message" .= ("dedent depth is too shallow" :: Text)]
