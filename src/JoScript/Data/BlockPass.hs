{-# LANGUAGE OverloadedStrings #-}
module JoScript.Data.BlockPass where

import Protolude

import Data.Aeson ((.=), (.:))
import qualified Data.Aeson.Types as A
import qualified Data.Aeson as A

import JoScript.Data.Position (Position)


data BlockPass = Bp { repr :: BpRepr, position :: Position }
  deriving (Eq, Show)

data BpRepr
  = BpLine Text
  | BpIndent
  | BpDedent
  | BpEnd
  deriving (Eq, Show)

instance A.ToJSON BlockPass where
  toJSON Bp{..} = A.object ["position" .= position, "repr" .= repr]

instance A.ToJSON BpRepr where
  toJSON (BpLine text) = A.object ["type" .= ("line"   :: Text), "data" .= text]
  toJSON BpIndent      = A.object ["type" .= ("indent" :: Text)]
  toJSON BpDedent      = A.object ["type" .= ("dedent" :: Text)]
  toJSON BpEnd         = A.object ["type" .= ("end"    :: Text)]

instance A.FromJSON BlockPass where
  parseJSON (A.Object o) = do
    position <- o .: "position"
    repr     <- o .: "repr" >>= decode
    pure (Bp repr position) where
      decode (A.Object o) = o .: "type" >>= withType
      decode _            = mempty

      withType :: Text -> A.Parser BpRepr
      withType "line"   = BpLine <$> o .: "data"
      withType "indent" = pure BpIndent
      withType "dedent" = pure BpDedent
      withType "end"    = pure BpEnd
      withType _        = mempty

  parseJSON _ = mempty


