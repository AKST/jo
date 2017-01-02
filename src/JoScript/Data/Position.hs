{-# LANGUAGE OverloadedStrings #-}
module JoScript.Data.Position ( Position(..)
                              , init
                              , update
                              , moveOver
                              ) where

import Protolude hiding (uncons)

import Data.Text (uncons)
import Data.Aeson ((.=), (.:))
import qualified Data.Aeson as A

data Position = Position { line :: Word64, column :: Word64 }
  deriving (Show, Eq, Ord)

startIndex :: Word64
startIndex = 1

init :: Position
init = Position startIndex startIndex

onNewline :: Position -> Position
onNewline (Position l _) = Position (l + 1) startIndex

moveRight :: Position -> Position
moveRight (Position l c) = Position l (c + 1)

update :: Char -> Position -> Position
update '\n' = onNewline
update _ = moveRight

moveOver :: Text -> Position -> Position
moveOver (uncons -> Just (h, t)) p = moveOver t (update h p)
moveOver _                       p = p

instance A.ToJSON Position where
  toJSON (Position line column) =
    A.object ["line" .= line, "column" .= column]

  toEncoding Position{..} =
    A.pairs ("line" .= line <> "column" .= column)

instance A.FromJSON Position where
  parseJSON (A.Object o) =
    Position <$> o .: "line"
             <*> o .: "column"
  parseJSON _ = mempty

