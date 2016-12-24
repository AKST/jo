{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module JoScript.Data.Position ( Position(..)
                              , initPosition
                              , updatePosition
                              ) where

import Prelude (Show, Char, (+))

import Control.Applicative ((<*>), (<$>))

import qualified Data.Aeson as A
import Data.Aeson ((.=), (.:))
import Data.Monoid ((<>), mempty)
import Data.Word (Word64)

data Position = Position { line :: Word64, column :: Word64 }
  deriving Show

startIndex :: Word64
startIndex = 1

initPosition :: Position
initPosition = Position startIndex startIndex

onNewline :: Position -> Position
onNewline (Position l _) = Position (l + 1) startIndex

moveRight :: Position -> Position
moveRight (Position l c) = Position l (c + 1)

updatePosition :: Char -> Position -> Position
updatePosition '\n' = onNewline
updatePosition _ = moveRight

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



-- class PositionShifts t where
--   shiftWith :: a -> Position -> Position
--
-- instance PositionShifts Text where
--   shiftWith text pos = iter 0 pos where
--     tlen = T.length text
--     iter i (Position line column)
--       | i >= length = Position line column
--       | otherwise =


