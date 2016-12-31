{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE RecordWildCards #-}
module JoScript.Data.Position ( Position(..)
                              , init
                              , update
                              , moveOver
                              ) where

import Prelude (Show, Eq, Ord, Char, (+))

import Control.Applicative ((<*>), (<$>))

import qualified Data.Aeson as A
import Data.Maybe(Maybe(..))
import Data.Aeson ((.=), (.:))
import Data.Monoid ((<>), mempty)
import Data.Word (Word64)
import Data.Text (Text)
import qualified Data.Text as T

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
moveOver (T.uncons -> Nothing)     p = p
moveOver (T.uncons -> Just (h, t)) p = update h p

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


