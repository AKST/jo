module JoScript.Data.Position ( Position(..)
                              , initPosition
                              , updatePosition
                              ) where

import Data.Word (Word64)
import Data.Text (Text)
import qualified Data.Text as T

data Position = Pos { _line :: Word64, _column :: Word64 }
  deriving Show

startIndex :: Word64
startIndex = 1

initPosition :: Position
initPosition = Pos startIndex startIndex

onNewline :: Position -> Position
onNewline (Pos l _) = Pos (l + 1) startIndex

moveRight :: Position -> Position
moveRight (Pos l c) = Pos l (c + 1)

updatePosition :: Char -> Position -> Position
updatePosition '\n' = onNewline
updatePosition _ = moveRight

-- class PositionShifts t where
--   shiftWith :: a -> Position -> Position
--
-- instance PositionShifts Text where
--   shiftWith text pos = iter 0 pos where
--     tlen = T.length text
--     iter i (Pos line column)
--       | i >= length = Pos line column
--       | otherwise =
--         let update = if Text.


