module JoScript.Util.Text (foldlM, readFloat, readInt) where

import Prelude (read)
import Protolude hiding (foldlM)

import qualified Data.Text as T

foldlM :: Monad m => (b -> Char -> m b) -> b -> Text -> m b
foldlM f init bs = impl (pure init) bs where
  impl acc bs
    | T.null bs = acc
    | otherwise = impl (acc >>= \acc' -> f acc' (T.head bs)) (T.tail bs)

readInt :: Text -> Word64
readInt = read . T.unpack

readFloat :: Text -> Float
readFloat = read . T.unpack


