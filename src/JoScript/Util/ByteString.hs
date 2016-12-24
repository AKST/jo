{-# LANGUAGE ViewPatterns #-}
module JoScript.Util.ByteString (foldlM) where

import qualified Prelude as Std

import Control.Applicative (pure, (<$>), (<*>))
import Control.Monad (Monad, (>>=))

import Data.Word (Word8)
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS

foldlM :: Monad m => (b -> Word8 -> m b) -> b -> ByteString -> m b
foldlM f init bs = impl (pure init) bs where
  impl acc bs
    | BS.null bs    = acc
    | Std.otherwise = impl (acc >>= \acc' -> f acc' (BS.head bs)) (BS.tail bs)
