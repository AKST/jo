{-# LANGUAGE LambdaCase #-}
module JoScript.Util.Text (foldlM, readFloat, readInt) where

import Prelude ((.))
import qualified Prelude as Std

import Control.Applicative (pure, (<$>), (<*>))
import Control.Monad (Monad, (>>=))


import Data.Either (Either(..))
import Data.Word (Word64)
import Data.Text (Text)
import qualified Data.Text as T

foldlM :: Monad m => (b -> Std.Char -> m b) -> b -> Text -> m b
foldlM f init bs = impl (pure init) bs where
  impl acc bs
    | T.null bs     = acc
    | Std.otherwise = impl (acc >>= \acc' -> f acc' (T.head bs)) (T.tail bs)

readInt :: Text -> Word64
readInt = Std.read . T.unpack

readFloat :: Text -> Std.Float
readFloat = Std.read . T.unpack


