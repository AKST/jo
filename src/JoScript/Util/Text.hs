module JoScript.Util.Text (foldlM) where

import qualified Prelude as Std

import Control.Applicative (pure, (<$>), (<*>))
import Control.Monad (Monad, (>>=))

import Data.Text (Text)
import qualified Data.Text as T

foldlM :: Monad m => (b -> Std.Char -> m b) -> b -> Text -> m b
foldlM f init bs = impl (pure init) bs where
  impl acc bs
    | T.null bs     = acc
    | Std.otherwise = impl (acc >>= \acc' -> f acc' (T.head bs)) (T.tail bs)
