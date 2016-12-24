{-# LANGUAGE GADTs #-}
module JoScript.Data.BlockPass where

import Prelude (Show)

import JoScript.Data.Position (Position)
import Data.Text (Text)

data BlockPass = Bp BpKind Position
  deriving Show

data BpKind where
  BpLine   :: Text -> BpKind
  BpIndent :: BpKind
  BpDedent :: BpKind
  BpEnd    :: BpKind
    deriving Show
