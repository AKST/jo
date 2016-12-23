{-# LANGUAGE GADTs #-}
module JoScript.Data.BlockPass where

import JoScript.Data.Position (Position)

data BlockPass = Bp BpKind Position
  deriving Show

data BpKind where
  BpLine   :: String -> BpKind
  BpIndent :: BpKind
  BpDedent :: BpKind
  BpEnd    :: BpKind
    deriving Show
