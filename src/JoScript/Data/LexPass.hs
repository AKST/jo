module JoScript.Data.LexPass where

import Prelude (Show, Int, Float)

import JoScript.Data.Position (Position)
import Data.Word
import Data.Text (Text)

data LexPass
  = Lp LpKind Position
  deriving Show

data LpNumber
  = LpInteger Int
  | LpFloat Float
  deriving Show

data LpKind
  = LpEnd
  | LpSpace Word64
  | LpNewline
  | LpIndent
  | LpDedent
  | LpPipe
  | LpBraceL
  | LpBraceR
  | LpQuote
  | LpColon
  | LpAssign
  | LpRestOperator
  | LpDotOperator
  | LpDecoratorPrefix
  | LpIdentifier Text
  | LpNumberLit LpNumber
  | LpString Text
  | LpComment Text
  deriving Show


toToken :: LpKind -> Position -> LexPass
toToken k p = Lp k p

