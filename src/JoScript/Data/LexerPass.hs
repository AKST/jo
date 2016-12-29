{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}
module JoScript.Data.LexerPass where

import Prelude (Show, Int, Float)

import JoScript.Data.Position (Position)
import Data.Word
import Data.Text (Text)
import Data.Aeson ((.=), (.:))
import JoScript.Util.Json (withObject)
import qualified Data.Aeson as A


data LexerPass = Lp { repr :: LpKind, position :: Position }
  deriving Show

data LpNumber
  = LpInteger Int
  | LpFloat Float
  deriving Show

data LpKind
  = LkEnd
  | LkSpace Word64
  | LkNewline
  | LkIndent
  | LkDedent
  | LkPipe
  | LkBraceL
  | LkBraceR
  | LkQuote
  | LkColon
  | LkAssign
  | LkRestOperator
  | LkDotOperator
  | LkDecoratorPrefix
  | LkIdentifier Text
  | LkNumberLit LpNumber
  | LkString Text
  | LkComment Text
  deriving Show

toToken :: LpKind -> Position -> LexerPass
toToken k p = Lp k p

kindName :: LpKind -> Text
kindName LkEnd    = "end"
kindName LkIndent = "indent"
kindName LkDedent = "dedent"
kindName LkPipe   = "pipe"
kindName LkBraceL = "brace-l"
kindName LkBraceR = "brace-r"
kindName LkQuote  = "quote"
kindName LkColon  = "colon"
kindName LkAssign = "assign"
kindName (LkString _) = "string"
kindName (LkComment _) = "comment"
kindName LkRestOperator = "rest"
kindName (LkIdentifier _) = "identifier"
kindName (LkNumberLit (LpInteger _)) = "number:integer"
kindName (LkNumberLit (LpFloat   _)) = "number:float"


instance A.ToJSON LexerPass where
  toJSON Lp{..} = A.object ["position" .= position, "repr" .= encode repr] where
    default' :: A.Value
    default' = A.object ["type" .= kindName repr]

    encode (LkString s)                = withObject ["value" .= s] default'
    encode (LkComment c)               = withObject ["value" .= c] default'
    encode (LkIdentifier i)            = withObject ["value" .= i] default'
    encode (LkNumberLit (LpFloat f))   = withObject ["value" .= f] default'
    encode (LkNumberLit (LpInteger i)) = withObject ["value" .= i] default'
    encode _                           = default'



