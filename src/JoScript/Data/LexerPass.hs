{-# LANGUAGE OverloadedStrings #-}
module JoScript.Data.LexerPass where

import Protolude

import JoScript.Data.Position (Position)
import Data.Word (Word64)
import Data.Aeson ((.=), (.:))
import JoScript.Util.Json (withObject)
import qualified Data.Aeson as A


data LexerPass = Lp { repr :: LpRepr, position :: Position }
  deriving (Eq, Show)

data LpNumber
  = LpInteger Word64
  | LpFloat Float
  deriving (Eq, Show)

data LpRepr
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
  deriving (Eq, Show)

toToken :: LpRepr -> Position -> LexerPass
toToken k p = Lp k p

kindName :: LpRepr -> Text
kindName LpEnd    = "end"
kindName LpIndent = "indent"
kindName LpDedent = "dedent"
kindName LpNewline = "newline"
kindName LpPipe   = "pipe"
kindName LpBraceL = "brace-l"
kindName LpBraceR = "brace-r"
kindName LpQuote  = "quote"
kindName LpColon  = "colon"
kindName LpAssign = "assign"
kindName (LpSpace _) = "space"
kindName (LpString _) = "string"
kindName (LpComment _) = "comment"
kindName LpRestOperator = "rest"
kindName LpDotOperator = "."
kindName (LpIdentifier _) = "identifier"
kindName (LpNumberLit (LpInteger _)) = "number:integer"
kindName (LpNumberLit (LpFloat   _)) = "number:float"


instance A.ToJSON LexerPass where
  toJSON Lp{..} = A.object ["position" .= position, "repr" .= encode repr] where
    default' :: A.Value
    default' = A.object ["type" .= kindName repr]

    encode (LpString s)                = withObject ["value" .= s] default'
    encode (LpComment c)               = withObject ["value" .= c] default'
    encode (LpIdentifier i)            = withObject ["value" .= i] default'
    encode (LpNumberLit (LpFloat f))   = withObject ["value" .= f] default'
    encode (LpNumberLit (LpInteger i)) = withObject ["value" .= i] default'
    encode _                           = default'



