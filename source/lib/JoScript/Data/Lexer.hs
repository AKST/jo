{-# LANGUAGE OverloadedStrings #-}
module JoScript.Data.Lexer where

import Protolude

import Data.Aeson ((.=))
import qualified Data.Aeson as A

import JoScript.Util.Json (withObject)
import JoScript.Data.Position (Position)



data LexerPass = Lp { repr :: LpRepr, position :: Position }
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
  | LpInteger Integer
  | LpFloat Double
  | LpString Text
  | LpComment Text
  deriving (Eq, Show)

data LpReprKind
  = LpKindEnd
  | LpKindSpace
  | LpKindNewline
  | LpKindIndent
  | LpKindDedent
  | LpKindPipe
  | LpKindBraceL
  | LpKindBraceR
  | LpKindQuote
  | LpKindColon
  | LpKindAssign
  | LpKindRestOperator
  | LpKindDotOperator
  | LpKindDecoratorPrefix
  | LpKindIdentifier
  | LpKindIntegerLit
  | LpKindFloatLit
  | LpKindString
  | LpKindComment
  deriving (Eq, Show)

--------------------------------------------------------------
--                       predicates                         --
--------------------------------------------------------------

reprKind :: LpRepr -> LpReprKind
reprKind LpEnd             = LpKindEnd
reprKind (LpSpace _)       = LpKindSpace
reprKind LpNewline         = LpKindNewline
reprKind LpIndent          = LpKindIndent
reprKind LpDedent          = LpKindDedent
reprKind LpPipe            = LpKindPipe
reprKind LpBraceL          = LpKindBraceL
reprKind LpBraceR          = LpKindBraceR
reprKind LpQuote           = LpKindQuote
reprKind LpColon           = LpKindColon
reprKind LpAssign          = LpKindAssign
reprKind LpRestOperator    = LpKindRestOperator
reprKind LpDotOperator     = LpKindDotOperator
reprKind LpDecoratorPrefix = LpKindDecoratorPrefix
reprKind (LpIdentifier _)  = LpKindIdentifier
reprKind (LpString _)      = LpKindString
reprKind (LpComment _)     = LpKindComment
reprKind (LpFloat _)       = LpKindFloatLit
reprKind (LpInteger _)     = LpKindIntegerLit

--------------------------------------------------------------
--                         helpers                          --
--------------------------------------------------------------

toToken :: LpRepr -> Position -> LexerPass
toToken k p = Lp k p

kindName :: LpReprKind -> Text
kindName LpKindEnd = "end"
kindName LpKindIndent = "indent"
kindName LpKindDedent = "dedent"
kindName LpKindNewline = "newline"
kindName LpKindPipe = "pipe"
kindName LpKindBraceL = "brace-l"
kindName LpKindBraceR = "brace-r"
kindName LpKindQuote = "quote"
kindName LpKindColon = "colon"
kindName LpKindAssign = "assign"
kindName LpKindSpace = "space"
kindName LpKindString = "string"
kindName LpKindComment = "comment"
kindName LpKindRestOperator = "rest"
kindName LpKindDotOperator = "."
kindName LpKindDecoratorPrefix = "decorator"
kindName LpKindIdentifier = "identifier"
kindName LpKindIntegerLit = "integer"
kindName LpKindFloatLit = "float"


instance A.ToJSON LexerPass where
  toJSON Lp{..} = A.object ["position" .= position, "repr" .= repr]

instance A.ToJSON LpReprKind where
  toJSON = A.toJSON . kindName

instance A.ToJSON LpRepr where
  toJSON token = withType token where

    default' :: A.Value
    default' = A.object ["type" .= reprKind token]

    withType (LpString s)     = withObject ["value" .= s] default'
    withType (LpComment c)    = withObject ["value" .= c] default'
    withType (LpIdentifier i) = withObject ["value" .= i] default'
    withType (LpFloat f)      = withObject ["value" .= f] default'
    withType (LpInteger i)    = withObject ["value" .= i] default'
    withType _                = default'



