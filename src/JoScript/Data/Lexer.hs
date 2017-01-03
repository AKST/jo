{-# LANGUAGE OverloadedStrings #-}
module JoScript.Data.Lexer where

import Protolude

import Data.Aeson ((.=))
import qualified Data.Aeson as A

import JoScript.Util.Json (withObject)
import JoScript.Data.Position (Position)



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

--------------------------------------------------------------
--                       predicates                         --
--------------------------------------------------------------

isEnd = (==) LpEnd

isSpace t
  | LpSpace _ <- t = True
  | otherwise      = False

isNewline = (==) LpNewline

isIndent = (==) LpIndent

isDedent = (==) LpDedent

isPipe = (==) LpPipe

isLBrace = (==) LpBraceL

isRBrace = (==) LpBraceR

isColon = (==) LpColon

isQuote = (==) LpQuote

isAssign = (==) LpAssign

isRestOperator = (==) LpRestOperator

isDotOperator = (==) LpDotOperator

isDecoratorPre = (==) LpDecoratorPrefix

isIdentifer token
  | LpIdentifier _ <- token = True
  | otherwise               = False

isFloat t
  | LpNumberLit (LpFloat _) <- t = True
  | otherwise                    = False

isInt t
  | LpNumberLit (LpInteger _) <- t = True
  | otherwise                      = False

isString t
  | LpString _ <- t = True
  | otherwise       = False

isComment t
  | LpComment _ <- t = True
  | otherwise        = False

--------------------------------------------------------------
--                         helpers                          --
--------------------------------------------------------------

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
kindName LpDecoratorPrefix = "decorator"
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



