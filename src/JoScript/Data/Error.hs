{-# LANGUAGE OverloadedStrings #-}
module JoScript.Data.Error where

import Protolude hiding (Location)

import Data.Aeson ((.=), (.:))
import qualified Data.Aeson.Types as A
import qualified Data.Aeson as A

import JoScript.Data.Position (Position)
import JoScript.Data.Block as Bp
import JoScript.Util.Json (withObject)

data Error = Error Repr Location
  deriving (Eq, Show)

data Location = Known Position
  deriving (Eq, Show)

data Repr
  = IndentError IndentErrorT
  | LexerError LexerErrorT
  | ParseError ParseErrorT
  deriving (Eq, Show)

data IndentErrorT
  = ShallowDedent
  deriving (Eq, Show)

data LexerErrorT
  = LUnexpectedEnd
  | LUnexpectedToken Bp.BpRepr
  | LUnknownTokenStart Char
  | LInvalidIntSuffix Char
  | LDuplicateDecimial
  deriving (Eq, Show)

data ParseErrorT
  = PUnexpectedEnd
  deriving (Eq, Show)

--------------------------------------------------------------
--                          exports                         --
--------------------------------------------------------------

known k p = Error k (Known p)

reprDescription :: Repr -> Text
reprDescription (IndentError _) = "text:block"
reprDescription (LexerError _) = "text:lexer"

lexerErrMsg :: LexerErrorT -> Text
lexerErrMsg LUnexpectedEnd         = "unexpected lexer ended"
lexerErrMsg (LUnexpectedToken _)   = "unexpected block token"
lexerErrMsg (LUnknownTokenStart _) = "unexpected character"
lexerErrMsg (LInvalidIntSuffix _)  = "integer was suffixed with invalid character"
lexerErrMsg LDuplicateDecimial     = "duplicated decimal place in float"

--------------------------------------------------------------
--                         instances                        --
--------------------------------------------------------------

instance A.ToJSON Error where
  toJSON (Error repr loc) = A.object [ "location" .= loc, "repr" .= repr]

instance A.ToJSON Location where
  toJSON (Known p) = withObject ["type" .= known] (A.toJSON p) where
    known = "known" :: Text

instance A.ToJSON Repr where
  toJSON t@(IndentError err) = withObject ["type" .= reprDescription t] (A.toJSON err)
  toJSON t@(LexerError  err) = withObject ["type" .= reprDescription t] (A.toJSON err)

instance A.ToJSON IndentErrorT where
  toJSON ShallowDedent = A.object ["message" .= ("dedent depth is too shallow" :: Text)]

instance A.ToJSON LexerErrorT where
  toJSON e@(LUnexpectedToken t)   = A.object ["message" .= lexerErrMsg e, "token" .= t]
  toJSON e@(LUnknownTokenStart c) = A.object ["message" .= lexerErrMsg e, "character" .= c]
  toJSON e@(LInvalidIntSuffix c)  = A.object ["message" .= lexerErrMsg e, "character" .= c]
  toJSON e                       = A.object ["message" .= lexerErrMsg e]
