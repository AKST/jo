{-# LANGUAGE OverloadedStrings #-}
module JoScript.Data.Error where

import Prelude (Show, Char)

import JoScript.Data.Position (Position)

import Control.Monad ((>>=))
import Control.Applicative ((<*>), (<$>), pure)
import qualified Data.Aeson.Types as A
import qualified Data.Aeson as A
import Data.Aeson ((.=), (.:))
import Data.Monoid ((<>), mempty)
import Data.Text (Text)

import JoScript.Data.BlockPass as Bp
import JoScript.Util.Json (withObject)

data Error = Error Repr Location
  deriving Show

data Location = Known Position
  deriving Show

data Repr
  = IndentError IndentErrorT
  | LexerError LexerErrorT
  deriving Show

data IndentErrorT
  = ShallowDedent
  deriving Show

data LexerErrorT
  = UnexpectedEnd
  | UnexpectedToken Bp.BpRepr
  | UnknownTokenStart Char
  | InvalidIntSuffix Char
  | DuplicateDecimial
  deriving Show

known k p = Error k (Known p)

reprDescription :: Repr -> Text
reprDescription (IndentError _) = "text:block"
reprDescription (LexerError _) = "text:lexer"

lexerErrMsg :: LexerErrorT -> Text
lexerErrMsg UnexpectedEnd         = "unexpected lexer ended"
lexerErrMsg (UnexpectedToken _)   = "unexpected block token"
lexerErrMsg (UnknownTokenStart _) = "unexpected character"
lexerErrMsg (InvalidIntSuffix _)  = "integer was suffixed with invalid character"
lexerErrMsg DuplicateDecimial     = "duplicated decimal place in float"

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
  toJSON e@(UnexpectedToken t)   = A.object ["message" .= lexerErrMsg e, "token" .= t]
  toJSON e@(UnknownTokenStart c) = A.object ["message" .= lexerErrMsg e, "character" .= c]
  toJSON e@(InvalidIntSuffix c)  = A.object ["message" .= lexerErrMsg e, "character" .= c]
  toJSON e                       = A.object ["message" .= lexerErrMsg e]
