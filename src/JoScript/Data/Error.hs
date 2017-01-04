{-# LANGUAGE OverloadedStrings #-}
module JoScript.Data.Error where

import Protolude hiding (Location)

import Data.Aeson ((.=))
import qualified Data.Aeson.Types as A


import JoScript.Data.Position (Position)
import JoScript.Data.Block as Bp
import JoScript.Data.Lexer as Lp
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
  | LUnexpectedCharacter Char
  | LUnknownTokenStart Char
  | LInvalidIntSuffix Char
  | LDuplicateDecimial
  deriving (Eq, Show)

data ParseErrorT
  = PUnexpectedEnd
  | PIncompleteAlt
  | PUnexpectedToken LpRepr
  deriving (Eq, Show)

--------------------------------------------------------------
--                          exports                         --
--------------------------------------------------------------

known :: Repr -> Position -> Error
known k p = Error k (Known p)

reprDescription :: Repr -> Text
reprDescription (IndentError _) = "text:block"
reprDescription (LexerError _) = "text:lexer"
reprDescription (ParseError _) = "text:parse"

lexerErrMsg :: LexerErrorT -> Text
lexerErrMsg LUnexpectedEnd           = "unexpected lexer ended"
lexerErrMsg (LUnexpectedToken _)     = "unexpected block token"
lexerErrMsg (LUnexpectedCharacter _) = "unexpected character"
lexerErrMsg (LUnknownTokenStart _)   = "unexpected character"
lexerErrMsg (LInvalidIntSuffix _)    = "integer was suffixed with invalid character"
lexerErrMsg LDuplicateDecimial       = "duplicated decimal place in float"

parseErrMsg :: ParseErrorT -> Text
parseErrMsg PUnexpectedEnd = "unexpected parse end during parse"
parseErrMsg PIncompleteAlt = "implementation error"
parseErrMsg PUnexpectedToken{} = "encounted unexpected token"

{- Determines which error is most recently occuring in a file -}
newestError :: Error -> Error -> Error
newestError a@(Error _ (Known al)) b@(Error _ (Known bl))
  | al > bl   = a
  | bl > al   = b
  | otherwise = a

--------------------------------------------------------------
--                         instances                        --
--------------------------------------------------------------

instance A.ToJSON Error where
  toJSON (Error repr loc) =  A.object ["location" .= loc, "repr" .= repr]

instance A.ToJSON Location where
  toJSON (Known p) = withObject ["type" .= knownS] (A.toJSON p) where
    knownS = "known" :: Text

instance A.ToJSON Repr where
  toJSON t = withObject ["level" .= reprDescription t] (withType t) where
    withType (IndentError err) = A.toJSON err
    withType (LexerError  err) = A.toJSON err
    withType (ParseError  err) = A.toJSON err

instance A.ToJSON IndentErrorT where
  toJSON ShallowDedent = A.object ["message" .= ("dedent depth is too shallow" :: Text)]

instance A.ToJSON LexerErrorT where
  toJSON t = withObject ["message" .= lexerErrMsg t] (withType t) where
    withType (LUnexpectedToken token) = A.object ["token" .= token]
    withType (LUnknownTokenStart c)   = A.object ["character" .= c]
    withType (LUnexpectedCharacter c) = A.object ["character" .= c]
    withType (LInvalidIntSuffix c)    = A.object ["character" .= c]
    withType _                        = A.object []

instance A.ToJSON ParseErrorT where
  toJSON t = withObject ["message" .= parseErrMsg t] (withType t) where
    withType (PUnexpectedToken t) = A.object ["token" .= t]
    withType ____________________ = A.object []

