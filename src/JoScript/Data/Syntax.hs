{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-- # This data structure is more about
 --
 -- About representing the 1:1 representation of the source
 -- syntax including the invokation of macros and syntaxtic
 -- sugar. It is not intended to be the internal
 -- representation (IR), or the abstract syntax tree.
 --}
module JoScript.Data.Syntax where

import Prelude (Show, Int, Float)

import Data.Eq (Eq)
import JoScript.Data.Position (Position)
import Data.Map (Map)
import Data.Word (Word64)
import Data.Text (Text)
import Data.Maybe (Maybe(..))
import Data.Aeson ((.=), (.:))
import Data.Sequence (Seq)
import qualified Data.Map as M
import qualified Data.Sequence as S
import qualified Data.Aeson as A

import JoScript.Util.Json (withObject)

import System.IO (FilePath)

type SynStringLitT = Text

newtype SynId = SynId { get :: Text }
  deriving (Show, Eq)

data Ref
  -- references
  = RefIdentity SynId

  -- expression properties
  | RefProperty SynExpr SynId
  deriving (Show, Eq)

data SynNumLitT
  = SynIntLit Word64
  | SynFltLit Float
  deriving (Show, Eq)

-- SynParamsApp
--   meaning: syntax for syntax invokation
--   example: [fn $apply $apply $apply *$apply $def: $apply $def: $apply]
data SynParamsApp = SynParamsApp
  { positional :: Seq SynExpr
  , rest       :: Maybe SynExpr
  , keywords   :: Map SynId SynExpr }
  deriving (Show, Eq)

-- SynParamsDef
--   meaning: syntax for declaring function arguments
--   example: fn := [$apply $apply $apply *$apply :$apply :$apply $apply: $def]
data SynParamsDef = SynParamsDef
  { positional :: Seq SynId
  , rest       :: Maybe SynId
  , keywords   :: Map SynId SynExpr }
  deriving (Show, Eq)

-- Top level data structure for representing an expression
data SynModule = SynModule { statements :: Seq SynExpr, fileName :: Text }
  deriving (Show, Eq)

-- data structure for representing an expression and it's position
data SynExpr = SynExpr { expr :: SynExprRepr, position :: Position }

-- data structure for representation of different syntax
data SynExprRepr where
  -- .define | .module | .case
  SynContextual :: SynId -> SynExprRepr

  -- @argument-one
  -- arg-two := {... some definition ...}
  SynDecorator  :: SynExpr    -> SynExpr -> SynExprRepr

  SynReference  :: Ref        -> SynExprRepr

  -- 'expression
  SynQuote      :: SynExpr    -> SynExprRepr

  -- in [fn a b c *d e: e]
  -- arg-1 = [fn ...
  -- arg-2 = ... a b c *d e: e]
  SynInvokation :: SynExpr -> SynParamsApp -> SynExprRepr

  -- arg-1 := arg-2
  SynDeclaration :: Ref -> SynExpr -> SynExprRepr

  -- fn a b c | arg-1 |
  --   arg-2.0
  --   arg-2.1
  --   arg-2.2
  --
  -- fn a b c |
  --   arg-2.0
  --   arg-2.1
  SynBlock :: SynParamsDef -> Seq SynExpr -> SynExprRepr

  -- just number literals
  SynNumLit :: SynNumLitT -> SynExprRepr

  -- string literals
  SynStringLit :: SynStringLitT -> SynExprRepr

  -- since this is the 1:1 representation of the syntax we
  -- should also track the comments
  SynComment :: Text -> SynExprRepr

deriving instance (Show (SynExpr))
deriving instance (Show (SynExprRepr))

deriving instance (Eq (SynExpr))
deriving instance (Eq (SynExprRepr))

exprType :: SynExprRepr -> Text
exprType (SynContextual _) = "contextual"
exprType (SynDecorator _ _)  = "decorator"
exprType (SynReference (RefIdentity _))  = "reference:identity"
exprType (SynReference (RefProperty _ _))  = "reference:property"
exprType (SynQuote _) = "quote"
exprType (SynInvokation _ _) = "invokation"
exprType (SynDeclaration (RefIdentity   _) _) = "declarion:identity"
exprType (SynDeclaration (RefProperty _ _) _) = "declarion:property"
exprType (SynBlock _ _) = "block"
exprType (SynNumLit (SynIntLit _)) = "integer"
exprType (SynNumLit (SynFltLit _)) = "float"
exprType (SynStringLit _) = "string"
exprType (SynComment _) = "comment"

instance A.ToJSON SynModule where
  toJSON SynModule{..} = A.object ["source-file" .= fileName, "statements" .= statements]

instance A.ToJSON SynExpr where
  toJSON SynExpr{..} = withObject ["position" .= position] (A.toJSON expr)

instance A.ToJSON SynId where
  toJSON (SynId text) = A.toJSON text

instance A.ToJSON Ref where
  toJSON (RefIdentity i) =
    A.object [ "type" .= ("identifier" :: Text)
             , "value" .= A.object ["name" .= i]]
  toJSON (RefProperty e i) =
    A.object [ "type" .= ("property" :: Text)
             , "value" .= A.object ["property" .= i, "namespace" .= e]]

instance A.ToJSON SynParamsApp where
  toJSON SynParamsApp{..} = A.object pairs where
    pairs = [ "positional" .= positional, "rest" .= rest, "keywords" .= keywordMap]

    keywordMap = A.object (M.foldlWithKey mapFold [] keywords) where
      mapFold acc (SynId text) v = (text .= v):acc

instance A.ToJSON SynParamsDef where
  toJSON SynParamsDef{..} = A.object pairs where
    pairs = [ "positional" .= positional, "rest" .= rest, "keywords" .= keywordMap]

    keywordMap = A.object (M.foldlWithKey mapFold [] keywords) where
      mapFold acc (SynId text) v = (text .= v):acc

instance A.ToJSON SynExprRepr where
  toJSON expr = A.object ["type" .= exprType expr, "value" .= A.object (forKind expr)] where
    forKind (SynContextual e)         = ["expression" .= e]
    forKind (SynDecorator d t)        = ["decorator" .= d, "target" .= t]
    forKind (SynReference r)          = ["reference" .= r]
    forKind (SynQuote quoted)         = ["quoted" .= quoted]
    forKind (SynInvokation expr args) = ["invoked" .= expr, "arguments" .= args]
    forKind (SynDeclaration r value)  = ["reference" .= r, "value" .= value]
    forKind (SynBlock def statements) = ["arguments" .= def, "statements" .= statements]
    forKind (SynNumLit (SynIntLit i)) = ["integer" .= i]
    forKind (SynNumLit (SynFltLit i)) = ["float" .= i]
    forKind (SynStringLit string)     = ["string" .= string]
    forKind (SynComment commment)     = ["contents" .= commment]
