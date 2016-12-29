{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module JoScript.Util.Strings (trim, multiline, toDocument) where

import Prelude ((.), (+), otherwise, Bool(..))
import qualified Prelude as Std

import Text.PrettyPrint.ANSI.Leijen (Doc)
import qualified Text.PrettyPrint.ANSI.Leijen as L

import Data.Eq
import Data.Word (Word64)
import Data.Monoid ((<>))
import Data.Functor (fmap, Functor)
import Data.Text (Text)
import qualified Data.Text as T
import Data.String (String)
import qualified Data.String as String

import Control.Lens (over, set)

import Language.Haskell.TH.Quote (QuasiQuoter)
import qualified Language.Haskell.TH.Quote as Quote


data TrimS
  = Init
  | CountIndent Word64
  | AfterIndent Word64
  | Newline Word64 Word64
  | TrailingNewlines Word64


trim :: String -> String
trim = withState Init T.empty where
  withState :: TrimS -> Text -> String -> String
  withState ____ ys []     = T.unpack ys

  withState Init ys xs = case xs of
    '\n':xs -> withState (CountIndent 0) ys xs
    ' ':xs  -> withState (CountIndent 1) ys xs
    other   -> withState (AfterIndent 0) ys other

  withState (CountIndent c) ys xs = case xs of
    ' ':xs -> withState (CountIndent (c + 1)) ys xs
    other  -> withState (AfterIndent c) ys other

  withState (AfterIndent c) ys xs = case xs of
    '\n':xs -> withState (Newline c 0)   ys' xs
      where ys' = T.snoc ys ' '
    x   :xs -> withState (AfterIndent c) ys' xs
      where ys' = T.snoc ys x

  withState (Newline a 0) ys xss@('\n':xs) = withState (TrailingNewlines a) ys xss
  withState (Newline a b) ys xss@(x:xs)
    | a == b    = withState (AfterIndent a) ys xss
    | x /= ' '  = Std.fail "invalid indent in line"
    | otherwise = withState (Newline a (b + 1)) ys xs

  withState (TrailingNewlines a) ys xs = case xs of
    '\n':xs -> withState (TrailingNewlines a) ys' xs
      where ys' = T.snoc ys '\n'
    _       -> withState (Newline a 0) ys' xs
      where ys' = T.snoc ys '\n'

data DocS = D { dBranch :: DocBranch, dOut :: Doc }
data DocBranch = AnyD | Word Text

toDocument :: String -> Doc
toDocument input = impl (D AnyD L.empty) input where

  out :: Functor f => (Doc -> f Doc) -> DocS -> f DocS
  out fn (D b o) = fmap (\o' -> D b o') (fn o)

  branch :: Functor f => (DocBranch -> f DocBranch) -> DocS -> f DocS
  branch fn (D b o) = fmap (\b' -> D b' o) (fn b)

  wordCont ' '  = False
  wordCont '\n' = False
  wordCont _    = True

  postPend x y = y <> x

  insert d acc = over out (postPend d) acc

  text = L.text . T.unpack

  impl :: DocS -> String -> Doc
  impl acc@(dBranch -> AnyD) xs = case xs of
    '\n':xs -> impl (insert L.hardline acc) xs
    ' ' :xs -> impl (insert L.softline acc) xs
    []      -> dOut acc
    xss     -> impl (set branch (Word "") acc) xss
  impl acc@(dBranch -> Word buff) xs = case xs of
    x:xs | wordCont x -> impl (set branch (Word (T.snoc buff x)) acc) xs
    []                -> dOut (insert (text buff) acc)
    xss               -> impl ((insert (text buff) . set branch AnyD) acc) xss



multiline :: QuasiQuoter
multiline = Quote.QuasiQuoter
  { Quote.quoteExp  = withExp . trim
  , Quote.quotePat  = invalid
  , Quote.quoteType = invalid
  , Quote.quoteDec  = invalid }
  where withExp a = [|String.fromString a|]

        invalid _ = Std.fail "Illeagl QuasiQuote for multiline string"

