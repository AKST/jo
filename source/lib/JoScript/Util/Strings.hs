{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExplicitNamespaces #-}
module JoScript.Util.Strings (trim, multiline, toDocument) where

import Prelude (fail, error)
import Protolude hiding ((<>), withState, error)

import Data.Monoid ((<>))
import Data.String (fromString)
import qualified Data.Text as T

import Control.Lens (over, set)

import Text.PrettyPrint.ANSI.Leijen (Doc)
import qualified Text.PrettyPrint.ANSI.Leijen as L

import Language.Haskell.TH.Quote (QuasiQuoter)
import qualified Language.Haskell.TH.Quote as Quote


data TrimS
  = Init
  | CountIndent Word64
  | AfterIndent Word64
  | Newline Word64 Word64
  | TrailingNewlines Word64


trim :: [Char] -> [Char]
trim = withState Init "" where
  withState :: TrimS -> Text -> [Char] -> [Char]
  withState ____ ys []     = T.unpack ys

  withState Init ys xss = case xss of
    '\n':xs -> withState (CountIndent 0) ys xs
    ' ':xs  -> withState (CountIndent 1) ys xs
    other   -> withState (AfterIndent 0) ys other

  withState (CountIndent c) ys xss = case xss of
    ' ':xs -> withState (CountIndent (c + 1)) ys xs
    other  -> withState (AfterIndent c) ys other

  withState (AfterIndent c) ys xss = case xss of
    '\n':xs -> withState (Newline c 0) (T.snoc ys ' ') xs
    x   :xs -> withState (AfterIndent c) (T.snoc ys x) xs
    []      -> error "impossible state"


  withState (Newline a 0) ys xss@('\n':_) = withState (TrailingNewlines a) ys xss
  withState (Newline a b) ys xss@(x:xs)
    | a == b    = withState (AfterIndent a) ys xss
    | x /= ' '  = fail "invalid indent in line"
    | otherwise = withState (Newline a (b + 1)) ys xs

  withState (TrailingNewlines a) ys xss = case xss of
    '\n':xs -> withState (TrailingNewlines a) (T.snoc ys '\n') xs
    _       -> withState (Newline a 0) (T.snoc ys '\n') xss

data DocS = D { dBranch :: DocBranch, dOut :: Doc }
data DocBranch = AnyD | Word Text

{-- # transforms a string into a document --}
toDocument :: [Char] -> Doc
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

  impl :: DocS -> [Char] -> Doc
  impl acc@(dBranch -> AnyD) xss = case xss of
    '\n':xs -> impl (insert L.hardline acc) xs
    ' ' :xs -> impl (insert L.softline acc) xs
    []      -> dOut acc
    _       -> impl (set branch (Word "") acc) xss
  impl acc@(dBranch -> Word buff) xss = case xss of
    x:xs | wordCont x -> impl (set branch (Word (T.snoc buff x)) acc) xs
    []                -> dOut (insert (text buff) acc)
    _                 -> impl ((insert (text buff) . set branch AnyD) acc) xss
  impl _ _ = error "impossible state"


{-- # multiline string template --}
multiline :: QuasiQuoter
multiline = Quote.QuasiQuoter
  { Quote.quoteExp  = withExp . trim
  , Quote.quotePat  = invalid
  , Quote.quoteType = invalid
  , Quote.quoteDec  = invalid }
  where withExp a = [|fromString a|]

        invalid _ = fail "Illeagl QuasiQuote for multiline string"

