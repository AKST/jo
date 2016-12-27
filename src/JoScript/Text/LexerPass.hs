module JoScript.Text.LexerPass where

import qualified Prelude as Std

import Data.Text (Text)

import JoScript.Data.Position (Position)
import JoScript.Data.BlockPass (BlockPass)
import JoScript.Data.LexerPass (LexerPass)
import qualified JoScript.Data.BlockPass as Bp
import qualified JoScript.Data.LexerPass as Lp
import qualified JoScript.Data.Error as Error

type ContCallback = Payload -> Int -> ContIt

data ContIt
  = Jump ContCallback
  | Fail Error.LexErrorT Position
  | Emit (Text -> Lp.LpKind) Int
  | Next
  deriving Std.Show

data Payload = P { line     :: Text
                 , position :: Position
                 , lexer    :: ContCallback
                 }

data State
  = Read
  | ReadMore Payload Int
  | Done
  | ContToken Payload Int
  | StartToken Text Position

--------------------------------------------------------------
--                          Lens                            --
--------------------------------------------------------------

line' :: Std.Functor f => (Text -> f Text) -> Payload -> f Payload
line' f (P ln p lx) = Std.fmap (\ln' -> P ln' p lx) (f ln)

position' :: Std.Functor f => (Position -> f Position) -> Payload -> f Payload
position' f (P ln p lx) = Std.fmap (\p' -> P ln p' lx) (f p)

lexer' :: Std.Functor f => (ContCallback -> f ContCallback) -> Payload -> f Payload
lexer' f (P ln p lx) = Std.fmap (\lx' -> P ln p lx') (f lx)

--------------------------------------------------------------
--                      Entry point                         --
--------------------------------------------------------------



