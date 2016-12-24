{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module JoScript.Text.BlockPass (runBlockPass) where


import Prelude ((+), ($))
import qualified Prelude as Std

import Data.Eq
import Data.Ord
import Data.Word (Word64)
import qualified Data.Maybe as Maybe
import qualified Data.Either as Either
import qualified Data.Text as T
import qualified Data.Conduit as C

import Control.Lens ((%=), (.=))
import Control.Monad ((>>=), (>>))
import Control.Applicative (pure)
import Control.Monad.Trans.Class (lift)
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Trans.State as S

import qualified JoScript.Data.Error as Error
import JoScript.Data.BlockPass hiding (position)
import JoScript.Data.Position ( Position(..)
                              , updatePosition
                              , initPosition
                              )

data It
  = Emit BlockPass
  | Fail Error.IndentErrorT Position
  | Exit
  | Cont

data Event
  = Dedent Word64 Position
  | Preline
  | InLine
  | Finish


data State = PS { branch :: Event
                , position :: Position
                , indentMemory :: [Word64]
                }

type ConduitE e i o = C.ConduitM i (Either.Either e o) Identity
type JoConduit = ConduitE Error.Error Std.Char BlockPass

--------------------------------------------------------------
--                          Lens                            --
--------------------------------------------------------------

position' :: Std.Functor f => (Position -> f Position) -> State -> f State
position' f (PS branch position m) = Std.fmap (\position'' -> PS branch position'' m) (f position)

memory' :: Std.Functor f => ([Word64] -> f [Word64]) -> State -> f State
memory' f (PS branch position m) = Std.fmap (\m'' -> PS branch position m'') (f m)

branch' :: Std.Functor f => (Event -> f Event) -> State -> f State
branch' f (PS branch position m) = Std.fmap (\branch'' -> PS branch'' position m) (f branch)

--------------------------------------------------------------
--                      Entry point                         --
--------------------------------------------------------------

runBlockPass :: JoConduit ()
runBlockPass = S.evalStateT implentation initial

initial :: State
initial = PS { branch = Preline
             , position = initPosition
             , indentMemory = []
             }

type BlockLexer = S.StateT State JoConduit

implentation :: BlockLexer ()
implentation =
  S.gets branch >>= withEvent >>= \case
    Emit pass -> lift (C.yield (Either.Right pass)) >> implentation
    Fail e p  -> lift (C.yield (Either.Left e'))
      where e' = Error.known (Error.IndentError e) p
    Cont -> implentation
    Exit -> pure ()

--------------------------------------------------------------
--                      Event loop                          --
--------------------------------------------------------------

withEvent :: Event -> BlockLexer It
withEvent Finish = pure Exit

withEvent InLine = do
  initial  <- S.gets position
  consumed <- readWhile ((/=) '\n')
  branch'  .= Preline
  pure $
    if consumed == ""
      then Cont
      else Emit (Bp (BpLine consumed) initial)

withEvent (Dedent newIndent origin) =
  S.gets indentMemory >>= \case

    -- when the previous indent was top level
    [] | newIndent /= 0 -> pure (Fail Error.ShallowDedent origin)
       | newIndent == 0 -> branch' .= InLine >> pure Cont

    -- when the previous indent was not top level
    lastIndent : others ->
      if newIndent < lastIndent then do
        memory' .= others
        pure (Emit (Bp BpDedent origin))
      else if newIndent == lastIndent then do
        branch' .= InLine
        pure Cont
      else
        pure (Fail Error.ShallowDedent origin)

withEvent Preline = do
  initial <- S.gets position
  indent  <- countWhile ((==) ' ')
  current <- currentIndent
  if indent == current then do
    branch' .= InLine
    pure Cont
  else if indent > current then do
    branch' .= InLine
    memory' %= ((:) indent)
    pure (Emit (Bp BpIndent initial))
  else do
    branch' .= (Dedent indent initial)
    pure Cont

--------------------------------------------------------------
--                        Util func                         --
--------------------------------------------------------------

consumeWhile :: (Std.Char -> Std.Bool) -> BlockLexer (Word64, T.Text)
consumeWhile consumePred = iter 0 T.empty where
  iter n t = lift C.await >>= \case
    Maybe.Just input ->
      if consumePred input then do
        position' %= updatePosition input
        iter (n + 1) (T.snoc t input)
      else do
        lift (C.leftover input)
        pure (n, t)
    Maybe.Nothing -> pure (n, t)

countWhile :: (Std.Char -> Std.Bool) -> BlockLexer Word64
countWhile f = Std.fmap Std.fst (consumeWhile f)

readWhile :: (Std.Char -> Std.Bool) -> BlockLexer T.Text
readWhile f = Std.fmap Std.snd (consumeWhile f)

currentIndent :: BlockLexer Word64
currentIndent = S.gets indentMemory >>= \case
  current:_ -> pure current
  [       ] -> pure 0

