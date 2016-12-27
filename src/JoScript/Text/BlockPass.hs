{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
module JoScript.Text.BlockPass (runBlockPass) where


import Prelude ((+), ($), flip, Char)
import qualified Prelude as Std

import Data.Eq
import Data.Ord
import Data.Word (Word64)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import qualified Data.Text as T
import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C

import Control.Lens ((%=), (.=), use)
import Control.Monad ((>>=), (>>), Monad, unless)
import Control.Applicative (pure, (<*))
import Control.Monad.Trans.Class (lift)
import Control.Monad.Identity (Identity)
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.State as S

import JoScript.Data.Error ( Error(..)
                           , Repr(IndentError)
                           , IndentErrorT(..)
                           , known
                           )
import JoScript.Util.Conduit (ConduitE, ResultConduit)
import JoScript.Data.BlockPass hiding (position)
import JoScript.Data.Position ( Position(..)
                              , updatePosition
                              , initPosition
                              )

data Branch
  = Dedent Word64 Position
  | Preline
  | InLine
  deriving Std.Show

data State = PS { branch :: Branch
                , position :: Position
                , indentMemory :: [Word64]
                }
  deriving Std.Show

--------------------------------------------------------------
--                          Lens                            --
--------------------------------------------------------------

position' :: Std.Functor f => (Position -> f Position) -> State -> f State
position' f (PS branch position m) = Std.fmap (\position'' -> PS branch position'' m) (f position)

memory' :: Std.Functor f => ([Word64] -> f [Word64]) -> State -> f State
memory' f (PS branch position m) = Std.fmap (\m'' -> PS branch position m'') (f m)

branch' :: Std.Functor f => (Branch -> f Branch) -> State -> f State
branch' f (PS branch position m) = Std.fmap (\branch'' -> PS branch'' position m) (f branch)

--------------------------------------------------------------
--                      Entry point                         --
--------------------------------------------------------------

type BlockConduit = ResultConduit Char BlockPass
type BlockLexer m = E.ExceptT Error (S.StateT State (BlockConduit m))

runBlockPass :: Monad m => BlockConduit m ()
runBlockPass =
  let s = E.runExceptT loop
   in S.evalStateT s initial >>= \case
      Right _____ -> pure ()
      Left except -> C.yield (Left except)

initial :: State
initial = PS { branch = Preline
             , position = initPosition
             , indentMemory = []
             }

--------------------------------------------------------------
--                      Event loop                          --
--------------------------------------------------------------

loop :: Monad m => BlockLexer m ()
loop = do
  empty <- liftConduit C.null
  if empty then do
    position <- use position'
    yieldElem (Bp BpEnd position)
  else do
    branch <- use branch'
    withBranch branch
    loop

withBranch :: Monad m => Branch -> BlockLexer m ()
withBranch Preline = do
  initial <- use position'
  indent  <- countWhile ((==) ' ')
  current <- currentIndent
  if indent == current then do
    branch' .= InLine
  else if indent > current then do
    branch' .= InLine
    memory' %= ((:) indent)
    yieldElem (Bp BpIndent initial)
  else do
    branch' .= (Dedent indent initial)

withBranch InLine = do
  initial  <- use position'
  consumed <- readWhile ((/=) '\n') <* consumeNext
  branch'  .= Preline
  unless (consumed == "") $
    yieldElem (Bp (BpLine consumed) initial)

withBranch (Dedent newIndent origin) = use memory' >>= \case
  -- when the previous indent was top level
  [] | newIndent /= 0 -> E.throwE (known (IndentError ShallowDedent) origin)
     | newIndent == 0 -> branch' .= InLine

  -- when the previous indent was not top level
  lastIndent : others ->
    if newIndent < lastIndent then do
      memory' .= others
      yieldElem (Bp BpDedent origin)
    else if newIndent == lastIndent
      then branch' .= InLine
      else E.throwE (known (IndentError ShallowDedent) origin)

--------------------------------------------------------------
--                        Util func                         --
--------------------------------------------------------------

yieldElem :: Monad m => BlockPass -> BlockLexer m ()
yieldElem e = liftConduit (C.yield (Right e))

consumeWhile :: Monad m => (Std.Char -> Std.Bool) -> BlockLexer m (Word64, T.Text)
consumeWhile predicate = iter 0 T.empty where
  iter n t = liftConduit C.await >>= \case
    Just (Left except) -> E.throwE except
    Just (Right input) ->
      if predicate input then do
        position' %= updatePosition input
        iter (n + 1) (T.snoc t input)
      else do
        liftConduit (C.leftover (Right input))
        pure (n, t)
    Nothing -> pure (n, t)

countWhile :: Monad m => (Std.Char -> Std.Bool) -> BlockLexer m Word64
countWhile f = Std.fmap Std.fst (consumeWhile f)

readWhile :: Monad m => (Std.Char -> Std.Bool) -> BlockLexer m T.Text
readWhile f = Std.fmap Std.snd (consumeWhile f)

currentIndent :: Monad m => BlockLexer m Word64
currentIndent = use memory' >>= \case
  current:_ -> pure current
  [       ] -> pure 0

consumeNext :: Monad m => BlockLexer m ()
consumeNext = liftConduit C.await >>= \case
  Nothing               -> pure ()
  Just (Left exception) -> E.throwE exception
  Just (Right c)        -> position' %= updatePosition c


--------------------------------------------------------------
--                        Wrap func                         --
--------------------------------------------------------------

{- whenever the monad stack is modified we'll only need
 - to update the lifting of conduit function calls here -}
liftConduit m = lift (lift m)

