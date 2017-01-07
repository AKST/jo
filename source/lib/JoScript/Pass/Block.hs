{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}
module JoScript.Pass.Block (runBlockPass) where


import Protolude hiding (State)

import qualified Data.Conduit as C
import qualified Data.Conduit.Combinators as C

import Control.Lens ((%=), (.=), use, snoc)

import JoScript.Util.Conduit (ResultConduit)
import JoScript.Data.Error (Error(..), Repr(IndentError), IndentErrorT(..), known)
import JoScript.Data.Block
import JoScript.Data.Position (Position(..))
import qualified JoScript.Data.Position as Position

data Branch
  = Dedent Word64 Position
  | Preline
  | InLine
  deriving Show

data State = PS { branch :: Branch
                , position :: Position
                , indentMemory :: [Word64]
                }
  deriving Show

--------------------------------------------------------------
--                          Lens                            --
--------------------------------------------------------------

position' :: Functor f => (Position -> f Position) -> State -> f State
position' f (PS branch position m) = fmap (\position'' -> PS branch position'' m) (f position)

memory' :: Functor f => ([Word64] -> f [Word64]) -> State -> f State
memory' f (PS branch position m) = fmap (\m'' -> PS branch position m'') (f m)

branch' :: Functor f => (Branch -> f Branch) -> State -> f State
branch' f (PS branch position m) = fmap (\branch'' -> PS branch'' position m) (f branch)

--------------------------------------------------------------
--                      Entry point                         --
--------------------------------------------------------------

type BlockConduit m = ResultConduit Char BlockPass m

newtype BlockLexer m a
  = BlockLexer { run :: ExceptT Error (StateT State (BlockConduit m)) a }
  deriving (Functor, Applicative, Monad, MonadError Error, MonadState State)

runBlockPass :: Monad m => BlockConduit m ()
runBlockPass =
  let s = runExceptT (run loop)
   in evalStateT s initial >>= \case
      Right _____ -> pure ()
      Left except -> C.yield (Left except)

initial :: State
initial = PS { branch = Preline
             , position = Position.init
             , indentMemory = []
             }

--------------------------------------------------------------
--                      Event loop                          --
--------------------------------------------------------------

loop :: Monad m => BlockLexer m ()
loop = do
  isEmpty <- liftConduit C.null
  if isEmpty then do
    position <- use position'
    yieldElem (Bp BpEnd position)
  else do
    branch <- use branch'
    withBranch branch
    loop

withBranch :: Monad m => Branch -> BlockLexer m ()
withBranch Preline = do
  startPos <- use position'
  indent   <- countWhile ((==) ' ')
  current  <- currentIndent
  if indent == current then do
    branch' .= InLine
  else if indent > current then do
    branch' .= InLine
    memory' %= ((:) indent)
    yieldElem (Bp BpIndent startPos)
  else do
    branch' .= (Dedent indent startPos)

withBranch InLine = do
  startPos  <- use position'
  consumed <- readWhile ((/=) '\n') <* consumeNext
  branch'  .= Preline
  unless (consumed == "") $
    yieldElem (Bp (BpLine consumed) startPos)

withBranch (Dedent newIndent origin) = use memory' >>= \case
  -- when the previous indent was top level
  [] | newIndent /= 0 -> throwError (known (IndentError ShallowDedent) origin)
     | otherwise      -> branch' .= InLine

  -- when the previous indent was not top level
  lastIndent : others ->
    if newIndent < lastIndent then do
      memory' .= others
      yieldElem (Bp BpDedent origin)
    else if newIndent == lastIndent
      then branch' .= InLine
      else throwError (known (IndentError ShallowDedent) origin)

--------------------------------------------------------------
--                        Util func                         --
--------------------------------------------------------------

yieldElem :: Monad m => BlockPass -> BlockLexer m ()
yieldElem e = liftConduit (C.yield (Right e))

consumeWhile :: Monad m => (Char -> Bool) -> BlockLexer m (Word64, Text)
consumeWhile predicate = iter 0 mempty where
  iter n t = liftConduit C.await >>= \case
    Just (Left except) -> throwError except
    Just (Right input) ->
      if predicate input then do
        position' %= Position.update input
        iter (n + 1) (snoc t input)
      else do
        liftConduit (C.leftover (Right input))
        pure (n, t)
    Nothing -> pure (n, t)

countWhile :: Monad m => (Char -> Bool) -> BlockLexer m Word64
countWhile f = fmap fst (consumeWhile f)

readWhile :: Monad m => (Char -> Bool) -> BlockLexer m Text
readWhile f = fmap snd (consumeWhile f)

currentIndent :: Monad m => BlockLexer m Word64
currentIndent = use memory' >>= \case
  current:_ -> pure current
  [       ] -> pure 0

consumeNext :: Monad m => BlockLexer m ()
consumeNext = liftConduit C.await >>= \case
  Nothing               -> pure ()
  Just (Left exception) -> throwError exception
  Just (Right c)        -> position' %= Position.update c


--------------------------------------------------------------
--                        Wrap func                         --
--------------------------------------------------------------

{- whenever the monad stack is modified we'll only need
 - to update the lifting of conduit function calls here -}
liftConduit :: Monad m => BlockConduit m a -> BlockLexer m a
liftConduit m = BlockLexer (lift (lift m))

