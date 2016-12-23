{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiWayIf #-}
module JoScript.Text.BlockPass (runBlockPass) where


import Data.Word
import qualified Data.Conduit as C

import Control.Lens
import Control.Applicative
import Control.Monad.Trans.Class
import Control.Monad.Trans.Except
import Control.Monad.Identity
import qualified Control.Monad.Trans.State as S

import qualified JoScript.Data.Error as Error
import JoScript.Data.BlockPass
import JoScript.Data.Position ( Position(..)
                              , updatePosition
                              , initPosition
                              )

data It where
  Emit :: BlockPass -> It
  Fail :: Error.IndentErrorT -> Position -> It
  Exit :: It
  Cont :: It

data Event where
  Dedent  :: Word64 -> Position -> Event
  Preline :: Event
  InLine  :: Event
  Finish  :: Event


data State = PS { branch :: Event
                , position :: Position
                , indentMemory :: [Word64]
                }

type ConduitE e i o = C.ConduitM i (Either e o) Identity
type JoConduit = ConduitE Error.Error Char BlockPass

--------------------------------------------------------------
--                          Lens                            --
--------------------------------------------------------------

position' :: Functor f => (Position -> f Position) -> State -> f State
position' f (PS branch position m) = fmap (\position'' -> PS branch position'' m) (f position)

memory' :: Functor f => ([Word64] -> f [Word64]) -> State -> f State
memory' f (PS branch position m) = fmap (\m'' -> PS branch position m'') (f m)

branch' :: Functor f => (Event -> f Event) -> State -> f State
branch' f (PS branch position m) = fmap (\branch'' -> PS branch'' position m) (f branch)

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
    Emit pass -> lift (C.yield (Right pass)) >> implentation
    Fail e p  -> lift (C.yield (Left e'))
      where e' = Error.known (Error.IndentError e) p
    Cont -> implentation
    Exit -> return ()

--------------------------------------------------------------
--                      Event loop                          --
--------------------------------------------------------------

withEvent :: Event -> BlockLexer It
withEvent Finish  = return Exit
withEvent InLine  = return Exit

withEvent (Dedent newIndent origin) =
  S.gets indentMemory >>= \case

    -- when the previous indent was top level
    [] | newIndent /= 0 ->
           return (Fail Error.ShallowDedent origin)
       | newIndent == 0 -> do
           branch' .= InLine
           return Cont

    -- when the previous indent was not top level
    lastIndent : others ->
      if newIndent < lastIndent then do
        memory' .= others
        return (Emit (Bp BpDedent origin))
      else if newIndent == lastIndent then do
        branch' .= InLine
        return Cont
      else
        return (Fail Error.ShallowDedent origin)



withEvent Preline = do
  initial <- S.gets position
  indent  <- consumeMatch ((==) ' ')
  current <- currentIndent
  if | indent == current -> do
         branch' .= InLine
         return Cont
     | indent > current -> do
         branch' .= InLine
         memory' %= ((:) indent)
         return (Emit (Bp BpIndent initial))
     | otherwise -> do
         branch' .= (Dedent indent initial)
         return Cont

--------------------------------------------------------------
--                        Util func                         --
--------------------------------------------------------------

consumeMatch :: (Char -> Bool) -> BlockLexer Word64
consumeMatch consumePred = iter 0 where
  iter n = lift C.await >>= \case
    Just input ->
      if consumePred input then do
        position' %= updatePosition input
        iter (n + 1)
      else do
        lift (C.leftover input)
        pure n
    Nothing -> pure n

currentIndent :: BlockLexer Word64
currentIndent = S.gets indentMemory >>= \case
  current:_ -> return current
  [       ] -> return 0

