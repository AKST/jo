{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module JoScript.Util.Conduit ( characterStream
                             , printAsJSON
                             , ConduitE
                             , ResultConduit
                             ) where

import Prelude ((.), ($))
import qualified Prelude as Std
import qualified Conduit as StdCon

import Control.Applicative (pure)
import Control.Monad (Monad, (>>=), (>>))
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Trans.Class (lift)
import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Writer as W

import Data.Void (Void)
import Data.Monoid ((<>))
import Data.Aeson ((.=))
import qualified Data.Aeson as A
import Data.Conduit ((=$=), (.|), mapOutput)
import Data.Conduit.List (mapFoldable)
import qualified Data.Conduit as C
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.ByteString.Lazy (putStr)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import System.IO (FilePath)

import JoScript.Data.Error (Error)
import qualified JoScript.Util.Json as Json
import qualified JoScript.Util.Text as TUtil


type ConduitE e i o = C.ConduitM (Either e i) (Either e o)
type ResultConduit i o = ConduitE Error i o


characterStream :: StdCon.MonadResource m => FilePath -> C.Source m (Either Error Std.Char)
characterStream f = mapOutput Right (StdCon.sourceFile f .| mapFoldable LT.unpack)

type Tldr m v = (StdCon.MonadResource m, A.ToJSON v)

printAsJSON :: (StdCon.MonadResource m, A.ToJSON v) => T.Text -> C.Sink (Either Error v) m ()
printAsJSON type' = impl where

  impl =
    let toJson :: (StdCon.MonadResource m, A.ToJSON v) => C.Sink (Either Error v) m A.Value
        toJson = W.runWriterT (E.runExceptT loop) >>= \case
          (Right _____, w) -> pure $ jobJSON  Nothing      w
          (Left except, w) -> pure $ jobJSON (Just except) w

     in do json <- toJson
           liftIO $ do
             putStr (A.encode json)
             putStr "\n"

  loop :: (StdCon.MonadResource m, A.ToJSON v) => E.ExceptT Error (W.WriterT [v] (C.Sink (Either Error v) m)) ()
  loop = lift (lift C.await) >>= \case
    Nothing             -> pure ()
    Just (Left except)  -> E.throwE except
    Just (Right update) -> lift (W.tell [update]) >> loop

  jobJSON :: A.ToJSON v => Maybe Error -> [v] -> A.Value
  jobJSON error' emitted =
    let (status, optional) = case error' of
          Nothing      -> ("ok"    :: T.Text, [])
          Just error'' -> ("error" :: T.Text, ["error" .= error''])
     in A.object (optional <> ["type" .= type', "status" .= status, "items" .= emitted])
