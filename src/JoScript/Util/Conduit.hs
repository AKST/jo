{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module JoScript.Util.Conduit ( characterStream
                             , printJsonCon
                             , ConduitE
                             , ResultConduit
                             , ResultSink
                             , Result
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
import qualified Data.Aeson.Encode.Pretty as A
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
import JoScript.Data.Config (DebugMode(..), debugModeText, FileBuildM)
import qualified JoScript.Util.Debug as Debug
import qualified JoScript.Util.Json as Json
import qualified JoScript.Util.Text as TUtil


type ConduitE e i o m = C.ConduitM (Either e i) (Either e o) (FileBuildM m)

type Result a            = Either Error a
type ResultConduit i o m = C.ConduitM (Result i) (Result o) m
type ResultSink i      m = C.ConduitM (Result i) Void m


--characterStream :: StdCon.MonadResource m => FilePath -> C.Source m (Either Error Std.Char)
characterStream f = mapOutput Right (StdCon.sourceFile f .| mapFoldable LT.unpack)

type Tldr m v = (StdCon.MonadResource m, A.ToJSON v)

printJsonCon :: (StdCon.MonadResource m, A.ToJSON v) => DebugMode -> C.Sink (Either Error v) m ()
printJsonCon (Debug type' pretty) = impl where

  impl =
    let toJson :: (StdCon.MonadResource m, A.ToJSON v) => C.Sink (Either Error v) m A.Value
        toJson = W.runWriterT (E.runExceptT loop) >>= \case
          (Right _____, w) -> pure $ jobJSON  Nothing      w
          (Left except, w) -> pure $ jobJSON (Just except) w

        config = A.defConfig { A.confIndent = A.Spaces 2, A.confNumFormat = A.Decimal }
        encode j = if pretty
          then A.encodePretty' config j
          else A.encode j

     in do json <- toJson
           liftIO $ do
             putStr (encode json)
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
      in A.object (optional <>
            [ "type" .= debugModeText type'
            , "status" .= status
            , "items" .= emitted])

