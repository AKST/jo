{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module JoScript.Util.Conduit ( characterStream
                             , printJsonCon
                             , ConduitE
                             , ResultConduit
                             , ResultSink
                             , Result
                             ) where

import Protolude hiding (sourceFile)
import Conduit (MonadResource, sourceFile)

import qualified Control.Monad.Trans.Except as E
import qualified Control.Monad.Trans.Writer as W

import Data.Aeson ((.=))
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Aeson as A
import Data.Conduit ((=$=), (.|), mapOutput)
import Data.Conduit.List (mapFoldable)
import qualified Data.Conduit as C
import qualified Data.Text.Lazy as LT

import JoScript.Data.Error (Error)
import JoScript.Data.Config (DebugMode(..), debugModeText, FileBuildM)
import qualified JoScript.Util.Json as Json
import qualified JoScript.Util.Text as TUtil


type ConduitE e i o m = C.ConduitM (Either e i) (Either e o) (FileBuildM m)

type Result a            = Either Error a
type ResultConduit i o m = C.ConduitM (Result i) (Result o) m
type ResultSink i      m = C.ConduitM (Result i) Void m


--characterStream :: MonadResource m => FilePath -> C.Source m (Either Error Std.Char)
characterStream f = mapOutput Right (sourceFile f .| mapFoldable LT.unpack)

type Tldr m v = (MonadResource m, A.ToJSON v)

printJsonCon :: Tldr m v => DebugMode -> C.Sink (Either Error v) m ()
printJsonCon (Debug type' pretty) = impl where

  impl =
    let toJson :: Tldr m v =>  C.Sink (Either Error v) m A.Value
        toJson = W.runWriterT (E.runExceptT loop) >>= \case
          (Right _____, w) -> pure $ jobJSON  Nothing      w
          (Left except, w) -> pure $ jobJSON (Just except) w

        config = A.defConfig { A.confIndent = A.Spaces 2, A.confNumFormat = A.Decimal }
        encode j = if pretty
          then A.encodePretty' config j
          else A.encode j

     in do json <- toJson
           liftIO $ do
             putStrLn (encode json)

  loop :: Tldr m v =>  E.ExceptT Error (W.WriterT [v] (C.Sink (Either Error v) m)) ()
  loop = lift (lift C.await) >>= \case
    Nothing             -> pure ()
    Just (Left except)  -> E.throwE except
    Just (Right update) -> lift (W.tell [update]) >> loop

  jobJSON :: A.ToJSON v => Maybe Error -> [v] -> A.Value
  jobJSON error' emitted =
    let (status, optional) = case error' of
          Nothing      -> ("ok"    :: Text, [])
          Just error'' -> ("error" :: Text, ["error" .= error''])
      in A.object (optional <>
            [ "type" .= debugModeText type'
            , "status" .= status
            , "items" .= emitted])

