{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module JoScript.Util.Conduit ( characterStream
                             , ConduitE
                             , ResultConduit
                             , ResultSink
                             , Result
                             ) where

import Protolude hiding (sourceFile)
import Conduit (MonadResource, sourceFile)

import Data.Conduit ((.|), mapOutput)
import Data.Conduit.List (mapFoldable)
import qualified Data.Conduit as C
import qualified Data.Text.Lazy as LT

import JoScript.Data.Error (Error)
import JoScript.Data.Config (FileBuildM)


type ConduitE e i o m = C.ConduitM (Either e i) (Either e o) (FileBuildM m)

type Result a            = Either Error a
type ResultConduit i o m = C.ConduitM (Result i) (Result o) m
type ResultSink i      m = C.ConduitM (Result i) Void m

characterStream :: MonadResource m => FilePath -> C.Source m (Either Error Char)
characterStream f = mapOutput Right (sourceFile f .| mapFoldable LT.unpack)

