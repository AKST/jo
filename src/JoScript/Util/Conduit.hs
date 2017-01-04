{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module JoScript.Util.Conduit ( characterStream
                             , ConduitE
                             , ResultConduit
                             , ResultSink
                             , Result
                             ) where

import Protolude hiding (sourceFile)
import Conduit (MonadResource, ResourceT, sourceFile)

import Data.Conduit ((.|), mapOutput)
import Data.Conduit.List (mapFoldable)
import qualified Data.Text as ST
import qualified Data.Conduit as C
import qualified Data.Text.Lazy as LT

import Control.Lens (view)

import JoScript.Data.Error (Error)
import JoScript.Data.Config (FileBuildM, FileBuildBase, filename')


type Inner          m = ResourceT (FileBuildM m)
type ConduitE e i o m = C.ConduitM (Either e i) (Either e o) (Inner m)

type Result a            = Either Error a
type ResultConduit i o m = C.ConduitM (Result i) (Result o) (Inner m)
type ResultSource    o m = C.ConduitM () (Result o) (Inner m)
type ResultSink i      m = C.ConduitM (Result i) Void (Inner m)

characterStream :: MonadResource m => ResultSource Char m ()
characterStream = do
  f <- ST.unpack <$> view filename'
  mapOutput Right (sourceFile f .| mapFoldable LT.unpack)

