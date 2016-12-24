{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
module JoScript.Util.Conduit ( characterStream
                             , printAsJSON
                             ) where

import qualified Prelude as Std
import qualified Conduit as StdCon

import Control.Applicative (pure)
import Control.Monad (Monad, (>>=), (>>))
import Control.Monad.IO.Class (MonadIO, liftIO)

import Data.Void (Void)
import Data.Conduit ((=$=), (.|))
import Data.Conduit.List (mapFoldable)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))
import Data.ByteString.Lazy (putStr)
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.Aeson as A
import qualified Data.Conduit as C
import qualified Data.Maybe as Maybe

import System.IO (FilePath)

import JoScript.Data.Error (Error)
import qualified JoScript.Util.Text as TUtil

characterStream :: StdCon.MonadResource m => FilePath -> C.Source m Std.Char
characterStream f = StdCon.sourceFile f .| mapFoldable LT.unpack


printAsJSON :: (StdCon.MonadResource m, A.ToJSON v) => C.Sink (Either Error v) m ()
printAsJSON = C.await >>= withUpdate where

  withUpdate Nothing = pure ()
  withUpdate (Just (Right r)) = print r >> printAsJSON
  withUpdate (Just (Left e))  = print e >> printAsJSON

  print d = liftIO (putStr (A.encode d) >> putStr "\n")

