{-# LANGUAGE OverloadedStrings #-}
module JoScript.Util.Debug
  ( PassOut(..)
  , PassDebug(..)
  , mode
  , printPass
  , consumeBlockPass
  , consumeLexerPass
) where

import Protolude hiding (ByteString)
import Conduit (MonadResource)

import Control.Monad.Trans.Writer (WriterT, tell, runWriterT)

import Data.Aeson ((.=))
import Data.Sequence (singleton)
import Data.ByteString.Lazy.Internal (ByteString)
import qualified Data.Conduit as C
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A

import JoScript.Data.Error (Error)
import JoScript.Data.LexerPass (LexerPass)
import JoScript.Data.BlockPass (BlockPass)
import JoScript.Data.Syntax (SynModule)
import JoScript.Data.Config (DebugKind(..), debugModeText)
import JoScript.Util.Conduit (Result)

data PassOut
  = POutLexer (Seq LexerPass)
  | POutBlock (Seq BlockPass)
  | POutSyntax SynModule
  deriving (Eq, Show)

data PassDebug = PassDebug { file :: FilePath, output :: PassOut, error :: Maybe Error }
  deriving (Eq, Show)

--------------------------------------------------------------
--                          exports                         --
--------------------------------------------------------------

type Impl m v a = ExceptT Error (WriterT (Seq v) (C.Sink (Result v) m)) a

consumePass' :: MonadResource m => (Seq a -> PassOut) -> FilePath -> C.Sink (Result a) m PassDebug
consumePass' fn filename = runImpl impl where
  runImpl i = runWriterT (runExceptT i) >>= \(error, written) ->
    pure (PassDebug filename (fn written) (leftToMaybe error))

  await :: MonadResource m => Impl m a (Maybe (Result a))
  await = lift (lift C.await)

  record :: MonadResource m => a -> Impl m a ()
  record item = lift (tell (singleton item))

  throw :: MonadResource m => Error -> Impl m a ()
  throw = throwError

  impl :: MonadResource m => Impl m a ()
  impl = await >>= \case
    Nothing        -> pure ()
    Just (Left  e) -> throw e
    Just (Right i) -> record i >> impl

consumeBlockPass :: MonadResource m => FilePath -> C.Sink (Result BlockPass) m PassDebug
consumeBlockPass = consumePass' POutBlock

consumeLexerPass :: MonadResource m => FilePath -> C.Sink (Result LexerPass) m PassDebug
consumeLexerPass = consumePass' POutLexer

printPass :: MonadIO m => Bool -> PassDebug -> m ()
printPass pretty pass = putStrLn (encode pass) where
  encode :: PassDebug -> ByteString
  encode = if pretty then A.encode else A.encodePretty' config

  config = A.defConfig { A.confIndent = A.Spaces 2, A.confNumFormat = A.Decimal }

--------------------------------------------------------------
--                         instances                        --
--------------------------------------------------------------

instance A.ToJSON PassDebug where
  toJSON PassDebug{..} = A.object ["file" .= file, "status" .= status, "output" .= output']
    where status = A.object (maybe ["type" .= ok] withErr error) where
            withErr e = ["type" .= err, "error" .= e]
            ok        = "ok" :: Text
            err       = "error" :: Text
          output' :: A.Value
          output' = A.object ["data" .= outData, "type" .= debugModeText (mode output)] where
            outData = case output of
              POutBlock output  -> A.toJSON output
              POutLexer output  -> A.toJSON output
              POutSyntax output -> A.toJSON output

--------------------------------------------------------------
--                        Util func                         --
--------------------------------------------------------------

mode (POutBlock _)  = DebugTextBlock
mode (POutLexer _)  = DebugTextLexer
mode (POutSyntax _) = DebugTextParse

