{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
module JoScript.Util.Debug
  ( PassDebug(..)
  , FileDebug(..)
  , mode
  , printPass
  , consumeSyntax
  , consumeBlockPass
  , consumeLexerPass
) where

import Protolude hiding (ByteString, error)

import Control.Lens (view)
import Control.Monad.Trans.Writer (WriterT, tell, runWriterT)

import Data.Aeson ((.=))
import Data.Sequence (singleton)
import Data.ByteString.Lazy.Internal (ByteString)
import qualified Data.Conduit as C
import qualified Data.Aeson as A
import qualified Data.Aeson.Encode.Pretty as A

import JoScript.Data.Error (Error)
import JoScript.Data.Lexer (LexerPass)
import JoScript.Data.Block (BlockPass)
import JoScript.Data.Syntax (SynModule)
import JoScript.Data.Config (DebugKind(..), debugModeText, FileBuildM(..), filename')
import JoScript.Util.Conduit (Result, ResultSink)

data PassDebug
  = PDebugLexer (Seq LexerPass)
  | PDebugBlock (Seq BlockPass)
  | PDebugSyntax (Maybe SynModule)
  deriving (Eq, Show)

data FileDebug = FileDebug { file :: Text, output :: PassDebug, error :: Maybe Error }
  deriving (Eq, Show)

--------------------------------------------------------------
--                          exports                         --
--------------------------------------------------------------

type Impl m v a = ExceptT Error (WriterT (Seq v) (C.Sink (Result v) m)) a

consumePass' :: Monad m => (Seq a -> PassDebug) -> ResultSink a m FileDebug
consumePass' fn = runImpl impl where
  runImpl i = runWriterT (runExceptT i) >>= \(error, written) -> do
    filename <- lift (view filename')
    pure (FileDebug filename (fn written) (leftToMaybe error))

  await :: Monad m => Impl m a (Maybe (Result a))
  await = lift (lift C.await)

  record :: a -> Impl m a ()
  record item = lift (tell (singleton item))

  throw :: Error -> Impl m a ()
  throw = throwError

  impl :: Monad m => Impl m a ()
  impl = await >>= \case
    Nothing        -> pure ()
    Just (Left  e) -> throw e
    Just (Right i) -> record i >> impl

consumeBlockPass :: Monad m => ResultSink BlockPass m FileDebug
consumeBlockPass = consumePass' PDebugBlock

consumeLexerPass :: Monad m => ResultSink LexerPass m FileDebug
consumeLexerPass = consumePass' PDebugLexer

consumeSyntax :: Monad m => Result SynModule -> FileBuildM m FileDebug
consumeSyntax result = impl where
  output = PDebugSyntax (rightToMaybe result)
  error  = leftToMaybe result
  impl = view filename' >>= \file ->
    pure (FileDebug { file, error, output })

printPass :: MonadIO m => Bool -> FileDebug -> m ()
printPass pretty filedebug = putStrLn (encode filedebug) where
  encode :: FileDebug -> ByteString
  encode = if pretty then A.encodePretty' config else A.encode

  config = A.defConfig { A.confIndent = A.Spaces 2, A.confNumFormat = A.Decimal }

--------------------------------------------------------------
--                         instances                        --
--------------------------------------------------------------

instance A.ToJSON FileDebug where
  toJSON FileDebug{..} = A.object ["file" .= file, "status" .= status, "data" .= output']
    where status = A.object (maybe ["type" .= ok] withErr error) where
            withErr e = ["type" .= err, "error" .= e]
            ok        = "ok" :: Text
            err       = "error" :: Text
          output' = A.object ["repr" .= outData, "type" .= debugModeText (mode output)] where
            outData = case output of
              PDebugBlock o  -> A.toJSON o
              PDebugLexer o  -> A.toJSON o
              PDebugSyntax o -> A.toJSON o

--------------------------------------------------------------
--                        Util func                         --
--------------------------------------------------------------

mode :: PassDebug -> DebugKind
mode (PDebugBlock _)  = DebugTextBlock
mode (PDebugLexer _)  = DebugTextLexer
mode (PDebugSyntax _) = DebugTextParse

