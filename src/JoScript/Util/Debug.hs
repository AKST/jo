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
import JoScript.Data.Config (DebugKind(..), debugModeText)
import JoScript.Util.Conduit (Result)

data PassDebug
  = PDebugLexer (Seq LexerPass)
  | PDebugBlock (Seq BlockPass)
  | PDebugSyntax (Maybe SynModule)
  deriving (Eq, Show)

data FileDebug = FileDebug { file :: FilePath, output :: PassDebug, error :: Maybe Error }
  deriving (Eq, Show)

--------------------------------------------------------------
--                          exports                         --
--------------------------------------------------------------

type Impl m v a = ExceptT Error (WriterT (Seq v) (C.Sink (Result v) m)) a

consumePass' :: Monad m => (Seq a -> PassDebug) -> FilePath -> C.Sink (Result a) m FileDebug
consumePass' fn filename = runImpl impl where
  runImpl i = runWriterT (runExceptT i) >>= \(error, written) ->
    pure (FileDebug filename (fn written) (leftToMaybe error))

  await :: Monad m => Impl m a (Maybe (Result a))
  await = lift (lift C.await)

  record :: Monad m => a -> Impl m a ()
  record item = lift (tell (singleton item))

  throw :: Monad m => Error -> Impl m a ()
  throw = throwError

  impl :: Monad m => Impl m a ()
  impl = await >>= \case
    Nothing        -> pure ()
    Just (Left  e) -> throw e
    Just (Right i) -> record i >> impl

consumeBlockPass :: Monad m => FilePath -> C.Sink (Result BlockPass) m FileDebug
consumeBlockPass = consumePass' PDebugBlock

consumeLexerPass :: Monad m => FilePath -> C.Sink (Result LexerPass) m FileDebug
consumeLexerPass = consumePass' PDebugLexer

consumeSyntax :: Monad m => FilePath -> Result SynModule -> m FileDebug
consumeSyntax file result = pure (FileDebug { file, error, output }) where
  error  = leftToMaybe result
  output = PDebugSyntax (rightToMaybe result)

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

