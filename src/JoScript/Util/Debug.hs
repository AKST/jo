{-# LANGUAGE OverloadedStrings #-}
module JoScript.Util.Debug
  ( PassOut(..)
  , PassDebug(..)
  , mode
  , printPass
  , consumeSyntax
  , consumeBlockPass
  , consumeLexerPass
) where

import Protolude hiding (ByteString)

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

data PassOut
  = POutLexer (Seq LexerPass)
  | POutBlock (Seq BlockPass)
  | POutSyntax (Maybe SynModule)
  deriving (Eq, Show)

data PassDebug = PassDebug { file :: FilePath, output :: PassOut, error :: Maybe Error }
  deriving (Eq, Show)

--------------------------------------------------------------
--                          exports                         --
--------------------------------------------------------------

type Impl m v a = ExceptT Error (WriterT (Seq v) (C.Sink (Result v) m)) a

consumePass' :: Monad m => (Seq a -> PassOut) -> FilePath -> C.Sink (Result a) m PassDebug
consumePass' fn filename = runImpl impl where
  runImpl i = runWriterT (runExceptT i) >>= \(error, written) ->
    pure (PassDebug filename (fn written) (leftToMaybe error))

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

consumeBlockPass :: Monad m => FilePath -> C.Sink (Result BlockPass) m PassDebug
consumeBlockPass = consumePass' POutBlock

consumeLexerPass :: Monad m => FilePath -> C.Sink (Result LexerPass) m PassDebug
consumeLexerPass = consumePass' POutLexer

consumeSyntax :: Monad m => FilePath -> Result SynModule -> m PassDebug
consumeSyntax file result = pure (PassDebug { file, error, output }) where
  error  = leftToMaybe result
  output = POutSyntax (rightToMaybe result)

printPass :: MonadIO m => Bool -> PassDebug -> m ()
printPass pretty pass = putStrLn (encode pass) where
  encode :: PassDebug -> ByteString
  encode = if pretty then A.encodePretty' config else A.encode

  config = A.defConfig { A.confIndent = A.Spaces 2, A.confNumFormat = A.Decimal }

--------------------------------------------------------------
--                         instances                        --
--------------------------------------------------------------

instance A.ToJSON PassDebug where
  toJSON PassDebug{..} = A.object ["file" .= file, "status" .= status, "data" .= output']
    where status = A.object (maybe ["type" .= ok] withErr error) where
            withErr e = ["type" .= err, "error" .= e]
            ok        = "ok" :: Text
            err       = "error" :: Text
          output' = A.object ["repr" .= outData, "type" .= debugModeText (mode output)] where
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

