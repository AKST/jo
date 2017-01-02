{-# LANGUAGE OverloadedStrings #-}
module JoScript.Util.Debug (PassOut(..), PassDebug(..)) where

import Data.Sequence (Seq)
import Data.Maybe (Maybe(..), maybe)
import Data.Text (Text)
import Data.Monoid ((<>))
import Data.Aeson ((.=))
import qualified Data.Aeson as A

import System.IO (FilePath)

import JoScript.Data.Error (Error)
import JoScript.Data.LexerPass (LexerPass)
import JoScript.Data.BlockPass (BlockPass)
import JoScript.Data.Syntax (SynModule)
import JoScript.Data.Config (DebugKind(..), debugModeText)

data PassOut = POutLexer (Seq LexerPass)
             | POutBlock (Seq BlockPass)
             | POutSyntax SynModule

mode (POutBlock _)  = DebugTextBlock
mode (POutLexer _)  = DebugTextLexer
mode (POutSyntax _) = DebugTextParse

data PassDebug
  = PassDebug { file :: FilePath
              , output :: PassOut
              , error :: Maybe Error }

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
