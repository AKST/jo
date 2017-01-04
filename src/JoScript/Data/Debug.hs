{-# LANGUAGE OverloadedStrings #-}
module JoScript.Data.Debug where

import Protolude hiding (ByteString, error)

import Data.Aeson ((.=))
import qualified Data.Aeson as A

import JoScript.Data.Error (Error)
import JoScript.Data.Lexer (LexerPass)
import JoScript.Data.Block (BlockPass)
import JoScript.Data.Syntax (SynModule)
import JoScript.Data.Config (DebugKind(..), debugModeText, FileBuildM(..), filename')

data PassDebug
  = PDebugLexer (Seq LexerPass)
  | PDebugBlock (Seq BlockPass)
  | PDebugSyntax (Maybe SynModule)
  deriving (Eq, Show)

data FileDebug = FileDebug { file :: Text, output :: PassDebug, error :: Maybe Error }
  deriving (Eq, Show)

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

