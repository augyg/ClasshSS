module Classh.TextPosition.WhiteSpace where

import Classh.Class.ShowTW
import Data.Default


-- Whitespace
data WhiteSpace
  = WS
  | WS_NoWrap
  | WS_Pre
  | WS_Preline
  | WS_PreWrap
  | WS_Break
  deriving (Show, Eq)

instance Default WhiteSpace where
  def = WS

instance ShowTW WhiteSpace where
  showTW = \case
    WS         -> "whitespace-normal"
    WS_NoWrap  -> "whitespace-nowrap"
    WS_Pre     -> "whitespace-pre"
    WS_Preline -> "whitespace-pre-line"
    WS_PreWrap -> "whitespace-pre-wrap"
    WS_Break   -> "whitespace-break-spaces"
