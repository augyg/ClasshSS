module Classh.TextPosition.Wrap where

import Classh.Class.ShowTW
import Data.Default



-- Text Wrap / Word Break
data Wrap
  = Wrap
  | NoWrap
  | Balance
  | Pretty
  deriving (Show, Eq)

instance Default Wrap where
  def = Wrap

instance ShowTW Wrap where
  showTW = \case
    Wrap    -> "break-normal"
    NoWrap  -> "break-keep"
    Balance -> "text-balance"
    Pretty  -> "text-pretty"
