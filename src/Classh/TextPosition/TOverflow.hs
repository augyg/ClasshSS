module Classh.TextPosition.TOverflow where


import Classh.Class.ShowTW
import Data.Default



-- Text Overflow
data TOverflow
  = TClip
  | TEllipsis
  | Truncate
  deriving (Show, Eq)

instance Default TOverflow where
  def = TClip

instance ShowTW TOverflow where
  showTW = \case
    TClip     -> "text-clip"
    TEllipsis -> "text-ellipsis"
    Truncate  -> "truncate"
