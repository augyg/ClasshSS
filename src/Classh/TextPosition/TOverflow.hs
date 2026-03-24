-- |
-- Module      : Classh.TextPosition.TOverflow
-- Description : Text overflow types
-- Copyright   : (c) Galen Sprout, 2024
-- License     : MIT
-- Maintainer  : galen.sprout@gmail.com

module Classh.TextPosition.TOverflow where


import Classh.Class.ShowTW (ShowTW(..))
import Data.Default (Default(..))



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
