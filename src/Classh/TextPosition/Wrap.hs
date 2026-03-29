-- |
-- Module      : Classh.TextPosition.Wrap
-- Description : Text wrap types
-- Copyright   : (c) Galen Sprout, 2024
-- License     : MIT
-- Maintainer  : galen.sprout@gmail.com

module Classh.TextPosition.Wrap where

import Classh.Class.ShowTW (ShowTW(..))
import Data.Default (Default(..))



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
