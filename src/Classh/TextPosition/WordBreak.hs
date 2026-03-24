-- |
-- Module      : Classh.TextPosition.WordBreak
-- Description : Word break types
-- Copyright   : (c) Galen Sprout, 2024
-- License     : MIT
-- Maintainer  : galen.sprout@gmail.com

module Classh.TextPosition.WordBreak where

import Classh.Class.ShowTW (ShowTW(..))
import Data.Default (Default(..))


-- Word Break (again, could alias Wrap)
data WordBreak
  = BNormal
  | BWords
  | BAll
  | BKeep
  deriving (Show, Eq)


instance Default WordBreak where
  def = BNormal


instance ShowTW WordBreak where
  showTW = \case
    BNormal -> "break-normal"
    BWords  -> "break-words"
    BAll    -> "break-all"
    BKeep   -> "break-keep"
