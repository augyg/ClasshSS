module Classh.TextPosition.WordBreak where

import Classh.Class.ShowTW
import Data.Default


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
