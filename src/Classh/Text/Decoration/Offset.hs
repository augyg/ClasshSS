module Classh.Text.Decoration.Offset where

import Classh.Class.ShowTW
import Classh.Class.IsCSS
import Classh.Internal.CSSSize
import Classh.Internal.TWNum

import Data.Default

instance Default TextDecOffset where
  def = TextDecOffset def

instance ShowTW TextDecOffset where
  showTW (TextDecOffset tnum) = "underline-offset-" <> (showTW tnum)
  showTW (TextDecOffset_Custom cssS) = "underline-offset-[" <> (renderCSS cssS) <> "]"
  --showTWWhen c tw = c <> ":" <> (showTW tw)

data TextDecOffset
  = TextDecOffset TWNum
  | TextDecOffset_Custom CSSSize
  deriving Show
