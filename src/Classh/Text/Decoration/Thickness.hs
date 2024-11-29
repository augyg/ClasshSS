module Classh.Text.Decoration.Thickness where

import Classh.Class.ShowTW
import Classh.Class.IsCSS

import Classh.Internal.TWNum as X
import Classh.Internal.CSSSize as X

import Data.Default

instance Default TextDecThickness where
  def = TextDecThickness def

instance ShowTW TextDecThickness where
  showTW thk = "decoration-" <> ( case thk of
                                    FromFont -> "from-font"
                                    TextDecThickness twnum -> showTW twnum
                                    TextDecThickness_Custom cssSize -> "[" <> (renderCSS cssSize) <> "]"
                                )
               
data TextDecThickness
  = FromFont
  | TextDecThickness TWNum
  | TextDecThickness_Custom CSSSize -- TODO: is this just size??
  deriving Show
