module Classh.Text.Decoration.Style where

import Classh.Class.ShowTW
import Classh.Internal.TShow

import Data.Default
import qualified Data.Text as T

instance Default TextDecStyle where
  def = Solid

instance ShowTW TextDecStyle where
  showTW = ((<>) "decoration-") . T.toLower . tshow

data TextDecStyle
  = Solid
  | Double
  | Dotted
  | Dashed
  | Wavy
  deriving Show
