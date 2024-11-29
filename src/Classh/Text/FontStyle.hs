module Classh.Text.FontStyle where

import Classh.Class.ShowTW
import Classh.Internal.Utils

import Data.Default
import qualified Data.Text as T


instance Default FontStyle where
  def = NotItalic

instance ShowTW FontStyle where
  showTW font = T.pack $ toKebabCase (show font)

data FontStyle
  = Italic
  | NotItalic
  deriving Show
