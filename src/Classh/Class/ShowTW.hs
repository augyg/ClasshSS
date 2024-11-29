module Classh.Class.ShowTW where

import qualified Data.Text as T

class ShowTW tw where
  showTW :: tw -> T.Text
