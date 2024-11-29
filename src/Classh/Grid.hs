module Classh.Grid where

import Classh.Class.ShowTW

import Data.Default
import qualified Data.Text as T

-- | TODO: Isolate in own module since only used for gridCol
data ColInt
  = Col1
  | Col2
  | Col3
  | Col4
  | Col5
  | Col6
  | Col7
  | Col8
  | Col9
  | Col10
  | Col11
  | Col12
  deriving Show

instance ShowTW ColInt where
  showTW = T.drop 3 . T.pack . show

instance Default ColInt where
  def = Col1
