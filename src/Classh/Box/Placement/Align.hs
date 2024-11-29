module Classh.Box.Placement.Align where

import Classh.Class.ShowTW

import Data.Default
import qualified Data.Text as T 

instance Default Align where
  def = A_Start

instance ShowTW Align where
  showTW = (<>) "content-" . T.toLower . T.drop 2 . T.pack . show

data Align
  = A_Start
  | A_End
  | A_Center
  | A_Baseline
  deriving Show
