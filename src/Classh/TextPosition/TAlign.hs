module Classh.TextPosition.TAlign where

import Classh.Class.ShowTW
import Data.Default


-- Text Align
data TAlign
  = TA_Left
  | TA_Center
  | TA_Right
  | TA_Justify
  | TA_Start
  | TA_End
  deriving (Show, Eq)


instance Default TAlign where
  def = TA_Left

instance ShowTW TAlign where
  showTW = \case
    TA_Left    -> "text-left"
    TA_Center  -> "text-center"
    TA_Right   -> "text-right"
    TA_Justify -> "text-justify"
    TA_Start   -> "text-start"
    TA_End     -> "text-end"


