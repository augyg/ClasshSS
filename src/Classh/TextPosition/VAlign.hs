module Classh.TextPosition.VAlign where


import Classh.Class.ShowTW
import Data.Default

-- Vertical Align
-- | TODO: Add Sub and Super values as Types accessed via TextConfigTW
data VAlign
  = TA_Base
  | TA_Top
  | TA_Mid
  | TA_Bottom
  | TA_TextTop
  | TA_TextBottom
  | Sub
  | Super
  deriving (Show, Eq)


instance Default VAlign where
  def = TA_Base


-- VAlign
instance ShowTW VAlign where
  showTW = \case
    TA_Base       -> "align-baseline"
    TA_Top        -> "align-top"
    TA_Mid        -> "align-middle"
    TA_Bottom     -> "align-bottom"
    TA_TextTop    -> "align-text-top"
    TA_TextBottom -> "align-text-bottom"
    Sub        -> "align-sub"
    Super      -> "align-super"

  
