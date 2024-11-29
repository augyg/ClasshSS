module Classh.Box.TWSize
  ( module X
  , twSize'
  , TWSize(..)
  , TWSizeOrFraction(..)
  ) where

import Classh.Class.HasCSSSize 
import Classh.Class.ShowTW
import Classh.Class.IsCSS
import Classh.Internal.TShow

import Classh.Internal.CSSSize as X
import Classh.Box.DivInt as X

import Data.Default
import qualified Data.Text as T

twSize' :: Float -> TWSizeOrFraction
twSize' = TWSize' . TWSize


-- there's TWSize and then TWSizeOrFraction
data TWSize
  = TWSize Float
  | TWSize_Custom CSSSize
  deriving Show

instance ShowTW TWSize where
  showTW = \case
    TWSize float ->
      if fromIntegral (truncate float :: Int) == float
      then tshow $ (truncate float :: Int)
      else tshow float
    TWSize_Custom c -> "[" <> renderCSS c <> "]"

data TWSizeOrFraction
  = TWSize' TWSize
  | TWFraction Int DivInt
  | TWSize_Full
  | TWSize_Screen
  | TWSize_Min
  | TWSize_Max
  | TWSize_Fit
  | TWSize_Auto
  deriving Show

instance ShowTW TWSizeOrFraction where
  showTW = \case
    TWSize' s -> showTW s
    TWFraction n d -> tshow n <> "/" <> showTW d
    class' -> T.toLower . T.drop 7 . tshow $ class'

instance Default TWSizeOrFraction where
  def = TWSize_Auto

instance HasCSSSize TWSize where
  pix = TWSize_Custom . Pixel
  pct = TWSize_Custom . Percent
  vh = TWSize_Custom . Vh
  vw = TWSize_Custom . Vw
  rem = TWSize_Custom . Rem

instance HasCSSSize TWSizeOrFraction where
  pix = TWSize' . TWSize_Custom . Pixel --px
  pct = TWSize' . TWSize_Custom . Percent --pct
  vh = TWSize' . TWSize_Custom . Vh --vh
  vw = TWSize' . TWSize_Custom . Vw--vw
  rem = TWSize' . TWSize_Custom . Rem -- Classh.rem
