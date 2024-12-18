--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Box.TWSize
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  'TWSize' and 'TWSizeOrFraction' are common patterns that exist among Tailwind
--  classes. There are a variety of uses such as width/height, padding and margin
--
--  As the names imply, TWSizeOrFraction is a superset containing the TWSize type
--
--  This also exposes a way to use CSS sizes, see 'CSSSize' and 'HasCSSSize' via
--  'TWSize'
--
--  Example use:
--
-- @
--  $(classh' [ padding . paddingT .~~ TWSize 8 ])
--  -- or with shorthand
--  $(classh' [ pt .~~ TWSize 8, w .~~ twSize' 8, mb .~~ pix 3 ])
-- @
--------------------------------------------------------------------------------



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

-- | Use a TWSize where the config is expecting a TWSizeOrFraction
twSize' :: Float -> TWSizeOrFraction
twSize' = TWSize' . TWSize


-- | The float component of classes like padding or margin or sizing
data TWSize
  = TWSize Float
  -- ^ TWSize x == "somePrefix-x"
  | TWSize_Custom CSSSize
  -- ^ Example output: pt-[3px] 
  deriving Show

instance ShowTW TWSize where
  showTW = \case
    TWSize float ->
      if fromIntegral (truncate float :: Int) == float
      then tshow $ (truncate float :: Int)
      else tshow float
    TWSize_Custom c -> "[" <> renderCSS c <> "]"

-- | https://tailwindcss.com/docs/width
-- | https://tailwindcss.com/docs/height
-- | etc
data TWSizeOrFraction
  = TWSize' TWSize
  -- ^ see TWSize 
  | TWFraction Int DivInt
  -- ^ Eg. w-11/12
  | TWSize_Full
  -- ^ == (h|w)-full
  | TWSize_Screen
  -- ^ (h|w)-screen
  | TWSize_Min
  -- ^ (h|w)-min
  | TWSize_Max
  -- ^ (h|w)-max 
  | TWSize_Fit
  -- ^ (h|w)-fit
  | TWSize_Auto
  -- ^ (h|w)-auto
  deriving Show

instance ShowTW TWSizeOrFraction where
  showTW = \case
    TWSize' s -> showTW s
    TWFraction n d -> tshow n <> "/" <> showTW d
    class' -> T.toLower . T.drop 7 . tshow $ class'

-- | > == TWSize_Auto
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
