{-# LANGUAGE RankNTypes #-}

--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Shorthand
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  A collection of shorthand ways to compress the size of tailwind setters.
--  Almost all are based on the characters used in tailwind. For example, 'pt', 'pb', 'pr',
--  and 'pl' all exist in tailwind, such as pl-5
--
--  If you are just getting started with Classh, I'd recommend reasoning from the 'BoxConfig' type
--  and 'TextConfigTW' first. 
--
--  This package aims to avoid forcing the user to know lenses
--
--  Example use:
--
-- @
--  $(classh' [ border . bWidth . borderWidth_t .~~ B2 ])
-- @
--------------------------------------------------------------------------------


module Classh.Shorthand where

import Classh.Text
import Classh.Box
import Classh.Setters
import Classh.Class.SetSides

import Control.Lens (Lens')
-- this is purely semantic compression for those familiar with Classh

type Setter a b = Lens' a b

-- | Set border radius side(s)
br_r, br_l, br_t, br_b, br_y, br_x, br :: Setter BoxConfig (WhenTW BorderRadius')
br_r = border . radius . r 
br_l = border . radius . l
br_t = border . radius . t
br_b = border . radius . b
br_y = border . radius . y
br_x = border . radius . x
br = border . radius . allS 

-- | Set border width side(s)
bw_r, bw_l, bw_t, bw_b, bw_y, bw_x, bw :: Setter BoxConfig (WhenTW BorderWidth)
bw_r = border . bWidth . r 
bw_l = border . bWidth . l
bw_t = border . bWidth . t
bw_b = border . bWidth . b
bw_y = border . bWidth . y
bw_x = border . bWidth . x
bw = border . bWidth . allS 

-- | Set border color side(s)
bc_r, bc_l, bc_t, bc_b, bc_y, bc_x, bc :: Setter BoxConfig (WhenTW Color)
bc_r = border . bColor . r 
bc_l = border . bColor . l
bc_t = border . bColor . t
bc_b = border . bColor . b
bc_y = border . bColor . y
bc_x = border . bColor . x
bc = border . bColor . allS 

-- | pos == position
pos :: Setter BoxConfig (WhenTW (Justify, Align))
pos = position

-- | Set width
width' :: Setter BoxConfig (WhenTW TWSizeOrFraction)
width' = sizingBand . size . width
-- | Set width
w :: Setter BoxConfig (WhenTW TWSizeOrFraction)
w = width'
-- | Set height
height' :: Setter BoxConfig (WhenTW TWSizeOrFraction)
height' = sizingBand . size . height
-- | Set height
h :: Setter BoxConfig (WhenTW TWSizeOrFraction)
h = height'

-- | Set BoxConfig max width 
maxW :: Setter BoxConfig (WhenTW DimensionConstraint)
maxW = sizingBand . maxSize . widthC
-- | Set BoxConfig min width
minW :: Setter BoxConfig (WhenTW DimensionConstraint)
minW = sizingBand . minSize . widthC
-- | Set BoxConfig max height
maxH :: Setter BoxConfig (WhenTW DimensionConstraint)
maxH = sizingBand . maxSize . heightC
-- | Set BoxConfig min height
minH :: Setter BoxConfig (WhenTW DimensionConstraint)
minH = sizingBand . minSize . heightC 

-- | Set margin on a given side(s)
mt, ml, mr, mb, mx, my, m :: Setter BoxConfig (WhenTW TWSize)
mt = margin . t
mb = margin . b
ml = margin . l
mr = margin . r
mx = margin . x
my = margin . y
m = margin . allS

-- | Set padding on a given side(s)
pt, pl, pr, pb, px, py, p :: Setter BoxConfig (WhenTW TWSize)
pt = padding . t
pb = padding . b
pl = padding . l
pr = padding . r
px = padding . x
py = padding . y
p = padding . allS

-- TODO: move to Classh.Constants
-- | Set text to italic 
t_italic :: TextConfigTW -> TextConfigTW 
t_italic = text_style .~~ Italic

denom12, denom6, denom5, denom4, denom3, denom2 :: Int -> TWSizeOrFraction
denom12 = div12
denom6 = div6
denom5 = div5
denom4 = div4
denom3 = div3
denom2 = div2

div12 :: Int -> TWSizeOrFraction
div12 = flip TWFraction D12

div6 :: Int -> TWSizeOrFraction
div6 = flip TWFraction D6

div5 :: Int -> TWSizeOrFraction
div5 = flip TWFraction D5

div4 :: Int -> TWSizeOrFraction
div4 = flip TWFraction D4

div3 :: Int -> TWSizeOrFraction
div3 = flip TWFraction D3

div2 :: Int -> TWSizeOrFraction
div2 = flip TWFraction D2
