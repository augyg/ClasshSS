module Classh.Shorthand where

import Classh.Text
import Classh.Box
import Classh.Setters
import Classh.Class.SetSides

import Control.Lens (Lens')
-- this is purely semantic compression for those familiar with Classh


br_r, br_l, br_t, br_b, br_y, br_x, br :: Lens' BoxConfig (WhenTW BorderRadius')
br_r = border . radius . r 
br_l = border . radius . l
br_t = border . radius . t
br_b = border . radius . b
br_y = border . radius . y
br_x = border . radius . x
br = border . radius . allS 


bw_r, bw_l, bw_t, bw_b, bw_y, bw_x, bw :: Lens' BoxConfig (WhenTW BorderWidth)
bw_r = border . bWidth . r 
bw_l = border . bWidth . l
bw_t = border . bWidth . t
bw_b = border . bWidth . b
bw_y = border . bWidth . y
bw_x = border . bWidth . x
bw = border . bWidth . allS 


bc_r, bc_l, bc_t, bc_b, bc_y, bc_x, bc :: Lens' BoxConfig (WhenTW Color)
bc_r = border . bColor . r 
bc_l = border . bColor . l
bc_t = border . bColor . t
bc_b = border . bColor . b
bc_y = border . bColor . y
bc_x = border . bColor . x
bc = border . bColor . allS 



-- | If I want to make this not undefined, then I need pos to be an
-- | actual field, which contains Justify and Align as its own record fields
pos :: Lens' BoxConfig (WhenTW (Justify, Align))
pos = position --lens undefined $ \cfg new -> cfg { _position =  new }


-- pos :: Lens' BoxConfig (Justify, Align)
-- pos = position . _Just
width', height', w, h :: Lens' BoxConfig (WhenTW TWSizeOrFraction)
width' = sizingBand . size . width
height' = sizingBand . size . height
w = width'
h = height'

minH, maxH, minW, maxW :: Lens' BoxConfig (WhenTW DimensionConstraint)
maxW = sizingBand . maxSize . widthC
minW = sizingBand . minSize . widthC
maxH = sizingBand . maxSize . heightC
minH = sizingBand . minSize . heightC 

mt, ml, mr, mb, mx, my, m :: Lens' BoxConfig (WhenTW TWSize)
mt = margin . t
mb = margin . b
ml = margin . l
mr = margin . r
mx = margin . x
my = margin . y
m = margin . allS


pt, pl, pr, pb, px, py, p :: Lens' BoxConfig (WhenTW TWSize)
pt = padding . t
pb = padding . b
pl = padding . l
pr = padding . r
px = padding . x
py = padding . y
p = padding . allS

t_italic :: TextConfigTW -> TextConfigTW 
t_italic = text_style .~~ Italic
