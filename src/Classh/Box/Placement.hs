--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Box.Placement
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Types to represent tailwind horizontal positioning (justification) for use by
--  the 'position' field of 'BoxConfig', this uses grid display
--
--  Classh only exposes a way to position an element's children in order to avoid
--  clashing position between 'self-positioning' and positioning by this same
--  element's parent
--
--  By choice this package chooses to hold vertical and horizontal position as
--  one singular (WhenTW a), so the position field is (WhenTW (Justify,Align)) 
--
--  Example use:
--
-- @
--  $(classh' [ position .~~ (A_Center, J_Center) ])
--  -- or with shorthand
--  $(classh' [ pos .~~ centered ]) 
--  -- where centered == (A_Center,J_Center)
-- @
--------------------------------------------------------------------------------


module Classh.Box.Placement
  ( module X
  , Matrix33(..)
  , topLeft
  , middleLeft
  , bottomLeft
  , topCenter
  , centered
  , bottomCenter
  , topRight
  , middleRight
  , bottomRight
  , centeredOnly
  ) where

import Classh.Box.Placement.Justify as X 
import Classh.Box.Placement.Align as X

import Classh.Responsive.WhenTW
import Classh.Responsive.ZipScreens 


-- | Eg tic tac toe, except we use to describe position of element
data Matrix33
  = UpL
  | UpM
  | UpR
  | MidL
  | MidM
  | MidR
  | DownL
  | DownM
  | DownR

-- | Constants describing a position for children
topLeft, middleLeft, bottomLeft, topCenter, centered, bottomCenter, topRight, middleRight, bottomRight
  :: (Justify, Align)
topLeft = (J_Start, A_Start)
middleLeft = (J_Start, A_Center)
bottomLeft = (J_Start, A_End)

topCenter = (J_Center, A_Start)
centered = (J_Center, A_Center)
bottomCenter = (J_Center, A_End)

topRight = (J_End, A_Start)
middleRight = (J_End, A_Center)
bottomRight = (J_End, A_End)

centeredOnly :: WhenTW (Justify, Align)
centeredOnly = only centered


-- TODO: create showTW which calls showTW on align and justify to make maintaining easier 
