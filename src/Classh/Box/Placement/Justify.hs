--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Box.Placement.Justify
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Types to represent tailwind horizontal positioning (justification) for use by
--  the 'position' field of 'BoxConfig', this uses grid display.
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


module Classh.Box.Placement.Justify where

import Classh.Class.ShowTW

import Data.Default
import qualified Data.Text as T

-- | > J_Start
instance Default Justify where
  def = J_Start

instance ShowTW Justify where
  showTW = (<>) "justify-items-" . T.toLower . T.drop 2 . T.pack . show


-- NOTE: although I cant do Start for either, I could create a class like HasStart
-- Also NOTE: there is no plan to make text align, rather you put it in a span and align it via this mechanism

-- | Based on https://tailwindcss.com/docs/justify-items
data Justify
  = J_Start
  | J_End
  | J_Center
  | J_Stretch
  deriving Show
