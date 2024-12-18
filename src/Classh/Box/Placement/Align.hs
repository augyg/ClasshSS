

--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Box.Placement.Align
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Types to represent tailwind vertical positioning (alignment) for use by
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
module Classh.Box.Placement.Align where

import Classh.Class.ShowTW

import Data.Default
import qualified Data.Text as T 

-- | > == A_Start
instance Default Align where
  def = A_Start

instance ShowTW Align where
  showTW = (<>) "content-" . T.toLower . T.drop 2 . T.pack . show

-- | Based on https://tailwindcss.com/docs/align-content
-- | Excluded classes are in order to reduce number of ways to do the same thing
-- | However if you really want to, you can use the _custom field of 'BoxConfig'
data Align
  = A_Start
  -- ^ == "content-start" 
  | A_End
  -- ^ == "content-end"
  | A_Center
  -- ^ == "content-center"
  | A_Baseline
  -- ^ == "content-baseline"
  deriving Show
