--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Box.Sizing.BoxSizingConstraint 
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Types to represent min/max constraints on a 'BoxConfig'
--
--  Any field named _someField has an associated lens `someField`
--  see @defaultNameTransform@ from Lens.Family.THCore
--
--  This package aims to avoid forcing the user to know lenses
--
--  Example use (shorthand is to-do):
--
-- @
--  $(classh' [ sizingBand . minSize . widthC .~~ DC_sm, sizingBand . maxSize . widthC .~~ DC_7xl ])
-- @
--------------------------------------------------------------------------------

module Classh.Box.Sizing.BoxSizingConstraint
  (
    -- * Config Type
    BoxSizingConstraint(..)
    -- * Auto Generated Lenses
  , widthC
  , heightC 
  , module X
  ) where

import Classh.Box.Sizing.DimensionConstraint as X
import Classh.Responsive.WhenTW
import Control.Lens (makeLenses)
import Data.Default

-- | > == BoxSizingConstraint DC_none DC_none
instance Default BoxSizingConstraint where
  def = BoxSizingConstraint def def

data BoxSizingConstraint = BoxSizingConstraint
  { _widthC :: WhenTW DimensionConstraint
  , _heightC :: WhenTW DimensionConstraint
  }
  deriving Show

makeLenses ''BoxSizingConstraint


instance Semigroup BoxSizingConstraint where
  (<>) a b = BoxSizingConstraint
    { _widthC  = _widthC a <> _widthC b
    , _heightC = _heightC a <> _heightC b
    }
