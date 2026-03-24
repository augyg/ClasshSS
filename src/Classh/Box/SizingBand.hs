{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Box.Sizing.BoxSizing
--  Description :  Responsive sizing band configuration
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Types to represent a BoxConfig's sizing band, ie min max and inner target size
--  for width and height
--
--  Any field named _someField has an associated lens `someField`
--  see @defaultNameTransform@ from Lens.Family.THCore
--
--  This package aims to avoid forcing the user to know lenses
--
--  Example use:
--
-- @
--  $(classh' [ sizingBand . sizing . width .~~ (TWSize' (TWSize 8)) ])
--  -- or with shorthand
--  $(classh' [ w .~~ twSize' 8, h .~~ twSize' 8 ]) 
-- @
--------------------------------------------------------------------------------

module Classh.Box.SizingBand
  (
    -- * Config Types
    BoxSizingBand(..)
  , Dimensions
    -- * Constants
  , fitToContents
    -- * Auto Generated Lenses
  , maxSize
  , minSize
  , size     
  , module BoxSizing
  , module BoxSizingConstraint
  , module TWSize
  ) where

import Classh.Box.Sizing.BoxSizing as BoxSizing
import Classh.Box.Sizing.BoxSizingConstraint as BoxSizingConstraint

import Classh.Class.ShowTW (ShowTW(..))
import Classh.Responsive.WhenTW as WhenTW
import Classh.Internal.Chain ((<&>))
import Classh.Responsive.ZipScreens as ZipScreens
import Classh.WithTransition as WT
import Classh.Box.Transition (TransitionProperty(..))

import Classh.Box.TWSize as TWSize

import Control.Lens (makeLenses)
import Data.Default (Default(..))

-- move to shorthand?
fitToContents :: (WhenTW (WithTransition TWSizeOrFraction), WhenTW (WithTransition TWSizeOrFraction))
fitToContents = (only (WithTransition TWSize_Fit Nothing), only (WithTransition TWSize_Fit Nothing))

instance ShowTW BoxSizingBand where
  showTW cfg = foldr (<&>) mempty
    [ renderWithTransitionTW (_widthC . _maxSize $ cfg) ((<>) "max-w-" . showTW) Transition_All
    , renderWithTransitionTW (_heightC . _maxSize $ cfg) ((<>) "max-h-" . showTW) Transition_All
    , renderWithTransitionTW (_widthC . _minSize $ cfg) ((<>) "min-w-" . showTW) Transition_All
    , renderWithTransitionTW (_heightC . _minSize $ cfg) ((<>) "min-h-" . showTW) Transition_All
    , showTW $ _size cfg
    ]

-- | Default does nothing
instance Default BoxSizingBand where
  def = BoxSizingBand def def def

data BoxSizingBand = BoxSizingBand
  { _maxSize :: BoxSizingConstraint
  , _minSize :: BoxSizingConstraint 
  , _size :: BoxSizing
  }
  deriving Show

-- TODO: remove
type Dimensions = (WhenTW TWSizeOrFraction, WhenTW TWSizeOrFraction)


makeLenses ''BoxSizingBand


instance Semigroup BoxSizingBand where
  (<>) a b = BoxSizingBand
    { _maxSize = _maxSize a <> _maxSize b
    , _minSize = _minSize a <> _minSize b
    , _size    = _size a <> _size b  -- right-biased override
    }
