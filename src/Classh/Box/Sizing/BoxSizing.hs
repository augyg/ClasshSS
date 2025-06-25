--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Box.Sizing.BoxSizing
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Types to represent a BoxConfig's target sizing (width+height) with
--  a TWSizeOrFraction 
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

module Classh.Box.Sizing.BoxSizing
  (
    -- * Config Types
    BoxSizing(..)
    -- * Auto Generated Lenses
  , width
  , height
  ) where 

import Classh.Internal.Chain
import Classh.Class.ShowTW
import Classh.Responsive.WhenTW
import Classh.Box.TWSize
import Data.Default
import Control.Lens (makeLenses)

-- | Holds information on target sizing, which will be overrided by constraints
data BoxSizing = BoxSizing
  { _width :: WhenTW TWSizeOrFraction
  , _height :: WhenTW TWSizeOrFraction
  }
  deriving Show

-- | > == BoxSizing TWSize_Auto TWSize_Auto
instance Default BoxSizing where
  def = BoxSizing def def

instance ShowTW BoxSizing where
  showTW cfg = foldr (<&>) mempty
    [ renderWhenTW (_width cfg) ((<>) "w-" . showTW)
    , renderWhenTW (_height cfg) ((<>) "h-" . showTW)
    ]

makeLenses ''BoxSizing

instance Semigroup BoxSizing where
  (<>) a b = BoxSizing
    { _width  = _width a <> _width b
    , _height = _height a <> _height b
    }
