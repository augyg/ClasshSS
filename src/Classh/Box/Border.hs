{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Box.Border
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Types to represent tailwind box's border config of 'BoxConfig'  
--
--  Any field named _someField has an associated lens `someField`
--  see @defaultNameTransform@ from Lens.Family.THCore
--
--  This package aims to avoid forcing the user to know lenses
--
--  Example use:
--
-- @
--  $(classh' [ border . bWidth . borderWidth_t .~~ B2 ])
-- @
--------------------------------------------------------------------------------

module Classh.Box.Border
  ( module X 
  -- * The Border type 
  , BorderConfig(..)
  -- * Border Sub-Types
  , BorderColorSides(..)
  , BorderRadiusCorners(..)
  , BorderWidthSides(..)
  -- * Options
  , BorderWidth(..)
  , BorderStyle(..)
  , BorderRadius'(..)
  -- * BorderConfig top-level Lenses
  , radius
  , bWidth
  , bStyle
  , bColor
  ) where

import Classh.Box.Border.Style as X
import Classh.Box.Border.Width as X
import Classh.Box.Border.Color as X
import Classh.Box.Border.Radius as X

import Classh.Internal.Chain
import Classh.Class.ShowTW
import Classh.Responsive.WhenTW

import Control.Lens hiding ((<&>))
import Data.Default

--------------------------------------------------------------------------------
-- |
-- NOTE: any field named _someField has an associated lens `someField`
-- see @defaultNameTransform@ from Lens.Family.THCore
--
-- This package aims to avoid forcing the user to know lenses
--------------------------------------------------------------------------------


-- | Holds all information about a Box's tailwind border classes
data BorderConfig = BorderConfig
  { _bStyle :: WhenTW BorderStyle
  -- ^ https://tailwindcss.com/docs/border-style
  , _bColor :: BorderColorSides
  -- ^ https://tailwindcss.com/docs/border-color
  , _bWidth :: BorderWidthSides
  -- ^ https://tailwindcss.com/docs/border-width
  , _radius :: BorderRadiusCorners
  -- ^ https://tailwindcss.com/docs/border-radius
  } deriving Show


instance Default BorderConfig where
  def = BorderConfig def def def def
 
instance ShowTW BorderConfig where
  showTW cfg = foldr (<&>) mempty
    [ showTW . _radius $ cfg
    , showTW . _bWidth $ cfg
    , showTW . _bColor $ cfg
    , renderWhenTW (_bStyle cfg) ((<>) "border-" . showTW)
    ]

makeLenses ''BorderConfig

instance Semigroup BorderConfig where
  (<>) a b = BorderConfig
    { _bStyle = _bStyle a <> _bStyle b
    , _bColor = _bColor a <> _bColor b
    , _bWidth = _bWidth a <> _bWidth b
    , _radius = _radius a <> _radius b
    }
