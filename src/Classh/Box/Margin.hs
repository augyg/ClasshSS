{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Box.Margin
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Types to represent tailwind box's margin by a 'TWSize'
--
--  Any field named _someField has an associated lens `someField`
--  see @defaultNameTransform@ from Lens.Family.THCore
--
--  This package aims to avoid forcing the user to know lenses
--
--  Example use:
--
-- @
--  $(classh' [ margin . marginT .~~ TWSize 8 ])
--  -- or with shorthand
--  $(classh' [ mt .~~ TWSize 8 ]) -- == "mt-8"
-- @
--------------------------------------------------------------------------------



module Classh.Box.Margin
  (
    module X
    -- * Config Type
  ,  BoxMargin(..)
    -- * Auto Generated Lenses 
  , marginT
  , marginB
  , marginL
  , marginR
  ) where

import Classh.Internal.Chain
import Classh.Class.ShowTW
import Classh.Class.SetSides
import Classh.Responsive.WhenTW

import Classh.Box.TWSize as X 

import Control.Lens hiding ((<&>))
import Data.Default

-- | > == BoxMargin [] [] [] []
instance Default BoxMargin where
  def = BoxMargin def def def def

instance ShowTW BoxMargin where
  showTW cfg = foldr (<&>) mempty
    [ renderWhenTW (_marginL cfg) ((<>) "ml-" . showTW)
    , renderWhenTW (_marginR cfg) ((<>) "mr-" . showTW)
    , renderWhenTW (_marginT cfg) ((<>) "mt-" . showTW)
    , renderWhenTW (_marginB cfg) ((<>) "mb-" . showTW)
    ]

-- | Type representing '_margin' field of 'BoxConfig'.
-- | based on https://tailwindcss.com/docs/margin
data BoxMargin = BoxMargin
  { _marginL :: WhenTW TWSize
  -- ^ see shorthand: @ml@ 
  , _marginR :: WhenTW TWSize
  -- ^ see shorthand: 'mr'
  , _marginT :: WhenTW TWSize
  -- ^ see shorthand: 'mt'
  , _marginB :: WhenTW TWSize
  -- ^ see shorthand: 'mb'
  } deriving Show

makeLenses ''BoxMargin

instance Semigroup BoxMargin where
  (<>) a_ b_ = BoxMargin
    { _marginL = _marginL a_ <> _marginL b_
    , _marginR = _marginR a_ <> _marginR b_
    , _marginT = _marginT a_ <> _marginT b_
    , _marginB = _marginB a_ <> _marginB b_
    }

-- | This is technically an illegal lens however if you ran 2 setters which overlap so that a /= b
-- | where a and b are the fields associated with respective separate fields, then classh' will
-- | most likely catch the error. Additionally, there is a lens way to access any field anyways
instance SetSides BoxMargin TWSize where
  l = marginL
  r = marginR
  b = marginB
  t = marginT
  x = lens _marginL $ \tw new -> tw { _marginL = new, _marginR = new }
  y =  lens _marginT $ \tw new -> tw { _marginT = new, _marginB = new }
  xy = lens _marginT $ \tw new -> tw { _marginT = new
                                     , _marginB = new
                                     , _marginL = new
                                     , _marginR = new
                                     }

