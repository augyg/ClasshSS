{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}

--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Box.Margin
--  Description :  Margin configuration for box model
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

import Classh.Internal.Chain ((<&>))
import Classh.Class.ShowTW (ShowTW(..))
import Classh.Class.SetSides as SetSides
import Classh.Responsive.WhenTW as WhenTW
import Classh.WithTransition as WT
import Classh.Box.Transition (TransitionProperty(..))

import Classh.Box.TWSize as X

import Control.Lens hiding ((<&>))
import Data.Default (Default(..))

-- | > == BoxMargin [] [] [] []
instance Default BoxMargin where
  def = BoxMargin def def def def

instance ShowTW BoxMargin where
  showTW cfg = foldr (<&>) mempty
    [ renderWithTransitionTW (_marginL cfg) ((<>) "ml-" . showTW) Transition_All
    , renderWithTransitionTW (_marginR cfg) ((<>) "mr-" . showTW) Transition_All
    , renderWithTransitionTW (_marginT cfg) ((<>) "mt-" . showTW) Transition_All
    , renderWithTransitionTW (_marginB cfg) ((<>) "mb-" . showTW) Transition_All
    ]

-- | Type representing '_margin' field of 'BoxConfig' (transitionable).
-- | based on https://tailwindcss.com/docs/margin
-- | Uses 'TWSizeOrFraction' instead of 'TWSize' to support 'TWSize_Auto'
-- | (e.g. @mx .~~ TWSize_Auto@ generates @mx-auto@)
data BoxMargin = BoxMargin
  { _marginL :: WhenTW (WithTransition TWSizeOrFraction)
  -- ^ see shorthand: @ml@
  , _marginR :: WhenTW (WithTransition TWSizeOrFraction)
  -- ^ see shorthand: 'mr'
  , _marginT :: WhenTW (WithTransition TWSizeOrFraction)
  -- ^ see shorthand: 'mt'
  , _marginB :: WhenTW (WithTransition TWSizeOrFraction)
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
instance SetSides BoxMargin (WithTransition TWSizeOrFraction) where
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

