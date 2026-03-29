{-# LANGUAGE FlexibleInstances #-}

-- |
-- Module      : Classh.Box.Border.Color
-- Description : Border color types
-- Copyright   : (c) Galen Sprout, 2024
-- License     : MIT
-- Maintainer  : galen.sprout@gmail.com

module Classh.Box.Border.Color where

import Classh.Class.ShowTW (ShowTW(..))
import Classh.Class.SetSides as SetSides
import Classh.Responsive.WhenTW as WhenTW
import Classh.Internal.Chain ((<&>))
import Classh.Color as Color
import Classh.WithTransition as WT
import Classh.Box.Transition (TransitionProperty(..))
import Data.Default (Default(..))
import Control.Lens (lens, makeLenses)


-- |Holds Border 'Color' by side (transitionable)
--
--  For example:
--
-- > elClass "div" $(classh' [ border . bColor . borderColor_t .~~ Black ])
-- > -- Or with shorthand
-- > elClass "div" $(classh' [ bc_t .~~ Black ])
-- > -- With transitions:
-- > elClass "div" $(classh' [ bc_t .~^ [("def", Black), ("hover", Red `withTransition` Duration_300)] ])
data BorderColorSides = BorderColorSides
  { _borderColor_l :: WhenTW (WithTransition ColorWithOpacity)
  -- ^ border-l-'Color' ... see https://tailwindcss.com/docs/border-color
  , _borderColor_r :: WhenTW (WithTransition ColorWithOpacity)
  -- ^ border-r-'Color' ... see https://tailwindcss.com/docs/border-color
  , _borderColor_t :: WhenTW (WithTransition ColorWithOpacity)
  -- ^ border-t-'Color' ... see https://tailwindcss.com/docs/border-color
  , _borderColor_b :: WhenTW (WithTransition ColorWithOpacity)
  -- ^ border-b-'Color' ... see https://tailwindcss.com/docs/border-color
  } deriving Show

instance Default BorderColorSides where
  def = BorderColorSides def def def def


instance ShowTW BorderColorSides where
  showTW cfg = foldr (<&>) mempty
    [ renderWithTransitionTW (_borderColor_l cfg) ((<>) "border-l-" . showTW) Transition_Colors
    , renderWithTransitionTW (_borderColor_r cfg) ((<>) "border-r-" . showTW) Transition_Colors
    , renderWithTransitionTW (_borderColor_t cfg) ((<>) "border-t-" . showTW) Transition_Colors
    , renderWithTransitionTW (_borderColor_b cfg) ((<>) "border-b-" . showTW) Transition_Colors
    ]

makeLenses ''BorderColorSides

-- | Like border-'Color', eg border-white
-- Now uses WithTransition ColorWithOpacity so .~~ will auto-wrap, and .~^ allows transitions
instance SetSides BorderColorSides (WithTransition ColorWithOpacity) where
  l = borderColor_l
  r = borderColor_r
  t = borderColor_t
  b = borderColor_b
  x = lens _borderColor_l $ \tw new -> tw { _borderColor_l = new, _borderColor_r = new }
  y = lens _borderColor_t $ \tw new -> tw { _borderColor_t = new, _borderColor_b = new }
  xy = lens _borderColor_t $ \tw new -> tw { _borderColor_t = new
                                           , _borderColor_b = new
                                           , _borderColor_l = new
                                           , _borderColor_r = new
                                           }


instance Semigroup BorderColorSides where
  (<>) a_ b_ = BorderColorSides
    { _borderColor_l = _borderColor_l a_ <> _borderColor_l b_
    , _borderColor_r = _borderColor_r a_ <> _borderColor_r b_
    , _borderColor_t = _borderColor_t a_ <> _borderColor_t b_
    , _borderColor_b = _borderColor_b a_ <> _borderColor_b b_
    }
