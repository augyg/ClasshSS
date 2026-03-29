{-# LANGUAGE TemplateHaskell #-}
--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Box.Transform
--  Description :  CSS transform types and configuration
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Types to represent tailwind transforms
--  see https://v3.tailwindcss.com/docs/scale
--  see https://v3.tailwindcss.com/docs/rotate
--  see https://v3.tailwindcss.com/docs/translate
--  see https://v3.tailwindcss.com/docs/skew
--  see https://v3.tailwindcss.com/docs/transform-origin
--
--  Example use:
--
-- @
--  $(classh' [ transform . scale .~^ [("def", Scale_100), ("hover", Scale_105), ("active", Scale_95)] ])
--  $(classh' [ transform . rotate .~^ [("def", Rotate_0), ("hover", Rotate_180)] ])
--  $(classh' [ transform . translateX .~~ Translate_0 ])
-- @
--------------------------------------------------------------------------------

module Classh.Box.Transform
  ( TransformConfig(..)
  , Scale(..)
  , Rotate(..)
  , Translate(..)
  , Skew(..)
  , TransformOrigin(..)
  , rotate
  , scale
  , translateX
  , translateY
  , skewX
  , skewY
  , transformOrigin
  ) where

import Classh.Class.ShowTW (ShowTW(..))
import Classh.Class.CompileStyle as CompileStyle
import Classh.Internal.TShow (tshow)
import Classh.Internal.Chain ((<&>))
import Classh.Responsive.WhenTW as WhenTW
import Classh.WithTransition as WT
import Classh.Box.Transition (TransitionProperty(..))
import Classh.Box.TWSize (TWSize, DivInt(..))
import Classh.Internal.CSSSize (CSSSize)
import Classh.Class.IsCSS (renderCSS)
import Control.Lens hiding ((<&>))
import Data.Default (Default(..))
import qualified Data.Text as T

-- | Scale transform
-- see https://v3.tailwindcss.com/docs/scale
data Scale
  = Scale_0
  | Scale_50
  | Scale_75
  | Scale_90
  | Scale_95
  | Scale_100
  | Scale_105
  | Scale_110
  | Scale_125
  | Scale_150
  | Scale_Custom T.Text  -- ^ e.g., Scale_Custom "102" for scale-[102%]
  deriving Show

instance Default Scale where
  def = Scale_100

instance ShowTW Scale where
  showTW = \case
    Scale_Custom val -> "scale-[" <> val <> "%]"
    other -> "scale-" <> (T.drop 6 . tshow $ other)

-- | Rotate transform
-- see https://v3.tailwindcss.com/docs/rotate
data Rotate
  = Rotate_0      -- ^ rotate-0: transform: rotate(0deg)
  | Rotate_1      -- ^ rotate-1: transform: rotate(1deg)
  | Rotate_2      -- ^ rotate-2: transform: rotate(2deg)
  | Rotate_3      -- ^ rotate-3: transform: rotate(3deg)
  | Rotate_6      -- ^ rotate-6: transform: rotate(6deg)
  | Rotate_12     -- ^ rotate-12: transform: rotate(12deg)
  | Rotate_45     -- ^ rotate-45: transform: rotate(45deg)
  | Rotate_90     -- ^ rotate-90: transform: rotate(90deg)
  | Rotate_180    -- ^ rotate-180: transform: rotate(180deg)
  | Rotate_Custom T.Text  -- ^ e.g., Rotate_Custom "17deg" for rotate-[17deg]
  deriving Show

instance Default Rotate where
  def = Rotate_0

instance ShowTW Rotate where
  showTW = \case
    Rotate_Custom val -> "rotate-[" <> val <> "]"
    other -> "rotate-" <> (T.drop 7 . tshow $ other)

-- | Translate transform (for X and Y axes)
-- see https://v3.tailwindcss.com/docs/translate
data Translate
  = Translate_0
  | Translate_Px
  | Translate_Full
  | Translate_TWSize TWSize         -- ^ Numeric spacing values (1, 2, 3.5, 4, etc.)
  | Translate_Fraction Int DivInt   -- ^ Fractional values (1/2, 1/3, 2/3, 1/4, etc.)
  | Translate_Custom CSSSize        -- ^ Custom CSS size (e.g., Rem 1.5, Percent 50)
  deriving Show

instance Default Translate where
  def = Translate_0

instance ShowTW Translate where
  showTW = \case
    Translate_0 -> "0"
    Translate_Px -> "px"
    Translate_Full -> "full"
    Translate_TWSize sz -> showTW sz
    Translate_Fraction num d -> tshow num <> "/" <> showTW d
    Translate_Custom css -> "[" <> renderCSS css <> "]"

-- | Skew transform (for X and Y axes)
-- see https://v3.tailwindcss.com/docs/skew
data Skew
  = Skew_0   -- ^ skew-{x|y}-0: transform: skew{X|Y}(0deg)
  | Skew_1   -- ^ skew-{x|y}-1: transform: skew{X|Y}(1deg)
  | Skew_2   -- ^ skew-{x|y}-2: transform: skew{X|Y}(2deg)
  | Skew_3   -- ^ skew-{x|y}-3: transform: skew{X|Y}(3deg)
  | Skew_6   -- ^ skew-{x|y}-6: transform: skew{X|Y}(6deg)
  | Skew_12  -- ^ skew-{x|y}-12: transform: skew{X|Y}(12deg)
  | Skew_Custom T.Text  -- ^ e.g., Skew_Custom "17deg" for skew-x-[17deg]
  deriving Show

instance Default Skew where
  def = Skew_0

instance ShowTW Skew where
  showTW = \case
    Skew_Custom val -> "[" <> val <> "]"
    other -> T.drop 5 . tshow $ other

-- | Transform origin
-- see https://v3.tailwindcss.com/docs/transform-origin
data TransformOrigin
  = Origin_Center        -- ^ origin-center
  | Origin_Top           -- ^ origin-top
  | Origin_TopRight      -- ^ origin-top-right
  | Origin_Right         -- ^ origin-right
  | Origin_BottomRight   -- ^ origin-bottom-right
  | Origin_Bottom        -- ^ origin-bottom
  | Origin_BottomLeft    -- ^ origin-bottom-left
  | Origin_Left          -- ^ origin-left
  | Origin_TopLeft       -- ^ origin-top-left
  | Origin_Custom T.Text -- ^ e.g., Origin_Custom "33% 75%" for origin-[33%_75%]
  deriving Show

instance Default TransformOrigin where
  def = Origin_Center

instance ShowTW TransformOrigin where
  showTW = \case
    Origin_Center -> "origin-center"
    Origin_Top -> "origin-top"
    Origin_TopRight -> "origin-top-right"
    Origin_Right -> "origin-right"
    Origin_BottomRight -> "origin-bottom-right"
    Origin_Bottom -> "origin-bottom"
    Origin_BottomLeft -> "origin-bottom-left"
    Origin_Left -> "origin-left"
    Origin_TopLeft -> "origin-top-left"
    Origin_Custom val -> "origin-[" <> val <> "]"

-- | Configuration for all transform properties
-- see https://v3.tailwindcss.com/docs/transform
-- All transform properties can be smoothly transitioned using transition-transform
data TransformConfig = TransformConfig
  { _rotate :: WhenTW (WithTransition Rotate)
  , _scale :: WhenTW (WithTransition Scale)
  , _translateX :: WhenTW (WithTransition Translate)
  , _translateY :: WhenTW (WithTransition Translate)
  , _skewX :: WhenTW (WithTransition Skew)
  , _skewY :: WhenTW (WithTransition Skew)
  , _transformOrigin :: WhenTW TransformOrigin  -- Origin doesn't transition, just changes instantly
  }
  deriving Show

makeLenses ''TransformConfig

instance Default TransformConfig where
  def = TransformConfig def def def def def def def

instance Semigroup TransformConfig where
  (<>) a b = TransformConfig
    { _rotate = _rotate a <> _rotate b
    , _scale = _scale a <> _scale b
    , _translateX = _translateX a <> _translateX b
    , _translateY = _translateY a <> _translateY b
    , _skewX = _skewX a <> _skewX b
    , _skewY = _skewY a <> _skewY b
    , _transformOrigin = _transformOrigin a <> _transformOrigin b
    }

instance CompileStyle TransformConfig where
  compileS cfg = do
    pure . foldr (<&>) mempty =<< sequenceA
      [ compileWithTransitionTW (_rotate cfg) showTW Transition_Transform
      , compileWithTransitionTW (_scale cfg) showTW Transition_Transform
      , compileWithTransitionTW (_translateX cfg) ((<>) "translate-x-" . showTW) Transition_Transform
      , compileWithTransitionTW (_translateY cfg) ((<>) "translate-y-" . showTW) Transition_Transform
      , compileWithTransitionTW (_skewX cfg) ((<>) "skew-x-" . showTW) Transition_Transform
      , compileWithTransitionTW (_skewY cfg) ((<>) "skew-y-" . showTW) Transition_Transform
      , compileWhenTW (_transformOrigin cfg) showTW
      ]

instance ShowTW TransformConfig where
  showTW cfg = foldr (<&>) mempty
    [ renderWithTransitionTW (_rotate cfg) showTW Transition_Transform
    , renderWithTransitionTW (_scale cfg) showTW Transition_Transform
    , renderWithTransitionTW (_translateX cfg) ((<>) "translate-x-" . showTW) Transition_Transform
    , renderWithTransitionTW (_translateY cfg) ((<>) "translate-y-" . showTW) Transition_Transform
    , renderWithTransitionTW (_skewX cfg) ((<>) "skew-x-" . showTW) Transition_Transform
    , renderWithTransitionTW (_skewY cfg) ((<>) "skew-y-" . showTW) Transition_Transform
    , renderWhenTW (_transformOrigin cfg) showTW
    ]
