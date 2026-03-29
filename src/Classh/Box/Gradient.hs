--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Box.Gradient
--  Description :  CSS gradient types and constructors
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Type-safe gradient support for Tailwind CSS gradients.
--
--  = Overview
--
--  This module provides 'GradientColor', a type that encompasses both solid
--  colors and gradient configurations. It replaces 'Color' in bgColor fields,
--  allowing gradients to be used anywhere colors are used.
--
--  = Quick Example
--
--  @
--  -- Solid color (most common):
--  bgColor .~~ solid acePrimary
--
--  -- Simple two-color gradient:
--  bgColor .~~ linearGradient To_R (hex \"4E366C\") White
--  -- Generates: bg-gradient-to-r from-[#4E366C] to-white
--
--  -- Gradient with stop positions:
--  bgColor .~~ linearGradientViaPos To_R
--    (stopAt (hex \"4E366C\") 10)
--    (stopAt (Pink C500) 30)
--    (stopAt White 90)
--  -- Generates: bg-gradient-to-r from-[#4E366C] from-10% via-pink-500 via-30% to-white to-90%
--  @
--
--------------------------------------------------------------------------------

module Classh.Box.Gradient
  (
    -- * Core Types
    GradientColor(..)
  , GradientConfig(..)
  , GradientDirection(..)
  , ColorStop(..)
  , StopPosition(..)
    -- * Solid Color Helper
  , solidColor
  , solidColorOpacity
    -- * Color Stop Helpers
  , stop
  , stopAt
  , stopWithOpacity
  , stopAtWithOpacity
    -- * Gradient Builders
  , linearGradient
  , linearGradientVia
  , linearGradientPos
  , linearGradientViaPos
  , gradientFrom
  ) where

import Classh.Class.ShowTW (ShowTW(..))
import Classh.Color as Color
import Classh.Internal.TShow (tshow)

import Data.Default (Default(..))
import qualified Data.Text as T

-- | Direction for linear gradients.
--
-- Maps to Tailwind's gradient direction classes like @bg-gradient-to-r@.
data GradientDirection
  = To_T    -- ^ to top
  | To_TR   -- ^ to top-right
  | To_R    -- ^ to right (most common)
  | To_BR   -- ^ to bottom-right
  | To_B    -- ^ to bottom
  | To_BL   -- ^ to bottom-left
  | To_L    -- ^ to left
  | To_TL   -- ^ to top-left
  deriving (Show, Eq)

-- | Percentage for stop positions (0-100).
--
-- Maps to Tailwind's position classes like @from-10%@, @via-30%@, @to-90%@.
newtype StopPosition = StopPosition Int
  deriving (Show, Eq)

-- | A color stop with optional position.
--
-- Uses 'ColorWithOpacity' which embeds opacity directly in the color.
--
-- === Examples
--
-- @
-- stop White                        -- Just the color
-- stopAt (Blue C500) 30             -- Color at 30%
-- stopWithOpacity (hex "181422") 90 -- Color with 90% opacity
-- stopAtWithOpacity (hex "181422") 90 0  -- Opacity + position
-- @
data ColorStop = ColorStop
  { _stop_color    :: ColorWithOpacity  -- ^ Color with optional opacity
  , _stop_position :: Maybe StopPosition
  } deriving (Show, Eq)

-- | Gradient configuration with direction and color stops.
data GradientConfig = GradientConfig
  { _gradient_direction :: GradientDirection
  , _gradient_from      :: ColorStop           -- ^ Starting color (required)
  , _gradient_via       :: Maybe ColorStop     -- ^ Middle color (optional)
  , _gradient_to        :: Maybe ColorStop     -- ^ Ending color (optional)
  } deriving (Show, Eq)

-- | Union type: either a solid color or a gradient.
--
-- This type replaces 'Color' in '_bgColor' and similar fields, allowing
-- both simple colors and gradients to be used interchangeably.
--
-- === Examples
--
-- @
-- solidColor White                    -- bg-white
-- solidColorOpacity (hex "1e40af") 50 -- bg-[#1e40af]/50
-- linearGradient To_R ...             -- bg-gradient-to-r from-...
-- @
data GradientColor
  = SolidColor ColorWithOpacity  -- ^ Solid color (with optional opacity via ColorWithOpacity)
  | GradientColor GradientConfig
  deriving (Show, Eq)

-- | Default is transparent solid color
instance Default GradientColor where
  def = SolidColor (color Transparent)

instance ShowTW GradientDirection where
  showTW To_T  = "to-t"
  showTW To_TR = "to-tr"
  showTW To_R  = "to-r"
  showTW To_BR = "to-br"
  showTW To_B  = "to-b"
  showTW To_BL = "to-bl"
  showTW To_L  = "to-l"
  showTW To_TL = "to-tl"

instance ShowTW StopPosition where
  showTW (StopPosition p) = tshow p <> "%"

-- | Helper to render a color stop with position.
-- Generates output like @from-blue-500 from-10%@, @via-pink-500/50@, or @to-[#hex]/90 to-100%@
renderStop :: T.Text -> ColorStop -> T.Text
renderStop prefix (ColorStop cwo mpos) =
  prefix <> "-" <> showTW cwo <>
  maybe "" (\(StopPosition p) -> " " <> prefix <> "-" <> tshow p <> "%") mpos

instance ShowTW GradientConfig where
  showTW (GradientConfig dir from mvia mto) =
    "bg-gradient-" <> showTW dir <> " " <>
    renderStop "from" from <>
    maybe "" (\v -> " " <> renderStop "via" v) mvia <>
    maybe "" (\t -> " " <> renderStop "to" t) mto

instance ShowTW GradientColor where
  showTW (SolidColor cwo) = showTW cwo
  showTW (GradientColor cfg) = showTW cfg

--------------------------------------------------------------------------------
-- Helper Functions
--------------------------------------------------------------------------------

-- | Create a solid (non-gradient) color.
--
-- Use this when you want a simple background color without any gradient.
--
-- === Example
--
-- @
-- bgColor .~~ solidColor White
-- bgColor .~~ solidColor (Blue C500)
-- @
solidColor :: Color -> GradientColor
solidColor c = SolidColor (color c)

-- | Create a solid color with opacity.
--
-- Use this for semi-transparent backgrounds. Outputs Tailwind's @/opacity@ syntax.
--
-- === Example
--
-- @
-- bgColor .~~ solidColorOpacity (hex "221326") 87
-- -- Generates: bg-[#221326]/87
--
-- bgColor .~~ solidColorOpacity (Blue C500) 50
-- -- Generates: bg-blue-500/50
-- @
solidColorOpacity :: Color -> Int -> GradientColor
solidColorOpacity c opacity = SolidColor (withOpacity c opacity)

-- | Create a color stop without opacity or position.
--
-- @
-- stop White  -- ColorStop (color White) Nothing
-- @
stop :: Color -> ColorStop
stop c = ColorStop (color c) Nothing

-- | Create a color stop at a specific position (0-100%).
--
-- @
-- stopAt (Blue C500) 30  -- Blue at 30%
-- @
stopAt :: Color -> Int -> ColorStop
stopAt c p = ColorStop (color c) (Just $ StopPosition p)

-- | Create a color stop with opacity but no position.
--
-- @
-- stopWithOpacity (hex "181422") 90  -- [#181422]/90
-- @
stopWithOpacity :: Color -> Int -> ColorStop
stopWithOpacity c opacity = ColorStop (withOpacity c opacity) Nothing

-- | Create a color stop with both opacity and position.
--
-- @
-- stopAtWithOpacity (hex "181422") 90 0  -- [#181422]/90 at 0%
-- @
stopAtWithOpacity :: Color -> Int -> Int -> ColorStop
stopAtWithOpacity c opacity pos = ColorStop (withOpacity c opacity) (Just $ StopPosition pos)

-- | Create a two-color linear gradient.
--
-- === Example
--
-- @
-- linearGradient To_R (hex \"4E366C\") White
-- -- Generates: bg-gradient-to-r from-[#4E366C] to-white
-- @
linearGradient :: GradientDirection -> Color -> Color -> GradientColor
linearGradient dir from to = GradientColor $ GradientConfig dir (stop from) Nothing (Just $ stop to)

-- | Create a three-color linear gradient with a middle color.
--
-- === Example
--
-- @
-- linearGradientVia To_BR (Purple C500) (Pink C500) White
-- -- Generates: bg-gradient-to-br from-purple-500 via-pink-500 to-white
-- @
linearGradientVia :: GradientDirection -> Color -> Color -> Color -> GradientColor
linearGradientVia dir from via to = GradientColor $ GradientConfig dir (stop from) (Just $ stop via) (Just $ stop to)

-- | Create a two-color gradient with explicit stop positions.
--
-- === Example
--
-- @
-- linearGradientPos To_R (stopAt (hex \"4E366C\") 10) (stopAt White 90)
-- -- Generates: bg-gradient-to-r from-[#4E366C] from-10% to-white to-90%
-- @
linearGradientPos :: GradientDirection -> ColorStop -> ColorStop -> GradientColor
linearGradientPos dir from to = GradientColor $ GradientConfig dir from Nothing (Just to)

-- | Create a three-color gradient with explicit stop positions.
--
-- === Example
--
-- @
-- linearGradientViaPos To_R
--   (stopAt (hex \"4E366C\") 10)
--   (stopAt (Pink C500) 30)
--   (stopAt White 90)
-- -- Generates: bg-gradient-to-r from-[#4E366C] from-10% via-pink-500 via-30% to-white to-90%
-- @
linearGradientViaPos :: GradientDirection -> ColorStop -> ColorStop -> ColorStop -> GradientColor
linearGradientViaPos dir from via to = GradientColor $ GradientConfig dir from (Just via) (Just to)

-- | Create a single-color gradient that fades to transparent.
--
-- === Example
--
-- @
-- gradientFrom To_R (hex \"4E366C\")
-- -- Generates: bg-gradient-to-r from-[#4E366C]
-- @
gradientFrom :: GradientDirection -> Color -> GradientColor
gradientFrom dir from = GradientColor $ GradientConfig dir (stop from) Nothing Nothing
