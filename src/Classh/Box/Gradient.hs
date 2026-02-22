--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Box.Gradient
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
    -- * Color Stop Helpers
  , stop
  , stopAt
    -- * Gradient Builders
  , linearGradient
  , linearGradientVia
  , linearGradientPos
  , linearGradientViaPos
  , gradientFrom
  ) where

import Classh.Class.ShowTW
import Classh.Color
import Classh.Internal.TShow

import Data.Default
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

-- | A color stop with an optional position.
--
-- === Examples
--
-- @
-- stop White              -- Just the color
-- stopAt (Blue C500) 30   -- Color at 30%
-- @
data ColorStop = ColorStop
  { _stop_color    :: Color
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
data GradientColor
  = SolidColor Color
  | GradientColor GradientConfig
  deriving (Show, Eq)

-- | Default is transparent solid color
instance Default GradientColor where
  def = SolidColor Transparent

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

-- | Helper to render a color stop with its position.
-- Generates output like @from-blue-500 from-10%@ or @via-pink-500 via-30%@
renderStop :: T.Text -> ColorStop -> T.Text
renderStop prefix (ColorStop color mpos) =
  prefix <> "-" <> showTW color <>
  maybe "" (\(StopPosition p) -> " " <> prefix <> "-" <> tshow p <> "%") mpos

instance ShowTW GradientConfig where
  showTW (GradientConfig dir from mvia mto) =
    "bg-gradient-" <> showTW dir <> " " <>
    renderStop "from" from <>
    maybe "" (\v -> " " <> renderStop "via" v) mvia <>
    maybe "" (\t -> " " <> renderStop "to" t) mto

instance ShowTW GradientColor where
  showTW (SolidColor color) = showTW color
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
solidColor = SolidColor

-- | Create a color stop without a position.
--
-- @
-- stop White  -- ColorStop White Nothing
-- @
stop :: Color -> ColorStop
stop c = ColorStop c Nothing

-- | Create a color stop at a specific position (0-100%).
--
-- @
-- stopAt (Blue C500) 30  -- Blue at 30%
-- @
stopAt :: Color -> Int -> ColorStop
stopAt c p = ColorStop c (Just $ StopPosition p)

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
