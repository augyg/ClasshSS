--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Color
--  Description :  Color types and constructors for Tailwind CSS colors
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  The overloaded Color type, which is used all over the place in tailwind classes
--
--  Example use:
--
-- @
--  elClass "div" $(classh' [ bgColor .~~ Black ]) $ textS $(classh' [text_color .~~ White]) "hey"
-- @
--------------------------------------------------------------------------------

module Classh.Color
  ( -- * Color Types
    Color(..)
  , ColorNum(..)
  , Hex(..)
    -- * Color with Opacity
  , ColorWithOpacity(..)
  , color
  , withOpacity
    -- * Hex Helper
  , hex
  ) where

import Classh.Class.ShowTW (ShowTW(..))
import Classh.Internal.TShow (tshow)

import Data.Default (Default(..))
import qualified Data.Text as T

-- | Shorthand application of hex code color 
hex :: T.Text -> Color
hex = Color_Custom . Hex

-- | 6 digit hex code representing RGB color 
newtype Hex = Hex { unHex :: T.Text } deriving (Eq, Show)

-- | > == Black 
instance Default Color where
  def = Black

-- | > == C950
instance Default ColorNum where
  def = C950

-- | Eg. see https://tailwindcss.com/docs/background-color
data Color
  = Inherit
  | Current
  | Transparent
  | Black
  | White
  | Slate ColorNum
  | Gray ColorNum
  | Zinc ColorNum
  | Neutral ColorNum
  | Stone ColorNum
  | Red ColorNum
  | Orange ColorNum
  | Amber ColorNum
  | Yellow ColorNum
  | Lime ColorNum
  | Green ColorNum
  | Emerald ColorNum
  | Teal ColorNum
  | Cyan ColorNum
  | Sky ColorNum
  | Blue ColorNum
  | Indigo ColorNum
  | Violet ColorNum
  | Purple ColorNum
  | Fuchsia ColorNum
  | Pink ColorNum
  | Rose ColorNum
  | Color_Custom Hex
  deriving (Show, Eq)

-- | Color with optional opacity (0-100).
--
-- Renders using Tailwind's @/opacity@ syntax when opacity is present.
-- When opacity is @Nothing@, renders as plain color (fully opaque).
--
-- === Example
--
-- @
-- color (Blue C500)           -- blue-500 (no opacity suffix)
-- withOpacity (Blue C500) 50  -- blue-500/50
-- withOpacity (hex "1e40af") 87  -- [#1e40af]/87
-- @
data ColorWithOpacity = ColorWithOpacity
  { _cwo_color   :: Color
  , _cwo_opacity :: Maybe Int  -- ^ Nothing = fully opaque, Just n = n% opacity
  } deriving (Show, Eq)

-- | Create a color without explicit opacity (fully opaque).
--
-- @
-- color (Blue C500)  -- blue-500
-- color White        -- white
-- @
color :: Color -> ColorWithOpacity
color c = ColorWithOpacity c Nothing

-- | Create a color with explicit opacity (0-100).
--
-- @
-- withOpacity (Blue C500) 50  -- blue-500/50
-- withOpacity (hex "1e40af") 87  -- [#1e40af]/87
-- @
withOpacity :: Color -> Int -> ColorWithOpacity
withOpacity c o = ColorWithOpacity c (Just o)

-- | Eg. see https://tailwindcss.com/docs/background-color
data ColorNum
 = C50
 | C100
 | C200
 | C300
 | C400
 | C500
 | C600
 | C700
 | C800
 | C900
 | C950
 deriving (Show, Eq)

-- There is no showTW for ColorNum since this will never be without an encompassing Color
-- TODO(galen): Should we just make this its own special function? and then its called by showTW; showColor
-- Because this is never used on its own either
instance ShowTW Color where
  showTW (Color_Custom (Hex h)) = "[#" <> h <> "]"
  showTW Inherit = "inherit"
  showTW Current = "current"
  showTW Transparent = "transparent"
  showTW Black = "black"
  showTW White = "white"
  showTW col = case T.words $ tshow col of
    c:(mag):[] -> (T.toLower c) <> "-" <> (T.drop 1 mag)
    _ -> "ClasshSS: failed on input" <> (tshow col)

-- | Renders color with optional opacity suffix.
-- @Nothing@ opacity renders plain color, @Just n@ renders @color/n@.
instance ShowTW ColorWithOpacity where
  showTW (ColorWithOpacity c Nothing) = showTW c
  showTW (ColorWithOpacity c (Just o)) = showTW c <> "/" <> tshow o
