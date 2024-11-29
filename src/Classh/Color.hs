module Classh.Color where

import Classh.Class.ShowTW
import Classh.Internal.TShow

import Data.Default
import qualified Data.Text as T


hex :: T.Text -> Color
hex = Color_Custom . Hex

newtype Hex = Hex { unHex :: T.Text } deriving (Eq, Show)

-- TODO: own defaults
instance Default Color where
  def = Black
  
instance Default ColorNum where
  def = C950

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

-- | There is no showTW for ColorNum since this will never be without an encompassing Color
-- | TODO(galen): Should we just make this its own special function? and then its called by showTW; showColor
-- | Because this is never used on its own either
instance ShowTW Color where
  showTW (Color_Custom (Hex h)) = "[#" <> h <> "]"
  showTW Inherit = "inherit"
  showTW Current = "current"
  showTW Transparent = "transparent"
  showTW Black = "black"
  showTW White = "white"
  showTW color = case T.words $ tshow color of
    c:(mag):[] -> (T.toLower c) <> "-" <> (T.drop 1 mag) -- T.words $ tshow color
    _ -> "ClasshSS: failed on input" <> (tshow color)
