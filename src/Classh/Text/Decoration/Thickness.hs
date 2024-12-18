--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Text.Decoration.Thickness
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Simple datatype to represent https://tailwindcss.com/docs/text-decoration-thickness
--
--  Example use:
--
-- @
--  $(classh' [ text_decoration . textDec_thickness .~~ TextDecThickness TW8 ])
-- @
--------------------------------------------------------------------------------

module Classh.Text.Decoration.Thickness where

import Classh.Class.ShowTW
import Classh.Class.IsCSS

import Classh.Internal.TWNum as X
import Classh.Internal.CSSSize as X

import Data.Default

-- | > == TextDecThickness TW1 ==> "decoration-1"
instance Default TextDecThickness where
  def = TextDecThickness def

instance ShowTW TextDecThickness where
  showTW thk = "decoration-" <> ( case thk of
                                    FromFont -> "from-font"
                                    TextDecThickness twnum -> showTW twnum
                                    TextDecThickness_Custom cssSize -> "[" <> (renderCSS cssSize) <> "]"
                                )

data TextDecThickness
  = FromFont
  -- ^ "decoration-from-font"
  | TextDecThickness TWNum
  -- ^ "decoration-(0|1|2|4|8)"
  | TextDecThickness_Custom CSSSize -- TODO: is this just size??
  -- ^ "decoration-[<css>]"
  deriving Show
