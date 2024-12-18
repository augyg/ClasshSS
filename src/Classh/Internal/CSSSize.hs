--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Internal.CSSSize
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Types to represent Tailwind sizes that allow use of CSS size
-- 
--  Example use:
--
-- @
--  $(classh' [ padding . paddingT .~~ pix 3 ])
--  -- or with shorthand
--  $(classh' [ pt .~~ pix 3 ])
-- @
--------------------------------------------------------------------------------


module Classh.Internal.CSSSize where

import Classh.Internal.TShow

-- we should make a Module tree: Classh.TWSize.(TWSize | CSSSize | Fraction)
import Classh.Class.IsCSS

-- | For use with Tailwind sizes that allow use of CSS size
data CSSSize
  = Pixel Int
  | Percent Int
  | Vh Int
  | Vw Int
  | Rem Float
  deriving Show

instance IsCSS CSSSize where
  renderCSS = \case
    Pixel int -> tshow int <> "px"
    Percent int -> tshow int <> "%"
    Vh int -> tshow int <> "vh"
    Vw int -> tshow int <> "vw"
    Rem float -> tshow float <> "rem"

