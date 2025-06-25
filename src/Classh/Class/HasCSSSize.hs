module Classh.Class.HasCSSSize where

import Classh.Internal.CSSSize

-- | Represents the ability to use raw CSS Sizing in a given instance/context
class HasCSSSize tw where
  pix :: Int -> tw 
  pct :: Int -> tw
  vh :: Int -> tw
  vw :: Int -> tw
  rem :: Float -> tw

-- Upstream
instance HasCSSSize CSSSize where
  pix = Pixel
  pct = Percent
  vh = Vh
  vw = Vw
  rem = Rem

