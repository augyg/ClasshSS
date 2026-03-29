-- |
-- Module      : Classh.Class.HasCSSSize
-- Description : HasCSSSize typeclass for sized types
-- Copyright   : (c) Galen Sprout, 2024
-- License     : MIT
-- Maintainer  : galen.sprout@gmail.com

module Classh.Class.HasCSSSize where

import Classh.Internal.CSSSize (CSSSize (Percent, Pixel, Rem, Vh, Vw))

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

