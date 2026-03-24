{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FunctionalDependencies #-}

-- |
-- Module      : Classh.Class.SetSides
-- Description : SetSides typeclass for side-specific property setters
-- Copyright   : (c) Galen Sprout, 2024
-- License     : MIT
-- Maintainer  : galen.sprout@gmail.com

module Classh.Class.SetSides where

import Classh.Responsive.WhenTW (WhenTW)
import Control.Lens (Lens')

-- | This class allows for shorthand for a config that is based on sides, such
-- | as padding or margin or border
class SetSides tw a | tw -> a where
  x :: Lens' tw (WhenTW a)
  y :: Lens' tw (WhenTW a)
  xy :: Lens' tw (WhenTW a)
  allS :: Lens' tw (WhenTW a)
  allS = xy
  l :: Lens' tw (WhenTW a)
  r :: Lens' tw (WhenTW a)
  b :: Lens' tw (WhenTW a)
  t :: Lens' tw (WhenTW a)
