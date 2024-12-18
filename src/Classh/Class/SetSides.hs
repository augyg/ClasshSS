{-# LANGUAGE MultiParamTypeClasses #-}

module Classh.Class.SetSides where

import Classh.Responsive.WhenTW
import Control.Lens

-- | This class allows for shorthand for a config that is based on sides, such
-- | as padding or margin or border
class SetSides tw a where
  x :: Lens' tw (WhenTW a)
  y :: Lens' tw (WhenTW a)
  xy :: Lens' tw (WhenTW a)
  allS :: Lens' tw (WhenTW a)
  allS = xy
  l :: Lens' tw (WhenTW a)
  r :: Lens' tw (WhenTW a)
  b :: Lens' tw (WhenTW a)
  t :: Lens' tw (WhenTW a)
