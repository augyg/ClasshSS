-- |
-- Module      : Classh.Class.HasCustom
-- Description : HasCustom typeclass for custom CSS escape hatch
-- Copyright   : (c) Galen Sprout, 2024
-- License     : MIT
-- Maintainer  : galen.sprout@gmail.com

module Classh.Class.HasCustom where

import Control.Lens (Lens')
import qualified Data.Text as T

-- | Allows for shorter applications of custom classes to a Box or Text 
class HasCustom tw where
  custom :: Lens' tw T.Text

