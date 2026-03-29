-- |
-- Module      : Classh.Class.IsCSS
-- Description : IsCSS typeclass for CSS representable types
-- Copyright   : (c) Galen Sprout, 2024
-- License     : MIT
-- Maintainer  : galen.sprout@gmail.com

module Classh.Class.IsCSS where

import qualified Data.Text as T

-- | A generic manner to turn CSS to the corresponding string 
class IsCSS css where
  renderCSS :: css -> T.Text
