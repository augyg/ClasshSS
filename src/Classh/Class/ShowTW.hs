-- |
-- Module      : Classh.Class.ShowTW
-- Description : ShowTW typeclass for Tailwind class rendering
-- Copyright   : (c) Galen Sprout, 2024
-- License     : MIT
-- Maintainer  : galen.sprout@gmail.com

module Classh.Class.ShowTW where

import qualified Data.Text as T

-- | Common interface to take a construct in this lib and turn it to the
-- | correct Tailwind class. This is the workhorse of the entire library
class ShowTW tw where
  showTW :: tw -> T.Text
