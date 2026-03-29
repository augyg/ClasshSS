-- |
-- Module      : Classh.Internal.TShow
-- Description : Type-level show utilities
-- Copyright   : (c) Galen Sprout, 2024
-- License     : MIT
-- Maintainer  : galen.sprout@gmail.com

module Classh.Internal.TShow where

import qualified Data.Text as T

tshow :: Show a => a -> T.Text
tshow = T.pack . show
