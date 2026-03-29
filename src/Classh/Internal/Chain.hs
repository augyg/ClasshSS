-- |
-- Module      : Classh.Internal.Chain
-- Description : Chain utility for function composition
-- Copyright   : (c) Galen Sprout, 2024
-- License     : MIT
-- Maintainer  : galen.sprout@gmail.com

module Classh.Internal.Chain where

import qualified Data.Text as T 

-- Chain like functionality
------ Helpers
infixr 0 <&>
(<&>) :: T.Text -> T.Text -> T.Text
a <&> b
  | a == "" = b
  | b == "" = a
  | otherwise = a <> " " <> b

