-- |
-- Module      : Classh.Internal.Utils
-- Description : Internal utility functions
-- Copyright   : (c) Galen Sprout, 2024
-- License     : MIT
-- Maintainer  : galen.sprout@gmail.com

module Classh.Internal.Utils where

import Data.Char (toLower, isUpper)

toKebabCase :: String -> String
toKebabCase [] = []
toKebabCase (x:xs) = toLower x : go xs
  where
    go [] = []
    go (y:ys)
      | isUpper y = '-' : toLower y : go ys
      | otherwise = y : go ys
 
