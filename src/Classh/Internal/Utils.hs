module Classh.Internal.Utils where

import Data.Char

toKebabCase :: String -> String
toKebabCase [] = []
toKebabCase (x:xs) = toLower x : go xs
  where
    go [] = []
    go (y:ys)
      | isUpper y = '-' : toLower y : go ys
      | otherwise = y : go ys
 
