-- |
-- Module      : Classh.Internal.TWNum
-- Description : Tailwind numeric value types
-- Copyright   : (c) Galen Sprout, 2024
-- License     : MIT
-- Maintainer  : galen.sprout@gmail.com
--
-- Common numerical system used across different Tailwind classes. This is just
-- a discrete version of Int.


module Classh.Internal.TWNum where

import Classh.Class.ShowTW (ShowTW(..))
import Classh.Internal.TShow (tshow)

import Data.Default (Default(..))
import qualified Data.Text as T


instance Default TWNum where
  def = TW1

--  TODO: see note about color / showColor ; showTWNum
instance ShowTW TWNum where
  showTW Auto = "auto"
  showTW x = T.drop 2 $ tshow x

data TWNum
  = Auto
  | TW0
  | TW1
  | TW2
  | TW4
  | TW8
  deriving Show
