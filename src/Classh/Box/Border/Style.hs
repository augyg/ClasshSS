-- |
-- Module      : Classh.Box.Border.Style
-- Description : Border style types
-- Copyright   : (c) Galen Sprout, 2024
-- License     : MIT
-- Maintainer  : galen.sprout@gmail.com

module Classh.Box.Border.Style where

import Classh.Class.ShowTW (ShowTW(..))
import Classh.Internal.TShow (tshow)
import Data.Default (Default(..))
import qualified Data.Text as T

-- | Border Style options, eg BSolid ==> "border-solid"
-- 
-- see https://tailwindcss.com/docs/border-style
data BorderStyle
  = BSolid
  | BDashed
  | BDotted
  | BDouble
  | BHidden
  | BNone
  deriving Show

instance Default BorderStyle where
  def = BSolid

instance ShowTW BorderStyle where
  showTW = T.toLower . T.drop 1 . tshow
