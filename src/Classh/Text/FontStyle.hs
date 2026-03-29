-- |
-- Module      : Classh.Text.FontStyle
-- Description : Font style types
-- Copyright   : (c) Galen Sprout, 2024
-- License     : MIT
-- Maintainer  : galen.sprout@gmail.com

module Classh.Text.FontStyle where

import Classh.Class.ShowTW (ShowTW(..))
import Classh.Internal.Utils (toKebabCase)

import Data.Default (Default(..))
import qualified Data.Text as T


instance Default FontStyle where
  def = NotItalic

instance ShowTW FontStyle where
  showTW font = T.pack $ toKebabCase (show font)

data FontStyle
  = Italic
  | NotItalic
  deriving Show
