-- |
-- Module      : Classh.TextPosition.Hyphen
-- Description : Hyphenation types
-- Copyright   : (c) Galen Sprout, 2024
-- License     : MIT
-- Maintainer  : galen.sprout@gmail.com

module Classh.TextPosition.Hyphen where

import Classh.Class.ShowTW (ShowTW(..))
import Data.Default (Default(..))
--import qualified Data.Text as T

-- Hyphens
data Hyphen
  = HNone
  | HManual
  | HAuto
  deriving (Show, Eq)

instance Default Hyphen where
  def = HNone

instance ShowTW Hyphen where
  showTW = \case
    HNone   -> "hyphens-none"
    HManual -> "hyphens-manual"
    HAuto   -> "hyphens-auto"

