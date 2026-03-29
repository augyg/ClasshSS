-- |
-- Module      : Classh.TextPosition.Lead
-- Description : Line height (leading) types
-- Copyright   : (c) Galen Sprout, 2024
-- License     : MIT
-- Maintainer  : galen.sprout@gmail.com

module Classh.TextPosition.Lead where

import Classh.Class.ShowTW (ShowTW(..))
import Data.Default (Default(..))
import qualified Data.Text as T

-- Line Height
data Lead
  = LHNone
  | LHTight
  | LHSnug
  | LHNormal
  | LHRelaxed
  | LHLoose
  | LH_Custom T.Text
  deriving (Show, Eq)

instance Default Lead where
  def = LHNormal

instance ShowTW Lead where
  showTW = \case
    LHNone         -> "leading-none"
    LHTight        -> "leading-tight"
    LHSnug         -> "leading-snug"
    LHNormal       -> "leading-normal"
    LHRelaxed      -> "leading-relaxed"
    LHLoose        -> "leading-loose"
    LH_Custom t    -> "leading-" <> t



