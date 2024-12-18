--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Box.Sizing.DimensionConstraint 
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Types to represent min/max constraints on a 'BoxConfig' for width and height
--
--  Any field named _someField has an associated lens `someField`
--  see @defaultNameTransform@ from Lens.Family.THCore
--
--  This package aims to avoid forcing the user to know lenses
--
--  Example use (shorthand is to-do):
--
-- @
--  $(classh' [ sizingBand . minSize . widthC .~~ DC_sm, sizingBand . maxSize . widthC .~~ DC_7xl ])
-- @
--------------------------------------------------------------------------------


module Classh.Box.Sizing.DimensionConstraint where

import Classh.Internal.TShow
import Classh.Class.ShowTW 
import Data.Default
import qualified Data.Text as T

-- TODO: this is technically wrong: there are different classes for width vs height but oh well for now
data DimensionConstraint
  = DC_0
  | DC_none
  | DC_xs
  | DC_sm
  | DC_md
  | DC_lg
  | DC_xl
  | DC_2xl
  | DC_3xl
  | DC_4xl
  | DC_5xl
  | DC_6xl
  | DC_7xl
  | DC_full
  | DC_min
  | DC_max
  | DC_fit
  | DC_prose
  | DC_screen_sm
  | DC_screen_md
  | DC_screen_lg
  | DC_screen_xl
  | DC_screen_2xl
  | DC_Custom T.Text
  deriving Show

instance Default DimensionConstraint where
  def = DC_none

instance ShowTW DimensionConstraint where
  showTW = \case
    DC_Custom t -> "[" <> t <> "]"
    x -> T.replace "_" "-" . T.drop 3 . tshow $ x
