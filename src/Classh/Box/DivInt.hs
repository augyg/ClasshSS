--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Box.Border
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  An Enum for use by 'TWFraction' of the 'TWSizeOrFraction' Enum for 'BoxSizing'
--
--  Example use:
--
-- @
--  $(classh' [ sizingBand . size . width .~~ TWFraction 11 D12 ])
--  -- or with shorthand
--  $(classh' [ w .~~ TWFraction 11 D12 ])
-- @
--------------------------------------------------------------------------------

module Classh.Box.DivInt where

import Classh.Class.ShowTW
import Classh.Internal.TShow

import Data.Default
import qualified Data.Text as T

-- | (w|h)-'DivInt', eg. w-11/12
data DivInt
  = D2
  | D3
  | D4
  | D5
  | D6
  | D12
  deriving Show

instance ShowTW DivInt where
  showTW = T.drop 1 . tshow

-- | > == D12
instance Default DivInt where
  def = {- Im the lead singer of -} D12 -- baby
