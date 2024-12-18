--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Grid
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Types to represent tailwind grids
-- 
--  Example use (with Classh.Reflex):
--
-- @
--  gridCol Col12 $ col [12] $ text "hey" 
-- @
--------------------------------------------------------------------------------


module Classh.Grid where

import Classh.Class.ShowTW

import Data.Default
import qualified Data.Text as T

-- | The number of Columns to split a row into. Classh will detect the use of this construct
-- | and ensure the grid class is applied alongside it and appear exactly once
data ColInt
  = Col1
  -- ^ "grid-cols-1"
  | Col2
  -- ^ "grid-cols-2"
  | Col3
  -- ^ "grid-cols-3"
  | Col4
  -- ^ "grid-cols-4"
  | Col5
  -- ^ "grid-cols-5"
  | Col6
  -- ^ "grid-cols-6"
  | Col7
  -- ^ "grid-cols-7"
  | Col8
  -- ^ "grid-cols-8"
  | Col9
  -- ^ "grid-cols-9"
  | Col10
  -- ^ "grid-cols-10"
  | Col11
  -- ^ "grid-cols-11"
  | Col12
  -- ^ "grid-cols-12"
  deriving Show

instance ShowTW ColInt where
  showTW = T.drop 3 . T.pack . show

-- | > == Col1
instance Default ColInt where
  def = Col1
