--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Text.Weight
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Types to represent tailwind text weight options: https://tailwindcss.com/docs/font-weight
--
--  Example use:
--
-- @
--  $(classh' [ text_weight .~~ Extrabold ])
-- @
--------------------------------------------------------------------------------


module Classh.Text.Weight where

import Classh.Internal.TShow
import Classh.Class.ShowTW

import Data.Default
import qualified Data.Text as T

-- | > == Normal
instance Default TextWeight where
  def = Normal

instance ShowTW TextWeight where
  showTW (TextWeight_Custom t) = "font-[" <> t <> "]"
  showTW (Black_TextWeight) = "font-black"
  showTW x = "font-" <> (T.toLower . tshow $ x )

data TextWeight
  = Thin
  -- ^ "font-thin"
  | Extralight
  -- ^ "font-extralight"
  | Light
  -- ^ "font-light"
  | Normal
  -- ^ "font-normal"
  | Medium
  -- ^ "font-medium"
  | Semibold
  -- ^ "font-semibold"
  | Bold
  -- ^ "font-bold"
  | Extrabold
  -- ^ "font-extrabold"
  | Black_TextWeight
  -- ^ "font-black"
  | TextWeight_Custom T.Text
  -- ^ eg. "font-[1100]"
  deriving Show
