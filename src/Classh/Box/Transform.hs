--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Box.Transform
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Types to represent tailwind scale transforms
--  see https://v3.tailwindcss.com/docs/scale
--
--  Example use:
--
-- @
--  $(classh' [ scale .~ [("def", Scale_100), ("hover", Scale_105), ("active", Scale_95)] ])
-- @
--------------------------------------------------------------------------------

module Classh.Box.Transform where

import Classh.Class.ShowTW
import Classh.Internal.TShow
import Data.Default
import qualified Data.Text as T

-- | Scale transform
-- see https://v3.tailwindcss.com/docs/scale
data Scale
  = Scale_0
  | Scale_50
  | Scale_75
  | Scale_90
  | Scale_95
  | Scale_100
  | Scale_105
  | Scale_110
  | Scale_125
  | Scale_150
  | Scale_Custom T.Text  -- ^ e.g., Scale_Custom "102" for scale-[102%]
  deriving Show

instance Default Scale where
  def = Scale_100

instance ShowTW Scale where
  showTW = \case
    Scale_Custom val -> "scale-[" <> val <> "%]"
    other -> "scale-" <> (T.drop 6 . tshow $ other)
