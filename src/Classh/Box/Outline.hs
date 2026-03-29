--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Box.Outline
--  Description :  Outline styling configuration
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Types to represent tailwind outline
--  see https://v3.tailwindcss.com/docs/outline-width
--      https://v3.tailwindcss.com/docs/outline-style
--
--  Example use:
--
-- @
--  $(classh' [ outline .~~ Outline_None ])
-- @
--------------------------------------------------------------------------------

module Classh.Box.Outline where

import Classh.Class.ShowTW (ShowTW(..))
import Classh.Internal.TShow (tshow)
import Data.Default (Default(..))
import qualified Data.Text as T

-- | Outline style
-- see https://v3.tailwindcss.com/docs/outline-style
data OutlineStyle
  = Outline_None    -- ^ outline-none
  | Outline         -- ^ outline (solid)
  | Outline_Dashed  -- ^ outline-dashed
  | Outline_Dotted  -- ^ outline-dotted
  | Outline_Double  -- ^ outline-double
  deriving Show

instance Default OutlineStyle where
  def = Outline

instance ShowTW OutlineStyle where
  showTW = \case
    Outline -> "outline"
    other -> "outline-" <> (T.toLower . T.drop 8 . tshow $ other)
