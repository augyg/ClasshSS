--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Box.Shadow
--  Description :  Shadow styling types
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Types to represent tailwind box shadow
--  see https://v3.tailwindcss.com/docs/box-shadow
--
--  Example use:
--
-- @
--  $(classh' [ shadow .~~ Shadow_Md ])
--  $(classh' [ shadow .~ [("def", Shadow_Md), ("hover", Shadow_Lg)] ])
-- @
--------------------------------------------------------------------------------

module Classh.Box.Shadow where

import Classh.Class.ShowTW (ShowTW(..))
import Classh.Internal.TShow (tshow)
import Data.Default (Default(..))
import qualified Data.Text as T

-- | Box shadow options
-- see https://tailwindcss.com/docs/box-shadow
data BoxShadow
  = Shadow_Sm      -- ^ shadow-sm: box-shadow: 0 1px 2px 0 rgb(0 0 0 / 0.05)
  | Shadow         -- ^ shadow: box-shadow: 0 1px 3px 0 rgb(0 0 0 / 0.1), 0 1px 2px -1px rgb(0 0 0 / 0.1)
  | Shadow_Md      -- ^ shadow-md: box-shadow: 0 4px 6px -1px rgb(0 0 0 / 0.1), 0 2px 4px -2px rgb(0 0 0 / 0.1)
  | Shadow_Lg      -- ^ shadow-lg: box-shadow: 0 10px 15px -3px rgb(0 0 0 / 0.1), 0 4px 6px -4px rgb(0 0 0 / 0.1)
  | Shadow_Xl      -- ^ shadow-xl: box-shadow: 0 20px 25px -5px rgb(0 0 0 / 0.1), 0 8px 10px -6px rgb(0 0 0 / 0.1)
  | Shadow_2Xl     -- ^ shadow-2xl: box-shadow: 0 25px 50px -12px rgb(0 0 0 / 0.25)
  | Shadow_Inner   -- ^ shadow-inner: box-shadow: inset 0 2px 4px 0 rgb(0 0 0 / 0.05)
  | Shadow_None    -- ^ shadow-none: box-shadow: 0 0 #0000
  | Shadow_Custom T.Text  -- ^ Arbitrary shadow value, e.g., Shadow_Custom "0_35px_60px_-15px_rgba(0,0,0,0.3)"
  deriving Show

instance Default BoxShadow where
  def = Shadow_None

instance ShowTW BoxShadow where
  showTW = \case
    Shadow -> "shadow"
    Shadow_Custom val -> "shadow-[" <> val <> "]"
    other -> "shadow-" <> (T.toLower . T.drop 7 . tshow $ other)
