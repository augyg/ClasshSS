--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Text.Size
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Types to represent tailwind text size options: https://tailwindcss.com/docs/font-size
--
--  Example use:
--
-- @
--  $(classh' [ text_size .~~ XL4 ])
-- @
--------------------------------------------------------------------------------



module Classh.Text.Size where


import Classh.Class.ShowTW

import Data.Default
import qualified Data.Text as T

-- | TODO: when this inevitably becomes its own package, make it easy to not import Defaults and make your own
instance Default TextSize where
  def = XL2 -- According to testing this is the default size `\_o_/'

instance ShowTW TextSize where
  showTW (TextSize_Custom t) = "text-[" <> t <> "]"
  showTW XS = "text-xs"
  showTW SM = "text-sm"
  showTW Base = "text-base"
  showTW LG = "text-lg"
  showTW XL = "text-xl"
  showTW XL2 = "text-2xl"
  showTW XL3 = "text-3xl"
  showTW XL4 = "text-4xl"
  showTW XL5 = "text-5xl"
  showTW XL6 = "text-6xl"
  showTW XL7 = "text-7xl"
  showTW XL8 = "text-8xl"
  showTW XL9 = "text-9xl"

data TextSize
  = XS
  -- ^ "text-xs"
  | SM
  -- ^ "text-sm"
  | Base
  -- ^ "text-base"
  | LG
  -- ^ "text-lg"
  | XL
  -- ^ "text-xl"
  | XL2 -- According to testing this is the default size `\_o_/`
  -- ^ "text-2xl"
  | XL3
  -- ^ "text-3xl"
  | XL4
  -- ^ "text-4xl"
  | XL5
  -- ^ "text-5xl"
  | XL6
  -- ^ "text-6xl"
  | XL7
  -- ^ "text-7xl"
  | XL8
  -- ^ "text-8xl"
  | XL9
  -- ^ "text-9xl"
  | TextSize_Custom T.Text
  -- ^ eg. "text-[9px]"
  deriving Show
