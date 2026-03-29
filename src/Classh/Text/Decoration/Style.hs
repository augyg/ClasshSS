--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Text.Decoration.Style
--  Description :  Text decoration style types
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Simple datatype to represent https://tailwindcss.com/docs/text-decoration-style
--
--  Example use:
--
-- @
--  $(classh' [ text_decoration . textDec_style .~~ Wavy ])
-- @
--------------------------------------------------------------------------------


module Classh.Text.Decoration.Style where

import Classh.Class.ShowTW (ShowTW(..))
import Classh.Internal.TShow (tshow)

import Data.Default (Default(..))
import qualified Data.Text as T

-- | > == Solid 
instance Default TextDecStyle where
  def = Solid

instance ShowTW TextDecStyle where
  showTW = ((<>) "decoration-") . T.toLower . tshow

data TextDecStyle
  = Solid
  -- ^ "decoration-solid" 
  | Double
  -- ^ "decoration-double"
  | Dotted
  -- ^ "decoration-dotted"
  | Dashed
  -- ^ "decoration-dashed"
  | Wavy
  -- ^ "decoration-wavy"
  deriving Show
