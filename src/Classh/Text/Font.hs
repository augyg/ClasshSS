
--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Text.Font
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Types to represent tailwind font family, https://tailwindcss.com/docs/font-family
--
--  Example use:
--
-- @
--  -- needs accompanying way to get Sarabun font, eg google fonts
--  $(classh' [ text_font .~~ Font_Custom "Sarabun" ])
--  -- simple example
--  $(classh' [ text_font .~~ Sans ])
-- @
--------------------------------------------------------------------------------


module Classh.Text.Font where

import Classh.Internal.TShow
import Classh.Class.ShowTW

import Data.Default
import qualified Data.Text as T


data Font
  = Sans
  -- ^ "font-sans"
  | Serif
  -- ^ "font-serif"
  | Mono
  -- ^ "font-mono"
  | Font_Custom T.Text
  -- ^ eg. font-[Sarabun]
  deriving Show

-- | > == Sans
instance Default Font where
  def = Sans

instance ShowTW Font where
  showTW = \case
    Font_Custom t -> "font-[" <> t <> "]"
    x -> ((<>) "font-") . T.toLower . tshow $ x

