--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Text.Decoration.LineType
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  These types are based on https://tailwindcss.com/docs/text-decoration
--
--  Example use:
--
-- @
--  $(classh' [ text_decoration . textDec_line .~~ Underline ])
-- @
--------------------------------------------------------------------------------


module Classh.Text.Decoration.LineType where

import Classh.Class.ShowTW
import Classh.Internal.TShow

import Data.Default
import qualified Data.Text as T

-- | > == NoUnderline
instance Default TextDecLineType where
  def = NoUnderline

instance ShowTW TextDecLineType where
  showTW = \case
    LineThrough -> "line-through"
    NoUnderline -> "no-underline"
    x -> T.toLower . tshow $ x

data TextDecLineType
  = Underline
  -- ^ "underline"
  | Overline
  -- ^ "overline"
  | LineThrough
  -- ^ "line-through"
  | NoUnderline
  -- ^ "no-underline"
  deriving Show
