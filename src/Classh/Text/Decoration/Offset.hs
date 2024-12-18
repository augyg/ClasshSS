--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Text.Decoration.Offset
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Simple datatype to represent https://tailwindcss.com/docs/text-underline-offset
--
--  Example use:
--
-- @
--  $(classh' [ text_decoration . textDec_offset .~~ TextDecOffSet TW1 ])
-- @
--------------------------------------------------------------------------------

module Classh.Text.Decoration.Offset where

import Classh.Class.ShowTW
import Classh.Class.IsCSS
import Classh.Internal.CSSSize
import Classh.Internal.TWNum

{-
TODOS:
Could TWNum have a variant of CSSSize ? then this almost becomes irrelevant as a leaf of
of the larger config?
-}

import Data.Default


instance Default TextDecOffset where
  def = TextDecOffset def

instance ShowTW TextDecOffset where
  showTW (TextDecOffset tnum) = "underline-offset-" <> (showTW tnum)
  showTW (TextDecOffset_Custom cssS) = "underline-offset-[" <> (renderCSS cssS) <> "]"

data TextDecOffset
  = TextDecOffset TWNum
  | TextDecOffset_Custom CSSSize
  deriving Show
