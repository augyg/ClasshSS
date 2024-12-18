--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Cursor
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Types to represent tailwind mouse interactivity
--  see https://tailwindcss.com/docs/cursor
--
--  Example use:
--
-- @
--  $(classh' [ text_cursor .~~ CursorPointer ])
-- @
--------------------------------------------------------------------------------


module Classh.Cursor where

import Classh.Class.ShowTW
import Classh.Internal.Utils

import Data.Default
import qualified Data.Text as T

data CursorStyle
  = CursorAuto
  | CursorDefault
  | CursorPointer
  | CursorWait
  | CursorText
  | CursorMove
  | CursorHelp
  | CursorNotAllowed
  | CursorNone
  | CursorContextMenu
  | CursorProgress
  | CursorCell
  | CursorCrosshair
  | CursorVerticalText
  | CursorAlias
  | CursorCopy
  | CursorNoDrop
  | CursorGrab
  | CursorGrabbing
  | CursorAllScroll
  | CursorColResize
  | CursorRowResize
  | CursorNResize
  | CursorEResize
  | CursorSResize
  | CursorWResize
  | CursorNEResize
  | CursorNWResize
  | CursorSEResize
  | CursorSWResize
  | CursorEWResize
  | CursorNSResize
  | CursorNESWResize
  | CursorNWSEResize
  | CursorZoomIn
  | CursorZoomOut
  deriving Show

-- | > == CursorDefault
instance Default CursorStyle where
  def = CursorDefault

instance ShowTW CursorStyle where
  showTW cursor = T.pack $ toKebabCase (show cursor)
