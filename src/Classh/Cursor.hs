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

instance Default CursorStyle where
  def = CursorDefault

instance ShowTW CursorStyle where
  showTW cursor = T.pack $ toKebabCase (show cursor)
