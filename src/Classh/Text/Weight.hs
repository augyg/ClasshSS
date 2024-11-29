module Classh.Text.Weight where

import Classh.Internal.TShow
import Classh.Class.ShowTW

import Data.Default
import qualified Data.Text as T


instance Default TextWeight where
  def = Normal

instance ShowTW TextWeight where
  showTW (TextWeight_Custom t) = "font-[" <> t <> "]"
  showTW (Black_TW) = "font-black"
  showTW x = "font-" <> (T.toLower . tshow $ x )

data TextWeight
  = Thin
  | Extralight
  | Light
  | Normal
  | Medium
  | Semibold
  | Bold
  | Extrabold
  | Black_TW
  | TextWeight_Custom T.Text
  deriving Show
