module Classh.Text.Font where

import Classh.Internal.TShow
import Classh.Class.ShowTW

import Data.Default
import qualified Data.Text as T


data Font
  = Sans
  | Serif
  | Mono
  | Font_Custom T.Text
  deriving Show

instance Default Font where
  def = Sans

instance ShowTW Font where
  showTW = \case
    Font_Custom t -> "font-[" <> t <> "]"
    x -> ((<>) "font-") . T.toLower . tshow $ x

