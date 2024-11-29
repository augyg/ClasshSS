module Classh.Text.Decoration.LineType where

import Classh.Class.ShowTW
import Classh.Internal.TShow

import Data.Default
import qualified Data.Text as T


instance Default TextDecLineType where
  def = NoUnderline

instance ShowTW TextDecLineType where
  showTW = \case
    LineThrough -> "line-through"
    NoUnderline -> "no-underline"
    x -> T.toLower . tshow $ x

data TextDecLineType
  = Underline
  | Overline
  | LineThrough
  | NoUnderline
  deriving Show
