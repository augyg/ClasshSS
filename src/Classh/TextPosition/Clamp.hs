module Classh.TextPosition.Clamp where


import Classh.Class.ShowTW
import Data.Default
import qualified Data.Text as T


-- Line Clamp
data Clamp
  = Clamp1
  | Clamp2
  | Clamp3
  | Clamp4
  | Clamp5
  | Clamp6
  | ClampNone
  | Clamp_Custom T.Text
  deriving (Show, Eq)

instance Default Clamp where
  def = ClampNone

-- Clamp
instance ShowTW Clamp where
  showTW = \case
    Clamp1          -> "line-clamp-1"
    Clamp2          -> "line-clamp-2"
    Clamp3          -> "line-clamp-3"
    Clamp4          -> "line-clamp-4"
    Clamp5          -> "line-clamp-5"
    Clamp6          -> "line-clamp-6"
    ClampNone       -> "line-clamp-none"
    Clamp_Custom t  -> "line-clamp-" <> t


