module Classh.TextPosition.Track where


import Classh.Class.ShowTW
import Data.Default
import qualified Data.Text as T


-- Letter Spacing
data Track
  = Tighter
  | Tight
  | TrackNormal
  | Wide
  | Wider
  | Widest
  | Spacing_Custom T.Text
  deriving (Show, Eq)

instance Default Track where
  def = TrackNormal


instance ShowTW Track where
  showTW = \case
    Tighter         -> "tracking-tighter"
    Tight           -> "tracking-tight"
    TrackNormal     -> "tracking-normal"
    Wide            -> "tracking-wide"
    Wider           -> "tracking-wider"
    Widest          -> "tracking-widest"
    Spacing_Custom t -> "tracking-" <> t
