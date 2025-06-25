module Classh.TextPosition.Content where


import Classh.Class.ShowTW
import Data.Default
import qualified Data.Text as T


-- Content Wildcard
newtype Content = Content_Custom T.Text
  deriving (Show, Eq)

instance Default Content where
  def = Content_Custom ""


instance ShowTW Content where
  showTW (Content_Custom t) = "content-" <> t

