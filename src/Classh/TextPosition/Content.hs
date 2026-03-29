-- |
-- Module      : Classh.TextPosition.Content
-- Description : Content sizing types
-- Copyright   : (c) Galen Sprout, 2024
-- License     : MIT
-- Maintainer  : galen.sprout@gmail.com

module Classh.TextPosition.Content where


import Classh.Class.ShowTW (ShowTW(..))
import Data.Default (Default(..))
import qualified Data.Text as T


-- Content Wildcard
newtype Content = Content_Custom T.Text
  deriving (Show, Eq)

instance Default Content where
  def = Content_Custom ""


instance ShowTW Content where
  showTW (Content_Custom t) = "content-" <> t

