module Classh.Class.IsCSS where

import qualified Data.Text as T

class IsCSS css where
  renderCSS :: css -> T.Text
