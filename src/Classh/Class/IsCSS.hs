module Classh.Class.IsCSS where

import qualified Data.Text as T

-- | A generic manner to turn CSS to the corresponding string 
class IsCSS css where
  renderCSS :: css -> T.Text
