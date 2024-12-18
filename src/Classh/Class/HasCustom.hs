module Classh.Class.HasCustom where

import Control.Lens 
import qualified Data.Text as T

-- | Allows for shorter applications of custom classes to a Box or Text 
class HasCustom tw where
  custom :: Lens' tw T.Text

