module Classh.Class.HasCustom where

import Control.Lens 
import qualified Data.Text as T

class HasCustom tw where
  custom :: Lens' tw T.Text

