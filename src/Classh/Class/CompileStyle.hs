module Classh.Class.CompileStyle where

import qualified Data.Text as T 

-- | Synonym to represent Classh's TH output
type CompiledS = T.Text

-- | Validate Classh options, in the case of Left this will fail at compile time 
class CompileStyle tw where
  compileS :: tw -> Either T.Text T.Text
