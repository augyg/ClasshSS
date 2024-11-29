module Classh.Class.CompileStyle where

import qualified Data.Text as T 

type CompiledS = T.Text

class CompileStyle tw where
  compileS :: tw -> Either T.Text T.Text
