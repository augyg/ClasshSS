module Classh.Internal.Chain where

import qualified Data.Text as T 

-- Chain like functionality
------ Helpers
infixr 0 <&>
(<&>) :: T.Text -> T.Text -> T.Text
a <&> b
  | a == "" = b
  | b == "" = a
  | otherwise = a <> " " <> b

