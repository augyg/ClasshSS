--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Internal.TWNum
--  common numerical system used across different Tailwind classes. This is just
--  a discrete version of Int
--------------------------------------------------------------------------------


module Classh.Internal.TWNum where

import Classh.Class.ShowTW
import Classh.Internal.TShow

import Data.Default
import qualified Data.Text as T


instance Default TWNum where
  def = TW1

--  TODO: see note about color / showColor ; showTWNum
instance ShowTW TWNum where
  showTW Auto = "auto"
  showTW x = T.drop 2 $ tshow x

data TWNum
  = Auto
  | TW0
  | TW1
  | TW2
  | TW4
  | TW8
  deriving Show
