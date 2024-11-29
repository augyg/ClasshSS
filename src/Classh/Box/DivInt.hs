module Classh.Box.DivInt where

import Classh.Class.ShowTW
import Classh.Internal.TShow

import Data.Default
import qualified Data.Text as T

-- i believe this is for TWSize fractionals

data DivInt
  = D2
  | D3
  | D4
  | D5
  | D6
  | D12
  deriving Show

instance ShowTW DivInt where
  showTW = T.drop 1 . tshow

instance Default DivInt where
  def = D12 -- baby
