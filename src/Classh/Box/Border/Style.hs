module Classh.Box.Border.Style where

import Classh.Class.ShowTW
import Classh.Internal.TShow
import Data.Default
import qualified Data.Text as T

-- | Border Style options, eg BSolid ==> "border-solid"
-- 
-- see https://tailwindcss.com/docs/border-style
data BorderStyle
  = BSolid
  | BDashed
  | BDotted
  | BDouble
  | BHidden
  | BNone
  deriving Show

instance Default BorderStyle where
  def = BSolid

instance ShowTW BorderStyle where
  showTW = T.toLower . T.drop 1 . tshow
