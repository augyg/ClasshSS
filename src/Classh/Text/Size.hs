module Classh.Text.Size where


import Classh.Class.ShowTW

import Data.Default
import qualified Data.Text as T

-- | TODO: when this inevitably becomes its own package, make it easy to not import Defaults and make your own
instance Default TextSize where
  def = XL2 -- According to testing this is the default size `\_o_/'

instance ShowTW TextSize where
  showTW (TextSize_Custom t) = "text-[" <> t <> "]"
  showTW XS = "text-xs"
  showTW SM = "text-sm"
  showTW Base = "text-base"
  showTW LG = "text-lg"
  showTW XL = "text-xl"
  showTW XL2 = "text-2xl"
  showTW XL3 = "text-3xl"
  showTW XL4 = "text-4xl"
  showTW XL5 = "text-5xl"
  showTW XL6 = "text-6xl"
  showTW XL7 = "text-7xl"
  showTW XL8 = "text-8xl"
  showTW XL9 = "text-9xl"

data TextSize
  = XS
  | SM
  | Base
  | LG
  | XL
  | XL2 -- According to testing this is the default size `\_o_/`
  | XL3
  | XL4
  | XL5
  | XL6
  | XL7
  | XL8
  | XL9
  | TextSize_Custom T.Text
  deriving Show
