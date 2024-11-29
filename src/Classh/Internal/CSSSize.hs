module Classh.Internal.CSSSize where

import Classh.Internal.TShow

-- we should make a Module tree: Classh.TWSize.(TWSize | CSSSize | Fraction)
import Classh.Class.IsCSS

-- | Right now this is just for custom uses in Tailwind
data CSSSize
  = Pixel Int
  | Percent Int
  | Vh Int
  | Vw Int
  | Rem Float
  deriving Show

instance IsCSS CSSSize where
  renderCSS = \case
    Pixel int -> tshow int <> "px"
    Percent int -> tshow int <> "%"
    Vh int -> tshow int <> "vh"
    Vw int -> tshow int <> "vw"
    Rem float -> tshow float <> "rem"

