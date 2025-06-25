module Classh.Box.Border.Color where

import Classh.Class.ShowTW
import Classh.Class.SetSides
import Classh.Responsive.WhenTW
import Classh.Internal.Chain
import Classh.Color
import Data.Default
import Control.Lens (lens, makeLenses)


-- |Holds Border 'Color' by side
-- 
--  For example:
-- 
-- > elClass "div" $(classh' [ border . bColor . borderColor_t .~~ Black ])
-- > -- Or with shorthand
-- > elClass "div" $(classh' [ bc_t .~~ Black ])
data BorderColorSides = BorderColorSides
  { _borderColor_l :: WhenTW Color
  -- ^ border-l-'Color' ... see https://tailwindcss.com/docs/border-color
  , _borderColor_r :: WhenTW Color
  -- ^ border-r-'Color' ... see https://tailwindcss.com/docs/border-color
  , _borderColor_t :: WhenTW Color
  -- ^ border-t-'Color' ... see https://tailwindcss.com/docs/border-color
  , _borderColor_b :: WhenTW Color
  -- ^ border-b-'Color' ... see https://tailwindcss.com/docs/border-color 
  } deriving Show

instance Default BorderColorSides where
  def = BorderColorSides def def def def


instance ShowTW BorderColorSides where
  showTW cfg = foldr (<&>) mempty
    [ renderWhenTW (_borderColor_l cfg) ((<>) "border-l-" . showTW)
    , renderWhenTW (_borderColor_r cfg) ((<>) "border-r-" . showTW)
    , renderWhenTW (_borderColor_t cfg) ((<>) "border-t-" . showTW)
    , renderWhenTW (_borderColor_b cfg) ((<>) "border-b-" . showTW)
    ]

makeLenses ''BorderColorSides

-- | Like border-'Color', eg border-white
instance SetSides BorderColorSides Color where
  l = borderColor_l
  r = borderColor_r
  t = borderColor_t
  b = borderColor_b
  x = lens _borderColor_l $ \tw new -> tw { _borderColor_l = new, _borderColor_r = new }
  y = lens _borderColor_t $ \tw new -> tw { _borderColor_t = new, _borderColor_b = new }
  xy = lens _borderColor_t $ \tw new -> tw { _borderColor_t = new
                                           , _borderColor_b = new
                                           , _borderColor_l = new
                                           , _borderColor_r = new
                                           }


instance Semigroup BorderColorSides where
  (<>) a_ b_ = BorderColorSides
    { _borderColor_l = _borderColor_l a_ <> _borderColor_l b_
    , _borderColor_r = _borderColor_r a_ <> _borderColor_r b_
    , _borderColor_t = _borderColor_t a_ <> _borderColor_t b_
    , _borderColor_b = _borderColor_b a_ <> _borderColor_b b_
    }
