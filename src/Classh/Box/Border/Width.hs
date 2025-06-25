module Classh.Box.Border.Width where

import Classh.Class.ShowTW
import Classh.Class.SetSides
import Classh.Class.IsCSS
import Classh.Responsive.WhenTW
import Classh.Internal.Chain
import Classh.Internal.CSSSize
import Classh.Internal.TShow
import Data.Default
import Control.Lens (lens, makeLenses)
import qualified Data.Text as T

-- |Holds 'BorderWidth' by side. 
--  see https://tailwindcss.com/docs/border-width
-- 
--  For example:
--
--  > elClass "div" $(classh' [ border . bWidth . borderWidth_t .~~ B2 ])
--  > -- Or with shorthand
--  > elClass "div" $(classh' [ bw_t .~~ B2 ])
data BorderWidthSides = BorderWidthSides
  { _borderWidth_l :: WhenTW BorderWidth
  , _borderWidth_r :: WhenTW BorderWidth
  , _borderWidth_t :: WhenTW BorderWidth
  , _borderWidth_b :: WhenTW BorderWidth
  } deriving Show

-- | Border Width options, eg. B0 ==> "border-0"
--
-- see https://tailwindcss.com/docs/border-width
data BorderWidth
  = B0
  | B1
  | B2
  | B4
  | B8
  | BW_Custom CSSSize
  deriving Show


instance Default BorderWidth where
  def = B0

instance ShowTW BorderWidth where
  showTW = \case
    B1 -> ""
    BW_Custom cssSize -> "-[" <> renderCSS cssSize <> "]"
    other -> "-" <> (T.drop 1 . tshow $ other)

instance Default BorderWidthSides where
  def = BorderWidthSides def def def def

instance ShowTW BorderWidthSides where
  showTW cfg = foldr (<&>) mempty
    [ renderWhenTW (_borderWidth_l cfg) ((<>) "border-l" . showTW)
    , renderWhenTW (_borderWidth_r cfg) ((<>) "border-r" . showTW)
    , renderWhenTW (_borderWidth_t cfg) ((<>) "border-t" . showTW)
    , renderWhenTW (_borderWidth_b cfg) ((<>) "border-b" . showTW)
    ]
    
makeLenses ''BorderWidthSides


-- | Like border-l-'BorderWidth', eg border-l-8
instance SetSides BorderWidthSides BorderWidth where
  l = borderWidth_l
  r = borderWidth_r
  t = borderWidth_t
  b = borderWidth_b
  x = lens _borderWidth_l $ \tw new -> tw { _borderWidth_l = new, _borderWidth_r = new }
  y = lens _borderWidth_b $ \tw new -> tw { _borderWidth_b = new, _borderWidth_t = new }
  xy = lens _borderWidth_b $ \tw new -> tw { _borderWidth_b = new
                                           , _borderWidth_t = new
                                           , _borderWidth_l = new
                                           , _borderWidth_r = new
                                           }


instance Semigroup BorderWidthSides where
  (<>) a_ b_ = BorderWidthSides
    { _borderWidth_l = _borderWidth_l a_ <> _borderWidth_l b_
    , _borderWidth_r = _borderWidth_r a_ <> _borderWidth_r b_
    , _borderWidth_t = _borderWidth_t a_ <> _borderWidth_t b_
    , _borderWidth_b = _borderWidth_b a_ <> _borderWidth_b b_
    }
