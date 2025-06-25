module Classh.Box.Border.Radius where

import Classh.Class.ShowTW
import Classh.Class.SetSides
import Classh.Class.IsCSS
import Classh.Responsive.WhenTW
import Classh.Internal.Chain
import Classh.Internal.CSSSize
import Classh.Internal.TShow
import Data.Default
import Control.Lens (Lens', lens, makeLenses)
import qualified Data.Text as T

-- |Holds 'BorderRadius' by corner
-- see https://tailwindcss.com/docs/border-radius
-- 
--  For example:
-- 
-- > elClass "div" $(classh' [ border . radius . borderRadius_tr .~~ R_3Xl, border . radius . borderRadius_tl .~~ R_3Xl ])
-- > -- Or with shorthand
-- > elClass "div" $(classh' [ br_t .~~ R_3Xl ])
data BorderRadiusCorners = BorderRadiusCorners
  { _borderRadius_tr :: WhenTW BorderRadius'
  , _borderRadius_tl :: WhenTW BorderRadius'
  , _borderRadius_br :: WhenTW BorderRadius'
  , _borderRadius_bl :: WhenTW BorderRadius'
  } deriving Show

-- | Border radius options, eg R_3Xl ==> "rounded-3xl"
--
-- see https://tailwindcss.com/docs/border-radius
data BorderRadius'
  = R_None
  | R_SM
  | R_Normal
  | R_Md
  | R_Lg
  | R_Xl
  | R_2Xl
  | R_3Xl
  | R_Full
  | R_Custom CSSSize
  deriving Show

instance Default BorderRadius' where
  def = R_None

instance ShowTW BorderRadius' where
  showTW = \case
    R_Custom cssSize -> "-[" <> renderCSS cssSize <> "]"
    R_Normal -> ""
    other -> "-" <> (T.toLower . T.drop 2 . tshow $ other)

makeLenses ''BorderRadiusCorners


instance Default BorderRadiusCorners where
  def = BorderRadiusCorners def def def def

-- TODO: stop overlaps through conditionals
instance ShowTW BorderRadiusCorners where
  showTW cfg = foldr (<&>) mempty
    [ renderWhenTW (_borderRadius_tr cfg) ((<>) "rounded-tr" . showTW)
    , renderWhenTW (_borderRadius_tl cfg) ((<>) "rounded-tl" . showTW)
    , renderWhenTW (_borderRadius_br cfg) ((<>) "rounded-br" . showTW)
    , renderWhenTW (_borderRadius_bl cfg) ((<>) "rounded-bl" . showTW)
    ]

-- | Like rounded-(t|r|b|l|tl|...)-'BorderRadius'', eg rounded-tl-xl
instance SetSides BorderRadiusCorners BorderRadius' where
  l = borderRadius_l
  r = borderRadius_r
  b = borderRadius_b
  t = borderRadius_t
  xy = lens _borderRadius_tl $ \tw new -> tw { _borderRadius_tl = new
                                             , _borderRadius_bl = new
                                             , _borderRadius_tr = new
                                             , _borderRadius_br = new
                                             }
  -- Effectively, due to corners;
  y = xy
  x = xy




borderRadius_l, borderRadius_r, borderRadius_t, borderRadius_b :: Lens' BorderRadiusCorners (WhenTW BorderRadius')
borderRadius_l = lens undefined $ \tw new -> tw { _borderRadius_tl = new, _borderRadius_bl = new }
borderRadius_r = lens undefined $ \tw new -> tw { _borderRadius_tr = new, _borderRadius_br = new }
borderRadius_t = lens undefined $ \tw new -> tw { _borderRadius_tl = new, _borderRadius_tr = new }
borderRadius_b = lens undefined $ \tw new -> tw { _borderRadius_bl = new, _borderRadius_br = new }



instance Semigroup BorderRadiusCorners where
  (<>) a_ b_ = BorderRadiusCorners
    { _borderRadius_tr = _borderRadius_tr a_ <> _borderRadius_tr b_
    , _borderRadius_tl = _borderRadius_tl a_ <> _borderRadius_tl b_
    , _borderRadius_br = _borderRadius_br a_ <> _borderRadius_br b_
    , _borderRadius_bl = _borderRadius_bl a_ <> _borderRadius_bl b_
    }
