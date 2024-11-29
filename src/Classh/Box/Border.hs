{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

module Classh.Box.Border where

-- | TODO: further break down this file

import Classh.Internal.Chain
import Classh.Internal.TShow
import Classh.Class.ShowTW
import Classh.Class.SetSides
import Classh.Class.IsCSS
import Classh.Responsive.WhenTW
import Classh.Color
import Classh.Internal.CSSSize

import Control.Lens hiding ((<&>))
import Data.Default
import qualified Data.Text as T




instance Default BorderConfig where
  def = BorderConfig def def def def

instance Default BorderRadiusCorners where
  def = BorderRadiusCorners def def def def

instance Default BorderWidthSides where
  def = BorderWidthSides def def def def

instance Default BorderColorSides where
  def = BorderColorSides def def def def

-- TODO: stop overlaps through conditionals
instance ShowTW BorderRadiusCorners where
  showTW cfg = foldr (<&>) mempty
    [ renderWhenTW (_borderRadius_tr cfg) ((<>) "rounded-tr" . showTW)
    , renderWhenTW (_borderRadius_tl cfg) ((<>) "rounded-tl" . showTW)
    , renderWhenTW (_borderRadius_br cfg) ((<>) "rounded-br" . showTW)
    , renderWhenTW (_borderRadius_bl cfg) ((<>) "rounded-bl" . showTW)
    ]

instance ShowTW BorderWidthSides where
  showTW cfg = foldr (<&>) mempty
    [ renderWhenTW (_borderWidth_l cfg) ((<>) "border-l" . showTW)
    , renderWhenTW (_borderWidth_r cfg) ((<>) "border-r" . showTW)
    , renderWhenTW (_borderWidth_t cfg) ((<>) "border-t" . showTW)
    , renderWhenTW (_borderWidth_b cfg) ((<>) "border-b" . showTW)
    ]

instance ShowTW BorderColorSides where
  showTW cfg = foldr (<&>) mempty
    [ renderWhenTW (_borderColor_l cfg) ((<>) "border-l-" . showTW)
    , renderWhenTW (_borderColor_r cfg) ((<>) "border-r-" . showTW)
    , renderWhenTW (_borderColor_t cfg) ((<>) "border-t-" . showTW)
    , renderWhenTW (_borderColor_b cfg) ((<>) "border-b-" . showTW)
    ]
 
instance ShowTW BorderConfig where
  showTW cfg = foldr (<&>) mempty
    [ showTW . _radius $ cfg
    , showTW . _bWidth $ cfg
    , showTW . _bColor $ cfg
    , renderWhenTW (_bStyle cfg) ((<>) "border-" . showTW)
    ]

data BorderConfig = BorderConfig
  { _bStyle :: WhenTW BorderStyle
  , _bColor :: BorderColorSides
  , _bWidth :: BorderWidthSides
  , _radius :: BorderRadiusCorners
  } deriving Show

data BorderColorSides = BorderColorSides
  { _borderColor_l :: WhenTW Color
  , _borderColor_r :: WhenTW Color
  , _borderColor_t :: WhenTW Color
  , _borderColor_b :: WhenTW Color
  } deriving Show

data BorderRadiusCorners = BorderRadiusCorners
  { _borderRadius_tr :: WhenTW BorderRadius'
  , _borderRadius_tl :: WhenTW BorderRadius'
  , _borderRadius_br :: WhenTW BorderRadius'
  , _borderRadius_bl :: WhenTW BorderRadius'
  } deriving Show

data BorderWidthSides = BorderWidthSides
  { _borderWidth_l :: WhenTW BorderWidth
  , _borderWidth_r :: WhenTW BorderWidth
  , _borderWidth_t :: WhenTW BorderWidth
  , _borderWidth_b :: WhenTW BorderWidth
  } deriving Show


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


-- DEPRECATED: use SetSides model
-- | TODO: move to SetSides lens model (t,b,r,l) all valid
data BorderRadius
  = None
  | RoundedSM Corner
  | Rounded Corner
  | RoundedMd Corner
  | RoundedLg Corner
  | RoundedXl Corner
  | Rounded2Xl Corner
  | Rounded3Xl Corner
  | RoundedFull Corner
  deriving Show

data Corner
  = All -- -> ""
  | S
  | E
  | T
  | R
  | B
  | L
  | SS
  | SE
  | EE
  | ES
  | TL
  | TR
  | BR
  | BL
  deriving Show

tshowCorner :: Corner -> T.Text
tshowCorner = \case
  All -> ""
  x -> ((<>) "-") . T.toLower . tshow $ x

instance Default BorderRadius where
  def = None

-- DEPRECATED
instance ShowTW BorderRadius where
  showTW = ((<>) "rounded") . \case
    None -> "-none"
    Rounded c -> tshowCorner c --"-" <> (T.toLower . tshow $ c) <> "-" <> "sm"
    RoundedSM c -> tshowCorner c <> "-" <> "sm"
    RoundedMd c -> tshowCorner c <> "-" <> "md"
    RoundedLg c -> tshowCorner c <> "-" <> "lg"
    RoundedXl c -> tshowCorner c <> "-" <> "xl"
    Rounded2Xl c -> tshowCorner c <> "-" <> "2xl"
    Rounded3Xl c -> tshowCorner c <> "-" <> "3xl"
    RoundedFull c -> tshowCorner c <> "-" <> "full"

makeLenses ''BorderRadiusCorners
makeLenses ''BorderWidthSides
makeLenses ''BorderColorSides
makeLenses ''BorderConfig

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


borderRadius_l, borderRadius_r, borderRadius_t, borderRadius_b :: Lens' BorderRadiusCorners (WhenTW BorderRadius')
borderRadius_l = lens undefined $ \tw new -> tw { _borderRadius_tl = new, _borderRadius_bl = new }
borderRadius_r = lens undefined $ \tw new -> tw { _borderRadius_tr = new, _borderRadius_br = new }
borderRadius_t = lens undefined $ \tw new -> tw { _borderRadius_tl = new, _borderRadius_tr = new }
borderRadius_b = lens undefined $ \tw new -> tw { _borderRadius_bl = new, _borderRadius_br = new }

