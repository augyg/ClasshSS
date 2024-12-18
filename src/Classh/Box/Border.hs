{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Box.Border
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  Types to represent tailwind box's border config of 'BoxConfig'  
--
--  Any field named _someField has an associated lens `someField`
--  see @defaultNameTransform@ from Lens.Family.THCore
--
--  This package aims to avoid forcing the user to know lenses
--
--  Example use:
--
-- @
--  $(classh' [ border . bWidth . borderWidth_t .~~ B2 ])
-- @
--------------------------------------------------------------------------------

module Classh.Box.Border
  (
  -- * The Border type 
    BorderConfig(..)
  -- * Border Sub-Types
  , BorderColorSides(..)
  , BorderRadiusCorners(..)
  , BorderWidthSides(..)
  -- * Options
  , BorderWidth(..)
  , BorderStyle(..)
  , BorderRadius'(..)
  -- * BorderConfig top-level Lenses
  , radius
  , bWidth
  , bStyle
  , bColor
  -- * BorderRadius Setters
  , borderRadius_tr
  , borderRadius_tl
  , borderRadius_br
  , borderRadius_bl
  , borderRadius_l
  , borderRadius_r
  , borderRadius_b
  , borderRadius_t
  -- * BorderWidth Setters
  , borderWidth_r
  , borderWidth_l
  , borderWidth_b
  , borderWidth_t
  -- * BorderColor Setters
  , borderColor_t
  , borderColor_b
  , borderColor_l
  , borderColor_r

  ) where

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

--------------------------------------------------------------------------------
-- |
-- NOTE: any field named _someField has an associated lens `someField`
-- see @defaultNameTransform@ from Lens.Family.THCore
--
-- This package aims to avoid forcing the user to know lenses
--------------------------------------------------------------------------------


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

-- | Holds all information about a Box's tailwind border classes
data BorderConfig = BorderConfig
  { _bStyle :: WhenTW BorderStyle
  -- ^ https://tailwindcss.com/docs/border-style
  , _bColor :: BorderColorSides
  -- ^ https://tailwindcss.com/docs/border-color
  , _bWidth :: BorderWidthSides
  -- ^ https://tailwindcss.com/docs/border-width
  , _radius :: BorderRadiusCorners
  -- ^ https://tailwindcss.com/docs/border-radius
  } deriving Show

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
makeLenses ''BorderWidthSides
makeLenses ''BorderColorSides
makeLenses ''BorderConfig

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


borderRadius_l, borderRadius_r, borderRadius_t, borderRadius_b :: Lens' BorderRadiusCorners (WhenTW BorderRadius')
borderRadius_l = lens undefined $ \tw new -> tw { _borderRadius_tl = new, _borderRadius_bl = new }
borderRadius_r = lens undefined $ \tw new -> tw { _borderRadius_tr = new, _borderRadius_br = new }
borderRadius_t = lens undefined $ \tw new -> tw { _borderRadius_tl = new, _borderRadius_tr = new }
borderRadius_b = lens undefined $ \tw new -> tw { _borderRadius_bl = new, _borderRadius_br = new }

