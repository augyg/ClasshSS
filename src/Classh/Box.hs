{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Box
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  The core interface to creating responsive elements, including images.  
--
--  Here is a common real example creating a box with some text in it, using reflex-dom to illustrate
--
-- > elClass "div" $(classh' [ padding . t .~~ pix 20, bgColor .~~ Gray C300 ]) $ do
-- >    text "Hey"
--
--  This module and all modules which it re-exports are your interface to writing typified classes
--  specifically for Box's ( Box == element )
--
-- Using Classh.Shorthand functions we can make this more ergonomic/take up less space
--
-- for example
-- > padding . t == pt
--
-- The above divs we have created ensure there is no \'classhes\'. For example, if we set the top padding but also the
-- y-padding then it will complain at compile time. Hence, the `$(..)` Template Haskell syntax. You can avoid this by
-- using classhUnsafe without this TH syntax. Classh's type system also enforces that you cannot use text config setters
-- in the same classh expression as one with 'BoxConfig' setters. This is due to the design goal to reduce spooky behavior
-- and misleading code. For example if we have multiple parent divs with text classes, then it will make it challenging to
-- find why a given piece of text appears as such, especially if we refactor components, the reason for its appearance would
-- be even more hidden 
--
-- Note that we can also use '.|~' and 'zipScreens' to easily create responsive boxes 
-- .|~ takes a list that goes from mobile (less than 640px) -> sm -> md -> lg -> xl -> 2xl (eg. padding) 
-- .~~ takes a singular value for all screen sizes (eg. background color / bgColor) 
-- The reason is because almost all properties are (WhenTW prop) which is a list of values by screen size 
-- this is based on https://tailwindcss.com/docs/responsive-design
--
-- We also have
-- (.~) which is mainly used for `custom` as the associated Record field is not a WhenTW but a String.
-- this is just a simple setter 
-- (.~+) appends the chosen value to what exists (perhaps in a default config)
-- (.|+) like .|~ except that it adds to what already exists (perhaps in a default config)
--
-- We can also add any arbitrary classes to the end of the TextConfigTW using its HasCustom instance
--------------------------------------------------------------------------------


module Classh.Box
  (
    -- * Core Config Type 
    BoxConfig(..)
  , module X
  -- * Auto Generated Lenses
  , colStart
  , colSpan
  , bgColor
  , bgOpacity
  , padding
  , margin
  , sizingBand
  , border
  , position
  , shadow
  , cursor
  , transform
  , box_custom
  ) where

-- Our goto module
import Classh.Class.HasCustom
import Classh.Class.ShowTW
import Classh.Class.CompileStyle
import Classh.Internal.Chain
import Classh.Internal.TShow

import Classh.Internal.TWNum as X
import Classh.Responsive.WhenTW as X
import Classh.Color as X
import Classh.Cursor as X
import Classh.Box.TWSize as X
import Classh.Box.Padding as X
import Classh.Box.Margin as X
import Classh.Box.SizingBand as X
import Classh.Box.Placement as X
import Classh.Box.Border as X
import Classh.Box.Shadow as X
import Classh.Box.Transition as X
import Classh.Box.Transform as X
import Classh.WithTransition as X

import Control.Lens hiding ((<&>), transform)
import Data.Default
import qualified Data.Text as T

data BoxConfig = BoxConfig
  { _colStart :: WhenTW Int
  , _colSpan :: WhenTW Int
  , _bgColor :: WhenTW (WithTransition Color)  -- Transitionable!
  , _bgOpacity :: WhenTW (WithTransition Int)  -- Transitionable! (1 5 10 .. 100 -- def == 519)
  , _padding :: BoxPadding
  , _margin :: BoxMargin
  , _sizingBand :: BoxSizingBand
  , _border :: BorderConfig -- { rounded, thickness, etc .. }
  , _position :: WhenTW (Justify, Align)
  , _shadow :: WhenTW (WithTransition BoxShadow)  -- Transitionable!
  , _cursor :: WhenTW CursorStyle
  , _transform :: TransformConfig  -- All transform properties (rotate, scale, translate, skew, origin)
  , _box_custom :: T.Text
  }
  deriving Show


makeLenses ''BoxConfig

------------  Defaults of Records

instance Default BoxConfig where
  def = BoxConfig def def def def def def def def def def def def ""


instance CompileStyle BoxConfig where
  compileS cfg = do
    pure . foldr (<&>) mempty =<< sequenceA
      [ compilePos (_position cfg)
      , compileWhenTW (_colStart cfg) ((<>) "col-start-" . tshow)
      , compileWhenTW (_colSpan cfg) ((<>) "col-span-" . tshow)
      , compileBorder (_border cfg)
      , compileSizingBand (_sizingBand cfg)
      , compilePadding (_padding cfg)
      , compileMargin (_margin cfg)
      , compileWithTransitionTW (_bgColor cfg) ((<>) "bg-" . showTW) Transition_Colors
      , compileWithTransitionTW (_bgOpacity cfg) ((<>) "bg-opacity-" . tshow) Transition_Opacity
      , compileWithTransitionTW (_shadow cfg) showTW Transition_Shadow
      , compileWhenTW (_cursor cfg) showTW
      , compileS (_transform cfg)
      , Right $ _box_custom cfg
      ]
      where
        compileBorder cfg' = pure . foldr (<&>) mempty =<< sequenceA
          [ compileWhenTW (_bStyle cfg') ((<>) "border-" . showTW)
          , compileBorderRadius (_radius cfg')
          , compileBorderWidth (_bWidth cfg')
          , compileBorderColor (_bColor cfg')
          , compileRing (_ring cfg')
          , compileWhenTW (_outline cfg') showTW
          ]

        compileBorderRadius cfg' = pure . foldr (<&>) mempty =<< sequenceA
          [ compileWithTransitionTW (_borderRadius_tr cfg') ((<>) "rounded-tr" . showTW) Transition_All
          , compileWithTransitionTW (_borderRadius_tl cfg') ((<>) "rounded-tl" . showTW) Transition_All
          , compileWithTransitionTW (_borderRadius_br cfg') ((<>) "rounded-br" . showTW) Transition_All
          , compileWithTransitionTW (_borderRadius_bl cfg') ((<>) "rounded-bl" . showTW) Transition_All
          ]

        compileBorderWidth cfg' = pure . foldr (<&>) mempty =<< sequenceA
          [ compileWithTransitionTW (_borderWidth_l cfg') ((<>) "border-l" . showTW) Transition_All
          , compileWithTransitionTW (_borderWidth_r cfg') ((<>) "border-r" . showTW) Transition_All
          , compileWithTransitionTW (_borderWidth_t cfg') ((<>) "border-t" . showTW) Transition_All
          , compileWithTransitionTW (_borderWidth_b cfg') ((<>) "border-b" . showTW) Transition_All
          ]

        compileBorderColor cfg' = pure . foldr (<&>) mempty =<< sequenceA
          [ compileWithTransitionTW (_borderColor_l cfg') ((<>) "border-l-" . showTW) Transition_Colors
          , compileWithTransitionTW (_borderColor_r cfg') ((<>) "border-r-" . showTW) Transition_Colors
          , compileWithTransitionTW (_borderColor_t cfg') ((<>) "border-t-" . showTW) Transition_Colors
          , compileWithTransitionTW (_borderColor_b cfg') ((<>) "border-b-" . showTW) Transition_Colors
          ]

        compileRing cfg' = pure . foldr (<&>) mempty =<< sequenceA
          [ compileWithTransitionTW (_ringWidth cfg') showTW Transition_All
          , compileWithTransitionTW (_ringColor cfg') ((<>) "ring-" . showTW) Transition_Colors
          , compileWithTransitionTW (_ringOpacity cfg') ((<>) "ring-opacity-" . tshow) Transition_Opacity
          ]

        compileSizingBand cfg' = pure . foldr (<&>) mempty =<< sequenceA
          [ compileWithTransitionTW (_widthC . _maxSize $ cfg') ((<>) "max-w-" . showTW) Transition_All
          , compileWithTransitionTW (_heightC . _maxSize $ cfg') ((<>) "max-h-" . showTW) Transition_All
          , compileWithTransitionTW (_widthC . _minSize $ cfg') ((<>) "min-w-" . showTW) Transition_All
          , compileWithTransitionTW (_heightC . _minSize $ cfg') ((<>) "min-h-" . showTW) Transition_All
          , compileWithTransitionTW (_width . _size $ cfg') ((<>) "w-" . showTW) Transition_All
          , compileWithTransitionTW (_height . _size $ cfg') ((<>) "h-" . showTW) Transition_All
          ]


        compileMargin cfg' = pure . foldr (<&>) mempty =<< sequenceA
          [ compileWithTransitionTW (_marginL cfg') ((<>) "ml-" . showTW) Transition_All
          , compileWithTransitionTW (_marginR cfg') ((<>) "mr-" . showTW) Transition_All
          , compileWithTransitionTW (_marginT cfg') ((<>) "mt-" . showTW) Transition_All
          , compileWithTransitionTW (_marginB cfg') ((<>) "mb-" . showTW) Transition_All
          ]

        compilePos posCfg = case f $ fmap fst posCfg of
          Left e -> Left e
          Right () -> Right $ foldr (<&>) mempty $ fmap
            (\(c,(jus,align)) ->
               let prefix = if c == "def" then "" else (c <> ":")
               in
                 prefix <> "grid" <&> prefix <> (showTW jus) <&> prefix <> (showTW align)
            ) $ posCfg
          where
            f [] = Right ()
            f (s:ss) =
              if elem s ss
              then Left $ s <> " exists twice"
              else f ss

  
instance ShowTW BoxConfig where
  showTW cfg = foldr (<&>) mempty
   [ renderWhenTW (_colStart cfg) ((<>) "col-start-" . tshow)
   , renderWhenTW (_colSpan cfg) ((<>) "col-span-" . tshow)
   , renderWithTransitionTW (_bgColor cfg) ((<>) "bg-" . showTW) Transition_Colors
   , renderWithTransitionTW (_bgOpacity cfg) ((<>) "bg-opacity-" . tshow) Transition_Opacity
   , showTW . _border $ cfg
   , showTW . _sizingBand $ cfg
   , showTW . _padding $ cfg
   , showTW . _margin $ cfg
   , foldr (<&>) mempty $ fmap
     (\(c,(jus,align)) ->
        let prefix = if c == "def" then "" else (c <> ":")
        in prefix <> "grid" <&> prefix <> (showTW jus) <&> prefix <> (showTW align)
     ) $ _position cfg
   , renderWithTransitionTW (_shadow cfg) showTW Transition_Shadow
   , showTW . _transform $ cfg
   , _box_custom cfg
   ]

instance HasCustom BoxConfig where
  custom = box_custom

instance Semigroup BoxConfig where
  (<>) a b = BoxConfig
    { _colStart   = _colStart a <> _colStart b
    , _colSpan    = _colSpan a  <> _colSpan b
    , _bgColor    = _bgColor a  <> _bgColor b
    , _bgOpacity  = _bgOpacity a <> _bgOpacity b
    , _padding    = _padding a  <> _padding b
    , _margin     = _margin a   <> _margin b
    , _sizingBand = _sizingBand a <> _sizingBand b
    , _border     = _border a   <> _border b
    , _position   = _position a <> _position b
    , _shadow     = _shadow a <> _shadow b
    , _cursor     = _cursor a <> _cursor b
    , _transform  = _transform a <> _transform b
    , _box_custom = _box_custom a <> _box_custom b
    }
