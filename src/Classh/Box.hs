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
import Classh.Box.TWSize as X
import Classh.Box.Padding as X
import Classh.Box.Margin as X
import Classh.Box.SizingBand as X
import Classh.Box.Placement as X
import Classh.Box.Border as X

import Control.Lens hiding ((<&>))
import Data.Default
import qualified Data.Text as T

data BoxConfig = BoxConfig
  { _colStart :: WhenTW Int
  , _colSpan :: WhenTW Int
  , _bgColor :: WhenTW Color
  , _bgOpacity :: WhenTW Int -- 1 5 10 .. 100 -- def == 519
  , _padding :: BoxPadding
  , _margin :: BoxMargin
  , _sizingBand :: BoxSizingBand
  , _border :: BorderConfig -- { rounded, thickness, etc .. }
  , _position :: WhenTW (Justify, Align)
  --, _text_align :: Align ... or should we set == position.align
  , _box_custom :: T.Text
  }
  deriving Show


makeLenses ''BoxConfig


------------  Defaults of Records

instance Default BoxConfig where
  def = BoxConfig def def def def def def def def def ""


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
      , compileWhenTW (_bgColor cfg) ((<>) "bg-" . showTW)
      , compileWhenTW (_bgOpacity cfg) ((<>) "bg-opacity-" . tshow)
      , Right $ _box_custom cfg
      ]
      where
        compileBorder cfg' = pure . foldr (<&>) mempty =<< sequenceA
          [ compileWhenTW (_bStyle cfg') ((<>) "border-" . showTW)
          , compileBorderRadius (_radius cfg')
          , compileBorderWidth (_bWidth cfg')
          , compileBorderColor (_bColor cfg')
          ]

        compileBorderRadius cfg' = pure . foldr (<&>) mempty =<< sequenceA
          [ compileWhenTW (_borderRadius_tr cfg') ((<>) "rounded-tr" . showTW)
          , compileWhenTW (_borderRadius_tl cfg') ((<>) "rounded-tl" . showTW)
          , compileWhenTW (_borderRadius_br cfg') ((<>) "rounded-br" . showTW)
          , compileWhenTW (_borderRadius_bl cfg') ((<>) "rounded-bl" . showTW)
          ]

        compileBorderWidth cfg' = pure . foldr (<&>) mempty =<< sequenceA
          [ compileWhenTW (_borderWidth_l cfg') ((<>) "border-l" . showTW)
          , compileWhenTW (_borderWidth_r cfg') ((<>) "border-r" . showTW)
          , compileWhenTW (_borderWidth_t cfg') ((<>) "border-t" . showTW)
          , compileWhenTW (_borderWidth_b cfg') ((<>) "border-b" . showTW)
          ]

        compileBorderColor cfg' = pure . foldr (<&>) mempty =<< sequenceA
          [ compileWhenTW (_borderColor_l cfg') ((<>) "border-l-" . showTW)
          , compileWhenTW (_borderColor_r cfg') ((<>) "border-r-" . showTW)
          , compileWhenTW (_borderColor_t cfg') ((<>) "border-t-" . showTW)
          , compileWhenTW (_borderColor_b cfg') ((<>) "border-b-" . showTW)
          ]

        compileSizingBand cfg' = pure . foldr (<&>) mempty =<< sequenceA
          [ compileWhenTW (_widthC . _maxSize $ cfg') ((<>) "max-w-" . showTW)
          , compileWhenTW (_heightC . _maxSize $ cfg') ((<>) "max-h-" . showTW)
          , compileWhenTW (_widthC . _minSize $ cfg') ((<>) "min-w-" . showTW)
          , compileWhenTW (_heightC . _minSize $ cfg') ((<>) "min-h-" . showTW)
          , compileWhenTW (_width . _size $ cfg') ((<>) "w-" . showTW)
          , compileWhenTW (_height . _size $ cfg') ((<>) "h-" . showTW)
          ]


        compileMargin cfg' = pure . foldr (<&>) mempty =<< sequenceA
          [ compileWhenTW (_marginL cfg') ((<>) "ml-" . showTW)
          , compileWhenTW (_marginR cfg') ((<>) "mr-" . showTW)
          , compileWhenTW (_marginT cfg') ((<>) "mt-" . showTW)
          , compileWhenTW (_marginB cfg') ((<>) "mb-" . showTW)
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
   , renderWhenTW (_bgColor cfg) ((<>) "bg-" . showTW)
   , renderWhenTW (_bgOpacity cfg) ((<>) "bg-opacity-" . tshow)
   , showTW . _border $ cfg
   , showTW . _sizingBand $ cfg
   , showTW . _padding $ cfg
   , showTW . _margin $ cfg
   , foldr (<&>) mempty $ fmap
     (\(c,(jus,align)) ->
        let prefix = if c == "def" then "" else (c <> ":")
        in prefix <> "grid" <&> prefix <> (showTW jus) <&> prefix <> (showTW align)
     ) $ _position cfg
   --, renderWhenTW (_position cfg) $ \(j,a) -> "grid " <> showTW j <> " " <> showTW a
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
    , _box_custom = _box_custom a <> _box_custom b
    }
