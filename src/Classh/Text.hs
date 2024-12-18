{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Text
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  The core interface to creating responsive text 
--
--  Here is a common real example creating a box with some text in it, using reflex-dom to illustrate
--
-- > elClass "div" "" $ do
-- >    textS $(classh' [text_size .|~ [XL,XL2], text_weight .~~ Bold]) "Hey"
--
--  This module and all modules which it re-exports are your interface to writing typified classes
--  specifically for text
--
--  Through CompileStyle, Classh enforces that 'TextConfigTW' is a seperate expression from 'BoxConfig'
--  this is because it undeniably helps to create modularity, reduce phantom CSS behaviour and makes
--  it easy to create themes to be shared by an application. For example
--
-- > defText = def { _text_font = Font_Custom "Sarabun" } -- perhaps we want all text to be Sarabun
-- > blueBrandTextSm someText = textS $(classh defText [ text_color .~~ Blue C950, text_size .|~ [XS,SM,Base,Lg]])
-- > elClass "div" "" $ blueBrandTextSm "Sign up now!"
--
-- Note that we can use '.|~' and 'zipScreens' to easily create responsive text
-- .|~ takes a list that goes from mobile (less than 640px) -> sm -> md -> lg -> xl -> 2xl (eg. text_size) 
-- .~~ takes a singular value for all screen sizes (eg. text_weight)
-- .~ is a simple setter that expects the type of the property, so the property text_color is a WhenTW Color
-- and so if we wanted to set a color on hover, we could do:
--
-- > $(classh' [ text_color .~ [("hover", hex "FFFFFF"), ("def", Blue C950)] ])
--
-- This will set text to white on hover, and normally otherwise "text-blue-950"
--
-- We also have
-- (.~+) appends the chosen value to what exists (perhaps in a default config)
-- (.|+) like .|~ except that it adds to what already exists (perhaps in a default config)
--
-- We can also add any arbitrary classes to the end of the TextConfigTW using its HasCustom instance 
--------------------------------------------------------------------------------

module Classh.Text
  (
    -- * Core Config Type 
    TextConfigTW(..)
    -- * Re-exports 
  , module X
  -- * Auto Generated Lenses 
  , text_size
  , text_weight
  , text_font
  , text_color
  , text_decoration
  , text_style
  , text_cursor
  , text_custom  
  ) where

import Classh.Class.HasCustom
import Classh.Class.CompileStyle
import Classh.Class.ShowTW
import Classh.Internal.Chain
import Classh.Responsive.WhenTW

import Classh.Color as X
import Classh.Cursor as X
import Classh.Text.Decoration as X
import Classh.Text.FontStyle as X
import Classh.Text.Font as X
import Classh.Text.Size as X
import Classh.Text.Weight as X

import Control.Lens (makeLenses)
import Data.Default
import qualified Data.Text as T

instance ShowTW TextConfigTW where
  showTW cfg = foldr (<&>) mempty
    [ renderWhenTW (_text_size cfg) showTW
    , renderWhenTW (_text_weight cfg) showTW
    , renderWhenTW (_text_font cfg) showTW
    , renderWhenTW (_text_style cfg) showTW
    , renderWhenTW (_text_cursor cfg) showTW
    , renderWhenTW (_text_color cfg) ((<>) "text-" . showTW)
    , showTW (_text_decoration cfg)
--    , "align-left"
    --, "text-" <> showTW align
    , _text_custom cfg
    ]

instance CompileStyle TextConfigTW where
  compileS cfg = pure . foldr (<&>) mempty =<< sequenceA
    [ compileWhenTW (_text_size cfg) showTW
    , compileWhenTW (_text_weight cfg) showTW
    , compileWhenTW (_text_font cfg) showTW
    , compileWhenTW (_text_style cfg) showTW
    , compileWhenTW (_text_cursor cfg) showTW
    , compileWhenTW (_text_color cfg) ((<>) "text-" . showTW)
    , compileDecoration (_text_decoration cfg)
    , Right $ _text_custom cfg
    ]
    where
      compileDecoration cfg' = pure . foldr (<&>) mempty =<< sequenceA
        [ compileWhenTW (_textDec_line cfg') showTW
        , compileWhenTW (_textDec_color cfg') ((<>) "decoration-" . showTW)
        , compileWhenTW (_textDec_style cfg') showTW
        , compileWhenTW (_textDec_thickness cfg') showTW
        , compileWhenTW (_textDec_offset cfg') showTW
        ]

-- | > TextConfigTW [] [] [] [] (def :: TextDecorationTW) [] [] ""
instance Default TextConfigTW where
  def = TextConfigTW def def def def def def def ""

data TextConfigTW = TextConfigTW
  { _text_size :: WhenTW TextSize
  , _text_weight :: WhenTW TextWeight
  , _text_font :: WhenTW Font -- many options -- EG. Sarabun -> font-[Sarabun]
  , _text_color :: WhenTW Color
  , _text_decoration :: TextDecorationTW
  , _text_style :: WhenTW FontStyle
  , _text_cursor :: WhenTW CursorStyle
  , _text_custom :: T.Text
  }

makeLenses ''TextConfigTW

instance HasCustom TextConfigTW where
  custom = text_custom

