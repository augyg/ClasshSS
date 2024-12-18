{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Text.Decoration
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  The core interface to text decorations, as the '_text_decoration' field of 'TextConfigTW'
--
--  Any field named _someField has an associated lens `someField`
--  see @defaultNameTransform@ from Lens.Family.THCore
--
--  This package aims to avoid forcing the user to know lenses
--
--  Example use:
--
-- @
--  -- Using Default instance 
--  $(classh' [ text_decoration .~ (def { _textDec_line = Underline, _textDec_style = Wavy } ) ])
--  -- simple example:
--  $(classh' [ text_decoration . textDec_line .~~ Underline ]) 
-- @
--------------------------------------------------------------------------------


module Classh.Text.Decoration
  (
    -- * Core Config Interface
    TextDecorationTW(..)
    -- * Auto Generated Lenses
  , textDec_line
  , textDec_color
  , textDec_thickness
  , textDec_offset
  , textDec_style
  , module X
  ) where

import Classh.Class.ShowTW
import Classh.Responsive.WhenTW
import Classh.Internal.Chain

import Classh.Color as X 
import Classh.Text.Decoration.Offset as X
import Classh.Text.Decoration.Thickness as X
import Classh.Text.Decoration.Style as X
import Classh.Text.Decoration.LineType as X

import Control.Lens (makeLenses)
import Data.Default

-- | > TextDecorationTW [] [] [] [] []
instance Default TextDecorationTW where
  def = TextDecorationTW def def def def def

instance ShowTW TextDecorationTW where
  showTW cfg = foldr (<&>) mempty
    [ renderWhenTW (_textDec_line cfg) showTW
    , renderWhenTW (_textDec_color cfg) ((<>) "decoration-" . showTW)
    , renderWhenTW (_textDec_style cfg) showTW
    , renderWhenTW (_textDec_thickness cfg) showTW
    , renderWhenTW (_textDec_offset cfg) showTW
    ]

-- | The core interface to Text decoration in tailwind
data TextDecorationTW = TextDecorationTW
  { _textDec_line :: WhenTW TextDecLineType
  -- ^ https://tailwindcss.com/docs/text-decoration
  , _textDec_color :: WhenTW Color
  -- ^ https://tailwindcss.com/docs/text-decoration-color
  , _textDec_style :: WhenTW TextDecStyle
  -- ^ https://tailwindcss.com/docs/text-decoration-style
  , _textDec_thickness :: WhenTW TextDecThickness
  -- ^ https://tailwindcss.com/docs/text-decoration-thickness
  , _textDec_offset :: WhenTW TextDecOffset
  -- ^ https://tailwindcss.com/docs/text-underline-offset
  }

makeLenses ''TextDecorationTW
