{-# LANGUAGE TemplateHaskell #-}

module Classh.Text.Decoration (module X, module Classh.Text.Decoration) where

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

data TextDecorationTW = TextDecorationTW
  { _textDec_line :: WhenTW TextDecLineType
  , _textDec_color :: WhenTW Color
  , _textDec_style :: WhenTW TextDecStyle
  , _textDec_thickness :: WhenTW TextDecThickness
  -- TODO: is this a standard system: Thickness?
  , _textDec_offset :: WhenTW TextDecOffset
  -- and is this a system? (the numbers chosen 0,1,2,4,8)
  }

makeLenses ''TextDecorationTW
