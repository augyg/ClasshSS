{-# LANGUAGE TemplateHaskell #-}

module Classh.Text (module X, module Classh.Text, TextConfigTW(..)) where

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
      compileDecoration cfg = pure . foldr (<&>) mempty =<< sequenceA
        [ compileWhenTW (_textDec_line cfg) showTW
        , compileWhenTW (_textDec_color cfg) ((<>) "decoration-" . showTW)
        , compileWhenTW (_textDec_style cfg) showTW
        , compileWhenTW (_textDec_thickness cfg) showTW
        , compileWhenTW (_textDec_offset cfg) showTW
        ]
    
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

