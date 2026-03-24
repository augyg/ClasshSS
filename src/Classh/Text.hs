{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Text
--  Description :  Text styling configuration type and lenses
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  TextConfigTW: The core type for styling text content.
--
--  = Overview
--
--  This module provides 'TextConfigTW', the configuration type for all text-specific
--  styling in ClasshSS. It handles typography properties like size, weight, font family,
--  color, and decoration.
--
--  TextConfigTW includes:
--
--  * Typography - Font size, weight, family, style
--  * Colors - Text color with responsive support
--  * Decoration - Underline, overline, strikethrough with style and thickness
--  * Interaction - Cursor styles
--
--  = Quick Example
--
--  Creating styled text with Reflex.Dom:
--
--  @
--  textS $(classh'
--    [ text_size .~~ XL3
--    , text_weight .~~ Bold
--    , text_color .~~ Gray C900
--    , text_font .~~ Font_Custom \"Sarabun\"
--    ]) \"Hello, ClasshSS!\"
--  @
--
--  = Separation from BoxConfig
--
--  ClasshSS enforces that 'TextConfigTW' and 'Classh.Box.BoxConfig' are used in separate
--  expressions. This design prevents phantom CSS behavior and improves modularity:
--
--  @
--  -- COMPILE ERROR: Cannot mix BoxConfig and TextConfigTW
--  $(classh' [ bgColor .~~ Blue C500, text_size .~~ XL ])
--
--  -- CORRECT: Separate expressions
--  elClass \"div\" $(classh' [ bgColor .~~ Blue C500 ]) $ do
--    textS $(classh' [ text_size .~~ XL ]) \"Text\"
--  @
--
--  = TextConfigTW Fields
--
--  == Typography
--
--  * '_text_size' - Font size from XS to XL9
--  * '_text_weight' - Font weight from Thin to Black
--  * '_text_font' - Font family (Sans, Serif, Mono, or custom)
--  * '_text_style' - Italic or normal
--
--  @
--  text_size .~~ XL2          -- text-2xl
--  text_weight .~~ Bold       -- font-bold
--  text_font .~~ Sans         -- font-sans
--  text_style .~~ Italic      -- italic
--  @
--
--  == Color
--
--  * '_text_color' - Text color with responsive and state support
--
--  @
--  -- Simple color
--  text_color .~~ Blue C500   -- text-blue-500
--
--  -- With hover
--  text_color .~ [(\"def\", Gray C900), (\"hover\", Blue C600)]
--  @
--
--  == Decoration
--
--  * '_text_decoration' - 'TextDecorationTW' for underline, overline, strikethrough
--
--  @
--  text_decoration . textDec_line .~~ Underline
--  text_decoration . textDec_color .~~ Blue C500
--  text_decoration . textDec_style .~~ Wavy
--  text_decoration . textDec_thickness .~~ Thickness_2
--  @
--
--  == Cursor
--
--  * '_text_cursor' - Mouse cursor style
--
--  @
--  text_cursor .~~ CursorPointer
--  @
--
--  == Custom Classes
--
--  * '_text_custom' - Arbitrary Tailwind classes
--
--  @
--  custom .~ \"text-center uppercase tracking-wide\"
--  @
--
--  = Responsive Text
--
--  Use '.|~' for responsive text sizing:
--
--  @
--  text_size .|~ [SM, Base, LG, XL, XL2, XL3]
--  --            mobile sm   md  lg  xl   2xl
--
--  -- Result: \"text-sm sm:text-base md:text-lg lg:text-xl xl:text-2xl 2xl:text-3xl\"
--  @
--
--  = Theming
--
--  TextConfigTW makes it easy to create reusable text themes:
--
--  @
--  -- Define a theme
--  defText :: TextConfigTW
--  defText = def & text_font .~~ Font_Custom \"Sarabun\"
--
--  -- Create styled text helpers
--  brandHeading :: Text -> Text
--  brandHeading content =
--    textS $(classh defText
--      [ text_size .|~ [XL2, XL3, XL4]
--      , text_weight .~~ Bold
--      , text_color .~~ Blue C950
--      ]) content
--
--  -- Use in components
--  elClass \"div\" \"\" $ brandHeading \"Sign up now!\"
--  @
--
--  = Common Patterns
--
--  == Heading
--
--  @
--  $(classh'
--    [ text_size .~~ XL3
--    , text_weight .~~ Bold
--    , text_color .~~ Gray C900
--    ])
--  @
--
--  == Body Text
--
--  @
--  $(classh'
--    [ text_size .~~ Base
--    , text_weight .~~ Normal
--    , text_color .~~ Gray C700
--    ])
--  @
--
--  == Link with Hover
--
--  @
--  $(classh'
--    [ text_color .~ [(\"def\", Blue C600), (\"hover\", Blue C800)]
--    , text_decoration . textDec_line .~ [(\"hover\", Underline)]
--    , text_cursor .~~ CursorPointer
--    ])
--  @
--
--  == Responsive Heading
--
--  @
--  $(classh'
--    [ text_size .|~ [XL, XL2, XL3, XL4]
--    , text_weight .~~ Bold
--    , text_color .|~ [Gray C800, Gray C900, Gray C950]
--    ])
--  @
--
--  = Integration with Reflex.Dom
--
--  ClasshSS provides helper functions for text in Reflex.Dom:
--
--  @
--  import Reflex.Dom.Core
--
--  -- Static text with styling
--  textS :: Text -> Text -> m ()
--  textS classes content = elClass \"span\" classes $ text content
--
--  -- Usage:
--  textS $(classh' [text_size .~~ XL, text_weight .~~ Bold]) \"Hello!\"
--  @
--
--  = Size Scale
--
--  TextSize options (from smallest to largest):
--
--  @
--  XS    -- text-xs    (0.75rem, 12px)
--  SM    -- text-sm    (0.875rem, 14px)
--  Base  -- text-base  (1rem, 16px)
--  LG    -- text-lg    (1.125rem, 18px)
--  XL    -- text-xl    (1.25rem, 20px)
--  XL2   -- text-2xl   (1.5rem, 24px)
--  XL3   -- text-3xl   (1.875rem, 30px)
--  XL4   -- text-4xl   (2.25rem, 36px)
--  XL5   -- text-5xl   (3rem, 48px)
--  XL6   -- text-6xl   (3.75rem, 60px)
--  XL7   -- text-7xl   (4.5rem, 72px)
--  XL8   -- text-8xl   (6rem, 96px)
--  XL9   -- text-9xl   (8rem, 128px)
--  @
--
--  = Weight Scale
--
--  TextWeight options:
--
--  @
--  Thin         -- font-thin (100)
--  Extralight   -- font-extralight (200)
--  Light        -- font-light (300)
--  Normal       -- font-normal (400)
--  Medium       -- font-medium (500)
--  Semibold     -- font-semibold (600)
--  Bold         -- font-bold (700)
--  Extrabold    -- font-extrabold (800)
--  Black_TextWeight -- font-black (900)
--  @
--
--  = Re-Exported Modules
--
--  This module re-exports all text-related modules for convenience:
--
--  * "Classh.Color" - Color types
--  * "Classh.Cursor" - Cursor styles
--  * "Classh.Text.Decoration" - Text decoration configuration
--  * "Classh.Text.FontStyle" - Italic and normal styles
--  * "Classh.Text.Font" - Font family types
--  * "Classh.Text.Size" - Text size types
--  * "Classh.Text.Weight" - Font weight types
--
--  = See Also
--
--  * "Classh" - Main module with Template Haskell functions
--  * "Classh.Box" - For element/box styling
--  * "Classh.Setters" - Operator documentation
--  * "Classh.Responsive.WhenTW" - Responsive value system
--  * @docs/features/TEXT_STYLING.md@ - Complete feature guide
--  * @docs/examples/@ - Real-world examples
--
--------------------------------------------------------------------------------

module Classh.Text
  (
    -- * Core Config Type 
    TextConfigTW(..)
    -- * Re-exports
  , module Color
  , module Cursor
  , module Decoration
  , module FontStyle
  , module Font
  , module Size
  , module Weight
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

import Classh.Class.HasCustom as HasCustom
import Classh.Class.CompileStyle as CompileStyle
import Classh.Class.ShowTW (ShowTW(..))
import Classh.Internal.Chain ((<&>))
import Classh.Responsive.WhenTW as WhenTW

import Classh.Color as Color
import Classh.Cursor as Cursor
import Classh.Text.Decoration as Decoration
import Classh.Text.FontStyle as FontStyle
import Classh.Text.Font as Font
import Classh.Text.Size as Size
import Classh.Text.Weight as Weight

import Control.Lens (makeLenses)
import Data.Default (Default(..))
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

-- | Configuration type for styling text content.
--
-- TextConfigTW contains all text-specific styling properties. It is intentionally
-- separate from 'Classh.Box.BoxConfig' to enforce modularity and prevent CSS conflicts.
--
-- === Field Overview
--
-- * Typography: '_text_size', '_text_weight', '_text_font', '_text_style'
-- * Color: '_text_color'
-- * Decoration: '_text_decoration' (underline, overline, strikethrough)
-- * Interaction: '_text_cursor'
-- * Escape hatch: '_text_custom'
--
-- === Examples
--
-- @
-- -- Simple heading
-- def & text_size .~~ XL3 & text_weight .~~ Bold
--
-- -- Responsive body text
-- def
--   & text_size .|~ [SM, Base, LG]
--   & text_color .~~ Gray C700
--
-- -- Themed text with custom font
-- def
--   & text_font .~~ Font_Custom \"Sarabun\"
--   & text_size .~~ Base
--   & text_color .~~ Blue C950
-- @
--
-- @since 0.1.0.0
data TextConfigTW = TextConfigTW
  { _text_size :: WhenTW TextSize
    -- ^ Font size (XS, SM, Base, LG, XL through XL9). Default: empty (browser default)
  , _text_weight :: WhenTW TextWeight
    -- ^ Font weight (Thin through Black_TextWeight). Default: empty (browser default)
  , _text_font :: WhenTW Font
    -- ^ Font family (Sans, Serif, Mono, or Font_Custom \"Name\"). Default: empty (browser default)
  , _text_color :: WhenTW ColorWithOpacity
    -- ^ Text color with optional opacity. Default: empty (browser default, usually black)
  , _text_decoration :: TextDecorationTW
    -- ^ Text decoration (underline, overline, strikethrough, color, style, thickness, offset)
  , _text_style :: WhenTW FontStyle
    -- ^ Font style (Italic or NotItalic). Default: empty (NotItalic)
  , _text_cursor :: WhenTW CursorStyle
    -- ^ Mouse cursor style. Default: empty (default cursor)
  , _text_custom :: T.Text
    -- ^ Arbitrary custom Tailwind classes. Default: empty string
  }

makeLenses ''TextConfigTW

instance HasCustom TextConfigTW where
  custom = text_custom

