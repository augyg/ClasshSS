{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}

--------------------------------------------------------------------------------
-- |
--  Module      :  Classh.Box
--  Description :  Box model configuration type and lenses
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  BoxConfig: The core type for styling HTML elements (divs, sections, images, etc.).
--
--  = Overview
--
--  This module provides 'BoxConfig', the primary configuration type for styling
--  HTML elements in ClasshSS. It encompasses all visual properties except text-specific
--  styling (which uses 'Classh.Text.TextConfigTW').
--
--  BoxConfig includes:
--
--  * Layout - Grid positioning, sizing, constraints
--  * Spacing - Padding and margin with responsive support
--  * Colors - Background colors and opacity (with transitions)
--  * Borders - Radius, width, color, style, rings, outlines
--  * Visual Effects - Shadows (with transitions)
--  * Transforms - Rotate, scale, translate, skew (with transitions)
--  * Positioning - Justify and align content
--  * Cursor - Mouse cursor styles
--
--  = Quick Example
--
--  Creating a styled card with Reflex.Dom:
--
--  @
--  elClass \"div\" $(classh'
--    [ bgColor .~~ White
--    , p .~~ TWSize 6
--    , br .~~ R_Lg
--    , shadow .~~ Shadow_Lg
--    , border . bWidth . allS .~~ B1
--    , border . bColor . allS .~~ Gray C200
--    ]) $ do
--      text \"Card content here\"
--  @
--
--  = BoxConfig Fields
--
--  == Grid Layout
--
--  * '_colStart' - Grid column start position (1-12)
--  * '_colSpan' - Grid column span (1-12)
--
--  @
--  $(classh' [ colStart .~~ 2, colSpan .~~ 4 ])
--  -- Result: \"col-start-2 col-span-4\"
--  @
--
--  == Background
--
--  * '_bgColor' - Background color (transitionable)
--  * '_bgOpacity' - Background opacity 1-100 (transitionable, default 100)
--
--  @
--  -- Simple background
--  bgColor .~~ Blue C500
--
--  -- With hover transition
--  bgColor .~^ [ (\"def\", noTransition (Blue C500))
--              , (\"hover\", Blue C600 \`withTransition\` Duration_300)
--              ]
--  @
--
--  == Spacing
--
--  * '_padding' - 'BoxPadding' with individual sides (transitionable)
--  * '_margin' - 'BoxMargin' with individual sides (transitionable)
--
--  @
--  -- Using shorthand (see "Classh.Shorthand")
--  pt .~~ TWSize 4   -- padding-top
--  pb .~~ TWSize 4   -- padding-bottom
--  px .~~ TWSize 6   -- padding-left and padding-right
--  py .~~ TWSize 2   -- padding-top and padding-bottom
--  p .~~ TWSize 4    -- all sides
--
--  -- Same pattern for margin: mt, mb, mx, my, m
--  @
--
--  == Sizing
--
--  * '_sizingBand' - 'BoxSizingBand' containing width, height, min/max constraints (all transitionable)
--
--  @
--  -- Using shorthand
--  w .~~ TWSize_Full           -- width: 100%
--  h .~~ pix 400               -- height: 400px
--  maxW .~~ TWSize_Screen      -- max-width: 100vw
--  minH .~~ pix 200            -- min-height: 200px
--
--  -- Fractional widths
--  w .~~ TWFraction 11 D12     -- width: 11/12
--  @
--
--  == Borders
--
--  * '_border' - 'BorderConfig' with radius, width, color, style, rings, outlines (most transitionable)
--
--  @
--  -- Using shorthand
--  br .~~ R_Lg                    -- rounded-lg (all corners)
--  br_t .~~ R_Md                  -- rounded-t-md (top corners)
--  bw .~~ B2                      -- border-2 (all sides)
--  bw_t .~~ B1                    -- border-t (top only)
--  bc .~~ Gray C300               -- border-gray-300 (all sides)
--
--  -- Full path for fine control
--  border . radius . borderRadius_tr .~~ R_Lg  -- Top-right corner only
--  border . bWidth . t .~~ B2                  -- Top border width
--  border . bColor . allS .~~ Gray C200        -- All sides border color
--  @
--
--  == Visual Effects
--
--  * '_shadow' - Box shadow (transitionable)
--
--  @
--  shadow .~~ Shadow_Md
--
--  -- With hover transition
--  shadow .~^ [ (\"def\", noTransition Shadow_Sm)
--             , (\"hover\", Shadow_Lg \`withTransition\` Duration_200)
--             ]
--  @
--
--  == Transforms
--
--  * '_transform' - 'TransformConfig' with rotate, scale, translate, skew, origin (all transitionable)
--
--  @
--  -- Simple rotation
--  transform . rotate .~~ Rotate_45
--
--  -- Scale with hover
--  transform . scale .~^ [ (\"def\", noTransition Scale_100)
--                        , (\"hover\", Scale_105 \`withTransition\` Duration_300)
--                        ]
--
--  -- Translation
--  transform . translateX .~~ Translate_TWSize (TWSize 4)
--  transform . translateY .~~ Translate_Fraction 1 D2  -- 50%
--  @
--
--  == Positioning
--
--  * '_position' - Tuple of ('Justify', 'Align') for content positioning
--
--  @
--  position .~~ (J_Center, A_Center)  -- Center content
--  position .~~ centered              -- Shorthand for above
--  position .~~ topLeft               -- Top-left alignment
--  @
--
--  == Cursor
--
--  * '_cursor' - Mouse cursor style
--
--  @
--  cursor .~~ CursorPointer
--  cursor .~~ CursorNotAllowed
--  @
--
--  == Custom Classes
--
--  * '_box_custom' - Arbitrary Tailwind classes (escape hatch)
--
--  @
--  custom .~ \"flex flex-col items-center gap-4\"
--  @
--
--  = Responsive Design
--
--  Most BoxConfig properties use 'WhenTW' for responsive values:
--
--  @
--  -- Different backgrounds at each breakpoint
--  bgColor .|~ [Gray C100, Gray C200, Gray C300, Gray C400, Gray C500, Gray C600]
--  --          mobile    sm         md         lg         xl         2xl
--
--  -- Responsive padding
--  p .|~ [TWSize 2, TWSize 4, TWSize 6, TWSize 8]
--  --    mobile    sm         md         lg (and larger)
--  @
--
--  = State-Based Styling
--
--  Use '.~^' for hover, focus, and other states:
--
--  @
--  bgColor .~^
--    [ (\"def\", noTransition (Blue C500))
--    , (\"hover\", Blue C600 \`withTransition\` Duration_300)
--    , (\"focus\", Blue C700 \`withTransition\` Duration_200)
--    ]
--  @
--
--  = Type Safety
--
--  BoxConfig enforces type safety at compile-time:
--
--  * Cannot mix BoxConfig and TextConfigTW setters in same expression
--  * Cannot set conflicting properties (e.g., @pt@ and @py@ together)
--  * Catches duplicate screen conditions
--
--  @
--  -- COMPILE ERROR: pt and py conflict (py sets both pt and pb)
--  $(classh' [ pt .~~ TWSize 4, py .~~ TWSize 2 ])
--  @
--
--  = Common Patterns
--
--  == Card Component
--
--  @
--  $(classh'
--    [ bgColor .~~ White
--    , br .~~ R_Lg
--    , shadow .~~ Shadow_Lg
--    , p .~~ TWSize 6
--    , border . bWidth . allS .~~ B1
--    , border . bColor . allS .~~ Gray C200
--    ])
--  @
--
--  == Centered Container
--
--  @
--  $(classh'
--    [ w .~~ TWFraction 11 D12
--    , mx .~~ TWSize_Auto
--    , p .~~ TWSize 8
--    ])
--  @
--
--  == Responsive Button
--
--  @
--  $(classh'
--    [ bgColor .~^ [(\"def\", noTransition (Blue C500)), (\"hover\", Blue C600 \`withTransition\` Duration_300)]
--    , px .|~ [TWSize 4, TWSize 6, TWSize 8]
--    , py .|~ [TWSize 2, TWSize 3, TWSize 4]
--    , br .~~ R_Md
--    , shadow .~^ [(\"def\", noTransition Shadow_Sm), (\"hover\", Shadow_Md \`withTransition\` Duration_200)]
--    ])
--  @
--
--  = Shorthand Helpers
--
--  For more ergonomic code, use shorthand from "Classh.Shorthand":
--
--  @
--  -- Instead of: padding . t .~~ TWSize 20
--  pt .~~ TWSize 20
--
--  -- Instead of: border . radius . allS .~~ R_Lg
--  br .~~ R_Lg
--  @
--
--  See "Classh.Shorthand" for complete list of shortcuts.
--
--  = Re-Exported Modules
--
--  This module re-exports all box-related modules for convenience:
--
--  * "Classh.Color" - Color types and hex colors
--  * "Classh.Cursor" - Cursor styles
--  * "Classh.Box.TWSize" - Size types and helpers
--  * "Classh.Box.Padding" - Padding configuration
--  * "Classh.Box.Margin" - Margin configuration
--  * "Classh.Box.SizingBand" - Width/height with constraints
--  * "Classh.Box.Placement" - Justify and Align types
--  * "Classh.Box.Border" - Border configuration
--  * "Classh.Box.Shadow" - Shadow types
--  * "Classh.Box.Transition" - Transition configuration
--  * "Classh.Box.Transform" - Transform types and configuration
--  * "Classh.WithTransition" - Transition builder system
--  * "Classh.Responsive.WhenTW" - Responsive value system
--
--  = See Also
--
--  * "Classh" - Main module with Template Haskell functions
--  * "Classh.Text" - For text-specific styling
--  * "Classh.Setters" - Operator documentation
--  * "Classh.Shorthand" - Ergonomic shortcuts
--  * @docs/features/BOX_STYLING.md@ - Complete feature guide
--  * @docs/examples/@ - Real-world examples
--
--------------------------------------------------------------------------------


module Classh.Box
  (
    -- * Core Config Type 
    BoxConfig(..)
  , module TWNum
  , module WhenTW
  , module Color
  , module Gradient
  , module Cursor
  , module TWSize
  , module Padding
  , module Margin
  , module SizingBand
  , module Placement
  , module Border
  , module Shadow
  , module Transition
  , module Transform
  , module WT
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

import Classh.Class.HasCustom as HasCustom
import Classh.Class.ShowTW (ShowTW(..))
import Classh.Class.CompileStyle as CompileStyle
import Classh.Internal.Chain ((<&>))
import Classh.Internal.TShow (tshow)

import Classh.Internal.TWNum as TWNum
import Classh.Responsive.WhenTW as WhenTW
import Classh.Color as Color
import Classh.Box.Gradient as Gradient
import Classh.Cursor as Cursor
import Classh.Box.TWSize as TWSize
import Classh.Box.Padding as Padding
import Classh.Box.Margin as Margin
import Classh.Box.SizingBand as SizingBand
import Classh.Box.Placement as Placement
import Classh.Box.Border as Border
import Classh.Box.Shadow as Shadow
import Classh.Box.Transition as Transition
import Classh.Box.Transform as Transform
import Classh.WithTransition as WT

import Control.Lens hiding ((<&>), transform)
import Data.Default (Default(..))
import qualified Data.Text as T

-- | Configuration type for styling HTML box elements (divs, sections, etc.).
--
-- BoxConfig contains all visual styling properties for HTML elements except
-- text-specific properties (which use 'Classh.Text.TextConfigTW').
--
-- === Field Overview
--
-- * Grid: '_colStart', '_colSpan'
-- * Background: '_bgColor' (transitionable), '_bgOpacity' (transitionable)
-- * Spacing: '_padding' (transitionable), '_margin' (transitionable)
-- * Sizing: '_sizingBand' (width, height, min/max - transitionable)
-- * Borders: '_border' (radius, width, color, style, rings)
-- * Layout: '_position' (justify, align)
-- * Effects: '_shadow' (transitionable)
-- * Interaction: '_cursor'
-- * Transforms: '_transform' (rotate, scale, translate, skew - transitionable)
-- * Escape hatch: '_box_custom'
--
-- === Examples
--
-- @
-- -- Simple box
-- def & bgColor .~~ Blue C500 & p .~~ TWSize 4
--
-- -- Card component
-- def
--   & bgColor .~~ White
--   & br .~~ R_Lg
--   & shadow .~~ Shadow_Lg
--   & p .~~ TWSize 6
--
-- -- Responsive container
-- def
--   & w .|~ [TWSize_Full, TWSize' (TWSize 64), TWSize' (TWSize 80)]
--   & mx .~~ TWSize_Auto
-- @
--
-- @since 0.1.0.0
data BoxConfig = BoxConfig
  { _colStart :: WhenTW Int
    -- ^ Grid column start position (1-12). Default: empty (no grid positioning)
  , _colSpan :: WhenTW Int
    -- ^ Grid column span (1-12). Default: empty (no grid span)
  , _bgColor :: WhenTW (WithTransition GradientColor)
    -- ^ Background color or gradient (transitionable). Default: empty (no background)
    -- Use 'solid' for simple colors, or gradient helpers like 'linearGradient'
  , _bgOpacity :: WhenTW (WithTransition Int)
    -- ^ Background opacity 1-100 (transitionable). Default: empty (100% opacity)
  , _padding :: BoxPadding
    -- ^ Padding on all sides (transitionable). See 'BoxPadding' for details
  , _margin :: BoxMargin
    -- ^ Margin on all sides (transitionable). See 'BoxMargin' for details
  , _sizingBand :: BoxSizingBand
    -- ^ Width, height, and size constraints (transitionable). See 'BoxSizingBand'
  , _border :: BorderConfig
    -- ^ Border configuration (radius, width, color, style, rings, outlines)
  , _position :: WhenTW (Justify, Align)
    -- ^ Content positioning with justify and align
  , _shadow :: WhenTW (WithTransition BoxShadow)
    -- ^ Box shadow (transitionable). Default: empty (no shadow)
  , _cursor :: WhenTW CursorStyle
    -- ^ Mouse cursor style. Default: empty (default cursor)
  , _transform :: TransformConfig
    -- ^ All CSS transforms (rotate, scale, translate, skew, origin - transitionable)
  , _box_custom :: T.Text
    -- ^ Arbitrary custom Tailwind classes. Default: empty string
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
      , compileBgColor (_bgColor cfg)
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
   , renderBgColorTW (_bgColor cfg)
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

-- | Render a GradientColor for bgColor without any prefix.
-- Returns the raw class(es) that need prefixing.
renderBgColorRaw :: GradientColor -> T.Text
renderBgColorRaw (SolidColor cwo) = "bg-" <> showTW cwo
renderBgColorRaw (GradientColor cfg) = showTW cfg

-- | Apply a prefix to each space-separated class.
-- "hover:" "bg-gradient-to-r from-blue-500" -> "hover:bg-gradient-to-r hover:from-blue-500"
applyPrefixToClasses :: T.Text -> T.Text -> T.Text
applyPrefixToClasses prefix classes =
  T.intercalate " " $ fmap (\c -> prefix <> c) $ T.words classes

-- | Custom render for bgColor that handles gradient multi-class output.
-- Gradients produce multiple space-separated classes that each need
-- the responsive/state prefix applied.
renderBgColorTW :: WhenTW (WithTransition GradientColor) -> T.Text
renderBgColorTW tws = foldr (<&>) mempty $
  fmap (\(c, WithTransition val mTransCfg) ->
    let prefix = if c == "def" then "" else (c <> ":")
        classes = renderBgColorRaw val
        valueClasses = applyPrefixToClasses prefix classes
        transitionClasses = case mTransCfg of
          Nothing -> mempty
          Just cfg ->
            let cssProp = transitionPropertyToCSSName Transition_Colors
                duration = T.drop 9 $ tshow (_transitionDuration cfg)
                timing = transitionTimingToCSSName (_transitionTiming cfg)
                delay = T.drop 6 $ tshow (_transitionDelay cfg)
                transValue = cssProp <> "_" <> duration <> "ms_" <> timing <> "_" <> delay <> "ms"
            in prefix <> "[transition:" <> transValue <> "]"
    in valueClasses <&> transitionClasses
  ) tws

-- | Custom compile for bgColor with duplicate checking.
compileBgColor :: WhenTW (WithTransition GradientColor) -> Either T.Text T.Text
compileBgColor tws = case checkDuplicates $ fmap fst tws of
  Left e -> Left e
  Right () -> Right $ renderBgColorTW tws
  where
    checkDuplicates [] = Right ()
    checkDuplicates (s:ss) =
      if elem s ss
      then Left $ s <> " exists twice"
      else checkDuplicates ss
