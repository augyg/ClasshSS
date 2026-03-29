{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}

--------------------------------------------------------------------------------
-- |
--  Module      :  Classh
--  Description :  Type-safe Tailwind CSS class generation via Template Haskell
--  Copyright   :  (c) 2024, Galen Sprout
--  License     :  BSD-style (see end of this file)
--
--  Maintainer  :  Galen Sprout <galen.sprout@gmail.com>
--  Stability   :  provisional
--  Portability :  portable
--
--  ClasshSS: Type-safe CSS-in-Haskell based on Tailwind CSS.
--
--  = Overview
--
--  ClasshSS provides a type-safe interface to Tailwind CSS classes with compile-time
--  validation. It prevents common mistakes like conflicting class definitions and
--  ensures your styles are valid before runtime.
--
--  The library revolves around two main configuration types:
--
--  * 'BoxConfig' - For styling HTML elements (layout, colors, borders, shadows, transforms)
--  * 'TextConfigTW' - For text styling (font, size, weight, color, decoration)
--
--  = Quick Start
--
--  A simple example creating a styled box with text using reflex-dom:
--
--  @
--  {-# LANGUAGE TemplateHaskell #-}
--  import Classh
--  import Reflex.Dom.Core
--
--  main :: IO ()
--  main = mainWidget $ do
--    elClass "div" $(classh' [ pt .~~ TWSize 20, bgColor .~~ Gray C300 ]) $ do
--      textS $(classh' [text_size .|~ [XL, XL2]]) "Hello, ClasshSS!"
--  @
--
--  This generates type-checked Tailwind classes: @\"pt-20 bg-gray-300\"@ for the div
--  and responsive text sizing for the content.
--
--  = Comprehensive Example
--
--  A complete example showing all ClasshSS features in a styled card component:
--
--  @
--  {-# LANGUAGE TemplateHaskell #-}
--
--  module Example where
--
--  import Classh
--  import Reflex.Dom.Core
--  import Reflex.Classh (textS, textPosition)  -- Note: from reflex-classh package
--
--  -- Complete example: styled card with positioned text
--  exampleCard :: (DomBuilder t m, PostBuild t m) => m ()
--  exampleCard =
--    -- BoxConfig: All element-level styling
--    elClass \"div\" $(classh'
--      [ -- Colors
--        bgColor .~~ White
--      , border . bColor . all .~~ Gray C200
--
--      , -- Spacing
--        p .~~ TWSize 6
--      , m .~~ TWSize 4
--
--      , -- Shape
--        br .~~ R_Lg
--      , border . bWidth . all .~~ B1
--
--      , -- Shadow & hover effect
--        shadow .~^ [ (\"def\", noTransition Shadow_Sm)
--                   , (\"hover\", Shadow_Lg \`withTransition\` Duration_300)
--                   ]
--
--      , -- Transform on hover
--        transform . scale .~^ [ (\"def\", noTransition Scale_100)
--                              , (\"hover\", Scale_105 \`withTransition\` Duration_200)
--                              ]
--
--      , -- Grid positioning (NOT flex - avoid flexbox)
--        colStart .~~ 2
--      , colSpan .~~ 4
--
--      , -- Cursor
--        cursor .~~ CursorPointer
--      ]) $ do
--        -- TextConfigTW: Text styling via textS (from reflex-classh)
--        textS $(classhText
--          [ text_color .~~ Gray C900
--          , text_size .~~ TextXl
--          , text_weight .~~ FontBold
--          ]) \"Card Title\"
--
--        -- TextPosition: Position text (from reflex-classh)
--        el \"p\" $ textPosition $(classhTextPos
--          [ textAlign .~~ TextCenter
--          , textTransform .~~ Uppercase
--          ]) $ text \"Centered uppercase text\"
--
--        -- More content with separate styling
--        textS $(classhText
--          [ text_color .~~ Gray C600
--          , text_size .~~ TextSm
--          ]) \"Card description text\"
--  @
--
--  __Key Concepts Shown:__
--
--  * BoxConfig applied to div via @classh'@
--  * TextConfigTW applied via @textS@ (from @reflex-classh@ package)
--  * TextPosition via @textPosition@ (from @reflex-classh@ package)
--  * Type separation: Cannot mix BoxConfig and TextConfigTW in the same @classh'@ call
--  * Responsive values with @.|~@ (not shown here - see docs)
--  * State-based transitions with @.~^@ (shadow, transform)
--  * Grid positioning (avoiding flexbox)
--  * Transform composition
--
--  For the full example with detailed explanations, see @docs\/EXAMPLE.md@.
--
--  = Template Haskell Functions
--
--  ClasshSS provides several Template Haskell functions for compile-time class generation:
--
--  * 'classh'' - Apply mutations to default config (most common)
--  * 'classh' - Apply mutations to custom base config
--  * 'classhUnsafe' - Runtime version without compile-time checking
--  * 'classhV'' - Single mutation variant of classh'
--  * 'classhV' - Single mutation variant of classh
--
--  == classh' - Default Config
--
--  Use when starting from scratch with no base configuration:
--
--  @
--  buttonClasses :: Text
--  buttonClasses = $(classh'
--    [ bgColor .~~ Blue C500
--    , px .~~ TWSize 6
--    , py .~~ TWSize 3
--    , br .~~ R_Md
--    ])
--  -- Compiles to: \"bg-blue-500 px-6 py-3 rounded-md\"
--  @
--
--  == classh - Custom Base Config
--
--  Use when you have a base theme or default configuration:
--
--  @
--  myTheme :: BoxConfig
--  myTheme = def & bgColor .~~ Gray C50 & p .~~ TWSize 4
--
--  customBox :: Text
--  customBox = $(classh myTheme
--    [ bgColor .~~ Blue C100  -- Overrides theme background
--    , br .~~ R_Lg
--    ])
--  @
--
--  == classhUnsafe - Runtime Generation
--
--  Use when you need runtime arguments (no compile-time checking):
--
--  @
--  dynamicClasses :: Color -> Text
--  dynamicClasses color = classhUnsafe [bgColor .~~ color]
--  @
--
--  = Operators
--
--  ClasshSS provides ergonomic operators for setting properties:
--
--  == .~~ (Set Constant)
--
--  Sets a value for all screen sizes:
--
--  @
--  bgColor .~~ Blue C500    -- bg-blue-500
--  br .~~ R_3Xl             -- rounded-3xl
--  shadow .~~ Shadow_Md     -- shadow-md
--  @
--
--  == .|~ (Set Responsive)
--
--  Sets responsive values for each breakpoint [mobile, sm, md, lg, xl, 2xl]:
--
--  @
--  w .|~ [TWSize' (TWSize 12), TWSize' (TWSize 24), TWSize' (TWSize 48)]
--  -- mobile: w-12, sm: w-24, md: w-48
--
--  text_size .|~ [Base, LG, XL, XL2]
--  -- mobile: text-base, sm: text-lg, md: text-xl, lg: text-2xl
--  @
--
--  == .~^ (Set with Transitions)
--
--  Sets stateful values (hover, focus, etc.) with transitions:
--
--  @
--  bgColor .~^
--    [ (\"def\", noTransition (Blue C600))
--    , (\"hover\", Blue C400 \`withTransition\` Duration_300 \`withTiming\` Ease_InOut)
--    ]
--  @
--
--  Available states: @\"def\"@, @\"hover\"@, @\"focus\"@, @\"active\"@
--
--  == .~ (Simple Setter)
--
--  Direct lens setter, mainly for the @custom@ field:
--
--  @
--  custom .~ \"flex items-center gap-4\"
--  @
--
--  == Additional Operators
--
--  * '.~+' - Append to existing WhenTW list
--  * '.++' - Extend with single conditional value
--  * '.|+' - Append responsive values to existing
--
--  = Shorthand Helpers
--
--  ClasshSS provides Tailwind-style shorthand for common properties:
--
--  @
--  -- Instead of: padding . t .~~ TWSize 4
--  pt .~~ TWSize 4
--
--  -- Instead of: border . radius . allS .~~ R_Md
--  br .~~ R_Md
--
--  -- Instead of: border . bWidth . allS .~~ B2
--  bw .~~ B2
--  @
--
--  Common shortcuts:
--
--  * Padding: 'pt', 'pb', 'pl', 'pr', 'px', 'py', 'p'
--  * Margin: 'mt', 'mb', 'ml', 'mr', 'mx', 'my', 'm'
--  * Border radius: 'br', 'br_t', 'br_b', 'br_l', 'br_r'
--  * Border width: 'bw', 'bw_t', 'bw_b', 'bw_l', 'bw_r'
--  * Border color: 'bc', 'bc_t', 'bc_b', 'bc_l', 'bc_r'
--  * Sizing: 'w', 'h', 'maxW', 'maxH', 'minW', 'minH'
--
--  = Common Use Cases
--
--  == Responsive Button
--
--  @
--  $(classh'
--    [ bgColor .~^ [(\"def\", noTransition (Blue C500)), (\"hover\", Blue C600 \`withTransition\` Duration_300)]
--    , px .|~ [TWSize 4, TWSize 6, TWSize 8]  -- Responsive padding
--    , py .|~ [TWSize 2, TWSize 3, TWSize 4]
--    , br .~~ R_Md
--    , shadow .~^ [(\"def\", noTransition Shadow_Sm), (\"hover\", Shadow_Md \`withTransition\` Duration_200)]
--    ])
--  @
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
--    [ w .~~ TWFraction 11 D12  -- 11/12 width
--    , mx .~~ TWSize_Auto       -- Center horizontally
--    , p .~~ TWSize 8
--    ])
--  @
--
--  = Compile-Time Safety
--
--  ClasshSS catches errors at compile-time:
--
--  @
--  -- ERROR: pt and py overlap (py sets both pt and pb)
--  $(classh' [ pt .~~ TWSize 4, py .~~ TWSize 2 ])
--
--  -- ERROR: Duplicate screen condition
--  $(classh' [ bgColor .~^ [(\"hover\", Blue C500), (\"hover\", Blue C600)] ])
--  @
--
--  The Template Haskell syntax @$(...)@ enables this validation. For runtime generation
--  without checking, use 'classhUnsafe'.
--
--  = See Also
--
--  * "Classh.Box" - BoxConfig type and all box styling properties
--  * "Classh.Text" - TextConfigTW type and text styling properties
--  * "Classh.Setters" - Detailed operator documentation
--  * "Classh.Shorthand" - All shorthand helper functions
--  * "Classh.WithTransition" - Transition system for smooth animations
--  * "Classh.Responsive.WhenTW" - Responsive design system
--
--  For comprehensive guides, see:
--
--  * @docs/GETTING_STARTED.md@ - Step-by-step tutorial
--  * @docs/MIGRATION_FROM_TAILWIND.md@ - For Tailwind CSS users
--  * @docs/core-concepts/OPERATORS.md@ - Complete operator reference
--  * @docs/examples/@ - Real-world component examples
--
--------------------------------------------------------------------------------

module Classh
  (
  -- * Write class string
     classh
  , classh'
  , classhUnsafe
  , classhV
  , classhV'
  , boxCSS
  -- * Re-Exports
  , module Box
  , module Text
  , module TextPosition
  , module Grid
  , module ZipScreens
  , module Setters
  , module Shorthand
  , module ShowTW
  , module CompileStyle
  , module HasCSSSize
  , module HasCustom
  , module IsCSS
  , module SetSides
  , module Chain
  , module TShow
  , module Utils
  -- * Extend
  , alsoF
  , also
  , applyFs
  -- * Group classes together
  , ClassCollection(..)
  -- * Deprecated
  , defaultClasses
  -- * Documenting types
  , Compiled
  , Expression
  , CompiledClassh
  ) where

import Classh.Box as Box
import Classh.Text as Text
import Classh.TextPosition as TextPosition
import Classh.Grid as Grid
import Classh.Responsive.ZipScreens as ZipScreens
import Classh.Setters as Setters
import Classh.Shorthand as Shorthand
import Classh.Class.ShowTW as ShowTW
import Classh.Class.CompileStyle as CompileStyle
import Classh.Class.HasCSSSize as HasCSSSize
import Classh.Class.HasCustom as HasCustom
import Classh.Class.IsCSS as IsCSS
import Classh.Class.SetSides as SetSides
import Classh.Internal.Chain as Chain
import Classh.Internal.TShow as TShow
import Classh.Internal.Utils as Utils

import Data.Default (Default(..))
import "template-haskell" Language.Haskell.TH as TH
import qualified Data.Text as T






{-
GOALS
-> Cut down the surface area of what is needed from CSS
-> Make an easy to integrate system through this limited CSS
-> Eliminate messing with typos and repeats
-> Encourage non-naive responsive code
-> Have more organized code for when an attribute is highly responsive
   Non eg. "py-2 ......... sm:py-4 ............... lg:pt-9"
-> Helper functions with a definite limited scope for the goal of self-documenting / easy to visualize DomBuilders

-}




{-
TODOS
-------------------------------------
->BoxConfig by Media Query
f :: [BySize] -> BoxConfig
         ^INTERNAL^
  [BoxConfig -> BoxConfig]

md = [ w = 4 , h = 9 , color = marigold ]
-------------------------------------
-> VelvetBox: a config type to describe a flex box like component
   -> Min-width
   -> Min-height  -- w+h mins so that content

Items }} Box }} GridLayout

Items: text, images, image content, radiobutton<--with-->label
Box: components, which contain items
GridLayout: Highly variably-sized element with tons of purposeful white-space

widthTotal <- element widthTotal $ do
  mkItem (w = 9)
  mkItem (w = 9)

|__<-- item -->____<--  item  -->__|  // Derive the min width to propogate upwards

mkItem :: WriterT WidthTotal

tell (w = 9)

-}

type Compiled = Q
type Expression = Exp
type CompiledClassh = Compiled Expression


defaultClasses :: T.Text
defaultClasses = ""

-- | Apply mutations to a base config at compile-time with validation.
--
-- This is the primary function for creating CSS classes with a custom base configuration.
-- It performs compile-time validation to catch errors like duplicate screen conditions
-- or conflicting property settings.
--
-- === Examples
--
-- Basic usage with custom base:
--
-- @
-- myTheme :: BoxConfig
-- myTheme = def & bgColor .~~ Gray C50 & p .~~ TWSize 4
--
-- customBox :: Text
-- customBox = $(classh myTheme
--   [ bgColor .~~ Blue C100  -- Override theme background
--   , br .~~ R_Lg            -- Add border radius
--   ])
-- @
--
-- With TextConfigTW:
--
-- @
-- myTextTheme :: TextConfigTW
-- myTextTheme = def & text_color .~~ Gray C900 & text_weight .~~ Normal
--
-- headingClasses :: Text
-- headingClasses = $(classh myTextTheme
--   [ text_size .~~ XL3
--   , text_weight .~~ Bold  -- Override theme weight
--   ])
-- @
--
-- === Compile-Time Errors
--
-- @
-- -- ERROR: pt and py conflict (py sets both pt and pb)
-- $(classh def [ pt .~~ TWSize 4, py .~~ TWSize 2 ])
--
-- -- ERROR: Duplicate \"hover\" condition
-- $(classh def [ bgColor .~^ [(\"hover\", Blue C500), (\"hover\", Red C500)] ])
-- @
--
-- @since 0.1.0.0
classh :: CompileStyle s => s -> [(s -> s)] -> Q Exp
classh base muts = case compileS $ foldl (\acc f -> f acc) base muts of
  Left e -> fail $ T.unpack e
  Right styleString -> [| styleString |]

-- | Apply mutations to default config at compile-time with validation.
--
-- This is the most commonly used function for generating CSS classes. It starts with
-- the default configuration ('def') and applies your mutations with compile-time checking.
--
-- === Examples
--
-- Simple box styling:
--
-- @
-- $(classh'
--   [ bgColor .~~ Blue C500
--   , p .~~ TWSize 8
--   , br .~~ R_Md
--   ])
-- -- Result: \"bg-blue-500 p-8 rounded-md\"
-- @
--
-- Responsive design:
--
-- @
-- $(classh'
--   [ w .|~ [TWSize' (TWSize 12), TWSize' (TWSize 24), TWSize' (TWSize 48)]
--   , bgColor .|~ [Gray C100, Gray C200, Gray C300]
--   ])
-- -- Result: \"w-12 sm:w-24 md:w-48 bg-gray-100 sm:bg-gray-200 md:bg-gray-300\"
-- @
--
-- With hover effects:
--
-- @
-- $(classh'
--   [ bgColor .~^ [ (\"def\", noTransition (Blue C500))
--                 , (\"hover\", Blue C600 \`withTransition\` Duration_300)
--                 ]
--   ])
-- @
--
-- Text styling:
--
-- @
-- $(classh'
--   [ text_size .~~ XL2
--   , text_weight .~~ Bold
--   , text_color .~~ Gray C900
--   ])
-- -- Result: \"text-2xl font-bold text-gray-900\"
-- @
--
-- === Type Inference
--
-- The return type is inferred from usage context. Works with both BoxConfig and TextConfigTW:
--
-- @
-- boxClasses :: Text
-- boxClasses = $(classh' [ bgColor .~~ Blue C500 ])
--
-- textClasses :: Text
-- textClasses = $(classh' [ text_color .~~ Blue C500 ])
-- @
--
-- @since 0.1.0.0
classh' :: (Default s, CompileStyle s) => [(s -> s)] -> Q Exp
classh' muts = case compileS $ foldl (\acc f -> f acc) def muts of
  Left e -> fail $ T.unpack e
  Right styleString -> [| styleString |]

-- | Runtime class generation without Template Haskell.
--
-- Use this when you need runtime arguments that can't be known at compile-time.
-- Note: This skips compile-time validation, so errors will only appear at runtime.
--
-- === When to Use
--
-- * When creating library functions that accept runtime parameters
-- * When working with dynamic values from user input
-- * When integrating with Reflex.Dom's 'Dynamic' types
--
-- === Examples
--
-- Dynamic color based on input:
--
-- @
-- coloredBox :: Color -> Text
-- coloredBox color = classhUnsafe [ bgColor .~~ color, p .~~ TWSize 4 ]
--
-- -- Usage:
-- coloredBox (Blue C500)  -- \"bg-blue-500 p-4\"
-- coloredBox (Red C600)   -- \"bg-red-600 p-4\"
-- @
--
-- With Reflex.Dom Dynamic:
--
-- @
-- dynClasses <- holdDyn (classhUnsafe [bgColor .~~ Gray C300]) updateEvent
-- elDynClass \"div\" dynClasses $ text \"Dynamic styling\"
-- @
--
-- In reusable components:
--
-- @
-- styledButton :: Text -> BoxConfig -> m ()
-- styledButton label customConfig = do
--   let classes = classhUnsafe [id]  -- Apply custom config
--   elClass \"button\" (boxCSS customConfig) $ text label
-- @
--
-- === Drawbacks
--
-- * No compile-time validation
-- * Can't catch conflicting properties or duplicate conditions
-- * Slightly less performant (runtime string generation)
--
-- @since 0.1.0.0
classhUnsafe :: (Default a, ShowTW a) => [a -> a] -> T.Text
classhUnsafe muts = showTW $ def `applyFs` muts

-- | Single-mutation variant of 'classh'.
--
-- Convenience function for applying exactly one mutation to a base config.
--
-- === Examples
--
-- @
-- -- Instead of:
-- $(classh myTheme [ bgColor .~~ Blue C500 ])
--
-- -- You can write:
-- $(classhV myTheme (bgColor .~~ Blue C500))
-- @
--
-- @since 0.1.0.0
classhV :: (CompileStyle a) => a -> (a -> a) -> Q Exp
classhV base mutation = classh base [mutation]

-- | Single-mutation variant of 'classh''.
--
-- Convenience function for applying exactly one mutation to the default config.
--
-- === Examples
--
-- @
-- -- Instead of:
-- $(classh' [ bgColor .~~ Blue C500 ])
--
-- -- You can write:
-- $(classhV' (bgColor .~~ Blue C500))
-- @
--
-- @since 0.1.0.0
classhV' :: (Default a, CompileStyle a) => (a -> a) -> Q Exp
classhV' mutation = classh' [mutation]


-- | Synonym for 'showTW' specialized to BoxConfig.
--
-- Converts a BoxConfig to its Tailwind CSS class string representation.
-- Useful when you already have a constructed BoxConfig value.
--
-- === Examples
--
-- @
-- myBox :: BoxConfig
-- myBox = def & bgColor .~~ Blue C500 & p .~~ TWSize 4
--
-- classes :: Text
-- classes = boxCSS myBox
-- -- Result: \"bg-blue-500 p-4\"
-- @
--
-- @since 0.1.0.0
boxCSS :: BoxConfig -> T.Text
boxCSS = showTW

-- | Append BoxConfig mutations to an existing class string.
--
-- Useful for extending a base set of classes with additional styling.
--
-- === Examples
--
-- @
-- baseClasses :: Text
-- baseClasses = \"container mx-auto\"
--
-- extendedClasses :: Text
-- extendedClasses = alsoF baseClasses [ bgColor .~~ Blue C50, p .~~ TWSize 8 ]
-- -- Result: \"container mx-auto bg-blue-50 p-8\"
-- @
--
-- @since 0.1.0.0
alsoF :: T.Text -> [BoxConfig -> BoxConfig] -> T.Text
alsoF s cfgMuts = s <> boxCSS (def `applyFs` cfgMuts)

-- | Append a BoxConfig to an existing class string.
--
-- Similar to 'alsoF' but takes a constructed BoxConfig instead of mutations.
--
-- === Examples
--
-- @
-- baseClasses :: Text
-- baseClasses = \"container mx-auto\"
--
-- additionalConfig :: BoxConfig
-- additionalConfig = def & bgColor .~~ Blue C50 & p .~~ TWSize 8
--
-- combined :: Text
-- combined = also baseClasses additionalConfig
-- -- Result: \"container mx-auto bg-blue-50 p-8\"
-- @
--
-- @since 0.1.0.0
also :: T.Text -> BoxConfig -> T.Text
also s cfg = s <> boxCSS cfg

-- | Apply a list of functions to a value in sequence.
--
-- This is a helper function used internally by ClasshSS to apply
-- mutations to configurations. Left fold over the functions.
--
-- === Examples
--
-- @
-- result = applyFs def
--   [ bgColor .~~ Blue C500
--   , p .~~ TWSize 4
--   , br .~~ R_Md
--   ]
-- @
--
-- @since 0.1.0.0
applyFs :: a -> [a -> a] -> a
applyFs in_ fs = foldl (\acc f -> f acc) in_ fs

-- | Collection of config mutations that can be applied together.
--
-- Useful for grouping related styling mutations that are frequently used together.
--
-- === Examples
--
-- @
-- buttonBase :: ClassCollection BoxConfig
-- buttonBase = ClassCollection
--   [ px .~~ TWSize 6
--   , py .~~ TWSize 3
--   , br .~~ R_Md
--   , text_weight .~~ Bold
--   ]
--
-- primaryButton :: Text
-- primaryButton = $(classh' $
--   getCollection buttonBase ++
--   [ bgColor .~~ Blue C500 ])
-- @
--
-- @since 0.1.0.0
newtype ClassCollection tw = ClassCollection { getCollection :: [tw -> tw] }

-- END OF MODULE




-- | A future research item for this library is how to work with accessibility
  -- do we need prerender ?
  -- is this gonna be reading in some config?
  -- is this a native thing?
  -- aria?
  -- will this mean scaling from same base accessibility based value?
  -- what features does this affect directly? indirectly?


-- | TODO:
  -- use template haskell to autogenerate types from a CSS file eg if classes blah and blah2 exist
  -- data UserCSS = Blah | Blah2




-------------------thought bubble ----------------------------------------------------------


-- module MyTWConfig where

-- def = TW.def & text_color .~~ aceBlue

-- then

-- module MyModule

-- import MyTWConfig
-- f = elTW def blank

-- ---

-- we could also have a defDerivative which functions just like V1 tailwindlib like so:
--   BoxConfig def:[]

--   where def here should be ("def", def)

--   and note we can make TWstring which could have an instance of IsString so that it can be written as a string literal

--   which helps cuz then TWString can have a default of "def"


-- Also for conceptual understanding:

--   fmap (,X) (enum screenSizes)  == ("def", X)
--   fmap (,X) (enum allConditions) == ("def", X)
--   fmap (,X) (enum allEvents) : ("def",X) : [] == ("def", X)
--   fmap (,def) (enum allEvents) == []


------------------------------------------------------------------------------------------------------
-- TODO:

-- CSS and Tailwind semigroups for folding into one CSSConfig OR TailwindClassConfig
-- where both render to a string

  -- Each config is only allowed to be set once -> alerting the dev if they asked for something like "red" and "blue"
  -- for the background color or similar illogical configurations

  -- Would be extra ideal to have this made clear at compile time


-- def
--   & leftPadding .~ 3
--   & styling .~ (def
--                 &  bg_color .~ Color_Custom (Hex "")
--                )


-- data BoxConfig = BoxConfig
--   { leftPadding :: T.Text
--   , bottomPadding :: T.Text
--   }



-- | Idea: elDynTW where we can update using the TW lib based on events and prev state

-- | Idea: for align and justify -> we have them as one setting of the datatype Position = TR -- TopRight | BL -- Bottom Left

-- | THIS DOESNT WORK: Grid causes other issues/effects - maybe we can negate this through defaults (eg. two styled texts begin to stack
-- | instead of go beside each other


-- data RowDivisor = RD1 | RD2 | RD3 | RD4 | RD6 | RD12


-- data Dimension
--   = DNum Float
--   | DFrac Int DivInt
--   | Full
--   | Screen
--   | Min
--   | Max
--   | Fit
--   | Dim_Custom T.Text

-- -- | todo: can we merge with gridCol Col1-12?
-- data IntGrid
--   = IG1
--   | IG2
--   | IG3
--   | IG4
--   | IG5
--   | IG6
--   | IG7
--   | IG8
--   | IG9
--   | IG10
--   | IG11
--   | IG12
