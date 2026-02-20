# ClasshSS

Type-safe CSS-in-Haskell based on Tailwind CSS.

## Overview

ClasshSS lets you write Tailwind-style CSS in Haskell with compile-time checking. It prevents common mistakes like conflicting class definitions and ensures your styles are valid before runtime.

Two main config types:
- `BoxConfig` - element styling (layout, colors, borders, shadows)
- `TextConfigTW` - text styling (font, size, weight, color)

## Basic Example

```haskell
import Classh
import Reflex.Dom.Core

-- Box with styled text
elClass "div" $(classh' [ pt .~~ TWSize 20, bgColor .~~ Gray C300 ]) $ do
  textS $(classh' [text_size .|~ [XL, XL2]]) "Hello"
```

## Operators

**`.~~`** - Set value for all screen sizes
```haskell
bgColor .~~ hex "281C40"
br .~~ R_3Xl
shadow .~~ Shadow_Md
```

**`.|~`** - Responsive values `[mobile, sm, md, lg, xl, 2xl]`
```haskell
w .|~ [TWSize 12, TWSize 24, TWSize 48]
text_size .|~ [Base, LG, XL, XL2]
```

**`.~^`** - Stateful values (hover, focus, etc.) with transitions
```haskell
bgColor .~^ [("def", noTransition $ hex "281C40")
           , ("hover", hex "7B4DF4" `withTransition` Duration_300)
           ]
```

**`.~`** - Simple setter (mainly for `custom`)
```haskell
custom .~ "flex items-center"
```

## Common Properties

### Box (BoxConfig)

```haskell
-- Spacing
pt, pb, pl, pr, px, py, p      -- padding
mt, mb, ml, mr, mx, my, m      -- margin

-- Sizing
w, h                            -- width, height
minW, maxW, minH, maxH         -- constraints

-- Border
br, bw, bc                     -- radius, width, color
br_t, br_b, br_l, br_r         -- individual corners

-- Colors
bgColor                        -- background
bc                             -- border color

-- Layout
pos                            -- position (justify, align)
colStart, colSpan              -- grid columns

-- Visual
shadow                         -- box shadow
```

### Text (TextConfigTW)

```haskell
text_size                      -- XS, SM, Base, LG, XL, XL2, XL3, etc.
text_color                     -- any Color
text_weight                    -- Light, Normal, Medium, Semibold, Bold, etc.
text_font                      -- Font_Sans, Font_Serif, Font_Custom "Name"
text_align                     -- Left, Center, Right, Justify
```

## Shorthand

Instead of `border . radius . allS`, use `br`:
```haskell
$(classh' [ br .~~ R_3Xl
          , bw .~~ B2
          , bc .~~ hex "7B4DF4"
          ])
```

## Colors

```haskell
-- Hex colors
hex "281C40"

-- Tailwind colors
Gray C300
Red C500
Blue C600

-- Standard
White
Black
```

## Transitions

```haskell
-- Basic
bgColor .~^ [("def", noTransition purple)
           , ("hover", lavender `withTransition` Duration_300)
           ]

-- With timing
shadow .~^ [("def", noTransition Shadow_Md)
          , ("hover", Shadow_Lg `withTransition` Duration_300
                                `withTiming` Ease_InOut)
          ]

-- All at once
bgColor .~^ [("def", noTransition blue)
           , ("hover", purple `withTransitionAll` Duration_300 Ease_In Delay_0)
           ]
```

## Template Haskell

**`classh'`** - Compile with defaults
```haskell
$(classh' [ bgColor .~~ hex "281C40" ])
```

**`classh`** - Compile with custom base config
```haskell
$(classh myBaseConfig [ text_size .~~ XL2 ])
```

**`classhUnsafe`** - Runtime (no TH, for library functions)
```haskell
classhUnsafe [ bgColor .~~ hex "281C40" ]
```

The TH versions check for conflicts at compile-time. For example, setting both `pt` and `py` will fail to compile since they overlap.

## Usage with Reflex

```haskell
-- Static classes
elClass "div" $(classh' [ w .~~ TWSize_Full, p .~~ TWSize 4 ]) $
  text "content"

-- Dynamic classes
dynClasses <- holdDyn (classhUnsafe [bgColor .~~ Gray C300]) $ ...
elDynClass "div" dynClasses $ text "content"

-- Text with styling
textS $(classh' [ text_size .~~ XL3
                , text_color .~~ hex "F3F1F8"
                , text_weight .~~ Bold
                ]) "Hello"
```

## Why ClasshSS?

- **Type safety** - Invalid CSS won't compile
- **No conflicts** - Can't accidentally set overlapping properties
- **Responsive by default** - Easy to write mobile-first styles
- **Transitions built-in** - Type-safe hover/focus states
- **Tailwind familiar** - If you know Tailwind, you know ClasshSS
- **Explicit layout** - Strict about width/height consumption, no flexbox magic

## Layout Philosophy

ClasshSS intentionally does NOT support `display: flex` or flexbox properties. The design philosophy is to be incredibly strict about layout and make it easy to see how each element "consumes" width or height from the page in a responsive, declarative manner.

For layouts requiring flex, use the `custom` field:
```haskell
custom .~ "flex flex-col items-center"
```

This keeps layout behavior explicit and separate from the type-safe styling that ClasshSS provides.

## License

BSD-style
