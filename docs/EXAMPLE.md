# ClasshSS Comprehensive Example

This is **the** example showing all ClasshSS concepts in one place.

## Complete Working Example

```haskell
{-# LANGUAGE TemplateHaskell #-}

module Example where

import Classh
import Reflex.Dom.Core
import Reflex.Classh (textS, textPosition)  -- Note: from reflex-classh package

-- Complete example: styled card with positioned text
exampleCard :: (DomBuilder t m, PostBuild t m) => m ()
exampleCard =
  -- BoxConfig: All element-level styling
  elClass "div" $(classh'
    [ -- Colors
      bgColor .~~ White
    , border . bColor . all .~~ Gray C200

    , -- Spacing
      p .~~ TWSize 6
    , m .~~ TWSize 4

    , -- Shape
      br .~~ R_Lg
    , border . bWidth . all .~~ B1

    , -- Shadow & hover effect
      shadow .~^ [ ("def", noTransition Shadow_Sm)
                 , ("hover", Shadow_Lg `withTransition` Duration_300)
                 ]

    , -- Transform on hover
      transform . scale .~^ [ ("def", noTransition Scale_100)
                            , ("hover", Scale_105 `withTransition` Duration_200)
                            ]

    , -- Grid positioning (NOT flex - avoid flexbox)
      colStart .~~ 2
    , colSpan .~~ 4

    , -- Cursor
      cursor .~~ CursorPointer
    ]) $ do
      -- TextConfigTW: Text styling via textS (from reflex-classh)
      textS $(classhText
        [ text_color .~~ Gray C900
        , text_size .~~ TextXl
        , text_weight .~~ FontBold
        ]) "Card Title"

      -- TextPosition: Position text (from reflex-classh)
      el "p" $ textPosition $(classhTextPos
        [ textAlign .~~ TextCenter
        , textTransform .~~ Uppercase
        ]) $ text "Centered uppercase text"

      -- More content with separate styling
      textS $(classhText
        [ text_color .~~ Gray C600
        , text_size .~~ TextSm
        ]) "Card description text"
```

## What This Example Shows

### 1. BoxConfig (Element-Level Styling)

Applied to the `<div>` via `classh'`:

- **Colors**: `bgColor`, `border . bColor`
- **Spacing**: `p` (padding), `m` (margin)
- **Shape**: `br` (border radius), `border . bWidth`
- **Shadows**: With state transitions using `.~^`
- **Transforms**: Scale on hover with transitions
- **Grid**: `colStart`, `colSpan` for grid positioning
- **Cursor**: Mouse cursor style

### 2. TextConfigTW (Text-Level Styling)

Applied via `textS` from **reflex-classh** package:

- **Color**: `text_color`
- **Size**: `text_size`
- **Weight**: `text_weight`

**Critical:** Cannot mix BoxConfig and TextConfigTW in the same `classh'` call!

### 3. TextPosition (Text Positioning)

Applied via `textPosition` from **reflex-classh** package:

- **Alignment**: `textAlign`
- **Transform**: `textTransform` (uppercase, lowercase, etc.)

### 4. The Four Operators

```haskell
.~~   -- Constant value (no responsive, no states)
.|~   -- Responsive values (mobile-first breakpoints)
.~^   -- State-based values (hover, focus, active)
.~    -- Direct setter (rarely used)
```

### 5. Type Separation

**This will NOT compile:**
```haskell
-- ERROR: Mixing BoxConfig and TextConfigTW!
$(classh'
  [ bgColor .~~ Blue C500      -- BoxConfig
  , text_color .~~ White       -- TextConfigTW - ERROR!
  ])
```

**Correct:**
```haskell
-- Separate configs, nested elements
elClass "div" $(classh' [bgColor .~~ Blue C500]) $
  textS $(classhText [text_color .~~ White]) "Text"
```

### 6. Responsive Design

```haskell
p .|~ [ ("mobile", TWSize 4)   -- 0px+
      , ("md", TWSize 6)        -- 768px+
      , ("lg", TWSize 8)        -- 1024px+
      ]
```

### 7. Transitions

```haskell
shadow .~^ [ ("def", noTransition Shadow_Sm)
           , ("hover", Shadow_Lg `withTransition` Duration_300)
           ]
```

### 8. Grid (Not Flexbox!)

ClasshSS supports grid positioning but **avoid flexbox** due to non-deterministic behavior:

```haskell
-- GOOD: Grid
colStart .~~ ColStart_2
colSpan .~~ ColSpan_4

-- BAD: Flexbox (non-deterministic)
custom .~ "flex justify-center"  -- AVOID!
```

## Common Patterns

### Simple Button

```haskell
simpleButton :: (DomBuilder t m, PostBuild t m) => Text -> m ()
simpleButton label =
  elClass "button" $(classh'
    [ bgColor .~~ Blue C500
    , px .~~ TWSize 6
    , py .~~ TWSize 3
    , br .~~ R_Md
    ]) $
      textS $(classhText [text_color .~~ White]) label
```

### Hover Effect

```haskell
hoverable :: Text
hoverable = $(classh'
  [ bgColor .~^ [ ("def", noTransition (Blue C500))
                , ("hover", Blue C600 `withTransition` Duration_200)
                ]
  ])
```

### Responsive Spacing

```haskell
responsivePadding :: Text
responsivePadding = $(classh'
  [ p .|~ [ ("mobile", TWSize 4)
          , ("md", TWSize 6)
          , ("lg", TWSize 8)
          ]
  ])
```

## Important Notes

### Functions from reflex-classh

These functions are in the separate **reflex-classh** package:
- `textS` - Apply TextConfigTW to text
- `textPosition` - Apply TextPosition to text

### Functions from ClasshSS

These are from the main **ClasshSS** package:
- `classh'` - Generate class string from config
- `classhText` - Same as `classh'`, semantic alias for TextConfigTW
- `classhTextPos` - Generate class string from TextPosition

### Avoid `custom` Field

The `custom` field bypasses type safety. Only use as last resort:

```haskell
-- DANGEROUS: custom can override type-safe properties!
$(classh'
  [ bgColor .~~ White
  , custom .~ "bg-blue-500"  -- Overrides bgColor! No compile error!
  ])

-- If you must use custom, place it FIRST:
$(classh'
  [ custom .~ "grid grid-cols-3"  -- No type-safe alternative
  , bgColor .~~ White             -- Type-safe, takes precedence
  ])
```

### Avoid Flexbox

Flexbox has non-deterministic sizing behavior. Use CSS Grid instead:

```haskell
-- AVOID
custom .~ "flex items-center justify-between"

-- PREFER
custom .~ "grid place-items-center"
```

## See Also

- **API Documentation**: Run `cabal haddock` or see Haddock in source files
- **Migration Guide**: [MIGRATION_FROM_TAILWIND.md](MIGRATION_FROM_TAILWIND.md)
