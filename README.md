# ClasshSS

Type-safe CSS-in-Haskell built on Tailwind CSS. Generates class strings with compile-time validation.

## Overview

ClasshSS is a library that generates Tailwind CSS class strings in Haskell with compile-time type safety. It provides two main configuration types:

- `BoxConfig` - Element styling (layout, colors, borders, shadows, spacing)
- `TextConfigTW` - Text styling (font, size, weight, color)

The library generates `Text` values containing Tailwind classes. These work with any DOM library (Reflex.Dom, Lucid, Blaze, IHP, etc.).

## Installation

Add to your `.cabal` file:
```cabal
build-depends:
    classhss
```

Or with Stack, add to `stack.yaml`:
```yaml
extra-deps:
  - classhss-0.1.0.0
```

## Quick Start

```haskell
{-# LANGUAGE TemplateHaskell #-}

import Classh
import Reflex.Dom.Core

-- Simple styled div
myDiv :: DomBuilder t m => m ()
myDiv = elClass "div" $(classh'
  [ bgColor .~~ Blue C500
  , p .~~ TWSize 4
  , br .~~ R_Md
  ]) $ text "Hello, ClasshSS!"
```

### Four Operators

- `.~~` - Constant value (no responsive, no states)
- `.|~` - Responsive values (mobile-first breakpoints)
- `.~^` - State-based values (hover, focus, active) with transitions
- `.~` - Direct setter (mainly for `custom` field)

## Complete Example

See **[docs/EXAMPLE.md](docs/EXAMPLE.md)** for a comprehensive example showing:
- BoxConfig applied to elements via `classh'`
- TextConfigTW for text styling (via `textS` from reflex-classh)
- TextPosition for text positioning (via `textPosition` from reflex-classh)
- Responsive design with `.|~`
- State-based transitions with `.~^`
- Grid positioning
- Transform composition

## Migrating from Tailwind

If you're familiar with Tailwind CSS, see **[docs/MIGRATION_FROM_TAILWIND.md](docs/MIGRATION_FROM_TAILWIND.md)** for:
- Class name mappings (e.g., `bg-blue-500` → `bgColor .~~ Blue C500`)
- How to translate responsive patterns
- How to translate hover/focus states
- Common migration patterns

## API Documentation

For complete API reference:
- Run `cabal haddock` to generate documentation
- See Haddock comments in source files (especially `src/Classh.hs`)
- The main module Haddock includes the comprehensive example

## Why ClasshSS?

- **Type safety** - Invalid CSS won't compile
- **No conflicts** - Can't accidentally set overlapping properties
- **Responsive by default** - Easy mobile-first design
- **Transitions built-in** - Type-safe hover/focus states
- **Tailwind familiar** - If you know Tailwind, you know ClasshSS

## Important Notes

### Type Separation

You cannot mix `BoxConfig` and `TextConfigTW` in the same `classh'` call. This is enforced by the type system:

```haskell
-- ERROR: Won't compile!
$(classh' [ bgColor .~~ Blue C500, text_color .~~ White ])

-- CORRECT: Separate configs, nested elements
elClass "div" $(classh' [bgColor .~~ Blue C500]) $
  textS $(classhText [text_color .~~ White]) "Text"
```

### Avoid Flexbox

ClasshSS recommends using CSS Grid instead of flexbox due to flexbox's non-deterministic sizing behavior. Use the `custom` field only as a last resort.

### The `custom` Field

The `custom` field bypasses type safety and can override type-safe properties. Only use it when absolutely necessary, and place it **first** in your config list so type-safe properties take precedence.

## License

BSD-style
