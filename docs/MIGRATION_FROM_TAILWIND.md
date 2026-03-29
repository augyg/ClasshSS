# Migrating from Tailwind CSS to ClasshSS

If you're already familiar with Tailwind CSS, this guide will help you quickly translate your knowledge to ClasshSS.

## Core Philosophy

ClasshSS follows Tailwind's utility-first approach but adds:
- **Type safety** - Invalid CSS won't compile
- **No runtime errors** - Catch mistakes at compile-time
- **Functional composition** - Leverage Haskell's strengths
- **Template Haskell** - Generate optimized class strings

## Quick Comparison

### HTML/JSX (Tailwind)
```html
<div class="bg-blue-500 p-8 rounded-lg hover:bg-blue-700">
  <h1 class="text-2xl font-bold text-white">Hello</h1>
</div>
```

### Haskell (ClasshSS)
```haskell
elClass "div" $(classh'
  [ bgColor .~^ [("def", noTransition (Blue C500)), ("hover", Blue C700 `withTransition` Duration_300)]
  , p .~~ TWSize 8
  , br .~~ R_Lg
  ]) $ do
    textS $(classh' [text_size .~~ XL2, text_weight .~~ Bold, text_color .~~ White]) "Hello"
```

## Class Name Mapping

### Colors

| Tailwind | ClasshSS |
|----------|----------|
| `bg-blue-500` | `bgColor .~~ Blue C500` |
| `bg-gray-300` | `bgColor .~~ Gray C300` |
| `bg-red-600` | `bgColor .~~ Red C600` |
| `bg-[#281C40]` | `bgColor .~~ hex "281C40"` |
| `text-blue-500` | `text_color .~~ Blue C500` |
| `border-gray-300` | `bc .~~ Gray C300` (shorthand for border color all sides) |

**Pattern:** `ColorFamily CShade`

Available shades: C50, C100, C200, C300, C400, C500, C600, C700, C800, C900, C950

### Spacing (Padding & Margin)

| Tailwind | ClasshSS |
|----------|----------|
| `p-4` | `p .~~ TWSize 4` |
| `pt-4` | `pt .~~ TWSize 4` |
| `pb-4` | `pb .~~ TWSize 4` |
| `px-4` | `px .~~ TWSize 4` |
| `py-4` | `py .~~ TWSize 4` |
| `pl-8` | `pl .~~ TWSize 8` |
| `pr-8` | `pr .~~ TWSize 8` |
| `m-4` | `m .~~ TWSize 4` |
| `mt-4`, `mb-4`, `ml-4`, `mr-4` | `mt/mb/ml/mr .~~ TWSize 4` |
| `mx-auto` | `mx .~~ TWSize_Auto` |
| `p-[20px]` | `p .~~ pix 20` (custom pixel value) |

**Pattern:** Same abbreviations, but use `.~~` operator and `TWSize` constructor

### Sizing

| Tailwind | ClasshSS |
|----------|----------|
| `w-64` | `w .~~ TWSize' (TWSize 64)` |
| `w-full` | `w .~~ TWSize_Full` |
| `w-screen` | `w .~~ TWSize_Screen` |
| `w-auto` | `w .~~ TWSize_Auto` |
| `w-1/2` | `w .~~ TWFraction 1 D2` |
| `w-11/12` | `w .~~ TWFraction 11 D12` |
| `w-[400px]` | `w .~~ TWSize_Custom (pix 400)` |
| `h-64` | `h .~~ TWSize' (TWSize 64)` |
| `max-w-screen` | `maxW .~~ TWSize_Screen` |
| `min-h-screen` | `minH .~~ TWSize_Screen` |

### Borders

| Tailwind | ClasshSS |
|----------|----------|
| `rounded-md` | `br .~~ R_Md` |
| `rounded-lg` | `br .~~ R_Lg` |
| `rounded-full` | `br .~~ R_Full` |
| `rounded-none` | `br .~~ R_None` |
| `rounded-t-lg` | `br_t .~~ R_Lg` (top corners) |
| `border-2` | `bw .~~ B2` (all sides) |
| `border-t-2` | `bw_t .~~ B2` (top only) |
| `border-gray-300` | `bc .~~ Gray C300` (all sides) |
| `border-solid` | `border . bStyle .~~ Solid` |

**Shortcuts:**
- `br` = border radius (all corners)
- `br_t`, `br_b`, `br_l`, `br_r` = individual sides
- `bw` = border width
- `bc` = border color

### Shadows

| Tailwind | ClasshSS |
|----------|----------|
| `shadow-sm` | `shadow .~~ Shadow_Sm` |
| `shadow` | `shadow .~~ Shadow` |
| `shadow-md` | `shadow .~~ Shadow_Md` |
| `shadow-lg` | `shadow .~~ Shadow_Lg` |
| `shadow-xl` | `shadow .~~ Shadow_Xl` |
| `shadow-2xl` | `shadow .~~ Shadow_2Xl` |
| `shadow-none` | `shadow .~~ Shadow_None` |

### Text Styling

| Tailwind | ClasshSS |
|----------|----------|
| `text-xs` | `text_size .~~ XS` |
| `text-sm` | `text_size .~~ SM` |
| `text-base` | `text_size .~~ Base` |
| `text-lg` | `text_size .~~ LG` |
| `text-xl` | `text_size .~~ XL` |
| `text-2xl` | `text_size .~~ XL2` |
| `text-3xl` | `text_size .~~ XL3` |
| `font-bold` | `text_weight .~~ Bold` |
| `font-semibold` | `text_weight .~~ Semibold` |
| `font-normal` | `text_weight .~~ Normal` |
| `italic` | `text_style .~~ Italic` |
| `text-center` | Use `text_align` in TextPosition |

### Transforms

| Tailwind | ClasshSS |
|----------|----------|
| `rotate-45` | `transform . rotate .~~ Rotate_45` |
| `rotate-90` | `transform . rotate .~~ Rotate_90` |
| `scale-100` | `transform . scale .~~ Scale_100` |
| `scale-105` | `transform . scale .~~ Scale_105` |
| `translate-x-4` | `transform . translateX .~~ Translate_TWSize (TWSize 4)` |
| `skew-x-3` | `transform . skewX .~~ Skew_3` |

## Responsive Design

### Tailwind
```html
<div class="text-sm md:text-base lg:text-lg xl:text-xl">
  Responsive text
</div>
```

### ClasshSS
```haskell
$(classh' [ text_size .|~ [SM, Base, LG, XL] ])
```

**Breakpoint mapping:**
```haskell
-- Tailwind            ClasshSS
-- (default)           [0] = mobile/base
-- sm:                 [1] = sm
-- md:                 [2] = md
-- lg:                 [3] = lg
-- xl:                 [4] = xl
-- 2xl:                [5] = 2xl

-- Example: different background at each breakpoint
bgColor .|~ [Gray C100, Gray C200, Gray C300, Gray C400, Gray C500, Gray C600]
--        mobile    sm         md         lg         xl         2xl
```

**Tips:**
- List order: `[mobile, sm, md, lg, xl, 2xl]`
- You don't need to provide all 6 values - fewer values work too
- Mobile-first: earlier values apply until overridden

## Hover & Focus States

### Tailwind
```html
<button class="bg-blue-500 hover:bg-blue-700 focus:ring-2">
  Click me
</button>
```

### ClasshSS
```haskell
$(classh'
  [ bgColor .~^ [ ("def", noTransition (Blue C500))
                , ("hover", Blue C700 `withTransition` Duration_300)
                ]
  , border . ring . ringWidth .~^ [ ("def", noTransition Ring_0)
                                  , ("focus", Ring_2 `withTransition` Duration_200)
                                  ]
  ])
```

**Available states:**
- `"def"` - Default/base state
- `"hover"` - Mouse hover
- `"focus"` - Keyboard/click focus
- `"active"` - Active state

**Important differences:**
- ClasshSS requires explicit `noTransition` for default state
- Transitions are built into the syntax with `withTransition`
- Can combine states with screen sizes (advanced)

## Transitions

### Tailwind
```html
<div class="transition duration-300 ease-in-out hover:scale-105">
  Hover me
</div>
```

### ClasshSS
```haskell
$(classh'
  [ transform . scale .~^ [ ("def", noTransition Scale_100)
                          , ("hover", Scale_105 `withTransition` Duration_300 `withTiming` Ease_InOut)
                          ]
  ])
```

**Transition durations:**
- `Duration_75`, `Duration_100`, `Duration_150`, `Duration_200`, `Duration_300`, `Duration_500`, `Duration_700`, `Duration_1000`

**Timing functions:**
- `Ease_Linear` (linear)
- `Ease_In` (ease-in)
- `Ease_Out` (ease-out)
- `Ease_InOut` (ease-in-out)

**Builder pattern:**
```haskell
value `withTransition` Duration_300                    -- Just duration
value `withTransition` Duration_300 `withTiming` Ease_In    -- Duration + timing
value `withTransition` Duration_300 `withDelay` Delay_100   -- Duration + delay

-- All at once:
value `withTransitionAll` Duration_300 Ease_InOut Delay_0
```

## Flexbox & Grid

### WARNING: Avoid Flexbox Due to Non-Determinism

**ClasshSS intentionally does not support flexbox** - and we **strongly recommend avoiding flexbox entirely** due to its non-deterministic behavior.

**Why avoid flexbox:**
- **Non-deterministic sizing** - Flex items can have unpredictable sizes depending on content
- **Layout instability** - Changes in one item can affect the entire flex container
- **Hard to reason about** - Complex interaction between flex properties makes debugging difficult
- **Browser inconsistencies** - Different browsers may render flex layouts differently

### Tailwind (with flexbox - NOT recommended)
```html
<div class="flex flex-col items-center gap-4">
  Content
</div>
```

### ClasshSS - Do NOT use flexbox
```haskell
-- DO NOT DO THIS - Non-deterministic!
$(classh'
  [ custom .~ "flex flex-col items-center gap-4"  -- AVOID!
  , bgColor .~~ Gray C50
  , p .~~ TWSize 4
  ])
```

**Recommended alternatives:**
- Use **CSS Grid** for 2D layouts (deterministic, explicit positioning)
- Use **fixed positioning** with padding/margin for simple layouts
- Use **absolute positioning** when appropriate

If you absolutely must use flexbox (not recommended), use the `custom` field, but understand the risks.

### Grid Layout (Supported)

ClasshSS supports grid positioning:

```haskell
$(classh'
  [ colStart .~~ 1        -- grid-column-start
  , colSpan .~~ 6         -- grid-column-span
  ])
```

## Common Patterns

### Card Component

**Tailwind:**
```html
<div class="bg-white rounded-lg shadow-lg p-6 border border-gray-200">
  Card content
</div>
```

**ClasshSS:**
```haskell
$(classh'
  [ bgColor .~~ White
  , br .~~ R_Lg
  , shadow .~~ Shadow_Lg
  , p .~~ TWSize 6
  , border . bWidth . allS .~~ B1
  , border . bColor . allS .~~ Gray C200
  ])
```

### Button with Hover

**Tailwind:**
```html
<button class="bg-blue-500 hover:bg-blue-700 text-white font-bold py-2 px-4 rounded">
  Button
</button>
```

**ClasshSS:**
```haskell
buttonClasses = $(classh'
  [ bgColor .~^ [("def", noTransition (Blue C500)), ("hover", Blue C700 `withTransition` Duration_200)]
  , py .~~ TWSize 2
  , px .~~ TWSize 4
  , br .~~ R_Normal
  ])

buttonText = $(classh' [text_color .~~ White, text_weight .~~ Bold])
```

### Container

**Tailwind:**
```html
<div class="container mx-auto px-4 max-w-screen-xl">
  Content
</div>
```

**ClasshSS:**
```haskell
$(classh'
  [ custom .~ "container"  -- Use Tailwind's container class
  , mx .~~ TWSize_Auto
  , px .~~ TWSize 4
  , maxW .~~ TWSize_Screen
  ])
```

### Responsive Grid

**Tailwind:**
```html
<div class="grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4">
  Items
</div>
```

**ClasshSS:**
```haskell
$(classh'
  [ custom .~ "grid grid-cols-1 md:grid-cols-2 lg:grid-cols-3 gap-4"
  ])
```

## Key Differences Summary

| Aspect | Tailwind | ClasshSS |
|--------|----------|----------|
| **Type Safety** | None (runtime strings) | Full (compile-time) |
| **Errors** | Appear in browser | Caught at compile-time |
| **Responsive** | `md:text-lg` | `text_size .|~ [SM, Base, LG]` |
| **Hover** | `hover:bg-blue-700` | `bgColor .~^ [("hover", Blue C700 ...)]` |
| **Transitions** | Manual classes | Built into state changes |
| **Flexbox** | Full support | Use `custom` field |
| **Custom values** | `w-[400px]` | `w .~~ pix 400` |
| **Color shades** | `-100` to `-900` | `C100` to `C900` |

## Advantages of ClasshSS

1. **No typos** - `bg-blue-50` vs `bg-blue-500`? Compiler catches it
2. **No conflicts** - Can't set `pt` and `py` together (compile error)
3. **Better IDE support** - Type-driven autocomplete
4. **Refactoring safe** - Rename, extract, compose with Haskell tools
5. **Per-property transitions** - Each property can have its own transition
6. **Explicit** - No magic, clear what's happening

## Migration Strategy

### Step 1: Start Small
Begin with simple components (buttons, cards) before tackling complex layouts.

### Step 2: Keep Tailwind for Layout
Use `custom` field for complex flexbox/grid layouts initially:
```haskell
custom .~ "flex flex-col md:flex-row gap-4"
```

### Step 3: Learn the Operators
- `.~~` for constants
- `.|~` for responsive
- `.~^` for states
- `.~` for custom

### Step 4: Use the Mapping Table
Keep the [Tailwind Mapping Reference](reference/TAILWIND_MAPPING.md) handy for quick lookups.

### Step 5: Leverage Type Safety
Let the compiler guide you - if something doesn't compile, it's usually a good thing!

## Common Pitfalls

### 1. Forgetting noTransition

**Wrong:**
```haskell
bgColor .~^ [("def", Blue C500), ("hover", Blue C700)]
```

**Right:**
```haskell
bgColor .~^ [("def", noTransition (Blue C500)), ("hover", Blue C700 `withTransition` Duration_300)]
```

### 2. Setting Conflicting Properties

**Wrong:**
```haskell
$(classh' [ pt .~~ TWSize 4, py .~~ TWSize 2 ])  -- COMPILE ERROR
```

`py` sets both `pt` and `pb`, so setting `pt` separately conflicts.

**Right:**
```haskell
$(classh' [ pt .~~ TWSize 4, pb .~~ TWSize 2 ])
```

### 3. Wrong List Length for Responsive

You can provide fewer than 6 values, but be aware of what you're doing:
```haskell
-- This works - remaining breakpoints inherit last value
text_size .|~ [SM, Base, LG]

-- Mobile: SM, sm: Base, md: LG, lg+: LG
```

## Next Steps

- **[Complete Tailwind Mapping](reference/TAILWIND_MAPPING.md)** - Comprehensive class mapping table
- **[Operator Reference](reference/OPERATOR_REFERENCE.md)** - All operators explained
- **[Examples](examples/)** - See ClasshSS in action
- **[Core Concepts](core-concepts/)** - Deep dive into ClasshSS features

Welcome to type-safe styling! 🎨
