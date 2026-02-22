{-# LANGUAGE OverloadedStrings #-}

module Main where

import Classh
import Classh.Box
import Classh.Box.Padding
import Classh.Box.Margin
import Classh.Box.Border
import Classh.Box.Shadow
import Classh.Color
import Classh.Box.Transition
import Classh.WithTransition
import Classh.Setters
import Classh.Class.CompileStyle
import Classh.Class.SetSides
import Control.Lens ((&))
import Data.Default (def)
import qualified Data.Text as T
import Data.Either (fromRight)

-- Test configurations
tests :: [(String, BoxConfig, String)]
tests =
  [ ("Background Color - Hover + Focus + Active", testBgAllStates, "Hover (blue), Focus (green), Click (red)")
  , ("Border Color - Hover + Focus + Active", testBorderAllStates, "Hover (purple border), Focus (yellow border), Click (pink border)")
  , ("Combined Bg + Border - All States", testCombined, "Both background and border should transition")
  , ("Responsive Breakpoints", testResponsive, "Resize window to see color change at breakpoints")
  , ("Shadow Transitions", testShadow, "Hover to see shadow transition")
  , ("Complex Combined", testComplex, "Background, border, shadow, and padding all transition")

  -- Comprehensive transition tests
  , ("Comprehensive Responsive Transitions", testComprehensiveResponsiveTransitions,
     "Resize window through ALL breakpoints (640px, 768px, 1024px, 1280px, 1536px). Each has unique transition timing! Hover for pink, focus for cyan.")
  , ("Stacked Transitions (Responsive + Hover)", testStackedTransitions,
     "Resize AND hover - see how responsive + interactive transitions combine with border color")
  , ("Delay Showcase", testDelayShowcase,
     "Compare delays: Hover (500ms wait) vs Focus (1000ms wait)")
  , ("Speed Comparison", testSpeedComparison,
     "Lightning fast (75ms) vs slow motion (1000ms)")
  ]

testBgAllStates :: BoxConfig
testBgAllStates = def
  & bgColor .~^ [ ("def", noTransition (solidColor (Gray C500)))
                , ("hover", solidColor (Blue C500) `withTransition` Duration_300 `withTiming` Ease_InOut)
                , ("focus", solidColor (Green C500) `withTransition` Duration_300 `withTiming` Ease_InOut)
                ]

testBorderAllStates :: BoxConfig
testBorderAllStates = def
  & border . bWidth . allS .~~ B4
  & border . bColor . allS .~^ [ ("def", noTransition (Gray C400))
                                , ("hover", Purple C500 `withTransition` Duration_300 `withTiming` Ease_InOut)
                                , ("focus", Yellow C500 `withTransition` Duration_300 `withTiming` Ease_InOut)
                                ]

testCombined :: BoxConfig
testCombined = def
  & bgColor .~^ [ ("def", noTransition (solidColor (Blue C600)))
                , ("hover", solidColor (Blue C400) `withTransition` Duration_300 `withTiming` Ease_InOut)
                , ("focus", solidColor (Green C600) `withTransition` Duration_300 `withTiming` Ease_InOut)
                ]
  & border . bWidth . allS .~~ B4
  & border . bColor . allS .~^ [ ("def", noTransition (Blue C800))
                                , ("hover", Blue C600 `withTransition` Duration_300 `withTiming` Ease_InOut)
                                , ("focus", Green C800 `withTransition` Duration_300 `withTiming` Ease_InOut)
                                ]

testResponsive :: BoxConfig
testResponsive = def
  & bgColor .|~ [ solidColor (Gray C800), solidColor (Red C600), solidColor (Orange C600), solidColor (Yellow C600), solidColor (Green C600), solidColor (Blue C600) ]

testShadow :: BoxConfig
testShadow = def
  & shadow .~^ [ ("def", noTransition Shadow_Sm)
                , ("hover", Shadow_Xl `withTransition` Duration_300)
                ]

testComplex :: BoxConfig
testComplex = def
  & bgColor .~^ [ ("def", noTransition (solidColor (Blue C600)))
                , ("hover", solidColor (Blue C400) `withTransition` Duration_300 `withTiming` Ease_InOut)
                ]
  & padding . x .~~ TWSize 4
  & padding . y .~~ TWSize 2
  & border . radius . allS .~~ R_Md
  & shadow .~^ [ ("def", noTransition Shadow_None)
                , ("hover", Shadow_Lg `withTransition` Duration_200)
                ]

-- Comprehensive test: All 6 responsive breakpoints with unique transitions
testComprehensiveResponsiveTransitions :: BoxConfig
testComprehensiveResponsiveTransitions = def
  & bgColor .~^
    [ ("def",   solidColor (Red C600)    `withTransition` Duration_500  `withTiming` Ease_Linear `withDelay` Delay_0)
    , ("sm",    solidColor (Orange C600) `withTransition` Duration_300  `withTiming` Ease_In     `withDelay` Delay_100)
    , ("md",    solidColor (Yellow C600) `withTransition` Duration_700  `withTiming` Ease_Out    `withDelay` Delay_150)
    , ("lg",    solidColor (Green C600)  `withTransition` Duration_200  `withTiming` Ease_InOut  `withDelay` Delay_0)
    , ("xl",    solidColor (Blue C600)   `withTransition` Duration_1000 `withTiming` Ease_Linear `withDelay` Delay_300)
    , ("2xl",   solidColor (Purple C600) `withTransition` Duration_500  `withTiming` Ease_InOut  `withDelay` Delay_75)
    , ("hover", solidColor (Pink C400)   `withTransition` Duration_150  `withTiming` Ease_Out    `withDelay` Delay_0)
    , ("focus", solidColor (Cyan C400)   `withTransition` Duration_300  `withTiming` Ease_InOut  `withDelay` Delay_200)
    ]

testStackedTransitions :: BoxConfig
testStackedTransitions = def
  & bgColor .~^
    [ ("def",   solidColor (Gray C800)   `withTransition` Duration_300 `withTiming` Ease_Linear)
    , ("sm",    solidColor (Gray C700)   `withTransition` Duration_300 `withTiming` Ease_In)
    , ("hover", solidColor (Green C500)  `withTransition` Duration_200 `withTiming` Ease_Out)
    ]
  & border . bWidth . allS .~~ B2
  & border . bColor . allS .~^
    [ ("def",   Gray C600   `withTransition` Duration_300)
    , ("hover", Green C400  `withTransition` Duration_200)
    ]

testDelayShowcase :: BoxConfig
testDelayShowcase = def
  & bgColor .~^
    [ ("def",   solidColor (Blue C600) `withTransition` Duration_300 `withTiming` Ease_InOut `withDelay` Delay_0)
    , ("hover", solidColor (Blue C400) `withTransition` Duration_300 `withTiming` Ease_InOut `withDelay` Delay_500)
    , ("focus", solidColor (Blue C200) `withTransition` Duration_300 `withTiming` Ease_InOut `withDelay` Delay_1000)
    ]

testSpeedComparison :: BoxConfig
testSpeedComparison = def
  & bgColor .~^
    [ ("def",   solidColor (Purple C600) `withTransition` Duration_75   `withTiming` Ease_Linear)
    , ("hover", solidColor (Purple C400) `withTransition` Duration_1000 `withTiming` Ease_Linear)
    ]

generateHTML :: IO ()
generateHTML = do
  let htmlHeader = T.unlines
        [ "<!DOCTYPE html>"
        , "<html lang=\"en\">"
        , "<head>"
        , "    <meta charset=\"UTF-8\">"
        , "    <meta name=\"viewport\" content=\"width=device-width, initial-scale=1.0\">"
        , "    <title>ClasshSS Transition Tests</title>"
        , "    <script src=\"https://cdn.tailwindcss.com\"></script>"
        , "    <style>"
        , "        .test-section { margin-bottom: 2rem; padding: 1.5rem; border: 2px solid #e5e7eb; border-radius: 0.5rem; }"
        , "        .code-block { background: #1e1e1e; color: #d4d4d4; padding: 0.75rem; border-radius: 0.375rem; font-family: monospace; font-size: 0.75rem; overflow-x: auto; margin-bottom: 0.75rem; }"
        , "        .screen-indicator { position: fixed; top: 1rem; right: 1rem; background: black; color: white; padding: 0.5rem 1rem; border-radius: 0.375rem; font-weight: bold; z-index: 1000; }"
        , "    </style>"
        , "</head>"
        , "<body class=\"bg-gray-50 p-8\">"
        , "    <div class=\"screen-indicator\">"
        , "        <span class=\"sm:hidden\">Base (&lt;640px)</span>"
        , "        <span class=\"hidden sm:inline md:hidden\">SM (≥640px)</span>"
        , "        <span class=\"hidden md:inline lg:hidden\">MD (≥768px)</span>"
        , "        <span class=\"hidden lg:inline xl:hidden\">LG (≥1024px)</span>"
        , "        <span class=\"hidden xl:inline 2xl:hidden\">XL (≥1280px)</span>"
        , "        <span class=\"hidden 2xl:inline\">2XL (≥1536px)</span>"
        , "    </div>"
        , "    <div class=\"max-w-6xl mx-auto\">"
        , "        <h1 class=\"text-4xl font-bold mb-8 text-gray-900\">ClasshSS Generated Transition Tests</h1>"
        ]

  let htmlFooter = T.unlines
        [ "        <div class=\"mt-8 p-6 bg-blue-50 border-l-4 border-blue-500 rounded-lg\">"
        , "            <h3 class=\"text-xl font-semibold mb-3 text-blue-900\">Instructions:</h3>"
        , "            <ul class=\"list-disc list-inside space-y-2 text-blue-800\">"
        , "                <li>Hover over each box to test transitions</li>"
        , "                <li>Tab or click to focus boxes (green state)</li>"
        , "                <li>Resize window to test responsive breakpoints</li>"
        , "                <li>Check if transitions are smooth or instant</li>"
        , "            </ul>"
        , "        </div>"
        , "    </div>"
        , "</body>"
        , "</html>"
        ]

  let testSections = map generateTestSection tests
  let fullHTML = htmlHeader <> T.concat testSections <> htmlFooter

  writeFile "test-output.html" (T.unpack fullHTML)
  putStrLn "Generated test-output.html"

generateTestSection :: (String, BoxConfig, String) -> T.Text
generateTestSection (name, cfg, description) =
  let classes = fromRight "ERROR" (compileS cfg)
  in T.unlines
    [ "        <div class=\"test-section\">"
    , "            <h2 class=\"text-2xl font-semibold mb-3\">" <> T.pack name <> "</h2>"
    , "            <div class=\"code-block\">" <> classes <> "</div>"
    , "            <div class=\"" <> classes <> " p-6 text-white text-center rounded-lg cursor-pointer\">"
    , "                " <> T.pack description
    , "            </div>"
    , "        </div>"
    ]

main :: IO ()
main = do
  generateHTML
  putStrLn "✓ Test complete! Open test-output.html in your browser to view."
