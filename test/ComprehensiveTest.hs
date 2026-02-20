{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Classh
import Classh.Box
import Classh.Box.Padding
import Classh.Box.Margin
import Classh.Box.Border
import Classh.Box.Shadow
import Classh.Box.Placement
import Classh.Box.TWSize
import Classh.Box.SizingBand
import Classh.Color
import Classh.Box.Transition
import Classh.WithTransition
import Classh.Setters
import Classh.Class.CompileStyle
import Classh.Class.SetSides
import Classh.Text
import Control.Lens ((&))
import Data.Default (def)
import qualified Data.Text as T

-- Box property tests
testBoxBasics :: BoxConfig
testBoxBasics = def
  & colStart .~~ 1
  & colSpan .~~ 6
  & bgColor .~~ Blue C500
  & bgOpacity .~~ 80

-- Responsive properties with .|~
testResponsive :: BoxConfig
testResponsive = def
  & bgColor .|~ [Gray C100, Gray C200, Gray C300, Gray C400, Gray C500, Gray C600]

-- Padding with SetSides shorthand
testPaddingSetSides :: BoxConfig
testPaddingSetSides = def
  & padding . b .~~ TWSize 4
  & padding . t .~~ TWSize 8
  & padding . x .~~ TWSize 2

-- Padding with transitions
testPaddingTransitions :: BoxConfig
testPaddingTransitions = def
  & padding . paddingB .~^ [ ("def", noTransition (TWSize 4))
                            , ("hover", TWSize 8 `withTransition` Duration_300)
                            ]

-- Margin tests
testMargin :: BoxConfig
testMargin = def
  & margin . marginL .~~ TWSize 2
  & margin . marginR .~~ TWSize 2
  & margin . y .~~ TWSize 4

-- Border tests
testBorder :: BoxConfig
testBorder = def
  & border . bWidth . b .~~ B2
  & border . bColor . allS .~~ Red C500
  & border . radius . borderRadius_tr .~~ R_Lg

-- Border with transitions
testBorderTransitions :: BoxConfig
testBorderTransitions = def
  & border . bColor . allS .~^ [ ("def", noTransition (Blue C500))
                                , ("hover", Red C500 `withTransition` Duration_200)
                                ]

-- Shadow tests
testShadow :: BoxConfig
testShadow = def
  & shadow .~~ Shadow_Lg

-- Shadow with transitions
testShadowTransitions :: BoxConfig
testShadowTransitions = def
  & shadow .~^ [ ("def", noTransition Shadow_Sm)
                , ("hover", Shadow_Xl `withTransition` Duration_300)
                ]

-- Position tests
testPosition :: BoxConfig
testPosition = def
  & position .~~ (J_Center, A_Center)

-- Sizing tests - TODO: Fix lens composition issue
-- testSizing :: BoxConfig
-- testSizing = def
--   & (sizingBand . w) .~~ TWSize 64
--   & (sizingBand . h) .~~ TWSize 32
--   & (sizingBand . maxW) .~~ TWSize_Screen

-- Combined complex test
testComplex :: BoxConfig
testComplex = def
  & bgColor .~^ [ ("def", noTransition (Blue C600))
                , ("hover", Blue C400 `withTransition` Duration_300 `withTiming` Ease_InOut)
                ]
  & padding . x .~~ TWSize 4
  & padding . y .~~ TWSize 2
  & border . radius . allS .~~ R_Md
  & shadow .~^ [ ("def", noTransition Shadow_None)
                , ("hover", Shadow_Lg `withTransition` Duration_200)
                ]

-- Text tests - TODO: Add Show instance for TextConfigTW
-- testText :: TextConfigTW
-- testText = def
--   & text_size .~~ XL
--   & text_weight .~~ Bold
--   & text_color .~~ White

testCase :: (CompileStyle a, Show a) => String -> a -> T.Text -> IO Bool
testCase name cfg expected = do
  putStrLn $ "\n" ++ replicate 80 '-'
  putStrLn $ "TEST: " ++ name
  putStrLn $ replicate 80 '-'
  case compileS cfg of
    Left err -> do
      putStrLn $ "❌ ERROR: " ++ show err
      return False
    Right result -> do
      let success = result == expected
      putStrLn $ "Output:"
      putStrLn $ "  " ++ show result
      putStrLn ""
      if success
        then putStrLn "✓ PASS"
        else do
          putStrLn "✗ FAIL"
          putStrLn $ "\nExpected:"
          putStrLn $ "  " ++ show expected
      return success

main :: IO ()
main = do
  putStrLn ""
  putStrLn $ replicate 80 '='
  putStrLn "                    ClasshSS Comprehensive Test Suite"
  putStrLn $ replicate 80 '='

  results <- sequenceA
    [ testCase "Box basics (colStart, colSpan, bgColor, bgOpacity)"
        testBoxBasics
        "col-start-1 col-span-6 bg-blue-500 bg-opacity-80"

    , testCase "Responsive bgColor (.|~)"
        testResponsive
        "bg-gray-100 sm:bg-gray-200 md:bg-gray-300 lg:bg-gray-400 xl:bg-gray-500 2xl:bg-gray-600"

    , testCase "Padding with SetSides shorthand (b, t, x)"
        testPaddingSetSides
        "pl-2 pr-2 pt-8 pb-4"

    , testCase "Padding with transitions"
        testPaddingTransitions
        "pb-4 hover:pb-8 hover:[transition:all_300ms_linear_0ms]"

    , testCase "Margin (marginL, marginR, marginY)"
        testMargin
        "ml-2 mr-2 mt-4 mb-4"

    , testCase "Border (width, color, radius)"
        testBorder
        "rounded-tr-lg border-b-2 border-l-red-500 border-r-red-500 border-t-red-500 border-b-red-500"

    , testCase "Border color with transitions"
        testBorderTransitions
        "border-l-blue-500 hover:border-l-red-500 hover:[transition:background-color,border-color,color,fill,stroke_200ms_linear_0ms] border-r-blue-500 hover:border-r-red-500 hover:[transition:background-color,border-color,color,fill,stroke_200ms_linear_0ms] border-t-blue-500 hover:border-t-red-500 hover:[transition:background-color,border-color,color,fill,stroke_200ms_linear_0ms] border-b-blue-500 hover:border-b-red-500 hover:[transition:background-color,border-color,color,fill,stroke_200ms_linear_0ms]"

    , testCase "Shadow"
        testShadow
        "shadow-lg"

    , testCase "Shadow with transitions"
        testShadowTransitions
        "shadow-sm hover:shadow-xl hover:[transition:box-shadow_300ms_linear_0ms]"

    , testCase "Position (center/center)"
        testPosition
        "grid justify-items-center content-center"

    -- , testCase "Sizing (w, h, maxW)"
    --     testSizing
    --     "w-64 h-32 max-w-screen"

    , testCase "Complex combined properties"
        testComplex
        "rounded-tr-md rounded-tl-md rounded-br-md rounded-bl-md pl-4 pr-4 pt-2 pb-2 bg-blue-600 hover:bg-blue-400 hover:[transition:background-color,border-color,color,fill,stroke_300ms_in-out_0ms] shadow-none hover:shadow-lg hover:[transition:box-shadow_200ms_linear_0ms]"

    -- , testCase "Text (size, weight, color)"
    --     testText
    --     "text-xl font-bold text-white"
    ]

  putStrLn ""
  putStrLn $ replicate 80 '='
  putStrLn "                              SUMMARY"
  putStrLn $ replicate 80 '='
  let passed = length $ filter id results
      total = length results
  putStrLn $ "Tests passed: " ++ show passed ++ "/" ++ show total
  putStrLn ""

  if and results
    then do
      putStrLn "✓ ALL TESTS PASSED!"
      putStrLn $ replicate 80 '='
    else do
      putStrLn "✗ SOME TESTS FAILED"
      putStrLn $ replicate 80 '='
