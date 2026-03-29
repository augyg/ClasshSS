{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Classh
import Classh.Class.CompileStyle
import Control.Lens ((&))
import Data.Default (def)
import qualified Data.Text as T

-- Test 1: Solid color renders correctly (backwards compat)
testSolid :: BoxConfig
testSolid = def
  & bgColor .~~ solidColor (Blue C500)

-- Test 2: Simple two-color gradient
testLinearGradient :: BoxConfig
testLinearGradient = def
  & bgColor .~~ linearGradient To_R (hex "4E366C") White

-- Test 3: Three-color gradient with via
testLinearGradientVia :: BoxConfig
testLinearGradientVia = def
  & bgColor .~~ linearGradientVia To_BR (Purple C500) (Pink C500) White

-- Test 4: Gradient with stop positions
testGradientPositions :: BoxConfig
testGradientPositions = def
  & bgColor .~~ linearGradientViaPos To_R
      (stopAt (hex "4E366C") 10)
      (stopAt (Pink C500) 30)
      (stopAt White 90)

-- Test 5: Single color gradient (fade to transparent)
testGradientFrom :: BoxConfig
testGradientFrom = def
  & bgColor .~~ gradientFrom To_R (hex "4E366C")

-- Test 6: Two-color gradient with positions
testLinearGradientPos :: BoxConfig
testLinearGradientPos = def
  & bgColor .~~ linearGradientPos To_B
      (stopAt (Blue C500) 0)
      (stopAt (Purple C500) 100)

-- Test 7: Gradient with hover transition
testGradientTransition :: BoxConfig
testGradientTransition = def
  & bgColor .~^ [ ("def", noTransition (solidColor (Blue C500)))
                , ("hover", linearGradient To_R (Purple C500) (Pink C500) `withTransition` Duration_300)
                ]

-- Test 8: Responsive gradients
testResponsiveGradient :: BoxConfig
testResponsiveGradient = def
  & bgColor .|~ [ solidColor (Gray C500)
                , linearGradient To_R (Blue C500) (Purple C500)
                ]

-- Test 9: All directions
testDirectionT :: BoxConfig
testDirectionT = def & bgColor .~~ linearGradient To_T (Blue C500) White

testDirectionTR :: BoxConfig
testDirectionTR = def & bgColor .~~ linearGradient To_TR (Blue C500) White

testDirectionBL :: BoxConfig
testDirectionBL = def & bgColor .~~ linearGradient To_BL (Blue C500) White

testDirectionL :: BoxConfig
testDirectionL = def & bgColor .~~ linearGradient To_L (Blue C500) White

testDirectionTL :: BoxConfig
testDirectionTL = def & bgColor .~~ linearGradient To_TL (Blue C500) White

-- Test 10: Solid color with opacity
testSolidColorOpacity :: BoxConfig
testSolidColorOpacity = def
  & bgColor .~~ solidColorOpacity (hex "221326") 87

-- Test 11: Gradient stop with opacity
testStopWithOpacity :: BoxConfig
testStopWithOpacity = def
  & bgColor .~~ linearGradientPos To_BR
      (stopAtWithOpacity (hex "181422") 90 0)
      (stopAt (hex "281C40") 100)

-- Test 12: Named color with opacity
testNamedColorOpacity :: BoxConfig
testNamedColorOpacity = def
  & bgColor .~~ solidColorOpacity (Blue C500) 50

-- Test 13: color helper (no opacity suffix)
testColorHelper :: BoxConfig
testColorHelper = def
  & bgColor .~~ solidColor White

testCase :: String -> BoxConfig -> T.Text -> IO Bool
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
  putStrLn "                       Gradient Feature Test Suite"
  putStrLn $ replicate 80 '='

  results <- sequenceA
    [ testCase "Solid color (backwards compat)"
        testSolid
        "bg-blue-500"

    , testCase "Two-color gradient"
        testLinearGradient
        "bg-gradient-to-r from-[#4E366C] to-white"

    , testCase "Three-color gradient with via"
        testLinearGradientVia
        "bg-gradient-to-br from-purple-500 via-pink-500 to-white"

    , testCase "Gradient with stop positions"
        testGradientPositions
        "bg-gradient-to-r from-[#4E366C] from-10% via-pink-500 via-30% to-white to-90%"

    , testCase "Single color gradient (fade to transparent)"
        testGradientFrom
        "bg-gradient-to-r from-[#4E366C]"

    , testCase "Two-color gradient with positions"
        testLinearGradientPos
        "bg-gradient-to-b from-blue-500 from-0% to-purple-500 to-100%"

    , testCase "Gradient with hover transition"
        testGradientTransition
        "bg-blue-500 hover:bg-gradient-to-r hover:from-purple-500 hover:to-pink-500 hover:[transition:background-color,border-color,color,fill,stroke_300ms_linear_0ms]"

    , testCase "Responsive gradients"
        testResponsiveGradient
        "bg-gray-500 sm:bg-gradient-to-r sm:from-blue-500 sm:to-purple-500"

    , testCase "Direction: to-t"
        testDirectionT
        "bg-gradient-to-t from-blue-500 to-white"

    , testCase "Direction: to-tr"
        testDirectionTR
        "bg-gradient-to-tr from-blue-500 to-white"

    , testCase "Direction: to-bl"
        testDirectionBL
        "bg-gradient-to-bl from-blue-500 to-white"

    , testCase "Direction: to-l"
        testDirectionL
        "bg-gradient-to-l from-blue-500 to-white"

    , testCase "Direction: to-tl"
        testDirectionTL
        "bg-gradient-to-tl from-blue-500 to-white"

    , testCase "Solid color with opacity (hex)"
        testSolidColorOpacity
        "bg-[#221326]/87"

    , testCase "Gradient stop with opacity"
        testStopWithOpacity
        "bg-gradient-to-br from-[#181422]/90 from-0% to-[#281C40] to-100%"

    , testCase "Named color with opacity"
        testNamedColorOpacity
        "bg-blue-500/50"

    , testCase "color helper (no opacity suffix)"
        testColorHelper
        "bg-white"
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
