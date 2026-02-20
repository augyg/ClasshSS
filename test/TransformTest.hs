{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Classh
import Classh.Class.CompileStyle
import Control.Lens ((&))
import Data.Default (def)
import qualified Data.Text as T

-- Test 1: Basic rotation
test1 :: BoxConfig
test1 = def
  & transform . rotate .~^ [("def", noTransition Rotate_0), ("hover", noTransition Rotate_180)]

-- Test 2: Scale with transition
test2 :: BoxConfig
test2 = def
  & transform . scale .~^ [ ("def", noTransition Scale_100)
                          , ("hover", Scale_105 `withTransition` Duration_300)
                          ]

-- Test 3: Translate X with TWSize
test3 :: BoxConfig
test3 = def
  & transform . translateX .~^ [("def", noTransition (Translate_TWSize (TWSize 4)))]

-- Test 4: Translate Y with fraction
test4 :: BoxConfig
test4 = def
  & transform . translateY .~^ [("def", noTransition (Translate_Fraction 1 D2))]

-- Test 5: Translate with custom CSS size
test5 :: BoxConfig
test5 = def
  & transform . translateX .~^ [("def", noTransition (Translate_Custom (Rem 1.5)))]

-- Test 6: Skew transform
test6 :: BoxConfig
test6 = def
  & transform . skewX .~^ [("def", noTransition Skew_0), ("hover", noTransition Skew_6)]

-- Test 7: Transform origin
test7 :: BoxConfig
test7 = def
  & transform . transformOrigin .~~ Origin_TopRight

-- Test 8: Combined transforms with transitions
test8 :: BoxConfig
test8 = def
  & transform . rotate .~^ [ ("def", noTransition Rotate_0)
                           , ("hover", Rotate_45 `withTransition` Duration_300)
                           ]
  & transform . scale .~^ [ ("def", noTransition Scale_100)
                          , ("hover", Scale_110 `withTransition` Duration_300)
                          ]

-- Test 9: All rotation values
test9 :: BoxConfig
test9 = def
  & transform . rotate .~^ [("def", noTransition Rotate_90)]

-- Test 10: All scale values
test10 :: BoxConfig
test10 = def
  & transform . scale .~^ [("def", noTransition Scale_95)]

-- Test 11: Translate with 0
test11 :: BoxConfig
test11 = def
  & transform . translateX .~^ [("def", noTransition Translate_0)]

-- Test 12: Translate with px
test12 :: BoxConfig
test12 = def
  & transform . translateY .~^ [("def", noTransition Translate_Px)]

-- Test 13: Translate with full
test13 :: BoxConfig
test13 = def
  & transform . translateX .~^ [("def", noTransition Translate_Full)]

-- Test 14: Custom rotate
test14 :: BoxConfig
test14 = def
  & transform . rotate .~^ [("def", noTransition (Rotate_Custom "17deg"))]

-- Test 15: Custom scale
test15 :: BoxConfig
test15 = def
  & transform . scale .~^ [("def", noTransition (Scale_Custom "102"))]

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
  putStrLn "                       Transform Feature Test Suite"
  putStrLn $ replicate 80 '='

  results <- sequenceA
    [ testCase "Test 1 (basic rotation)"
        test1
        "rotate-0 hover:rotate-180"
    , testCase "Test 2 (scale with transition)"
        test2
        "scale-100 hover:scale-105 hover:[transition:transform_300ms_linear_0ms]"
    , testCase "Test 3 (translate X with TWSize)"
        test3
        "translate-x-4"
    , testCase "Test 4 (translate Y with fraction)"
        test4
        "translate-y-1/2"
    , testCase "Test 5 (translate X with custom CSS size)"
        test5
        "translate-x-[1.5rem]"
    , testCase "Test 6 (skew transform)"
        test6
        "skew-x-0 hover:skew-x-6"
    , testCase "Test 7 (transform origin)"
        test7
        "origin-top-right"
    , testCase "Test 8 (combined transforms with transitions)"
        test8
        "rotate-0 hover:rotate-45 hover:[transition:transform_300ms_linear_0ms] scale-100 hover:scale-110 hover:[transition:transform_300ms_linear_0ms]"
    , testCase "Test 9 (rotate 90)"
        test9
        "rotate-90"
    , testCase "Test 10 (scale 95)"
        test10
        "scale-95"
    , testCase "Test 11 (translate X 0)"
        test11
        "translate-x-0"
    , testCase "Test 12 (translate Y px)"
        test12
        "translate-y-px"
    , testCase "Test 13 (translate X full)"
        test13
        "translate-x-full"
    , testCase "Test 14 (custom rotate)"
        test14
        "rotate-[17deg]"
    , testCase "Test 15 (custom scale)"
        test15
        "scale-[102%]"
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
