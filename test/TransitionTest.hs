{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Classh
import Classh.Class.CompileStyle
import Control.Lens ((&))
import Data.Default (def)
import qualified Data.Text as T

-- Test 1: Backwards compatible - no transitions
test1 :: BoxConfig
test1 = def
  & bgColor .~~ solidColor (Gray C500)
  & colSpan .~~ 2

-- Test 2: Using new (.~^) operator with builder pattern
test2 :: BoxConfig
test2 = def
  & bgColor .~^ [ ("def", noTransition (solidColor (Gray C500)))
                , ("hover", solidColor (Gray C300) `withTransition` Duration_300)
                ]

-- Test 3: Builder pattern with chaining
test3 :: BoxConfig
test3 = def
  & bgColor .~^ [ ("def", noTransition (solidColor (Purple C600)))
                , ("hover", solidColor (Purple C300) `withTransition` Duration_300 `withTiming` Ease_InOut)
                , ("focus", solidColor (Indigo C500) `withTransition` Duration_500 `withTiming` Ease_Out `withDelay` Delay_100)
                ]

-- Test 4: All-at-once style
test4 :: BoxConfig
test4 = def
  & bgColor .~^ [ ("def", noTransition (solidColor (Purple C600)))
                , ("sm", withTransitionAll (solidColor (Indigo C500)) Duration_300 Ease_InOut Delay_0)
                , ("hover", solidColor (Purple C300) `withTransition` Duration_500)
                ]

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
  putStrLn "                       Transition Feature Test Suite"
  putStrLn $ replicate 80 '='

  results <- sequenceA
    [ testCase "Test 1 (backwards compatible)"
        test1
        "col-span-2 bg-gray-500"
    , testCase "Test 2 (hover with transition)"
        test2
        "bg-gray-500 hover:bg-gray-300 hover:[transition:background-color,border-color,color,fill,stroke_300ms_linear_0ms]"
    , testCase "Test 3 (builder pattern with chaining)"
        test3
        "bg-purple-600 hover:bg-purple-300 hover:[transition:background-color,border-color,color,fill,stroke_300ms_in-out_0ms] focus:bg-indigo-500 focus:[transition:background-color,border-color,color,fill,stroke_500ms_out_100ms]"
    , testCase "Test 4 (all-at-once style)"
        test4
        "bg-purple-600 sm:bg-indigo-500 sm:[transition:background-color,border-color,color,fill,stroke_300ms_in-out_0ms] hover:bg-purple-300 hover:[transition:background-color,border-color,color,fill,stroke_500ms_linear_0ms]"
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
