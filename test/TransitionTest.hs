{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}

module Main where

import Classh
import Classh.Box
import Classh.Color
import Classh.Box.Transition
import Classh.WithTransition
import Classh.Setters
import Classh.Class.CompileStyle

-- Test 1: Backwards compatible - no transitions
test1 :: BoxConfig
test1 = def
  & bgColor .~~ (Gray C500)
  & colSpan .~~ 2

-- Test 2: Using new (.~^) operator with builder pattern
test2 :: BoxConfig
test2 = def
  & bgColor .~^ [ ("def", noTransition (Gray C500))
                , ("hover", (Gray C300) `withTransition` Duration_300)
                ]

-- Test 3: Builder pattern with chaining
test3 :: BoxConfig
test3 = def
  & bgColor .~^ [ ("def", noTransition Purple)
                , ("hover", Lavender `withTransition` Duration_300 `withTiming` Ease_InOut)
                , ("focus", Indigo `withTransition` Duration_500 `withTiming` Ease_Out `withDelay` Delay_100)
                ]

-- Test 4: All-at-once style
test4 :: BoxConfig
test4 = def
  & bgColor .~^ [ ("def", noTransition Purple)
                , ("sm", Indigo `withTransitionAll` Duration_300 Ease_InOut Delay_0)
                , ("hover", Lavender `withTransition` Duration_500)
                ]

main :: IO ()
main = do
  putStrLn "Test 1 (backwards compatible):"
  case compileS test1 of
    Left err -> putStrLn $ "Error: " ++ show err
    Right result -> putStrLn $ "  " ++ show result

  putStrLn "\nTest 2 (hover with transition):"
  case compileS test2 of
    Left err -> putStrLn $ "Error: " ++ show err
    Right result -> putStrLn $ "  " ++ show result

  putStrLn "\nTest 3 (builder pattern with chaining):"
  case compileS test3 of
    Left err -> putStrLn $ "Error: " ++ show err
    Right result -> putStrLn $ "  " ++ show result

  putStrLn "\nTest 4 (all-at-once style):"
  case compileS test4 of
    Left err -> putStrLn $ "Error: " ++ show err
    Right result -> putStrLn $ "  " ++ show result
