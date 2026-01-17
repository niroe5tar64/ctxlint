-- Learning: Main executable for sandbox
-- 目的: 学習したモジュールをテストする

module Main where

import Basics.Syntax
import Basics.Functions
import Basics.TypeClasses

main :: IO ()
main = do
  putStrLn "=== Learning Sandbox ==="

  -- Test basic syntax
  putStrLn "\n[Syntax]"
  print $ add 3 4
  print $ factorial 5
  print $ sumList [1, 2, 3, 4, 5]

  -- Test functions
  putStrLn "\n[Functions]"
  print $ double [1, 2, 3]
  print $ applyTwice (+10) (5 :: Integer)
  print $ filterPositive [-2, -1, 0, 1, 2]
  print $ product' [1, 2, 3, 4, 5]

  -- Test type classes
  putStrLn "\n[Type Classes]"
  printDescription (42 :: Int)
  printDescription (Text "Hello, Haskell!")
