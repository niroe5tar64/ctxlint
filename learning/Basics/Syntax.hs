-- Learning: Basic Haskell Syntax
-- 目的: 変数、型、基本的な式を学ぶ

module Basics.Syntax where

-- 型宣言付き定数
myNumber :: Int
myNumber = 5

-- 型シグネチャ付き関数
add :: Int -> Int -> Int
add a b = a + b

-- パターンマッチを使った関数
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- リストの操作
sumList :: [Int] -> Int
sumList [] = 0
sumList (x:xs) = x + sumList xs

-- 練習問題
-- TODO: 他の関数を実装してみる
