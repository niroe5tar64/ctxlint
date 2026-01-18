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

-- 演習問題

-- 演習1: リストの長さを返す（Preludeのlengthを使わずに）
myLength :: [a] -> Int
myLength [] = 0
myLength (_:xs) = 1 + myLength xs

-- 演習2: リストを逆順にする（Preludeのreverseを使わずに）
myReverse :: [a] -> [a]
myReverse [] = []
myReverse (x:xs) = myReverse xs ++ [x]

-- 演習3: n番目の要素を取得
-- 0-indexed で n 番目の要素を返す
myNth :: [a] -> Int -> a
myNth [] _ = error "Index out of bounds"
myNth (x:_) 0 = x
myNth (_:xs) n = myNth xs (n - 1)
