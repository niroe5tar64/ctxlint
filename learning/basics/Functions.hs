-- Learning: Haskell Functions
-- 目的: 高階関数、ラムダ式、関数合成を学ぶ

module Basics.Functions where

-- ラムダ式
double :: [Int] -> [Int]
double xs = map (\x -> x * 2) xs

-- 高階関数
applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

-- 関数合成
compose :: (b -> c) -> (a -> b) -> a -> c
compose f g x = f (g x)

-- filter の例
filterPositive :: [Int] -> [Int]
filterPositive xs = filter (> 0) xs

-- fold の例
product' :: [Int] -> Int
product' = foldr (*) 1

-- 練習問題
-- TODO: より複雑な高階関数を実装してみる
