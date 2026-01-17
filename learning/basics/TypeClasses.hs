-- Learning: Type Classes
-- 目的: 型クラスの基本を理解する

module Basics.TypeClasses where

-- 独自の型クラスを定義
class Describable a where
  describe :: a -> String

-- Int インスタンス
instance Describable Int where
  describe n = "This is the number: " ++ show n

-- String インスタンス
instance Describable String where
  describe s = "This is the text: " ++ s

-- 型クラスを使う関数
printDescription :: Describable a => a -> IO ()
printDescription x = putStrLn (describe x)

-- 練習問題
-- TODO: 他の型クラスインスタンスを実装してみる
