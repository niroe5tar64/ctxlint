-- Learning: Type Classes
-- 目的: 型クラスの基本を理解する

module Basics.TypeClasses where

-- 独自の型クラスを定義
class Describable a where
  describe :: a -> String

-- Int インスタンス
instance Describable Int where
  describe n = "This is the number: " ++ show n

-- Text型を定義（String型シノニムの問題を回避するため）
newtype Text = Text String

-- Text インスタンス
instance Describable Text where
  describe (Text s) = "This is the text: " ++ s

-- 型クラスを使う関数
printDescription :: Describable a => a -> IO ()
printDescription x = putStrLn (describe x)

-- 練習問題
-- TODO: 他の型クラスインスタンスを実装してみる
