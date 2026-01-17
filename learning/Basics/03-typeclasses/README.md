# Step 3: 型クラス

## 学習目標

このステップで以下を習得します：

- [ ] 型クラスとは何か
- [ ] 標準の型クラス（Eq, Ord, Show, Read）
- [ ] 独自の型クラス定義
- [ ] インスタンスの実装
- [ ] 型制約の書き方

## 対応ファイル

- `../TypeClasses.hs` - サンプルコードと演習問題

## 前提知識

Step 1（基本構文）と Step 2（関数型プログラミング）を完了していること。

## 学習手順

### 1. TypeClasses.hs を読む

```bash
code learning/Basics/TypeClasses.hs
```

### 2. GHCi で動作確認

```bash
cd learning && cabal repl
```

```haskell
ghci> :load Basics.TypeClasses

-- 独自型クラスの使用
ghci> describe (42 :: Int)
"This is the number: 42"

ghci> describe (Text "hello")
"This is the text: hello"

-- IO アクション
ghci> printDescription (42 :: Int)
This is the number: 42
```

## 重要な概念

### 型クラスとは

型クラスは「この型はこういう操作ができる」という約束（インターフェース）。

```haskell
-- Eq型クラス: 等値比較ができる型
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool

-- Show型クラス: 文字列に変換できる型
class Show a where
  show :: a -> String

-- Ord型クラス: 順序比較ができる型
class Eq a => Ord a where  -- Eq が前提条件
  compare :: a -> a -> Ordering
  (<), (<=), (>), (>=) :: a -> a -> Bool
```

### 標準型クラスを調べる

```haskell
-- 型の情報を表示
ghci> :info Int
type Int :: *
data Int = GHC.Types.I# GHC.Prim.Int#
instance Eq Int
instance Ord Int
instance Show Int
instance Read Int
instance Num Int
...

-- 型クラスの定義を表示
ghci> :info Eq
type Eq :: * -> Constraint
class Eq a where
  (==) :: a -> a -> Bool
  (/=) :: a -> a -> Bool
```

### 独自の型クラスを定義

```haskell
-- 型クラスの定義
class Describable a where
  describe :: a -> String

-- Int に対するインスタンス
instance Describable Int where
  describe n = "This is the number: " ++ show n
```

### deriving による自動導出

多くの標準型クラスは自動的に導出できる。

```haskell
-- データ型定義と自動導出
data Color = Red | Green | Blue
  deriving (Eq, Ord, Show, Read)

-- 使用例
ghci> Red == Green
False

ghci> Red < Blue
True

ghci> show Green
"Green"

ghci> read "Blue" :: Color
Blue
```

### 型制約

関数が特定の型クラスを要求する場合、`=>` の左側に制約を書く。

```haskell
-- a は Eq のインスタンスでなければならない
elem :: Eq a => a -> [a] -> Bool

-- a は Show のインスタンスでなければならない
printDescription :: Describable a => a -> IO ()
printDescription x = putStrLn (describe x)

-- 複数の制約
showAndCompare :: (Show a, Ord a) => a -> a -> String
showAndCompare x y = show x ++ " vs " ++ show y ++ ": " ++ result
  where result = case compare x y of
          LT -> "first is smaller"
          EQ -> "equal"
          GT -> "first is larger"
```

### newtype と型クラス

`String` に直接インスタンスを定義できない場合、`newtype` でラップする。

```haskell
-- String は [Char] の別名なので、直接インスタンスを定義すると問題になる
-- newtype でラップして解決
newtype Text = Text String

instance Describable Text where
  describe (Text s) = "This is the text: " ++ s
```

## 演習問題

`TypeClasses.hs` の末尾に以下を実装してみましょう：

### 演習1: Shape 型と Describable インスタンス

```haskell
-- 図形を表すデータ型
data Shape = Circle Double | Rectangle Double Double
  deriving (Show)

-- Describable インスタンスを実装
instance Describable Shape where
  describe = undefined  -- TODO: 実装する
  -- Circle 5.0 -> "Circle with radius 5.0"
  -- Rectangle 3.0 4.0 -> "Rectangle 3.0 x 4.0"
```

### 演習2: Area 型クラス

```haskell
-- 面積を計算できる型クラス
class Area a where
  area :: a -> Double

-- Shape に対して Area インスタンスを実装
instance Area Shape where
  area = undefined  -- TODO: 実装する
  -- Circle r -> pi * r * r
  -- Rectangle w h -> w * h
```

### 演習3: 型制約を使った関数

```haskell
-- Describable かつ Area な型のリストを受け取り
-- 各要素の説明と面積を表示する
describeAreas :: (Describable a, Area a) => [a] -> IO ()
describeAreas = undefined  -- TODO: 実装する
```

## 解答例

<details>
<summary>演習1の解答</summary>

```haskell
data Shape = Circle Double | Rectangle Double Double
  deriving (Show)

instance Describable Shape where
  describe (Circle r) = "Circle with radius " ++ show r
  describe (Rectangle w h) = "Rectangle " ++ show w ++ " x " ++ show h
```

</details>

<details>
<summary>演習2の解答</summary>

```haskell
class Area a where
  area :: a -> Double

instance Area Shape where
  area (Circle r) = pi * r * r
  area (Rectangle w h) = w * h
```

</details>

<details>
<summary>演習3の解答</summary>

```haskell
describeAreas :: (Describable a, Area a) => [a] -> IO ()
describeAreas xs = mapM_ printOne xs
  where
    printOne x = putStrLn $ describe x ++ " (area: " ++ show (area x) ++ ")"
```

</details>

## 完了チェック

- [ ] 型クラスの概念を理解した
- [ ] `deriving` で標準型クラスを導出できる
- [ ] 独自の型クラスを定義できる
- [ ] インスタンスを実装できる
- [ ] 型制約 `=>` を使った関数を書ける
- [ ] 演習問題を少なくとも1つ解いた

## 次のステップ

Basics を完了したら、`learning/practice/` に進みます：

1. **parsing/** - Parsec を使ったパーサー実装
2. **validation/** - Maybe/Either を使ったバリデーション
3. **cli/** - CLI アプリケーション構築

これらは ctxlint 本体の実装に直結するスキルです。
