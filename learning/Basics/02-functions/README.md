# Step 2: 関数型プログラミング

## 学習目標

このステップで以下を習得します：

- [ ] 高階関数（map, filter, fold）
- [ ] ラムダ式（無名関数）
- [ ] 部分適用とカリー化
- [ ] 関数合成

## 対応ファイル

- `../Functions.hs` - サンプルコードと演習問題

## 前提知識

Step 1（基本構文）を完了していること。

## 学習手順

### 1. Functions.hs を読む

```bash
code learning/Basics/Functions.hs
```

### 2. GHCi で動作確認

```bash
cd learning && cabal repl
```

```haskell
ghci> :load Basics.Functions

-- ラムダ式
ghci> double [1,2,3]
[2,4,6]

-- 高階関数
ghci> applyTwice (+3) 10
16

-- 関数合成
ghci> compose (*2) (+1) 5
12

-- filter
ghci> filterPositive [-1, 2, -3, 4]
[2,4]

-- fold
ghci> product' [1,2,3,4]
24
```

## 重要な概念

### 高階関数

関数を引数に取る、または関数を返す関数。

```haskell
-- map: 各要素に関数を適用
ghci> map (+1) [1,2,3]
[2,3,4]

-- filter: 条件を満たす要素だけ抽出
ghci> filter even [1,2,3,4,5]
[2,4]

-- foldr: 右から畳み込み
ghci> foldr (+) 0 [1,2,3]
6
```

### ラムダ式

名前のない関数を `\引数 -> 式` で書く。

```haskell
-- 通常の関数
addOne x = x + 1

-- ラムダ式で同じこと
\x -> x + 1

-- 使用例
ghci> map (\x -> x * 2) [1,2,3]
[2,4,6]

-- 複数引数
ghci> (\x y -> x + y) 3 5
8
```

### 部分適用

Haskellの関数は自動的にカリー化されている。

```haskell
-- add は2引数の関数
add :: Int -> Int -> Int
add x y = x + y

-- 1つだけ適用すると「残りを待つ関数」が返る
ghci> :t add 5
add 5 :: Int -> Int

ghci> let addFive = add 5
ghci> addFive 3
8

-- セクション（演算子の部分適用）
ghci> map (*2) [1,2,3]    -- 2を掛ける関数
[2,4,6]

ghci> map (2*) [1,2,3]    -- 2に掛ける関数（同じ結果）
[2,4,6]

ghci> map (10-) [1,2,3]   -- 10から引く
[9,8,7]

ghci> map (-10) [1,2,3]   -- エラー！（負の10と解釈される）
ghci> map (subtract 10) [1,2,3]  -- 正しい書き方
[-9,-8,-7]
```

### 関数合成

`(.)` 演算子で関数を合成。右から左に適用される。

```haskell
-- f . g は「まず g を適用し、その結果に f を適用」
ghci> :t (.)
(.) :: (b -> c) -> (a -> b) -> a -> c

-- 例: 2倍してから1を足す
ghci> let f = (+1) . (*2)
ghci> f 5
11  -- 5*2 = 10, 10+1 = 11

-- パイプライン的に読むなら & を使う（Data.Function）
ghci> import Data.Function
ghci> 5 & (*2) & (+1)
11
```

### ポイントフリースタイル

引数を明示的に書かないスタイル。

```haskell
-- 引数を明示
sumList xs = foldr (+) 0 xs

-- ポイントフリー（引数を省略）
sumList = foldr (+) 0

-- 関数合成でポイントフリー
doubleAndSum xs = sum (map (*2) xs)
doubleAndSum = sum . map (*2)  -- ポイントフリー
```

## 演習問題

`Functions.hs` の末尾に以下を実装してみましょう：

### 演習1: map を使わずに map を実装

```haskell
myMap :: (a -> b) -> [a] -> [b]
myMap = undefined  -- TODO: 実装する
```

### 演習2: filter を使わずに filter を実装

```haskell
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter = undefined  -- TODO: 実装する
```

### 演習3: foldr を使って sum を実装

```haskell
mySum :: [Int] -> Int
mySum = undefined  -- TODO: foldr を使って実装
```

### 演習4: 関数合成で実装

```haskell
-- 文字列のリストを受け取り、各文字列の長さを2倍したリストを返す
-- 例: ["ab", "cde"] -> [4, 6]
doubleLengths :: [String] -> [Int]
doubleLengths = undefined  -- TODO: map と (.) を使って実装
```

## 解答例

<details>
<summary>演習1の解答</summary>

```haskell
myMap :: (a -> b) -> [a] -> [b]
myMap _ []     = []
myMap f (x:xs) = f x : myMap f xs
```

</details>

<details>
<summary>演習2の解答</summary>

```haskell
myFilter :: (a -> Bool) -> [a] -> [a]
myFilter _ []     = []
myFilter p (x:xs)
  | p x       = x : myFilter p xs
  | otherwise = myFilter p xs
```

</details>

<details>
<summary>演習3の解答</summary>

```haskell
mySum :: [Int] -> Int
mySum = foldr (+) 0
```

</details>

<details>
<summary>演習4の解答</summary>

```haskell
doubleLengths :: [String] -> [Int]
doubleLengths = map ((*2) . length)
```

</details>

## 完了チェック

- [ ] `map`, `filter`, `foldr` の動作を理解した
- [ ] ラムダ式を使って関数を書ける
- [ ] 部分適用の仕組みを理解した
- [ ] 関数合成 `(.)` を使える
- [ ] 演習問題を少なくとも2つ解いた

次: `03-typeclasses/README.md` へ進む
