# Step 1: Haskell 基本構文

## 学習目標

このステップで以下を習得します：

- [ ] 型宣言と変数定義
- [ ] 関数定義の書き方
- [ ] パターンマッチの使い方
- [ ] リスト操作の基本

## 対応ファイル

- `../Syntax.hs` - サンプルコードと演習問題

## 学習手順

### 1. まず Syntax.hs を読む

```bash
# エディタで開く
code learning/Basics/Syntax.hs
```

コードを読みながら、以下のポイントを確認：

- `::` は「型は〜である」を意味する
- `=` は「定義する」を意味する（代入ではない）
- 関数の引数は括弧なしでスペース区切り

### 2. GHCi で動作確認

```bash
cd learning && cabal repl
```

```haskell
-- モジュールをロード
ghci> :load Basics.Syntax

-- 定数を確認
ghci> myNumber
5

-- 関数を呼び出す
ghci> add 3 5
8

-- パターンマッチの関数
ghci> factorial 5
120

-- リスト操作
ghci> sumList [1,2,3,4,5]
15
```

### 3. 型を調べる

```haskell
-- :type または :t で型を表示
ghci> :t myNumber
myNumber :: Int

ghci> :t add
add :: Int -> Int -> Int

ghci> :t factorial
factorial :: Int -> Int
```

## 重要な概念

### 型宣言

```haskell
-- 型シグネチャ（型の宣言）
myNumber :: Int
-- 値の定義
myNumber = 5
```

### 関数定義

```haskell
-- 型: Int を2つ受け取って Int を返す
add :: Int -> Int -> Int
-- 定義: a と b を受け取って a + b を返す
add a b = a + b
```

### パターンマッチ

```haskell
factorial :: Int -> Int
factorial 0 = 1                    -- 0 の場合は 1
factorial n = n * factorial (n-1)  -- それ以外は再帰
```

### リストのパターンマッチ

```haskell
sumList :: [Int] -> Int
sumList []     = 0           -- 空リストなら 0
sumList (x:xs) = x + sumList xs  -- 先頭 x と残り xs に分解
```

## 演習問題

`Syntax.hs` の末尾に以下の関数を実装してみましょう：

### 演習1: 長さを求める

```haskell
-- リストの長さを返す（Preludeのlengthを使わずに）
myLength :: [a] -> Int
myLength = undefined  -- TODO: 実装する
```

**ヒント**: `sumList` と同じパターンを使う

### 演習2: リストを逆順にする

```haskell
-- リストを逆順にする（Preludeのreverseを使わずに）
myReverse :: [a] -> [a]
myReverse = undefined  -- TODO: 実装する
```

**ヒント**: 空リストと `(x:xs)` のパターンマッチ

### 演習3: n番目の要素を取得

```haskell
-- 0-indexed で n 番目の要素を返す
myNth :: [a] -> Int -> a
myNth = undefined  -- TODO: 実装する
```

**ヒント**: インデックス 0 の場合と それ以外の場合

## 解答例

演習を解いた後に確認してください。

<details>
<summary>演習1の解答</summary>

```haskell
myLength :: [a] -> Int
myLength []     = 0
myLength (_:xs) = 1 + myLength xs
```

</details>

<details>
<summary>演習2の解答</summary>

```haskell
myReverse :: [a] -> [a]
myReverse []     = []
myReverse (x:xs) = myReverse xs ++ [x]
```

</details>

<details>
<summary>演習3の解答</summary>

```haskell
myNth :: [a] -> Int -> a
myNth (x:_)  0 = x
myNth (_:xs) n = myNth xs (n - 1)
myNth []     _ = error "Index out of bounds"
```

</details>

## 完了チェック

以下ができたら次のステップへ：

- [ ] `Syntax.hs` のコードを読んで理解した
- [ ] GHCi で各関数を実行して動作を確認した
- [ ] 演習問題を少なくとも1つ解いた
- [ ] `references/notes.md` に学んだことをメモした

次: `02-functions/README.md` へ進む
