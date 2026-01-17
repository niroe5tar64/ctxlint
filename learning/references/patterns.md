# Haskell パターン集

このファイルは、Haskellで頻出する実装パターンをまとめたリファレンスです。

## パターンマッチ

### 基本形
```haskell
case value of
  Pattern1 -> expression1
  Pattern2 -> expression2
  _ -> defaultExpression
```

## 高階関数

### map, filter, fold
```haskell
map (+1) [1, 2, 3]
filter (>0) [-1, 0, 1]
foldr (+) 0 [1, 2, 3]
```

## エラーハンドリング

### Maybe型
```haskell
case maybeValue of
  Just x -> x
  Nothing -> defaultValue
```

### Either型
```haskell
case result of
  Right value -> value
  Left error -> handleError error
```

## 関数合成

```haskell
(f . g) x = f (g x)
```

## モナドの基本

### do記法
```haskell
do
  x <- action1
  y <- action2
  return (x + y)
```

## 型宣言テンプレート

```haskell
data MyType = Constructor1 Type1 | Constructor2 Type2

type Alias = ActualType

class MyClass a where
  method :: a -> Result

instance MyClass ConcreteType where
  method x = implementation
```

## 追記予定
- レコード構文のパターン
- レンズ (lens) の基本
- 制約と型シグネチャ
