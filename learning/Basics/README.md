# Haskell 基礎学習ガイド

このディレクトリでHaskellの基礎を段階的に学びます。

## 学習の進め方

### 1. 各トピックの学習サイクル

```
1. README.md を読む     → 概念を理解
2. 対応する .hs を読む  → コード例を確認
3. GHCi で試す          → 動作を体験
4. 演習問題を解く       → 理解を定着
5. notes.md に記録      → 振り返り用メモ
```

### 2. GHCi の使い方

```bash
# REPLを起動
cd learning && cabal repl

# モジュールをロード
ghci> :load Basics.Syntax

# 関数を試す
ghci> add 3 5
8

# 型を確認
ghci> :type add
add :: Int -> Int -> Int

# ファイル変更後にリロード
ghci> :reload
```

### 3. 学習順序

| 順番 | ディレクトリ | 対応ファイル | 学習内容 |
|------|-------------|-------------|---------|
| 1 | `01-syntax/` | `Syntax.hs` | 型、変数、パターンマッチ |
| 2 | `02-functions/` | `Functions.hs` | 高階関数、ラムダ式、関数合成 |
| 3 | `03-typeclasses/` | `TypeClasses.hs` | 型クラス定義、インスタンス実装 |

## ディレクトリ構成

```
Basics/
├── README.md           # このファイル（全体ガイド）
├── 01-syntax/
│   └── README.md       # 構文学習の詳細ガイド
├── 02-functions/
│   └── README.md       # 関数学習の詳細ガイド
├── 03-typeclasses/
│   └── README.md       # 型クラス学習の詳細ガイド
├── Syntax.hs           # 構文のサンプルコード
├── Functions.hs        # 関数のサンプルコード
└── TypeClasses.hs      # 型クラスのサンプルコード
```

## 学習のコツ

### コンパイラエラーを味方にする

Haskellのコンパイラエラーは非常に親切です。エラーが出たら：

1. **型エラー**: 期待される型と実際の型を比較する
2. **構文エラー**: インデントや括弧を確認する
3. **未定義エラー**: import や定義漏れを確認する

### 型を明示的に書く

学習段階では型シグネチャを必ず書きましょう：

```haskell
-- 良い例：型が明示的
add :: Int -> Int -> Int
add a b = a + b

-- 避ける例：型推論に頼りすぎ
add a b = a + b
```

### 小さく試す

新しい概念を学んだら、すぐにGHCiで試してください：

```haskell
-- 1. まず最小限のコードで試す
ghci> map (+1) [1,2,3]
[2,3,4]

-- 2. 動作を確認してから応用する
ghci> map (*2) [1,2,3]
[2,4,6]
```

## 次のステップ

基礎を終えたら `learning/practice/` に進みます：

- `practice/parsing/` - Parsecを使ったパーサー実装
- `practice/validation/` - バリデーションロジック
- `practice/cli/` - CLIアプリケーション構築

これらは ctxlint 本体の実装に直結するスキルです。
