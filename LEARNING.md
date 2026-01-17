# Haskell 学習ガイド

このディレクトリは、Haskell初心者がctxlintプロジェクトのコードを書く前に基礎を習得するための学習スペースです。

## ディレクトリ構成

```
learning/
├── basics/                 # Haskellの基本を学ぶ
│   ├── 01-syntax/         # 変数、型、基本的な式
│   ├── 02-functions/      # 関数型プログラミング
│   └── 03-typeclasses/    # 型クラスとインスタンス
├── practice/              # 実践的な技能を磨く
│   ├── parsing/           # パーサー実装
│   ├── validation/        # バリデーションロジック
│   └── cli/               # CLI実装
├── references/            # 参考資料
│   ├── notes.md           # 学習進捗ノート
│   ├── patterns.md        # Haskellパターン集
│   └── links.md           # 参考リンク集
├── sandbox.cabal          # 学習用Cabalプロジェクト
└── Main.hs                # 学習コードのテスト実行
```

## 学習パス

### ステップ1: 基礎構文（`basics/01-syntax/`）

Haskellの基本的な構文を学びます：

- 型宣言と変数
- 関数定義
- パターンマッチ
- リスト・タプル・レコード

```bash
# ファイルを編集して練習
vim learning/Basics/Syntax.hs

# テストして動作確認
cd learning && cabal build && cabal exec sandbox-exe
```

### ステップ2: 関数型プログラミング（`basics/02-functions/`）

関数型プログラミングの重要なパターンを学びます：

- 高階関数 (map, filter, fold)
- ラムダ式
- 部分適用とカリー化
- 関数合成

### ステップ3: 型クラス（`basics/03-typeclasses/`）

Haskellの型クラスシステムを理解します：

- 型クラスの定義
- インスタンスの実装
- 制約の書き方

### ステップ4: パーサー実装（`practice/parsing/`）

Parsecを使ったパーサー実装を学びます：

- 基本的なパーサー
- マークダウンのパーサー
- エラーハンドリング

このステップで習得したスキルは、後々ctxlintのマークダウンパーサに直結します。

### ステップ5: バリデーション（`practice/validation/`）

データ検証のロジックを実装します：

- Maybe/Either型を使ったエラーハンドリング
- カスタム検証型の設計
- 複数エラーの集約

このステップで習得したスキルは、ctxlintのバリデーションロジックに活用できます。

## 学習中の注意点

1. **型安全性を意識する**
   - コンパイラのエラーメッセージをしっかり読む
   - 型を明示的に書いて理解を深める

2. **GHCiでインタラクティブに試す**
   ```bash
   cd learning
   cabal repl
   ```
   GHCiプロンプトで関数をテストして動作を確認できます

3. **参考資料を活用する**
   - `references/notes.md`: 学習進捗をここに記録
   - `references/patterns.md`: よく使うパターンをまとめる
   - `references/links.md`: 参考リンクをリストアップ

## 本体プロジェクトへの移行

`learning/` ディレクトリでスキルを習得したら、以下のプロジェクト構成で実装を開始します：

```
src/
├── Ctxlint/
│   ├── Types.hs           # コアデータ型（状態遷移を含む）
│   ├── Parser.hs          # Specのパーサー
│   └── Validator.hs       # Specのバリデーション
└── Main.hs

app/
└── Main.hs                # CLI入口

test/
└── Main.hs                # テストスイート
```

この時点では、`learning/` の成果をベースに、より実用的なコード設計ができるようになっているはずです。

## クリーンアップ

本体開発が始まって、学習ディレクトリが不要になったら：

```bash
rm -rf learning/
```

`.gitignore` ですでに除外設定をしているので、gitの履歴に影響しません。

---

**Happy Learning! 🚀**
