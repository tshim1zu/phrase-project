# jphrase

**日本語テキストから頻出フレーズを検出**

Detect frequent phrases from Japanese texts

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Python 3.7+](https://img.shields.io/badge/python-3.7+-blue.svg)](https://www.python.org/downloads/)

## 特徴

- 📝 **簡単に使える**: シンプルなAPIで、数行のコードで実行可能
- 🚀 **高速**: N-gramベースの効率的なアルゴリズム
- 🎯 **柔軟**: 豊富なパラメータでカスタマイズ可能
- 📊 **多様な形式**: CSV/TSV/TXT/Excel対応
- 🧪 **テスト済み**: 包括的なテストスイート
- 🔍 **用途**: SNSトレンド分析、ニュース話題抽出、頻出キーワード発見

## インストール

```bash
pip install -r requirements.txt
```

または開発モードでインストール：

```bash
pip install -e .
```

## クイックスタート

### まずは試してみる（ファイル不要）

```python
from jphrase import PhraseExtracter

# デモデータですぐに試せます
df = PhraseExtracter.demo()
print(df)
```

### ファイルから抽出

```python
from jphrase import PhraseExtracter

# ファイルから直接抽出
df = PhraseExtracter.from_file("input.txt")
print(df)
```

### テキストリストから抽出

```python
from jphrase import PhraseExtracter

sentences = [
    "フォローありがとうございます",
    "フォローしてください",
    "プレゼントキャンペーン開催中"
]

extractor = PhraseExtracter()
df = extractor.get_dfphrase(sentences)
print(df)
```

### パラメータをカスタマイズ

```python
from jphrase import PhraseExtracter

# パラメータを指定
df = PhraseExtracter.from_file(
    "input.txt",
    min_count=10,      # 10回以上出現
    max_length=20,     # 最大20文字
    verbose=1          # 進捗表示
)
```

### 複数ファイルから抽出

```python
from jphrase import PhraseExtracter

# 複数ファイルをまとめて処理
files = ["file1.txt", "file2.txt", "file3.txt"]
df = PhraseExtracter.from_files(files, min_count=5)
```

### 結果をエクスポート

```python
from jphrase import PhraseExtracter

extractor = PhraseExtracter()
df = extractor.extract("input.txt")

# 各種形式で出力
extractor.export_csv(df, "output.csv")      # CSV
extractor.export_json(df, "output.json")    # JSON
extractor.export_excel(df, "output.xlsx")   # Excel
```

## 主要な機能

### 便利なクラスメソッド

- `PhraseExtracter.from_file()` - ファイルから直接抽出
- `PhraseExtracter.from_files()` - 複数ファイルから抽出

### インスタンスメソッド

- `extract()` - ファイルからフレーズ抽出
- `get_dfphrase()` - テキストリストからフレーズ抽出
- `export_csv()` - CSV形式で出力
- `export_json()` - JSON形式で出力
- `export_excel()` - Excel形式で出力

### 対応ファイル形式

- テキストファイル (`.txt`, `.text`)
- CSV (`.csv`)
- TSV (`.tsv`)

## パラメータ

主要なパラメータ：

| パラメータ | デフォルト | 説明 |
|-----------|-----------|------|
| `min_count` | 6 | フレーズの最小出現回数 |
| `max_length` | 16 | フレーズの最大文字数 |
| `min_length` | 4 | フレーズの最小文字数 |
| `threshold_originality` | 0.5 | 類似フレーズ除去の閾値 |
| `verbose` | 1 | 進捗表示（0:非表示, 1:表示） |
| `knowns` | [] | 優先的に抽出したい既知語 |

詳細は [USAGE.md](USAGE.md) を参照してください。

## 使用例

### SNSテキスト分析

```python
from jphrase import PhraseExtracter

extractor = PhraseExtracter(min_count=10, max_length=20)
df = extractor.extract("tweets.csv")
extractor.export_excel(df, "sns_phrases.xlsx")
```

### 複数ファイルからの専門用語抽出

```python
from jphrase import PhraseExtracter

files = ["article1.txt", "article2.txt", "article3.txt"]
df = PhraseExtracter.from_files(
    files,
    min_count=5,
    max_length=30,
    threshold_originality=0.7
)
```

### ワンライナー

```python
from jphrase import PhraseExtracter

# 抽出して即座にCSV出力
extractor = PhraseExtracter()
extractor.export_csv(
    PhraseExtracter.from_file("input.txt", min_count=10),
    "output.csv"
)
```

## プロジェクト構造

```
phrase-project/
├── jphrase/              # メインパッケージ
│   ├── __init__.py       # パッケージ初期化
│   ├── constants.py      # 定数定義
│   ├── patterns.py       # 正規表現パターン
│   ├── extracter.py      # メインクラス
│   ├── utils.py          # ユーティリティ関数
│   └── example.ipynb     # サンプルノートブック
├── tests/                # テストスイート
│   ├── test_constants.py
│   ├── test_patterns.py
│   ├── test_extracter.py
│   └── test_utils.py
├── requirements.txt      # 依存パッケージ
├── setup.py             # セットアップスクリプト
├── README.md            # このファイル
├── USAGE.md             # 詳細な使用ガイド
└── TESTING.md           # テストガイド
```

## 開発

### テストの実行

```bash
# 依存パッケージのインストール
pip install -r requirements-dev.txt

# テストの実行
pytest

# カバレッジレポート付き
pytest --cov=jphrase --cov-report=html
```

詳細は [TESTING.md](TESTING.md) を参照してください。

## ドキュメント

- **[POSITIONING.md](POSITIONING.md)** - ⭐ このツールの位置づけと設計思想（必読）
- [USAGE.md](USAGE.md) - 詳細な使用ガイド
- [OPTIMIZATION.md](OPTIMIZATION.md) - パラメータ最適化ガイド
- [DATA_SOURCES.md](DATA_SOURCES.md) - データ取得方法
- [THEORY.md](THEORY.md) - 理論的考察と限界
- [TESTING.md](TESTING.md) - テスト実行方法
- [example.ipynb](jphrase/example.ipynb) - Jupyter Notebookサンプル

## ライセンス

MIT License

## 作者

Takeshi SHIMIZU

## 貢献

Issue や Pull Request は大歓迎です！

## 変更履歴

### v0.1.0
- 初回リリース
- **位置づけの明確化**: 頻出フレーズ検出ツールとして定義
- モジュール分割（constants, patterns, extracter, utils）
- 便利なユーティリティメソッド追加
  - `from_file()`, `from_files()`
  - `export_csv()`, `export_json()`, `export_excel()`
- データソースモジュール（Wikipedia, 青空文庫対応）
- パラメータ最適化機能（教師なし/教師あり）
- 評価モジュール（内部指標ベース）
- 包括的なテストスイート
- 充実したドキュメント
  - POSITIONING.md（位置づけと設計思想）
  - THEORY.md（理論的考察）
  - OPTIMIZATION.md（最適化ガイド）
  - DATA_SOURCES.md（データ取得方法）
