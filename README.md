# jphrase

**日本語テキストから頻出フレーズを検出**

Detect frequent phrases from Japanese texts

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Python 3.7+](https://img.shields.io/badge/python-3.7+-blue.svg)](https://www.python.org/downloads/)

![jphrase](https://youtu.be/Kifc1gX9ceQ)

## 特徴

- 📝 **簡単に使える**: シンプルなAPIで、数行のコードで実行可能
- 🚀 **高速**: N-gramベースの効率的なアルゴリズム
- 🎯 **柔軟**: 豊富なパラメータでカスタマイズ可能
- ⚙️ **エビデンスベースのプリセット**: Optunaで最適化された用途別パラメータ（NEW!）
- 📊 **多様な形式**: CSV/TSV/TXT/Excel対応
- 🔤 **エンコーディング自動検出**: UTF-8、Shift-JIS、EUC-JPなど自動判別
- 💬 **文字列リスト対応**: ファイルだけでなく、テキストデータを直接処理可能
- 🔍 **類似度分析**: 複数ファイル間のコピペ検出・重複分析
- 🧪 **テスト済み**: 包括的なテストスイート（85テスト）
- 🎯 **用途**: SNSトレンド分析、ニュース話題抽出、頻出キーワード発見、コピペ検出

## インストール

```bash
pip install japhrase
```

類似度分析機能を使う場合（オプション）：

```bash
pip install japhrase[similarity]
```

全ての機能をインストール：

```bash
pip install japhrase[all]
```

または開発モードでインストール：

```bash
pip install -e .
```

## クイックスタート

### まずは試してみる（ファイル不要）

```python
from japhrase import PhraseExtracter

# デモデータですぐに試せます
df = PhraseExtracter.demo()
print(df)
```

### エビデンスベースのプリセットを使う（NEW!）

```python
from japhrase import PhraseExtracter

# SNS向け最適化パラメータ
extractor = PhraseExtracter.preset('sns')
df = extractor.extract("tweets.txt")

# ニュース向け最適化パラメータ
extractor = PhraseExtracter.preset('news')
df = extractor.extract("articles.txt")

# 小説向けパラメータ
extractor = PhraseExtracter.preset('novel')
df = extractor.extract("novel.txt")

# レポート採点向けパラメータ
extractor = PhraseExtracter.preset('report')
df = extractor.extract("reports.txt")

# 利用可能なプリセット一覧を表示
PhraseExtracter.list_presets()
```

### ファイルから抽出

```python
from japhrase import PhraseExtracter

# ファイルから直接抽出
df = PhraseExtracter.from_file("input.txt")
print(df)
```

### テキストリストから抽出

```python
from japhrase import PhraseExtracter

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
from japhrase import PhraseExtracter

# パラメータを指定
df = PhraseExtracter.from_file(
    "input.txt",
    min_count=10,      # 10回以上出現
    max_length=20,     # 最大20文字
    verbose=1          # 進捗表示
)
```

### エンコーディング自動検出

```python
from japhrase import PhraseExtracter

# encoding='auto'で自動検出（デフォルト）
df = PhraseExtracter.from_file("shift_jis_file.txt")  # 自動でShift-JISを検出

# エンコーディングを明示指定も可能
df = PhraseExtracter.from_file("input.txt", encoding="utf-8")
```

### 文字列リストから直接抽出

```python
from japhrase import PhraseExtracter

extractor = PhraseExtracter(min_count=2)

# ファイルパスではなく、文字列リストを直接渡せる
texts = [
    "フォローありがとうございます",
    "フォローしてください",
    "プレゼントキャンペーン開催中"
]
df = extractor.extract(texts)  # ファイル不要！
```

### 複数ファイル間の類似度分析・コピペ検出（NEW!）

```python
from japhrase import SimilarityAnalyzer

# 複数ファイルの類似度を分析
analyzer = SimilarityAnalyzer(method='auto')  # 自動選択（実用性重視）
matrix = analyzer.compare_files([
    "doc1.txt",
    "doc2.txt",
    "doc3.txt"
])

# 類似度行列を表示
print(matrix)
#           doc1.txt  doc2.txt  doc3.txt
# doc1.txt      1.00      0.85      0.32
# doc2.txt      0.85      1.00      0.28
# doc3.txt      0.32      0.28      1.00

# 類似ペアを抽出（閾値70%以上）
pairs = analyzer.find_similar_pairs(matrix, threshold=0.7)
print(pairs)
# [{'item1': 'doc1.txt', 'item2': 'doc2.txt', 'similarity': 0.85}]

# 相関行列をCSVでエクスポート
analyzer.export_matrix(matrix, "similarity.csv")

# ヒートマップを生成（要: matplotlib, seaborn）
analyzer.export_heatmap(matrix, "heatmap.png")
```

#### 類似度計算手法の選択

```python
# 手法を明示指定
analyzer = SimilarityAnalyzer(method='levenshtein')  # 正確だが遅い
analyzer = SimilarityAnalyzer(method='jaccard')      # 高速
analyzer = SimilarityAnalyzer(method='cosine')       # 長文向け
analyzer = SimilarityAnalyzer(method='auto')         # 自動選択（推奨）
```

- **`levenshtein`**: レーベンシュタイン距離（正確、短文向け）
- **`jaccard`**: N-gram Jaccard係数（高速、バランス型）
- **`cosine`**: TF-IDFコサイン類似度（長文向け）
- **`auto`**: テキスト長に応じて自動選択（デフォルト、実用性重視）

### 複数ファイルから抽出

```python
from japhrase import PhraseExtracter

# 複数ファイルをまとめて処理
files = ["file1.txt", "file2.txt", "file3.txt"]
df = PhraseExtracter.from_files(files, min_count=5)
```

### 結果をエクスポート

```python
from japhrase import PhraseExtracter

extractor = PhraseExtracter()
df = extractor.extract("input.txt")

# 各種形式で出力
extractor.export_csv(df, "output.csv")      # CSV
extractor.export_json(df, "output.json")    # JSON
extractor.export_excel(df, "output.xlsx")   # Excel
```

## 主要な機能

### 便利なクラスメソッド

- `PhraseExtracter.preset()` - エビデンスベースのプリセットで初期化（NEW!）
- `PhraseExtracter.list_presets()` - 利用可能なプリセット一覧を表示（NEW!）
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

詳細は [USAGE.md](docs/USAGE.md) を参照してください。

## プリセット（NEW!）

Optunaによる最適化実験で得られたエビデンスベースのパラメータセットを提供しています。

### 利用可能なプリセット

| プリセット | 用途 | パラメータ |
|-----------|------|-----------|
| `sns` | SNS/Twitter向け | min_count=6, max_length=9, min_length=5, threshold_originality=0.52 |
| `news` | ニュース/記事向け | min_count=5, max_length=10, min_length=3, threshold_originality=0.64 |
| `novel` | 小説向け | min_count=4, max_length=16, min_length=3, threshold_originality=0.6 |
| `report` | レポート/論文採点向け | min_count=10, max_length=24, min_length=4, threshold_originality=0.78 |
| `default` | デフォルト設定 | min_count=6, max_length=16, min_length=4, threshold_originality=0.5 |

### プリセットの使い方

```python
from japhrase import PhraseExtracter

# SNS向けプリセット
extractor = PhraseExtracter.preset('sns')
df = extractor.extract("tweets.txt")

# 小説向けプリセット（繰り返し表現の抽出）
extractor = PhraseExtracter.preset('novel')
df = extractor.extract("novel.txt")

# レポート採点向けプリセット（定型表現・学術用語の検出）
extractor = PhraseExtracter.preset('report')
df = extractor.extract("reports.txt")

# パラメータの一部を上書き
extractor = PhraseExtracter.preset('sns', min_count=10)

# 利用可能なプリセット一覧を表示
PhraseExtracter.list_presets()
```

## 使用例

### SNSテキスト分析

```python
from japhrase import PhraseExtracter

extractor = PhraseExtracter(min_count=10, max_length=20)
df = extractor.extract("tweets.csv")
extractor.export_excel(df, "sns_phrases.xlsx")
```

### 複数ファイルからの専門用語抽出

```python
from japhrase import PhraseExtracter

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
from japhrase import PhraseExtracter

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
# 開発依存関係のインストール
pip install -e ".[dev]"

# テストの実行
pytest

# カバレッジレポート付き
pytest --cov=jphrase --cov-report=html
```

詳細は [TESTING.md](docs/TESTING.md) を参照してください。

## ドキュメント

- **[POSITIONING.md](docs/POSITIONING.md)** - ⭐ このツールの位置づけと設計思想（必読）
- [USAGE.md](docs/USAGE.md) - 詳細な使用ガイド
- [OPTIMIZATION.md](docs/OPTIMIZATION.md) - パラメータ最適化ガイド
- [DATA_SOURCES.md](docs/DATA_SOURCES.md) - データ取得方法
- [THEORY.md](docs/THEORY.md) - 理論的考察と限界
- [TESTING.md](docs/TESTING.md) - テスト実行方法
- [example.ipynb](examples/example.ipynb) - Jupyter Notebookサンプル

## ライセンス

MIT License

## 作者

Takeshi SHIMIZU

## 貢献

Issue や Pull Request は大歓迎です！

## 変更履歴

### v0.1.3
- **エビデンスベースのプリセット機能**: Optunaで最適化された用途別パラメータ
  - `PhraseExtracter.preset('sns')` でSNS向け最適パラメータを使用
  - `PhraseExtracter.preset('news')` でニュース向け最適パラメータを使用
  - `PhraseExtracter.list_presets()` でプリセット一覧を表示
  - 30試行のベイズ最適化によるエビデンスベースのパラメータ設定
- **類似度分析機能**: 複数ファイル/テキスト間の類似度分析・コピペ検出
  - `SimilarityAnalyzer`クラスを追加
  - 3種類の類似度計算手法を実装：
    - レーベンシュタイン距離（正確、短文向け）
    - N-gram Jaccard係数（高速、バランス型）
    - TF-IDFコサイン類似度（長文向け）
  - 自動選択モード（テキスト長に応じて最適な手法を選択）
  - 類似度行列の生成と可視化（ヒートマップ）
  - 類似ペアの自動抽出（閾値指定可能）
  - CSV/Excel/JSON出力対応
- **開発ツール**: Optunaによるハイパーパラメータ最適化（開発用）
  - `OptunaOptimizer`クラスを追加（dev依存関係）
  - ベイズ最適化（TPEサンプラー）による効率的な探索
  - 実験結果の保存と可視化機能
- オプション依存関係を追加：
  - `pip install japhrase[similarity]` で類似度分析機能をインストール
  - `pip install japhrase[dev]` で開発ツール（Optuna含む）をインストール
  - `python-Levenshtein`, `scikit-learn`, `matplotlib`, `seaborn`, `optuna`
- テストスイートを拡充（85テスト、全てパス）

### v0.1.2
- **エンコーディング自動検出**: chardetライブラリを使用した自動検出機能
  - UTF-8、Shift-JIS、EUC-JP、CP932などを自動判別
  - `encoding='auto'`がデフォルト（明示指定も可能）
- **文字列リスト入力対応**: `extract()`メソッドが文字列リストを直接受け取れるように
  - ファイルパスまたはリスト/タプルを自動判別
  - ファイルを作成せずに直接テキストデータを処理可能
- 依存関係に`chardet>=4.0.0`を追加
- 包括的なテスト追加（エンコーディング検出、文字列入力対応）

### v0.1.1
- パッケージ名とモジュール名の不一致を修正
  - モジュール名を`jphrase`から`japhrase`に変更
  - `pip install japhrase` → `from japhrase import` で統一

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
