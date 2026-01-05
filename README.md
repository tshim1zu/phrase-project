# jphrase

**日本語テキストから頻出フレーズを検出**

Detect frequent phrases from Japanese texts

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Python 3.7+](https://img.shields.io/badge/python-3.7+-blue.svg)](https://www.python.org/downloads/)

## 📺 解説動画

**[▶️ このプロジェクトの解説動画を見る](https://youtu.be/Kifc1gX9ceQ)**

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

## 変更履歴

### v0.1.4 - UX/DX改善版 🚀

#### 📌 Phase 1: CLIツール化（スクリプト不要）

インストール直後からターミナルで完結。Pythonコード不要。

```bash
# 基本分析
japhrase extract input.txt
japhrase analyze manuscript.txt --abstract abstract.txt

# ユースケース駆動（最もシンプル）
japhrase use-case academic_writing --body paper.txt --abstract abstract.txt
japhrase use-case novel_revision --v1 draft1.txt --v2 draft2.txt

# ワークフロー実行（複雑な処理）
japhrase workflow workflow.yaml --parallel --max-workers 8

# 品質チェック（Linter）
japhrase check document.txt --config .japhrase.toml
```

**実装コマンド:**
- `extract` - フレーズ抽出
- `kwic` - KWIC逆引き検索
- `check-divergence` - あらすじ vs 本文チェック
- `detect-habits` - 個人の口癖検出
- `analyze` - 統合分析レポート
- `use-case` - ユースケース別ワークフロー
- `use-case-list` - ユースケース一覧
- `workflow` - YAMLワークフロー実行
- `config` - 設定ファイル表示
- `check` - 品質チェック（Linter）

#### 📌 Phase 2: ワークフロー/パイプラインエンジン

複数タスクを依存関係で管理。DAG検証、並列実行対応。

```yaml
# workflow.yaml
name: "Complete Manuscript Check"
tasks:
  - id: extract_phrases
    type: extract
    input: manuscript.txt
    output: results/phrases.csv

  - id: check_divergence
    type: check_divergence
    inputs: [abstract.txt, manuscript.txt]
    depends_on: [extract_phrases]

  - id: detect_habits
    type: detect_habits
    input: manuscript.txt
    depends_on: [extract_phrases]
```

```bash
japhrase workflow workflow.yaml --parallel --max-workers 8
```

**実装クラス:**
- `WorkflowDefinition` - YAML定義解析・検証
- `WorkflowEngine` - DAG実行エンジン
- `TaskRegistry` - タスク関数レジストリ
- 循環依存検出、トポロジカルソート対応

#### 📌 Phase 3: ユースケース駆動インターフェース

「何をしたいか」を指定するだけ。プリセット自動適用。

```bash
# 学位論文の品質チェック
japhrase use-case academic_writing --body thesis.txt --abstract abstract.txt --corpus past_papers/

# 小説原稿の推敲分析
japhrase use-case novel_revision --v1 draft1.txt --v2 draft2.txt --v3 draft3.txt

# ブログ記事の最適化
japhrase use-case blog_writing --body article.txt --corpus past_articles/

# SNS投稿の表記統一
japhrase use-case sns_content --body tweet.txt -o report.txt

# 編集者向けチェック
japhrase use-case editing --body manuscript.txt
```

**利用可能なユースケース:**
- `academic_writing` - 学位論文・学術論文
- `novel_revision` - 小説原稿推敲
- `blog_writing` - ブログ記事執筆
- `sns_content` - SNS投稿
- `editing` - 編集者向けチェック

#### 📌 Phase 4: 設定ファイル対応

`.japhrase.toml` または `.japhrase.yml` をプロジェクトルートに配置。パラメータ毎回指定不要。

```toml
# .japhrase.toml
[global]
preset = "novel"

[analysis]
min_count = 5
max_length = 20
threshold_originality = 0.6

[filter]
ignore = ["zutto", "teki", "http"]
knowns = ["character_name"]

[check]
forbidden_phrases = ["bad_word"]
required_keywords = {"important" = 2}
spelling_rules = {"standard" = ["variant"]}
min_length = 100
```

```bash
# 自動で .japhrase.toml を読み込む
japhrase extract input.txt

# CLIオプションで上書き（優先順位: CLI > ファイル）
japhrase extract input.txt --min-count 20

# 設定を表示
japhrase config
```

**対応形式:**
- `.japhrase.toml` (推奨)
- `.japhrase.yml`
- `japhrase.toml`
- `japhrase.yml`
- 自動探索: 現在ディレクトリから5階層上まで

#### 📌 Phase 5: Linterモード（品質チェック）

品質をテストとして検証。CI/CD統合対応。

```bash
japhrase check document.txt --config .japhrase.toml -o report.txt
# Exit code: 0（成功）or 1（エラー検出）
```

**チェック機能:**
- **禁止ワード検出** - 指定したワードの出現を検出
- **必須キーワード確認** - 重要キーワードの不足を警告
- **表記ゆれ検出** - 標準表記への統一を提案
- **文書長チェック** - 最小/最大文字数の検証
- **段落構成チェック** - 最小段落数の確認

**設定例:**
```toml
[check]
forbidden_phrases = ["古い表現", "非推奨"]
required_keywords = {"必須" = 2, "キーワード" = 1}
spelling_rules = {"ユーザー" = ["ユーザ"]}
min_length = 100
max_length = 5000
min_paragraphs = 3
```

**メリット:**
- GitHub Actions などの CI/CD に統合可能
- プルリクエストで表記ゆれを防止
- ドキュメント品質を自動保証

#### 📊 テスト統計

- **全テスト: 182個（100% 成功）**
- Phase 1: 10個
- Phase 2: 16個
- Phase 3: 12個
- Phase 4: 10個
- Phase 5: 16個

#### 📚 ドキュメント追加

- [REVIEW_RESPONSE.md](docs/REVIEW_RESPONSE.md) - review.md への対応報告
- [ARCHITECTURE_IMPROVEMENTS.md](docs/ARCHITECTURE_IMPROVEMENTS.md) - アーキテクチャ改善ガイド
- [examples/workflows/README.md](examples/workflows/README.md) - ワークフロー使用ガイド

#### 💾 新しいモジュール

- `japhrase/cli.py` - CLIコマンド実装（Click）
- `japhrase/workflow.py` - ワークフロー/DAGエンジン（networkx）
- `japhrase/use_cases.py` - ユースケース駆動インターフェース
- `japhrase/config.py` - 設定ファイル管理
- `japhrase/checker.py` - 品質チェック（Linter）

#### 📦 新しい依存関係

- `click>=8.0.0` - CLI
- `networkx>=2.6` - DAG
- `PyYAML>=6.0` - YAML設定ファイル
- `tomli` (Python <3.11) - TOML設定ファイル

---

### v0.1.5 - 統計的スコアリング強化版 📊

**統計的手法でスコアリングを極限まで強化** - ideas.mdの提案を実装

#### 🎯 新機能: PMI（自己相互情報量）

PMI（Pointwise Mutual Information）を導入し、文字の結合度を統計的に評価

```python
from japhrase import PhraseExtracter

# PMIを有効化
extractor = PhraseExtracter(
    min_count=5,
    use_pmi=True,     # ← PMI有効
    pmi_weight=1.0    # PMI重み係数
)

df = extractor.extract("input.txt")
```

**効果:**
- ✅ 「機械学習」「ニューラルネットワーク」など結合度が高い専門用語を上位に
- ❌ 「ていう」「みたいな」など単なる組み合わせを下位に
- **PMI計算**: `log(P(phrase) / product(P(char_i)))`

#### 🎯 新機能: 分岐エントロピー（Branching Entropy）

単語境界を統計的に推定し、フレーズの信頼度を判定

```python
# 分岐エントロピーを有効化
extractor = PhraseExtracter(
    min_count=5,
    use_branching_entropy=True,  # ← BE有効
    entropy_weight=1.0           # エントロピー重み係数
)

df = extractor.extract("input.txt")
```

**効果:**
- ✅ 「機械学習」のように前後で異なる文字が来る（高エントロピー）→ 単語境界と判定
- ❌ 「機械学」のように後ろに「習」しか来ない（低エントロピー）→ 単語途中と判定
- **エントロピー計算**: `-sum(p(x) * log(p(x)))`

#### 💡 両方を組み合わせた「最強」設定

```python
# PMI + 分岐エントロピー両方有効（統計ガチ勢向け）
extractor = PhraseExtracter(
    min_count=5,
    max_length=16,
    use_pmi=True,
    use_branching_entropy=True,
    pmi_weight=1.0,
    entropy_weight=1.0
)

df = extractor.extract("input.txt")
# スコア計算: 基本スコア × (1 + pmi) × (1 + boundary_score)
```

#### 📈 実装統計

- **新テストファイル**: `tests/test_statistical_scoring.py` (21テスト)
- **新メソッド**: `calculate_pmi()`, `calculate_branching_entropy()`
- **拡張メソッド**: `hold_higherrank()` (PMI/BE統合スコアリング)
- **全テスト**: 203個（182 既存 + 21 新規）

#### ⚙️ パラメータ説明

```python
PhraseExtracter(
    use_pmi=False,              # PMI使用（デフォルト: False）
    use_branching_entropy=False, # 分岐エントロピー使用（デフォルト: False）
    pmi_weight=1.0,             # PMI重み係数（デフォルト: 1.0）
    entropy_weight=1.0,         # エントロピー重み係数（デフォルト: 1.0）
    # その他既存パラメータは変更なし
)
```

**後方互換性**: デフォルトではPMI/BEを使用しないため、既存コードへの影響なし

#### 🧪 テスト内容

- **PMI計算テスト**: 結合度の高低、数値安定性
- **エントロピー計算テスト**: 単語境界、単語途中、正規化
- **統合テスト**: PMI/BE両方有効時のスコアリング
- **互換性テスト**: 既存機能との干渉なし
- **エッジケース**: 空入力、長文、特殊文字、Unicode

#### 📖 理論背景

ideas.md の以下の提案を実装：
- 「隣接種類数」と「分岐エントロピー」の導入 (精度強化)
- スコアリング関数の統計的刷新 (PMI導入)

詳細は [docs/ideas.md](docs/ideas.md) を参照してください。

---

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
