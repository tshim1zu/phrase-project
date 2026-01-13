# japhrase

**統計的自然言語処理エンジン：PMI/エントロピーベースのフレーズ抽出・要約・共起分析**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Python 3.8+](https://img.shields.io/badge/python-3.8+-blue.svg)](https://www.python.org/downloads/)
[![Tests](https://img.shields.io/badge/tests-200%2B%20passing-brightgreen)](https://github.com/tshim1zu/japhrase)
[![PyPI](https://img.shields.io/badge/version-0.2.0-blue)](https://pypi.org/project/japhrase/)

## 🚀 クイックスタート

```python
from japhrase import PhraseExtracter, Summarizer, CooccurrenceAnalyzer

# 1. フレーズ抽出
df = PhraseExtracter.from_file("input.txt")
print(df)

# 2. 統計的要約
summarizer = Summarizer()
summary = summarizer.summarize(text, ratio=0.3)

# 3. 共起語分析（キャラ分析・評判分析）
analyzer = CooccurrenceAnalyzer()
df = analyzer.analyze(text, "ターゲット単語")
```

## ✨ 主な機能

### 📊 コア機能（v0.2.0）

| 機能 | 説明 | 用途 |
|-----|------|------|
| **PhraseExtracter** | PMI/エントロピーベースの統計的フレーズ抽出 | SNS分析、ニュース抽出、キーワード検出 |
| **Summarizer** ⭐ NEW | 統計的要約エンジン（LLM不要・高速・確実） | テキスト圧縮、アブストラクト生成 |
| **CooccurrenceAnalyzer** ⭐ NEW | 共起語分析による特徴語抽出 | キャラクター分析、製品評判分析、トレンド背景分析 |
| **ComparisonAnalyzer** | Good/Bad プロンプト比較 | ComfyUI最適化、プロンプト改善 |
| **SimilarityAnalyzer** | 複数テキスト間の類似度分析 | コピペ検出、重複分析 |

### 🎯 その他の機能

- 📊 **統計的スコアリング**: PMI（自己相互情報量）と分岐エントロピー
- 🎯 **エビデンスベースプリセット**: Optunaで最適化された用途別設定（SNS/ニュース/小説）
- 🔍 **テキストセグメンテーション**: 文書を最適な長さに自動分割
- 📁 **複数形式対応**: TXT、CSV、TSV、Excel
- 🔤 **エンコーディング自動検出**: UTF-8、Shift-JIS など自動判別
- ⚡ **高速処理**: N-gramベースの効率的アルゴリズム
- 🧪 **200+テスト**: 包括的なテストスイート完備

## 📦 インストール

```bash
# 基本機能
pip install japhrase

# 全機能（プロンプト分析含む）
pip install japhrase[all]

# 開発環境
pip install -e ".[dev]"
```

## 📖 ドキュメント

| ドキュメント | 内容 |
|-----------|------|
| **[USAGE.md](docs/USAGE.md)** | **詳細な使用ガイド** |
| [API_REFERENCE.md](docs/API_REFERENCE.md) | API リファレンス |
| [POSITIONING.md](docs/POSITIONING.md) | 設計思想と位置づけ |
| [CHANGELOG.md](docs/CHANGELOG.md) | 変更履歴 |
| [DEVELOPMENT.md](docs/DEVELOPMENT.md) | 開発者ガイド |

## 🎯 用途別ガイド

### 1. フレーズ抽出（基本）

```python
from japhrase import PhraseExtracter

# SNS向けプリセット
extractor = PhraseExtracter.preset('sns')
df = extractor.extract("tweets.txt")
extractor.export_csv(df, "output.csv")
```

**詳細**: [USAGE.md](docs/USAGE.md)

### 2. 統計的要約 ⭐ 新機能（v0.2.0）

```python
from japhrase import Summarizer

# LLMを使わない・高速・確実な要約
summarizer = Summarizer()

# テキストを30%に圧縮
summary = summarizer.summarize(text, ratio=0.3)
print(summary)
```

**特徴:**
- ✅ ハルシネーション（嘘）がない
- ✅ LLM不要で高速
- ✅ PMI/エントロピーベースの統計的根拠

**詳細**: `examples/summarize_demo.py`

### 3. 共起語分析 ⭐ 新機能（v0.2.0）

```python
from japhrase import CooccurrenceAnalyzer

# キャラクター・製品・トレンドワードの特徴を分析
analyzer = CooccurrenceAnalyzer(window_size=50)

# 「AI」の周辺に特異的に出現する言葉を抽出
df = analyzer.analyze(text, "AI", top_n=10)
print(df)  # [自動運転, セキュリティ, 透明性, ...]
```

**用途:**
- キャラクター分析（性格・外見・口癖の自動抽出）
- 製品評判分析（「画面」の周りに「綺麗」があるか等）
- トレンドワードの背景分析

**詳細**: `examples/cooccurrence_demo.py`

### 4. プロンプト比較分析

Good と Bad のプロンプトを比較して、成功テンプレートと失敗パターンを抽出：

```bash
python scripts/run_comfy_analysis.py \
  --good good_prompts.txt \
  --bad bad_prompts.txt \
  --top-n 10
```

**詳細**: [USAGE.md](docs/USAGE.md)

### 5. 類似度分析

```python
from japhrase import SimilarityAnalyzer

analyzer = SimilarityAnalyzer()
matrix = analyzer.compare_files(["doc1.txt", "doc2.txt", "doc3.txt"])
analyzer.export_heatmap(matrix, "similarity.png")
```

## 🧪 テスト

```bash
# テスト実行
pytest

# カバレッジ付き実行
pytest --cov=japhrase

# 200+ テストがすべてパス
```

## 📋 プロジェクト構造

```
japhrase/
├── __init__.py
├── extracter.py           # ★ フレーズ抽出（メインエンジン）
├── summarizer.py          # ★ 統計的要約（v0.2.0新機能）
├── cooccurrence.py        # ★ 共起語分析（v0.2.0新機能）
├── comparison_analyzer.py  # プロンプト比較分析
├── similarity.py          # 類似度分析
├── patterns.py            # 正規表現パターン
├── config.py              # 設定管理
├── utils.py               # ユーティリティ
├── cli.py                 # コマンドラインツール
├── segmenter.py           # テキストセグメンテーション
├── evaluation.py          # 評価フレームワーク
├── optimization.py        # パラメータ最適化
└── ...

examples/
├── example.ipynb          # ノートブック例
├── summarize_demo.py      # ★ 要約デモ（v0.2.0新機能）
├── cooccurrence_demo.py   # ★ 共起分析デモ（v0.2.0新機能）
└── ...

docs/
├── API_REFERENCE.md       # API リファレンス
├── USAGE.md               # 詳細ガイド
├── POSITIONING.md         # 設計思想
└── ...
```

## 🔗 便利なリンク

- **GitHub**: https://github.com/tshim1zu/japhrase
- **PyPI**: https://pypi.org/project/japhrase/
- **Issues**: https://github.com/tshim1zu/japhrase/issues

## 📄 ライセンス

MIT License

## 👤 作者

Takeshi SHIMIZU

---

**📍 次に読むドキュメント:**

| リンク | 対象 | 内容 |
|-------|------|------|
| [USAGE.md](docs/USAGE.md) | 利用者 | 詳細な使い方とサンプルコード |
| [API_REFERENCE.md](docs/API_REFERENCE.md) | 開発者 | 全APIのリファレンス |
| [POSITIONING.md](docs/POSITIONING.md) | 技術者 | 設計思想と統計的背景 |
| [examples/summarize_demo.py](examples/summarize_demo.py) | 要約機能 | 要約エンジンの実行例 |
| [examples/cooccurrence_demo.py](examples/cooccurrence_demo.py) | 共起分析 | 共起分析の実行例 |
