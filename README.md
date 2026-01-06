# japhrase

**日本語テキストから頻出フレーズを統計的に抽出**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Python 3.8+](https://img.shields.io/badge/python-3.8+-blue.svg)](https://www.python.org/downloads/)
[![Tests](https://img.shields.io/badge/tests-200%2B%20passing-brightgreen)](https://github.com/tshim1zu/japhrase)

## 🚀 クイックスタート

```python
from japhrase import PhraseExtracter

# ファイルから抽出（数行で完結）
df = PhraseExtracter.from_file("input.txt")
print(df)

# または、テキストリストから直接抽出
texts = ["テキスト1", "テキスト2"]
df = PhraseExtracter().extract(texts)
```

## ✨ 主な機能

- 📊 **統計的スコアリング**: PMI（自己相互情報量）と分岐エントロピーで精度向上
- 🎯 **エビデンスベースプリセット**: Optunaで最適化された用途別設定
- 🔍 **ComfyUI プロンプト最適化** ⭐ NEW: Good/Bad の比較から最適要素を自動抽出
- 📁 **複数形式対応**: TXT、CSV、TSV、Excel
- 🔤 **エンコーディング自動検出**: UTF-8、Shift-JIS など自動判別
- ⚡ **高速処理**: N-gramベースの効率的アルゴリズム
- 🧪 **200+テスト**: 包括的なテストスイート完備

## 📦 インストール

```bash
# 基本機能
pip install japhrase

# ComfyUI プロンプト最適化機能も使う場合
pip install japhrase[comfy]

# 全機能（類似度分析含む）
pip install japhrase[all]

# 開発環境
pip install -e ".[dev]"
```

## 📖 ドキュメント

| ドキュメント | 内容 |
|-----------|------|
| **[COMFY_OPTIMIZATION.md](docs/COMFY_OPTIMIZATION.md)** | **ComfyUI プロンプト最適化ガイド（NEW!）** |
| [USAGE.md](docs/USAGE.md) | 詳細な使用ガイド |
| [API_REFERENCE.md](docs/API_REFERENCE.md) | API リファレンス |
| [POSITIONING.md](docs/POSITIONING.md) | 設計思想と位置づけ |
| [DEVELOPMENT.md](docs/DEVELOPMENT.md) | 開発者ガイド |

## 🎯 用途別ガイド

### ComfyUI プロンプト最適化（NEW!）

Good と Bad のプロンプトを比較して、最適な要素を自動抽出：

```bash
python scripts/run_comfy_analysis.py \
  --good good_prompts.txt \
  --bad bad_prompts.txt \
  --comfy-format \
  --top-n 10
```

**出力:** Must Use / Must Avoid タグ

詳細は [COMFY_OPTIMIZATION.md](docs/COMFY_OPTIMIZATION.md) を参照

### テキスト分析

```python
from japhrase import PhraseExtracter

# SNS向けプリセット
extractor = PhraseExtracter.preset('sns')
df = extractor.extract("tweets.txt")
extractor.export_csv(df, "output.csv")
```

### 複数ファイルの類似度分析

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
├── extracter.py           # メインのフレーズ抽出クラス
├── comparison_analyzer.py  # ComfyUI 分析（NEW!）
├── similarity.py          # 類似度分析
├── patterns.py            # 正規表現パターン
├── config.py              # 設定管理
├── utils.py               # ユーティリティ
├── cli.py                 # コマンドラインツール
└── ...

docs/
├── COMFY_OPTIMIZATION.md  # ComfyUI ガイド（NEW!）
├── API_REFERENCE.md       # API リファレンス
├── USAGE.md               # 詳細ガイド
├── POSITIONING.md         # 設計思想
└── ...

scripts/
├── run_comfy_analysis.py  # ComfyUI 分析実行スクリプト
├── generate_comfy_toy_dataset.py
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
- ComfyUI を使う方: [COMFY_OPTIMIZATION.md](docs/COMFY_OPTIMIZATION.md)
- 詳細な使い方: [USAGE.md](docs/USAGE.md)
- 設計思想を知りたい: [POSITIONING.md](docs/POSITIONING.md)
