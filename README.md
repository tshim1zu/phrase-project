# japhrase

**統計的日本語テキスト分析エンジン：辞書なし・形態素解析なしのフレーズ抽出と計量言語学ツール**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Python 3.8+](https://img.shields.io/badge/python-3.8+-blue.svg)](https://www.python.org/downloads/)
[![PyPI](https://img.shields.io/pypi/v/japhrase)](https://pypi.org/project/japhrase/)
[![Tests](https://img.shields.io/badge/tests-290%2B%20passing-brightgreen)](https://github.com/tshim1zu/japhrase)

## クイックスタート

```bash
pip install japhrase
```

```python
from japhrase import PhraseExtracter, SimilarityAnalyzer, StylometryAnalyzer

# 1. フレーズ抽出（辞書不要で未知語を検出）
df = PhraseExtracter.from_file("input.txt")
print(df)

# 2. テキスト間の類似度
analyzer = SimilarityAnalyzer()
matrix = analyzer.compare_files(["doc1.txt", "doc2.txt"])

# 3. 語彙の豊かさを測定
stylo = StylometryAnalyzer()
result = stylo.analyze_full(text)

# 4. テキストの汚染を検出
from japhrase.contamination import scan
print(scan(text).explain())
```

## 機能一覧

### コア：フレーズ抽出

| 機能 | 説明 | 用途 |
|------|------|------|
| **PhraseExtracter** | N-gram + PMI + 分岐エントロピーによるフレーズ抽出 | 未知語検出、SNS分析、キーワード発見 |
| **プリセット** | Optuna最適化済みパラメータ（sns / news / novel / report） | テキスト種別ごとの最適抽出 |
| **from_file** | ファイル直接読込（エンコーディング自動検出） | UTF-8 / Shift-JIS / EUC-JP 対応 |

### テキスト比較・分析

| 機能 | 説明 | 用途 |
|------|------|------|
| **SimilarityAnalyzer** | Jaccard / Levenshtein / コサイン類似度 | コピペ検出、重複分析 |
| **DistributionComparator** | JS距離、対数尤度比(G²)、Effect Size、キーネス | 2テキスト間の語彙分布比較 |
| **CollocationScorer** | PMI、MI³、t値、z値、Log-Dice、Delta-P | 語の結びつきの強さ（6指標） |
| **CooccurrenceAnalyzer** | ターゲット語の周辺に特異的に出現する語を抽出 | 共起語分析、キャラ分析、評判分析 |
| **TemporalAnalyzer** | バースト検出、語彙飽和度、トレンド追跡 | 複数テキスト間の時系列変化 |

### 計量言語学

| 機能 | 説明 | 指標 |
|------|------|------|
| **StylometryAnalyzer** | 語彙多様性の定量測定 | TTR、MATTR、Hapax比、Brunet's W、Honoré's R、Simpson's D、Heaps則 |
| **ComplexityAnalyzer** | テキスト複雑度・情報密度 | パープレキシティ、圧縮率、語彙密度、情報率 |
| **StatisticalScorer** | フレーズの統計的有意性 | カイ二乗、相互情報量、Zipf異常、信頼区間 |

### テキスト品質

| 機能 | 説明 | 用途 |
|------|------|------|
| **TextVariantDetector** | 表記ゆれ検出 | サーバー/サーバ、出来る/できる |
| **Summarizer** | 統計的要約（LLM不要、ハルシネーションなし） | テキスト圧縮 |
| **WritingHabitDetector** | 高頻度×低PMIで書き癖を検出 | 文章癖の発見 |

### 汚染検出（8軸異常検知）

| 軸 | 検出対象 |
|----|---------|
| encoding | 文字化け、不正Unicode |
| structural | 括弧不整合、マージ痕、メタデータ混入、作業注釈、空行過多 |
| duplicate | 段落/文の重複（テキスト内・テキスト間） |
| repetition | フレーズの異常反復 |
| distribution | 分布断絶、外来語彙の局所集中 |
| complexity | 圧縮率の局所異常 |
| consistency | 句読点揺れ（、vs,）、漢字/ひらがな揺れ |
| language | 言語ブロック混在 |

```python
from japhrase.contamination import scan, quick_check, batch_scan, compare

quick_check(text)                    # True / False
scan(text).explain()                 # 何が問題で、どこで、どう直すか
batch_scan({"ch1": t1, "ch2": t2})   # 一括スキャン
compare(text_a, text_b)              # テキスト間比較
```

### 応用：執筆ワークフロー

| 機能 | 用途 |
|------|------|
| **PreflightChecker** | 公開前の品質ゲート（GO/NOGO + 0-100スコア） |
| **EPDashboard** | 話数間の語彙推移・テンポ変化 |
| **HabitDriftDetector** | 書き癖の時系列追跡 |
| **JPENDivergenceChecker** | 和英翻訳の品質乖離検出 |
| **CharacterStylometry** | キャラ文体指紋・JSD分離度マトリクス |
| **PartHealthReport** | A〜E 5段階の健康診断 |

## 用途別ガイド

### フレーズ抽出

```python
from japhrase import PhraseExtracter

# プリセットを使う
extractor = PhraseExtracter.preset('news')
df = extractor.extract(sentences)

# PMI + 分岐エントロピーで高精度抽出
extractor = PhraseExtracter(min_count=3, use_pmi=True, use_branching_entropy=True)
df = extractor.extract(sentences)
```

### テキスト比較

```python
from japhrase import DistributionComparator
from collections import Counter

comp = DistributionComparator()
result = comp.compare(Counter(freq_a), Counter(freq_b))
print(f"JS距離: {result.jsd:.4f}")
print(comp.generate_report(freq_a, freq_b))
```

### 語彙分析

```python
from japhrase import StylometryAnalyzer

stylo = StylometryAnalyzer()
adv = stylo.analyze_advanced_diversity(text)
print(f"Hapax比: {adv['hapax_ratio']}")
print(f"MATTR: {stylo.analyze_mattr(text)['mattr']}")
print(f"Heaps β: {stylo.analyze_vocabulary_growth(text)['heaps_beta']}")
```

### 健康診断（連載原稿）

```python
from japhrase.applied import PartHealthReport

grade = PartHealthReport().diagnose(
    {"ch1": t1, "ch2": t2, "ch3": t3},
    characters=["田中", "鈴木", "佐藤"],
)
print(grade.report())
```

## インストール

```bash
pip install japhrase                  # コア（numpy, pandas, scipy）
pip install japhrase[similarity]      # + sklearn, Levenshtein
pip install japhrase[viz]             # + matplotlib, seaborn
pip install japhrase[all]             # 全部入り
```

Python 3.8+

## ドキュメント

| ドキュメント | 内容 |
|-----------|------|
| [USAGE.md](docs/USAGE.md) | 詳細な使用ガイド |
| [API_REFERENCE.md](docs/API_REFERENCE.md) | API リファレンス |
| [POSITIONING.md](docs/POSITIONING.md) | 設計思想と位置づけ |

## テスト

```bash
pytest    # 290件以上
```

## English Summary

**japhrase** is a dictionary-free Japanese text analysis engine. Core feature: statistical phrase extraction using N-gram + PMI + branching entropy — finds unknown words, neologisms, and domain-specific terms without morphological analysis. Also provides: text similarity, distribution comparison (JSD, G², keyness), collocation scoring (6 metrics), vocabulary richness (7 metrics), text complexity, 8-axis contamination detection, and editorial workflow tools. Pure math, no LLM, 290+ tests, numpy/scipy only.

## ライセンス

MIT License — Takeshi SHIMIZU
