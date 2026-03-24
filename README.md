# japhrase

**辞書もLLMも使わずに、テキストからフレーズを見つける。**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Python 3.8+](https://img.shields.io/badge/python-3.8+-blue.svg)](https://www.python.org/downloads/)
[![PyPI](https://img.shields.io/pypi/v/japhrase)](https://pypi.org/project/japhrase/)
[![Tests](https://img.shields.io/badge/tests-290%2B%20passing-brightgreen)](https://github.com/tshim1zu/japhrase)

```bash
pip install japhrase
```

## 何をするツールか

日本語テキストの中から、繰り返し出現するフレーズを統計だけで検出する。MeCab も辞書ファイルも外部 AI も要らない。テキストを入れれば、そこに何度も現れているフレーズが出てくる——それが既知語であろうと、辞書に載っていない新語・造語・専門用語であろうと関係ない。

## 方法論

### N-gram による頻度抽出

テキストを「連続する N 文字」の断片（N-gram）に分解し、その出現頻度を数える。

たとえば「大規模言語モデル」という8文字が、テキスト中に10回現れていれば、それは統計的に「頻出するフレーズ」である。この方法は辞書を参照しない。テキストの中で実際に繰り返されているかどうかだけを見る。だから、辞書に未収録の新語でも、テキスト中で使われていれば発見できる。

```python
from japhrase import PhraseExtracter

extractor = PhraseExtracter(min_count=2, max_length=12, min_length=2)
df = extractor.extract(sentences)
```

```
         seqchar  freq
            生成AI    45    ← 辞書にない新語でも頻度があれば見つかる
    大規模言語モデル    10
プロンプトエンジニアリング     5
```

ただし、N-gram の頻度だけでは「である」「している」のような意味のないフレーズも大量に拾ってしまう。そこで PMI を使う。

### PMI（自己相互情報量）による洗練

PMI は「2つの要素が偶然一緒に現れる確率」と「実際に一緒に現れた頻度」の比を測る指標である。

- **PMI が高い** = 偶然では説明できないほど一緒に出現している = 意味的に結合している
  - 例：「大規模」と「言語モデル」→ 個別の出現確率から期待される共起よりはるかに多い → PMI 高い
- **PMI が低い** = たまたま隣り合っているだけ
  - 例：「である」→ どんな文脈でも出現する → PMI 低い

```python
extractor = PhraseExtracter(min_count=3, use_pmi=True)
df = extractor.extract(sentences)
# PMI が高いフレーズだけが残る → 意味のある結合だけを抽出
```

### 分岐エントロピーによる境界検出

分岐エントロピーは「あるフレーズの次に来る文字の多様性」を測る。フレーズの境界（切れ目）では、次に来る文字の種類が急に増える。この性質を使って、フレーズの自然な切れ目を統計的に特定する。

```python
extractor = PhraseExtracter(min_count=3, use_pmi=True, use_branching_entropy=True)
```

### 3つの方法論の関係

```
N-gram 頻度 → 「繰り返されているもの」を見つける（広く拾う）
     ↓
PMI → 「意味のある結合」だけを残す（精度を上げる）
     ↓
分岐エントロピー → 「フレーズの自然な境界」を特定する（切れ目を正確にする）
```

この3層が japhrase のコアである。

## プリセット

テキスト種別ごとに最適化されたパラメータセット（Optuna による実験的最適化済み）。

```python
extractor = PhraseExtracter.preset('sns')     # SNS/Twitter（短文・高頻度）
extractor = PhraseExtracter.preset('news')    # ニュース（専門用語重視）
extractor = PhraseExtracter.preset('novel')   # 小説（繰り返し表現・長め）
extractor = PhraseExtracter.preset('report')  # 論文/レポート（定型・学術用語）
```

## ファイルからの読み込み

```python
df = PhraseExtracter.from_file("input.txt")
# エンコーディング自動検出（UTF-8 / Shift-JIS / EUC-JP）
```

## コアの上に積み上げた分析機能

フレーズ抽出エンジンを土台として、以下の統計分析機能を提供する。

### テキスト比較・分析

| 機能 | 説明 | 主な指標 |
|------|------|---------|
| **SimilarityAnalyzer** | 複数テキスト間の類似度計算 | Jaccard / Levenshtein / コサイン |
| **DistributionComparator** | 2テキスト間の語彙分布比較 | JS距離、対数尤度比(G²)、Effect Size、キーネス |
| **CollocationScorer** | 語の結びつきの強さを多角評価 | PMI、MI³、t値、z値、Log-Dice、Delta-P |
| **CooccurrenceAnalyzer** | ターゲット語の周辺に特異的に出現する語を抽出 | コンテキスト頻度、特異度スコア |
| **TemporalAnalyzer** | 複数テキスト間の時系列変化を追跡 | バースト検出、語彙飽和度、トレンド |

### 計量言語学

| 機能 | 説明 | 主な指標 |
|------|------|---------|
| **StylometryAnalyzer** | 語彙多様性の定量測定 | TTR、MATTR、Hapax比、Brunet's W、Honoré's R、Simpson's D、Heaps則 |
| **ComplexityAnalyzer** | テキスト複雑度・情報密度 | パープレキシティ、圧縮率、語彙密度、情報率 |
| **StatisticalScorer** | フレーズの統計的有意性評価 | カイ二乗、相互情報量、Zipf異常、Wilson信頼区間 |

### テキスト品質

| 機能 | 説明 |
|------|------|
| **TextVariantDetector** | 表記ゆれ検出（サーバー/サーバ、出来る/できる） |
| **WritingHabitDetector** | 高頻度×低PMIで書き癖を検出 |
| **Summarizer** | 統計的要約（LLM不要） |

### その他

- **汚染検出**: 文字化け・重複・句読点揺れ等を8軸で検出（`japhrase.contamination`）
- **執筆ワークフロー**: 公開前品質ゲート、話数間推移、キャラ文体指紋等（`japhrase.applied`）
- **NMF文書ベクトル化** / **ストリーミング処理** / **パラメータ自動最適化** / **自動インサイト生成**
- **出力形式**: CSV、JSON、Excel、HTML レポート

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
pytest                   # 290件以上
pytest --cov=japhrase    # カバレッジ付き
```

## English Summary

**japhrase** finds phrases in Japanese text without a dictionary or LLM. It uses N-gram frequency analysis to detect recurring patterns, then refines results with PMI (pointwise mutual information) to distinguish meaningful collocations from noise, and branching entropy to identify natural phrase boundaries. On top of this extraction core: text similarity, distribution comparison (JSD, G², keyness), collocation scoring (6 metrics), vocabulary richness (7 metrics), text complexity, and more. Pure math, no external AI, 290+ tests, numpy/scipy only.

## ライセンス

MIT License — Takeshi SHIMIZU
