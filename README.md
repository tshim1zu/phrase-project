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

## N-gram による頻度抽出

テキストを「連続する N 文字」の断片（N-gram）に分解し、出現頻度を数える。

たとえば生成 AI に関する記事を入力すると：

```python
from japhrase import PhraseExtracter

extractor = PhraseExtracter(min_count=2, max_length=10, min_length=2)
df = extractor.extract(sentences)
```

```
     seqchar  freq
      ている    12   ← 意味のないフレーズも拾ってしまう
      生成AI    10
  大規模言語モデル     4
      である     4   ← これも意味がない
```

「生成AI」「大規模言語モデル」は見つかった。しかし「ている」「である」のような、どんなテキストにも現れる意味のないフレーズも一緒に出てくる。N-gram の頻度だけでは、意味のある結合と偶然の並びを区別できない。

## PMI による洗練

ここで PMI（自己相互情報量）を使う。PMI は「2つの要素が偶然一緒に現れる確率」と「実際に一緒に現れた頻度」の比を測る。

```python
extractor = PhraseExtracter(min_count=2, use_pmi=True)
df = extractor.extract(sentences)
```

```
     seqchar  freq    pmi
      生成AI    10   10.0   ← PMI が高い = 意味のある結合
  大規模言語モデル     4   10.0   ← 偶然ではない
      である     4    7.2   ← PMI が低い = ありふれた並び
```

PMI が高いフレーズは、構成要素が偶然隣り合っただけでは説明できないほど頻繁に共起している。「生成」と「AI」がこれほど一緒に出現するのは、それが1つの意味単位だからだ。一方「である」はどんな文脈にも現れるので PMI が低くなる。

PMI を有効にすることで、**意味的に結合したフレーズだけを上位に浮かび上がらせる**ことができる。

## 分岐エントロピーによる境界検出

さらに分岐エントロピーを加えると、フレーズの自然な切れ目を特定できる。「あるフレーズの次にどんな文字が来るか」の多様性を測り、多様性が急に上がる位置をフレーズの境界とみなす。

```python
extractor = PhraseExtracter(min_count=3, use_pmi=True, use_branching_entropy=True)
df = extractor.extract(sentences)
```

N-gram が「繰り返されているもの」を広く拾い、PMI が「意味のある結合」だけを残し、分岐エントロピーが「切れ目」を正確にする。この3層が japhrase のコアである。

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
| **CooccurrenceAnalyzer** | ターゲット語の周辺の特異語を抽出 | コンテキスト頻度、特異度スコア |
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

## 使い方を調べる

### CLI

```bash
japhrase --help                     # コマンド一覧
japhrase extract --help             # フレーズ抽出の引数とオプション
japhrase stats --help               # 統計出力の形式指定
japhrase check --help               # 文書品質チェック
japhrase detect-habits --help       # 書き癖検出
```

### Python

全クラス・全関数に docstring がある。`help()` で引数・戻り値・使用例が出る。

```python
# フレーズ抽出
from japhrase import PhraseExtracter
help(PhraseExtracter)               # クラス全体
help(PhraseExtracter.extract)       # 個別メソッド
help(PhraseExtracter.preset)        # プリセットの使い方

# 統計エンジン
from japhrase import DistributionComparator, CollocationScorer
from japhrase import StylometryAnalyzer, ComplexityAnalyzer, TemporalAnalyzer
help(DistributionComparator)        # 2テキスト間の分布比較
help(CollocationScorer)             # 語の結びつきの強さ（6指標）
help(StylometryAnalyzer)            # 語彙多様性（7指標）
help(ComplexityAnalyzer)            # テキスト複雑度
help(TemporalAnalyzer)              # 時系列分析

# 汚染検出
from japhrase.contamination import scan, quick_check, compare, batch_scan
help(scan)                          # 8軸汚染スキャン
help(quick_check)                   # True/False だけ返す最小API

# 執筆ワークフロー
from japhrase.applied import PreflightChecker, EPDashboard, PartHealthReport
help(PreflightChecker)              # 公開前品質ゲート
help(EPDashboard)                   # 話数間推移ダッシュボード
help(PartHealthReport)              # A〜E 健康診断
```

### ドキュメント

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

**japhrase** finds phrases in Japanese text without a dictionary or LLM. It scans for recurring N-gram patterns, then uses PMI (pointwise mutual information) to separate meaningful collocations from noise — "生成AI" ranks high because its components co-occur far more than chance predicts, while "である" ranks low. Branching entropy further refines phrase boundaries. On top of this core: text similarity, distribution comparison (JSD, G², keyness), collocation scoring (6 metrics), vocabulary richness (7 metrics), text complexity, and more. Pure math, no external AI, 290+ tests, numpy/scipy only.

## ライセンス

MIT License — Takeshi SHIMIZU
