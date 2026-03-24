# japhrase

**辞書もLLMも使わずに、テキストから単語を見つける。**

japhrase は日本語テキストの中から、繰り返し出現するフレーズを統計だけで検出する。MeCab も辞書ファイルも外部 AI も要らない。テキストを入れれば、そこに何度も現れている言葉が出てくる——それが既知語であろうと、辞書に載っていない新語・造語・専門用語であろうと関係ない。

さらに PMI（自己相互情報量）と分岐エントロピーを使うことで、「偶然の並び」と「意味のある結合」を区別できる。「大規模言語モデル」は偶然8文字並んだのではなく、意味的に結合した1つのフレーズだと、統計が教えてくれる。

この抽出エンジンを土台に、テキスト間の類似度比較・語彙分布の統計的比較・計量言語学的な多様性計測といった分析機能群を積み上げている。

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Python 3.8+](https://img.shields.io/badge/python-3.8+-blue.svg)](https://www.python.org/downloads/)
[![PyPI](https://img.shields.io/pypi/v/japhrase)](https://pypi.org/project/japhrase/)
[![Tests](https://img.shields.io/badge/tests-290%2B%20passing-brightgreen)](https://github.com/tshim1zu/japhrase)

## インストール

```bash
pip install japhrase                  # コア（numpy, pandas, scipy）
pip install japhrase[similarity]      # + sklearn, Levenshtein（高度な類似度分析）
pip install japhrase[viz]             # + matplotlib, seaborn（可視化）
pip install japhrase[all]             # 全部入り
```

## クイックスタート

```python
from japhrase import PhraseExtracter, SimilarityAnalyzer, StylometryAnalyzer

# 辞書なしでテキストから頻出フレーズを抽出
df = PhraseExtracter.from_file("input.txt")
print(df)
#          seqchar  freq
#             生成AI    45
#     大規模言語モデル    10
# プロンプトエンジニアリング   5

# 複数テキスト間の類似度を計算
matrix = SimilarityAnalyzer().compare_files(["doc1.txt", "doc2.txt", "doc3.txt"])

# 語彙の豊かさを7指標で定量化
result = StylometryAnalyzer().analyze_full(text)

# テキストの汚染（文字化け・重複・句読点揺れ等）を8軸で検出
from japhrase.contamination import scan
print(scan(text).explain())
```

## 主な機能

### フレーズ抽出（コア）

| 機能 | 説明 | 用途 |
|------|------|------|
| **PhraseExtracter** | N-gram + PMI + 分岐エントロピーによる統計的フレーズ抽出 | 未知語検出、SNS分析、キーワード発見 |
| **プリセット** | Optuna最適化済みパラメータ（sns / news / novel / report） | テキスト種別ごとの最適化 |
| **from_file** | ファイル直接読込（UTF-8 / Shift-JIS / EUC-JP 自動検出） | バッチ処理 |

```python
# PMI + 分岐エントロピーで意味的に有意な結合だけを抽出
ext = PhraseExtracter(min_count=3, use_pmi=True, use_branching_entropy=True)
df = ext.extract(sentences)

# テキスト種別に応じたプリセット
ext = PhraseExtracter.preset('news')  # ニュース向け最適パラメータ
```

### テキスト比較・分析

| 機能 | 説明 | 主な指標 |
|------|------|---------|
| **SimilarityAnalyzer** | 複数テキスト間の類似度計算 | Jaccard / Levenshtein / コサイン |
| **DistributionComparator** | 2テキスト間の語彙分布比較 | JS距離、対数尤度比(G²)、Effect Size、キーネス |
| **CollocationScorer** | 語の結びつきの強さを多角評価 | PMI、MI³、t値、z値、Log-Dice、Delta-P |
| **CooccurrenceAnalyzer** | ターゲット語の周辺に特異的に出現する語を抽出 | コンテキスト頻度、特異度スコア |
| **TemporalAnalyzer** | 複数テキスト間の時系列変化を追跡 | バースト検出、語彙飽和度、トレンド |

```python
from japhrase import DistributionComparator
from collections import Counter

# 2テキストの語彙分布がどれだけ違うかを定量化
comp = DistributionComparator()
result = comp.compare(Counter(freq_a), Counter(freq_b))
print(f"JS距離: {result.jsd:.4f}")              # 0=同一、1=完全に別
print(comp.generate_report(freq_a, freq_b))      # キーネス分析付きレポート
```

### 計量言語学

| 機能 | 説明 | 主な指標 |
|------|------|---------|
| **StylometryAnalyzer** | 語彙多様性の定量測定 | TTR、MATTR、Hapax比、Brunet's W、Honoré's R、Simpson's D、Heaps則 |
| **ComplexityAnalyzer** | テキスト複雑度・情報密度 | パープレキシティ、圧縮率、語彙密度、情報率 |
| **StatisticalScorer** | フレーズの統計的有意性評価 | カイ二乗、相互情報量、Zipf異常、Wilson信頼区間 |

```python
from japhrase import StylometryAnalyzer

stylo = StylometryAnalyzer()
adv = stylo.analyze_advanced_diversity(text)
print(f"Hapax比: {adv['hapax_ratio']}")       # 一度しか使わなかった語の割合
print(f"Simpson's D: {adv['simpsons_d']}")    # 語彙の多様度（1に近いほど多様）

mattr = stylo.analyze_mattr(text)
print(f"MATTR: {mattr['mattr']}")             # テキスト長に依存しない語彙多様性

growth = stylo.analyze_vocabulary_growth(text)
print(f"Heaps β: {growth['heaps_beta']}")     # 語彙の増加速度
```

### テキスト品質

| 機能 | 説明 | 用途 |
|------|------|------|
| **TextVariantDetector** | 表記ゆれ検出（統計ベース） | サーバー/サーバ、出来る/できる |
| **Summarizer** | 統計的要約（LLM不要、ハルシネーションなし） | テキスト圧縮、アブストラクト生成 |
| **WritingHabitDetector** | 高頻度×低PMIで書き癖を検出 | 文章癖の発見、スタイル分析 |

## その他の機能

- **汚染検出**: 文字化け・重複・句読点揺れ等を8軸で検出（`from japhrase.contamination import scan`）
- **テキストセグメンテーション**: 文書を最適な長さに自動分割
- **エンコーディング自動検出**: UTF-8 / Shift-JIS / EUC-JP を自動判別
- **ストリーミング処理**: 大規模テキストのチャンク単位処理
- **パラメータ自動最適化**: テキスト特性に応じた抽出パラメータの自動推定
- **自動インサイト生成**: 抽出結果から「何が重要か」を統計的に提示
- **NMF文書ベクトル化**: 文書-トピック行列によるテーマ分析
- **執筆ワークフロー**: 公開前品質ゲート、話数間推移、書き癖追跡、和英乖離検出、キャラ文体指紋、健康診断（`japhrase.applied`）
- **複数出力形式**: CSV、JSON、Excel、HTML レポート

## ドキュメント

| ドキュメント | 内容 |
|-----------|------|
| [USAGE.md](docs/USAGE.md) | 詳細な使用ガイド |
| [API_REFERENCE.md](docs/API_REFERENCE.md) | API リファレンス |
| [POSITIONING.md](docs/POSITIONING.md) | 設計思想と位置づけ |
| [CHANGELOG.md](docs/CHANGELOG.md) | 変更履歴 |

## テスト

```bash
pytest                            # 290件以上
pytest --cov=japhrase             # カバレッジ付き
```

## English Summary

**japhrase** is a dictionary-free Japanese text analysis engine. Core: statistical phrase extraction using N-gram + PMI + branching entropy — finds unknown words, neologisms, and domain-specific terms without morphological analysis. Built on top: text similarity (Jaccard/Levenshtein/cosine), distribution comparison (JSD, G², keyness), collocation scoring (PMI, MI³, t-score, z-score, Log-Dice, Delta-P), vocabulary richness (7 metrics: Hapax, MATTR, Brunet's W, Honoré's R, Simpson's D, Heaps' law), text complexity (perplexity, compression ratio, lexical density), 8-axis contamination detection, and editorial workflow tools. Pure math, no LLM, 290+ tests, numpy/scipy only.

## ライセンス

MIT License — Takeshi SHIMIZU
