# japhrase

**統計的日本語テキスト分析エンジン：辞書なし・形態素解析なしのフレーズ抽出と計量言語学ツール**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Python 3.8+](https://img.shields.io/badge/python-3.8+-blue.svg)](https://www.python.org/downloads/)
[![PyPI](https://img.shields.io/pypi/v/japhrase)](https://pypi.org/project/japhrase/)
[![Tests](https://img.shields.io/badge/tests-290%2B%20passing-brightgreen)](https://github.com/tshim1zu/japhrase)

N-gram の頻度分析と PMI（自己相互情報量）を軸に、日本語テキストの中に潜むパターンを統計的に検出する。辞書に載っていない新語・専門用語・造語でも、繰り返されていれば見つかる。その上に、類似度分析・分布比較・語彙多様性計測・汚染検出といった計量言語学ツール群を備える。外部AI不要。numpy + scipy だけで動く。

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

### 汚染検出（8軸異常検知）

テキストの物理的破損・混入・不整合を8つの独立した検出器で多角的に評価する。

| 軸 | 検出対象 | 確度 |
|----|---------|------|
| **encoding** | 文字化け、不正Unicode、制御文字 | 確定的 |
| **structural** | 括弧不整合、マージ痕、メタデータ混入、作業注釈残存、空行過多 | 確定的 |
| **duplicate** | 段落/文の完全一致・ほぼ一致の重複（テキスト内・テキスト間） | 確定的 |
| **repetition** | 短区間内のフレーズ異常反復 | 高い |
| **distribution** | 語彙分布の断絶、外来語彙の局所集中 | 中程度 |
| **complexity** | 圧縮率の局所異常（極端な繰り返し / ランダムデータ） | 中程度 |
| **consistency** | 句読点揺れ（、vs,/。vs.）、漢字/ひらがな揺れ、カタカナ長音揺れ | 中程度 |
| **language** | 言語ブロック混在（日本語中に突然英語ブロック等） | 中程度 |

```python
from japhrase.contamination import scan, quick_check, batch_scan, compare

# 汚染あり？
quick_check(text)                           # True / False

# 詳細：何が問題で、どこにあって、どう直すか
profile = scan(text)
print(profile.explain())

# 複数テキストを一括チェック
result = batch_scan({"ch1": t1, "ch2": t2, "ch3": t3})
print(result.contaminated_keys)             # 汚染ありのキーリスト

# 2テキスト間の比較
result = compare(text_a, text_b)
print(result.report())
```

### 応用：執筆ワークフロー

統計エンジンを組み合わせた、連載原稿管理向けの高水準ツール群。

| 機能 | 説明 |
|------|------|
| **PreflightChecker** | 公開前の品質ゲート（GO / WARN / NOGO 判定 + 0-100スコア） |
| **EPDashboard** | 話数間の語彙推移・テンポ変化・伏線バースト検出 |
| **HabitDriftDetector** | 書き癖の時系列追跡（悪化/改善のスパークライン可視化） |
| **JPENDivergenceChecker** | 和英翻訳の品質乖離検出（話数ごとの翻訳ロス率） |
| **CharacterStylometry** | キャラクター文体指紋・JSD分離度マトリクス |
| **PartHealthReport** | パート全体の A〜E 5段階健康診断（6項目 + 改善優先度） |

```python
from japhrase.applied import PartHealthReport

grade = PartHealthReport().diagnose(
    {"ch1": t1, "ch2": t2, "ch3": t3},
    characters=["田中", "鈴木", "佐藤"],
)
print(grade.report())
# → 総合: B (78.3/100)
# → 🟢 語彙健康度 A (95.2) | 🟡 テンポ C (62.1) | 🟢 書き癖 A (93.3) | ...
```

## その他の機能

- **テキストセグメンテーション**: 文書を最適な長さに自動分割
- **エンコーディング自動検出**: UTF-8 / Shift-JIS / EUC-JP を自動判別
- **ストリーミング処理**: 大規模テキストのチャンク単位処理
- **パラメータ自動最適化**: テキスト特性に応じた抽出パラメータの自動推定
- **自動インサイト生成**: 抽出結果から「何が重要か」を統計的に提示
- **NMF文書ベクトル化**: 文書-トピック行列によるテーマ分析
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
