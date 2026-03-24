# japhrase

**辞書なし・形態素解析なしで、日本語テキストから未知のフレーズを見つけ出す。**

N-gram の頻度分析と PMI（自己相互情報量）を組み合わせて、テキストの中に繰り返し出現するパターンを統計的に検出する。「生成AI」「大規模言語モデル」「プロンプトエンジニアリング」——辞書に載っていない新語・専門用語・造語でも、繰り返されていれば見つかる。

その上に、テキスト間の類似度・語彙分布の比較・結合度分析・語彙多様性計測・情報密度計測・汚染検出といった計量言語学の統計ツール群を備える。全て numpy + scipy だけで動く。LLM 不要。

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Python 3.8+](https://img.shields.io/badge/python-3.8+-blue.svg)](https://www.python.org/downloads/)
[![PyPI](https://img.shields.io/pypi/v/japhrase)](https://pypi.org/project/japhrase/)
[![Tests](https://img.shields.io/badge/tests-290%2B%20passing-brightgreen)](https://github.com/tshim1zu/japhrase)

```bash
pip install japhrase
```

---

## 1. フレーズ抽出

japhrase の原点。辞書を使わずに、テキスト中の頻出パターンを統計で見つける。

```python
from japhrase import PhraseExtracter

sentences = [
    'ChatGPTの登場により生成AIが注目を集めている。',
    '大規模言語モデルは生成AIの中核技術である。',
    '企業における生成AIの導入事例が増加している。',
    # ...
]

extractor = PhraseExtracter(min_count=2, max_length=12, min_length=2)
df = extractor.extract(sentences)
```

```
         seqchar  freq
            生成AI    45    ← 辞書にない新語でも検出
    大規模言語モデル    10
プロンプトエンジニアリング     5
```

### 仕組み

1. テキストを N-gram（連続する N 文字の断片）に分解
2. 頻度が閾値以上のものを抽出
3. ノイズパターンをフィルタで除去
4. 類似フレーズを独自性スコア（originality）で統合

### プリセット

テキスト種別ごとの最適パラメータ（Optuna 実験済み）。

```python
extractor = PhraseExtracter.preset('sns')     # SNS/Twitter（短文・高頻度）
extractor = PhraseExtracter.preset('news')    # ニュース（専門用語重視）
extractor = PhraseExtracter.preset('novel')   # 小説（繰り返し表現・長め）
extractor = PhraseExtracter.preset('report')  # 論文/レポート（定型・学術用語）
```

### PMI・分岐エントロピーによる高度な抽出

```python
extractor = PhraseExtracter(min_count=3, use_pmi=True, use_branching_entropy=True)
```

PMI が高い = 統計的に有意な結合（「大規模」+「言語モデル」は偶然ではない）。PMI が低い = ありふれた組合せ（「である」「している」等）。

---

## 2. テキスト類似度・重複検出

複数テキスト間の類似度を計算し、コピペ検出・重複分析を行う。

```python
from japhrase import SimilarityAnalyzer

analyzer = SimilarityAnalyzer(method='jaccard')  # or 'levenshtein', 'cosine', 'auto'
matrix = analyzer.compare_files(["doc1.txt", "doc2.txt", "doc3.txt"])
pairs = analyzer.find_similar_pairs(matrix, threshold=0.7)
```

---

## 3. 分布比較（2テキスト間の語彙の違い）

2つのテキストの語彙分布を統計的に比較する。対数尤度比（G²）、JS 距離、効果量（Log Ratio）、キーネス分析を一括で実行。

```python
from japhrase import DistributionComparator
from collections import Counter

comp = DistributionComparator()
result = comp.compare(Counter(freq_a), Counter(freq_b))
print(f"JS距離: {result.jsd:.4f}")        # 0 = 同一、1 = 完全に別
print(comp.generate_report(freq_a, freq_b))
```

「テキスト A にだけ多い語」「テキスト B にだけ多い語」をキーネススコア付きで抽出できる。

---

## 4. コロケーション分析（語の結びつきの強さ）

1つのフレーズを構成する語同士がどれだけ強く結びついているかを6つの指標で測る。

```python
from japhrase import CollocationScorer

scorer = CollocationScorer()
df = scorer.score_phrases(df_phrases, full_text)
# → PMI, MI³, t値, z値, Log-Dice, Delta-P
```

| 指標 | 特性 |
|------|------|
| PMI | 低頻度の結合に敏感（レアな組合せを発見） |
| t-score | 高頻度の結合に敏感（よくある組合せを重視） |
| Log-Dice | コーパスサイズに依存しない（最も安定） |
| Delta-P | 方向性がある（A→B と B→A の非対称性を測る） |

---

## 5. 共起語分析

特定のキーワードの周辺に特異的に出現する語を抽出する。

```python
from japhrase import CooccurrenceAnalyzer

analyzer = CooccurrenceAnalyzer(window_size=50)
df = analyzer.analyze(text, "機械学習", top_n=10)
```

---

## 6. 語彙多様性（計量文学）

テキストの語彙の豊かさを複数の統計指標で定量化する。

```python
from japhrase import StylometryAnalyzer

stylo = StylometryAnalyzer()
result = stylo.analyze_full(text)
```

| 指標 | 何を測るか |
|------|----------|
| TTR / MATTR | 語彙多様性（MATTR はテキスト長に依存しない） |
| Hapax比 | 一度しか使わなかった語の割合（高い = 洗練） |
| Brunet's W / Honoré's R | 長文に耐える多様性指標 |
| Simpson's D | 同じ語に2回当たる確率の逆数 |
| Heaps' Law (β) | 語彙の増加速度（高い = 新語が出続ける） |

---

## 7. テキスト複雑度・情報密度

テキストの「難しさ」「密度」「冗長度」を複数の角度から測る。

```python
from japhrase import ComplexityAnalyzer

cx = ComplexityAnalyzer()
result = cx.analyze(text)
# → perplexity, compression_ratio, lexical_density, information_rate
```

| 指標 | 意味 |
|------|------|
| パープレキシティ | N-gram の予測困難度（高い = 多様・難解） |
| 圧縮率 | zlib 圧縮後のサイズ比（低い = 繰り返しが多い） |
| 語彙密度 | 内容語 / 全語の比率（高い = 情報が詰まっている） |
| 情報率 | 各文が提供する新情報の割合 |

---

## 8. 表記ゆれ検出

同じ語の表記が揺れている箇所を検出する。

```python
from japhrase import TextVariantDetector

detector = TextVariantDetector()
variants = detector.detect(text)
```

---

## 9. 時系列分析（複数テキスト間の推移）

複数テキストを時系列として並べ、語彙飽和度・トレンド・バースト（突発的使用）を追跡する。

```python
from japhrase import TemporalAnalyzer

ta = TemporalAnalyzer()
result = ta.analyze_series(docs, labels)
bursts = ta.detect_bursts(docs)
```

---

## 10. テキスト汚染検出（8軸異常検知）

テキストの物理的破損・混入・不整合を8軸で検出する。

```python
from japhrase.contamination import scan, quick_check, batch_scan, compare

quick_check(text)         # → True / False
profile = scan(text)
print(profile.explain())  # → 何が問題で、どこにあって、どう直すか
```

8軸: エンコーディング / 構造 / 重複 / 反復 / 分布 / 複雑度 / 一貫性 / 言語混在

---

## 11. 統計的要約

PMI とエントロピーに基づく統計的要約。LLM を使わない。ハルシネーションがない。

```python
from japhrase import Summarizer
summary = Summarizer().summarize(text, ratio=0.3)
```

---

## 12. 執筆ワークフロー向け応用機能

上記の統計エンジンを組み合わせた、連載原稿管理向けのツール群。

| 機能 | 用途 |
|------|------|
| **PreflightChecker** | 公開前の品質ゲート（GO/NOGO + 0-100スコア） |
| **EPDashboard** | 話数間の語彙推移・テンポ変化 |
| **HabitDriftDetector** | 書き癖の時系列追跡 |
| **JPENDivergenceChecker** | 和英翻訳の品質乖離検出 |
| **CharacterStylometry** | キャラ文体指紋・分離度マトリクス |
| **PartHealthReport** | A〜E 5段階の健康診断 |
| **WritingHabitDetector** | 高頻度×低PMI による書き癖検出 |

---

## インストール

```bash
pip install japhrase                  # コア（numpy, pandas, scipy）
pip install japhrase[similarity]      # + sklearn, Levenshtein
pip install japhrase[viz]             # + matplotlib, seaborn
pip install japhrase[all]             # 全部入り
```

Python 3.8+

## テスト

```bash
pytest    # 290件以上、4秒で全パス
```

## English Summary

**japhrase** is a dictionary-free Japanese phrase extraction engine. It finds unknown words, neologisms, and domain-specific terms by statistical frequency analysis (N-gram + PMI + branching entropy) — no morphological analysis required. On top of the extraction core: text similarity, distribution comparison (JSD, G², keyness), collocation scoring (PMI, t-score, Log-Dice, Delta-P), vocabulary richness (7 metrics), text complexity (perplexity, compression), 8-axis contamination detection, and temporal analysis across document series. Pure math, no LLM, 290+ tests, runs on numpy/scipy alone.

## ライセンス

MIT License — Takeshi SHIMIZU

**japhrase**: 辞書にない言葉を、統計で見つける。
