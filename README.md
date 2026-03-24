# japhrase

**辞書なし・形態素解析なしで、日本語テキストから未知のフレーズを見つけ出す。**

N-gram の頻度分析と PMI（自己相互情報量）を組み合わせて、テキストの中に繰り返し出現するパターンを統計的に検出する。「生成AI」「大規模言語モデル」「プロンプトエンジニアリング」——辞書に載っていない新語・専門用語・造語でも、繰り返されていれば見つかる。

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Python 3.8+](https://img.shields.io/badge/python-3.8+-blue.svg)](https://www.python.org/downloads/)
[![PyPI](https://img.shields.io/pypi/v/japhrase)](https://pypi.org/project/japhrase/)
[![Tests](https://img.shields.io/badge/tests-290%2B%20passing-brightgreen)](https://github.com/tshim1zu/japhrase)

```bash
pip install japhrase
```

---

## フレーズ抽出（コア機能）

### 基本的な使い方

```python
from japhrase import PhraseExtracter

sentences = [
    'ChatGPTの登場により生成AIが注目を集めている。',
    '生成AIはテキストだけでなく画像生成にも使われる。',
    '大規模言語モデルは生成AIの中核技術である。',
    '生成AIの倫理的課題について議論が活発化している。',
    '企業における生成AIの導入事例が増加している。',
    # ...（以下省略）
]

extractor = PhraseExtracter(min_count=2, max_length=12, min_length=2)
df = extractor.extract(sentences)
print(df[['seqchar', 'freq']].sort_values('freq', ascending=False))
```

```
         seqchar  freq
            生成AI    45    ← 辞書に載っていない新語でも検出
    大規模言語モデル    10
プロンプトエンジニアリング     5
```

辞書を一切使わずに「生成AI」「大規模言語モデル」「プロンプトエンジニアリング」を見つけている。これが japhrase の原点。

### 仕組み

1. テキストを N-gram（連続する N 文字の断片）に分解する
2. 頻度が `min_count` 以上のものを抽出する
3. 短すぎるもの・ノイズパターンをフィルタで除去する
4. 類似フレーズを独自性スコア（originality）で統合する

オプションで PMI（自己相互情報量）や分岐エントロピーを有効にすると、統計的に有意な結合だけを残せる。

### ファイルから直接読み込む

```python
df = PhraseExtracter.from_file("input.txt", min_count=3)
```

エンコーディングは自動検出（UTF-8 / Shift-JIS / EUC-JP）。

### プリセット

テキストの種類に応じた最適パラメータが用意されている（Optuna による実験的最適化済み）。

```python
extractor = PhraseExtracter.preset('sns')     # SNS/Twitter向け（短文、高頻度）
extractor = PhraseExtracter.preset('news')    # ニュース向け（専門用語重視）
extractor = PhraseExtracter.preset('novel')   # 小説向け（繰り返し表現、長め）
extractor = PhraseExtracter.preset('report')  # レポート/論文向け（定型表現、学術用語）
```

### PMI を使った高度な抽出

PMI を有効にすると、「偶然の共起」と「意味的な結合」を区別できる。

```python
extractor = PhraseExtracter(min_count=3, use_pmi=True, use_branching_entropy=True)
df = extractor.extract(sentences)
# PMI が高い = 統計的に有意な結合（「大規模」+「言語モデル」は偶然ではない）
# PMI が低い = よくある組合せ（「である」「している」等）
```

---

## 書き癖検出

高頻度 × 低PMI = 「意味的な必然性がないのに繰り返されている」= 書き癖。

```python
from japhrase import WritingHabitDetector

text = '彼は歩いた。彼は走った。彼は止まった。彼は考えた。彼は歩いた。彼は走った。'
detector = WritingHabitDetector(min_count=2, max_pmi=5.0)
df = detector.detect(text)
print(df[['phrase', 'count', 'pmi', 'habit_score']].head())
```

```
  phrase  count   pmi  habit_score
    彼は      8  2.61       0.979    ← 最も強い書き癖
    た。      8  2.61       0.979
  た。彼は    7  2.42       0.913
```

著者比較、カテゴリ別分析（高頻度型 / 低PMI型 / バランス型）、CSV出力にも対応。

---

## 共起語分析

特定のキーワードの「周辺に特異的に出現する語」を抽出する。キャラクター分析、評判分析、トレンド背景分析に使える。

```python
from japhrase import CooccurrenceAnalyzer

analyzer = CooccurrenceAnalyzer(window_size=50)
df = analyzer.analyze(text, "機械学習", top_n=10)
```

---

## テキスト類似度・重複検出

複数テキスト間の類似度を計算。Jaccard / Levenshtein / コサイン類似度に対応。

```python
from japhrase import SimilarityAnalyzer

analyzer = SimilarityAnalyzer(method='jaccard')
matrix = analyzer.compare_files(["doc1.txt", "doc2.txt", "doc3.txt"])
pairs = analyzer.find_similar_pairs(matrix, threshold=0.7)
```

---

## 表記ゆれ検出

同じ語の表記が揺れている箇所を統計的に検出する。

```python
from japhrase import TextVariantDetector

detector = TextVariantDetector()
variants = detector.detect(text)
# → [('サーバー', 'サーバ'), ('出来る', 'できる'), ...]
```

---

## 統計的要約

LLM を使わない、PMI とエントロピーに基づく統計的要約。ハルシネーションがない。

```python
from japhrase import Summarizer

summarizer = Summarizer()
summary = summarizer.summarize(text, ratio=0.3)
```

---

## 計量文学（語彙の定量分析）

語彙の豊かさを複数の統計指標で測定する。

```python
from japhrase import StylometryAnalyzer

stylo = StylometryAnalyzer()
result = stylo.analyze_full(text)
```

| 指標 | 何を測るか |
|------|----------|
| TTR / MATTR | 語彙多様性（MATTR はテキスト長に依存しない） |
| Hapax比 | 一度しか使わなかった語の割合 |
| Brunet's W / Honoré's R | 長文に耐える語彙多様性指標 |
| Simpson's D | 「同じ語に2回当たる確率」の逆数 |
| Heaps' Law | 語彙の増加速度（β が高い = 新語が出続ける） |

---

## テキスト汚染検出（8軸異常検知）

コピペミス・文字化け・句読点の揺れ・段落重複——テキストの「壊れ」を8軸で検出する。

```python
from japhrase.contamination import scan, quick_check

# 汚染があるかどうかだけ知りたい
quick_check(text)  # → True（汚染あり）/ False

# 詳細を知りたい
profile = scan(text)
print(profile.explain())  # → 何が問題で、どこにあって、どう直すか
```

8軸: エンコーディング / 構造 / 重複 / 反復 / 分布 / 複雑度 / 一貫性（句読点揺れ）/ 言語混在

```python
# 複数テキストを一括チェック
from japhrase.contamination import batch_scan
result = batch_scan({"第1話": t1, "第2話": t2, "第3話": t3})
print(result.contaminated_keys)  # → ['第2話']

# 2テキスト間の比較
from japhrase.contamination import compare
result = compare(text_a, text_b)
print(result.report())
```

---

## 分布比較・コロケーション分析

2つのテキスト間の語彙分布の違いを精密に測定する。

```python
from japhrase import DistributionComparator
from collections import Counter

comp = DistributionComparator()
result = comp.compare(Counter(freq_a), Counter(freq_b))
print(f"JS距離: {result.jsd:.4f}")  # 0=同一、1=完全に別
print(comp.generate_report(freq_a, freq_b))
```

語の結びつきの強さを6つの指標で多角評価する。

```python
from japhrase import CollocationScorer

scorer = CollocationScorer()
df = scorer.score_phrases(df_phrases, full_text)
# → PMI, MI³, t値, z値, Log-Dice, Delta-P の6指標
```

---

## 応用機能（執筆ワークフロー）

統計エンジンを組み合わせた、原稿管理向けの高水準ツール。

| 機能 | 用途 |
|------|------|
| **PreflightChecker** | 公開前の品質ゲート（GO/NOGO判定 + 0-100スコア） |
| **EPDashboard** | 話数間の語彙推移・テンポ変化・伏線バースト検出 |
| **HabitDriftDetector** | 書き癖の時系列追跡（悪化/改善をスパークラインで可視化） |
| **JPENDivergenceChecker** | 和英翻訳の品質乖離検出 |
| **CharacterStylometry** | キャラ文体指紋・JSD分離度マトリクス |
| **PartHealthReport** | パート全体の A〜E 5段階健康診断 |

```python
from japhrase.applied import PartHealthReport

report = PartHealthReport()
grade = report.diagnose(話数テキスト群, characters=["田中", "鈴木", "佐藤"])
print(grade.report())
# → 総合: B (78.3/100)
# → 🟢 語彙健康度 A | 🟡 テンポ C | 🟢 書き癖 A | ...
```

---

## インストール

```bash
pip install japhrase                  # コア機能（numpy, pandas, scipy のみ）
pip install japhrase[viz]             # + matplotlib, seaborn（可視化）
pip install japhrase[similarity]      # + sklearn, Levenshtein（高度な類似度）
pip install japhrase[all]             # 全部入り
```

必要環境: Python 3.8+

## テスト

```bash
pytest    # 290件以上、4秒で全パス
```

## English Summary

**japhrase** is a dictionary-free Japanese phrase extraction engine. It finds unknown words, neologisms, and domain-specific terms by statistical frequency analysis (N-gram + PMI + entropy) — no morphological analysis required. Built on top of the extraction engine: writing habit detection, vocabulary richness measurement (7 metrics including Hapax, MATTR, Heaps' law), 8-axis text contamination scanning, and editorial workflow tools for serial fiction. Pure math, no LLM, 290+ tests, runs on numpy/scipy alone.

## ライセンス

MIT License — Takeshi SHIMIZU

**japhrase**: 辞書にない言葉を、統計で見つける。
