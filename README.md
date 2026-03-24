# japhrase

**日本語テキストから「見えないパターン」を統計で炙り出す。**

形態素解析なし。LLM不要。API不要。N-gram と PMI と情報理論だけで、テキストの中に潜む頻出フレーズ・書き癖・語彙の偏り・テキストの破損を検出する。

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Python 3.8+](https://img.shields.io/badge/python-3.8+-blue.svg)](https://www.python.org/downloads/)
[![PyPI](https://img.shields.io/pypi/v/japhrase)](https://pypi.org/project/japhrase/)
[![Tests](https://img.shields.io/badge/tests-290%2B%20passing-brightgreen)](https://github.com/tshim1zu/japhrase)

```bash
pip install japhrase
```

---

## これが → こうなる

### 1. テキストから頻出フレーズを抽出する

japhrase の原点。テキストを入れると、統計的に有意な繰り返しフレーズが出てくる。辞書不要・形態素解析不要。N-gram の頻度と PMI（自己相互情報量）で判定する。

**入力:**
```python
from japhrase import PhraseExtracter

sentences = [
    '機械学習は最近注目されている技術です。',
    '機械学習には様々な手法があります。',
    '深層学習は機械学習の一分野です。',
    '自然言語処理は機械学習の応用分野です。',
]
ext = PhraseExtracter(min_count=2, max_length=10)
df = ext.get_dfphrase(sentences)
print(df[['seqchar', 'freq']])
```

**出力:**
```
  seqchar  freq
  機械学習     4
```

> 4回出現する「機械学習」が自動で抽出される。辞書に載っていない造語や業界用語でも、繰り返されていれば見つかる。

---

### 2. 書き癖を検出する

高頻度なのに意味的結合が弱い（PMI が低い）フレーズ = 書き癖。無意識に繰り返している言い回しを炙り出す。

**入力:**
```python
from japhrase import WritingHabitDetector

text = '彼は歩いた。彼は走った。彼は止まった。彼は考えた。彼は歩いた。彼は走った。'
detector = WritingHabitDetector(min_count=2, max_pmi=5.0)
df = detector.detect(text)
print(df[['phrase', 'count', 'pmi', 'habit_score']].head())
```

**出力:**
```
  phrase  count   pmi  habit_score
    彼は      8  2.61       0.979    ← 最も強い書き癖
    た。      8  2.61       0.979
  た。彼は    7  2.42       0.913
```

> 「彼は」が8回。PMI が低い = 「彼」と「は」の結びつきに意味的な必然性がない = 無意識の繰り返し。

---

### 3. 語彙の豊かさを測る

同じ長さの文章でも、語彙の多様性はまるで違う。7つの統計指標で定量化する。

**入力A（豊かな文章）:**
> 朝靄の中を歩いていると、足元で小さな花が揺れた。名前は知らない。淡い紫色の花弁が、露を含んで光っている。

**入力B（単調な文章）:**
> 男は歩いた。男は止まった。男はまた歩いた。男は見た。男は聞いた。

**出力:**
```
入力A → Hapax比 0.97 / Simpson 0.9996 → 「語彙が非常に豊か」
入力B → Hapax比 0.00 / Simpson 0.9657 → 「単調」
```

> **Hapax比** = 一度しか使わなかった語の割合。高いほど同じ言葉に頼っていない。

```python
from japhrase import StylometryAnalyzer

stylo = StylometryAnalyzer()
result = stylo.analyze_advanced_diversity(text)
print(result['hapax_ratio'])    # → 0.97
print(result['assessment'])     # → 「語彙が非常に豊か」
```

---

### 4. テキストの汚染を検出する

コピペミス・文字化け・句読点の揺れ・段落重複——テキストの「壊れ」を8軸で多角的に検出する。

**入力:**
```python
from japhrase.contamination import scan

text = '正常な文章。\nâ€™ここに文字化け。\n「閉じない台詞'
profile = scan(text)
```

**出力:**
```
print(profile.overall)    # → 16/100（汚染あり）
print(profile.explain())
# → ⚠️ 汚染スコア: 16/100
# → ■ エンコーディング (55/100, 1件)
# →   L2: mojibakeパターン: 'â€™'
# →   💡 文字化け箇所を正しい文字に置換してください。
# → ■ 構造 (39/100, 1件)
# →   L3: 開き括弧「が閉じていない
# →   💡 括弧の対応を確認してください。
```

> 8軸: エンコーディング / 構造 / 重複 / 反復 / 分布 / 複雑度 / 一貫性（句読点揺れ）/ 言語混在

```python
# もっと簡単に
from japhrase.contamination import quick_check
quick_check(text)  # → True（汚染あり）

# 複数テキストを一括チェック
from japhrase.contamination import batch_scan
result = batch_scan({"第1話": t1, "第2話": t2, "第3話": t3})
print(result.contaminated_keys)  # → ['第2話']
```

---

### 5. この原稿、公開して大丈夫？

品質スコア (0-100) と GO / NOGO 判定を出す。

```python
from japhrase.applied import PreflightChecker

checker = PreflightChecker()
result = checker.check(原稿, lang='jp')
print(result.verdict)        # → 'GO'
print(result.quality_score)  # → 100
```

```
普通の原稿 → ✅ GO (100点)
数値だらけ → ❌ NOGO (25点)  SKIP=30件
```

---

### 6. 話数を追うごとに品質がどう変わったか

```python
from japhrase.applied import EPDashboard

dashboard = EPDashboard()
result = dashboard.analyze({"第1話": t1, "第2話": t2, "第3話": t3})
print(f"語彙飽和度: {result.vocab_saturation:.2f}")
print(result.report())
```

```
  話数    MATTR  Hapax   新語
  第1話  0.933  0.970   143
  第2話  0.927  0.961   110   ← 新語が減っている
  第3話  0.917  0.956   117

  語彙飽和度: 0.62（後半で新語が減少）
```

---

### 7. パート全体を A〜E で診断

```python
from japhrase.applied import PartHealthReport

report = PartHealthReport()
grade = report.diagnose(話数テキスト群, characters=["田中", "鈴木", "佐藤"])
print(grade.report())
```

```
  総合: B (78.3/100)
  🟢 語彙健康度   A (95.2)
  🟡 テンポ       C (62.1)
  🟢 書き癖負債   A (93.3)
  🟢 キャラ分離度 B (78.0)
```

---

## 全機能一覧

### コア（フレーズ抽出・テキストマイニング）

| 機能 | 説明 |
|------|------|
| **PhraseExtracter** | N-gram + PMI ベースの頻出フレーズ抽出（原点） |
| **CooccurrenceAnalyzer** | 特定語の周辺に出現する共起語を分析 |
| **WritingHabitDetector** | 高頻度×低PMI で書き癖を自動検出 |
| **SimilarityAnalyzer** | Jaccard / Levenshtein / コサイン類似度 |
| **Summarizer** | 統計ベースのテキスト要約 |
| **TextVariantDetector** | 表記ゆれの検出 |

### 統計エンジン（計量言語学）

| 機能 | 指標 |
|------|------|
| **StylometryAnalyzer** | Hapax比, Brunet's W, Honoré's R, Simpson's D, MATTR, Heaps則 |
| **ComplexityAnalyzer** | パープレキシティ, 圧縮率, 語彙密度, 情報率 |
| **DistributionComparator** | JS距離, 対数尤度比(G²), キーネス, Effect Size |
| **CollocationScorer** | PMI, MI³, t値, z値, Log-Dice, Delta-P |
| **TemporalAnalyzer** | バースト検出, 語彙飽和度, トレンド追跡 |
| **StatisticalScorer** | カイ二乗, 相互情報量, Zipf異常, 信頼区間 |

### 汚染検出（8軸異常検知）

| 軸 | 検出対象 |
|----|---------|
| **encoding** | 文字化け・制御文字 |
| **structural** | 括弧不整合・マージ痕・メタデータ混入・作業注釈・空行過多 |
| **duplicate** | 段落/文の重複 |
| **repetition** | フレーズの異常反復 |
| **distribution** | 分布断絶・外来語彙 |
| **complexity** | 圧縮率の局所異常 |
| **consistency** | 句読点揺れ（、vs,）・漢字/ひらがな揺れ |
| **language** | 言語ブロック混在 |

### 応用機能（執筆ワークフロー）

| 機能 | 用途 |
|------|------|
| **PreflightChecker** | 公開前の品質ゲート（GO/NOGO判定） |
| **EPDashboard** | 話数間の語彙推移・テンポ変化 |
| **HabitDriftDetector** | 書き癖の時系列追跡 |
| **JPENDivergenceChecker** | 和英翻訳の品質乖離検出 |
| **CharacterStylometry** | キャラ文体指紋・分離度マトリクス |
| **PartHealthReport** | パート全体の A〜E 健康診断 |

---

## インストール

```bash
pip install japhrase                  # コア機能
pip install japhrase[viz]             # + matplotlib, seaborn
pip install japhrase[similarity]      # + sklearn, Levenshtein
pip install japhrase[all]             # 全部入り
```

必要環境: Python 3.8+

## テスト

```bash
pytest    # 290件以上、4秒で全パス
```

---

## English Summary

**japhrase** is a pure-math Japanese text analysis engine. It extracts frequent phrases using N-gram + PMI (no morphological analysis needed), detects writing habits, measures vocabulary richness (Hapax, MATTR, Heaps' law), and scans for 8 types of text contamination (encoding errors, duplicates, punctuation inconsistency, etc.) — all without LLMs or external APIs. 290+ tests, runs on numpy/scipy alone.

## ライセンス

MIT License — 清水健

**japhrase**: テキストの中に隠れたパターンを、数学で見つける。
