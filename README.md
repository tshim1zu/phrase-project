# japhrase

**日本語テキストの品質を、数学で測る。**

原稿を入れると、語彙の豊かさ・文章の難しさ・書き癖・キャラの混ざり具合が数字で返ってくる。LLM不要。API不要。numpy と scipy だけで動く。

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Python 3.8+](https://img.shields.io/badge/python-3.8+-blue.svg)](https://www.python.org/downloads/)
[![Tests](https://img.shields.io/badge/tests-290%2B%20passing-brightgreen)](https://github.com/tshim1zu/japhrase)

---

## これが → こうなる

### 1. 「語彙が豊かかどうか」がわかる

同じ「小説の文章」でも、語彙の豊かさはまるで違う。japhrase はそれを数値化する。

**入力A（豊かな文章）:**
> 朝靄の中を歩いていると、足元で小さな花が揺れた。名前は知らない。淡い紫色の花弁が、露を含んで光っている。道の先に古い石橋が見えた。欄干には苔が生え、長い年月を感じさせる。

**入力B（単調な文章）:**
> 男は歩いた。男は止まった。男はまた歩いた。男は見た。男は聞いた。男は歩いた。男は止まった。男はまた歩いた。

**出力:**
```
入力A → Hapax比 0.97 / Simpson多様度 0.9996 → 「語彙が非常に豊か」
入力B → Hapax比 0.00 / Simpson多様度 0.9657 → 「標準的な語彙多様性」
```

> **Hapax比** = 「一度しか使わなかった語」の割合。高いほど同じ言葉に頼っていない。

```python
from japhrase import StylometryAnalyzer

stylo = StylometryAnalyzer()
result = stylo.analyze_advanced_diversity(原稿テキスト)
print(result['hapax_ratio'])   # → 0.97
print(result['assessment'])    # → 「語彙が非常に豊か」
```

---

### 2. 「繰り返しが多すぎないか」がわかる

文章を圧縮ソフトにかけると、繰り返しが多いほどよく縮む。この原理で冗長度を測る。

**入力A（多様な文章）:**
> 朝靄の中を歩いていると、足元で小さな花が揺れた……（略）

**入力B（繰り返し文章）:**
> 男は歩いた。男は止まった。男はまた歩いた……（略）

**出力:**
```
入力A → 圧縮率 0.74 / 情報率 0.97 → 各文が新しい情報を提供している
入力B → 圧縮率 0.21 / 情報率 0.22 → 同じことの繰り返し
```

> **圧縮率が低い** = よく縮む = 同じパターンばかり。**情報率が低い** = 前の文と同じことを言い直している。

```python
from japhrase import ComplexityAnalyzer

cx = ComplexityAnalyzer()
result = cx.analyze(原稿テキスト)
print(result['compression_ratio'])  # → 0.74
print(result['information_rate'])   # → 0.97
```

---

### 3. 「この原稿、公開して大丈夫？」に答える

原稿を入れると、品質スコア (0-100) と GO / NOGO 判定が返る。

**入力A（普通の小説原稿）:**
> 朝靄の中を歩いていると……石橋を渡り、丘の上に着いた。風が強い。遠くに街の灯が見えた……（800字）

**入力B（数値だらけの原稿）:**
> 体温は36.5度に達した。心拍数は毎分90回を記録した。血圧は120mmHgである。3.5時間の経過後、速度は10km/hまで低下した……

**出力:**
```
入力A → ✅ GO (100点)   SKIP=0件 DRAG=0件
入力B → ❌ NOGO (25点)  SKIP=30件 ← 数値+単位が多すぎて読者が離脱する
```

> **SKIP** = 地の文に数値+単位がある箇所。小説の地の文に「36.5度」「120mmHg」があると読者は飛ばし読みする。

```python
from japhrase.applied import PreflightChecker

checker = PreflightChecker()
result = checker.check(原稿テキスト, lang='jp')
print(result.verdict)        # → 'GO'
print(result.quality_score)  # → 100
print(result.report())       # → 全指標の内訳
```

---

### 4. 「話数ごとに品質がどう変わったか」が見える

複数話の原稿を入れると、語彙の推移・新語の減少・隣接話の距離が一覧で出る。

**入力:**
```python
{"第1話": ep01_text, "第2話": ep02_text, "第3話": ep03_text}
```

**出力:**
```
  話数     字数  MATTR  Hapax    CR     IR   新語
  第1話     540 0.933  0.970  0.739  0.973   143
  第2話     563 0.927  0.961  0.729  0.888   110
  第3話     807 0.917  0.956  0.726  0.876   117

  語彙飽和度: 0.62 （後半で新語が減っている）
  MATTRトレンド: -0.008 （やや語彙が枯れてきている）
```

> **語彙飽和度** = 後半の新語数 / 前半の新語数。1.0 なら安定、0.5 以下なら語彙が枯渇。

```python
from japhrase.applied import EPDashboard

dashboard = EPDashboard()
result = dashboard.analyze(話数テキスト群)
print(f"語彙飽和度: {result.vocab_saturation:.2f}")
print(result.report())
```

---

### 5. 「書き癖が悪化してないか」がわかる

複数話を通して、同じ言い回しが増えているか・減っているかを追跡する。

**出力例:**
```
  悪化中の癖:
    「しかし」 freq=18 slope=+2.100 PMI=1.8 → habitual（書き癖）
        ▁▃█   ← 第3話で急増

  改善中の癖:
    「だった」 freq=12 slope=-1.500
        █▃▁   ← 第3話で減少
```

> **スパークライン**（▁▃█）は話数ごとの出現頻度の推移。右肩上がりなら悪化中。

```python
from japhrase.applied import HabitDriftDetector

detector = HabitDriftDetector()
result = detector.analyze(話数テキスト群)
print(result.report())
```

---

### 6. 「キャラの語彙が混ざってないか」がわかる

キャラ名を指定すると、台詞の語彙傾向を分析し、キャラ間の「分離度」を測る。

**出力例:**
```
  【田中】 台詞: 120字 / MATTR: 0.92 / 固有語: まあ, 別に, どうでも
  【鈴木】 台詞: 95字 / MATTR: 0.88 / 固有語: 報告, 確認, 了解
  【佐藤】 台詞: 80字 / MATTR: 0.90 / 固有語: 仮説, データ, 興味深い

  分離度マトリクス（JSD — 高いほどよく分離されている）:
            田中    鈴木    佐藤
  田中      ---    0.452   0.381
  鈴木     0.452   ---    0.510
  佐藤     0.381  0.510   ---

  ⚠ 最も混ざりやすい: 田中 ↔ 佐藤 (0.381)
```

```python
from japhrase.applied import CharacterStylometry

cs = CharacterStylometry()
fps = cs.build_fingerprints(話数テキスト群, ["田中", "鈴木", "佐藤"])
print(cs.full_report(fps))
```

---

### 7. 「パート全体の健康状態」を A〜E で診断

全話数を入れると、6項目の診断結果と改善優先度が出る。

**出力例:**
```
  【健康診断レポート】 第1部
    総合: B (78.3/100)

    🟢 語彙健康度         A (95.2)  MATTR=0.926 / Hapax=0.961
    🟡 テンポ健康度       C (62.1)  PP=1.1 / CR=0.731
    🟢 語彙成長           B (70.6)  飽和度=0.62
    🟢 書き癖負債         A (93.3)  悪化 1/15癖
    🟢 キャラ分離度       B (78.0)  最小JSD=0.381
    🟢 没入度             A (100)   SKIP=0 DRAG=0

  【改善優先度】
    1. テンポの改善（現在 C）
```

```python
from japhrase.applied import PartHealthReport

report = PartHealthReport()
grade = report.diagnose(話数テキスト群, characters=["田中", "鈴木", "佐藤"])
print(grade.report())
```

---

## その仕組み（統計エンジン一覧）

上の機能は、以下の統計エンジンの組み合わせで動いている：

| エンジン | 一言で | 指標の例 |
|---------|-------|---------|
| **StylometryAnalyzer** | 語彙の豊かさ | Hapax比、Brunet's W、MATTR、Heaps則 |
| **ComplexityAnalyzer** | 文章の難しさ | パープレキシティ、圧縮率、語彙密度 |
| **DistributionComparator** | 2文章の違い | JS距離、対数尤度比(G²)、キーネス |
| **CollocationScorer** | 語の結びつき | PMI、t値、Log-Dice、Delta-P |
| **TemporalAnalyzer** | 時系列の変化 | バースト検出、語彙飽和度 |
| **StatisticalScorer** | フレーズの有意性 | カイ二乗、相互情報量、Zipf異常 |

全て numpy + scipy のみで計算。外部サービス不要。

---

## インストール

```bash
pip install japhrase
```

必要なもの: Python 3.8+、numpy、pandas、scipy

## テスト

```bash
pytest                 # 290件以上、4秒で全パス
```

---

## English Summary

**japhrase** is a pure-math text intelligence engine for Japanese (with English support). Feed it prose and get back vocabulary richness scores, complexity metrics, writing habit drift detection, character voice separation, and publish-readiness verdicts — all computed with chi-squared tests, JSD, PMI, compression theory, and Heaps' law. No LLM, no API keys, no internet required. 290+ tests, runs on numpy/scipy alone.

## ライセンス

MIT License — 清水健

**japhrase**: 良い文章は、文字数以上の測り方に値する。
