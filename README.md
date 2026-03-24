# japhrase

**日本語テキストの品質を数学で測る統計エンジン**

語彙の豊かさ、文章の複雑度、書き癖の変化、キャラクターの文体差——すべてを統計指標で定量化する。LLM不要・API不要・ネット不要。純粋な数学だけで動く。

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Python 3.8+](https://img.shields.io/badge/python-3.8+-blue.svg)](https://www.python.org/downloads/)
[![Tests](https://img.shields.io/badge/tests-290%2B%20passing-brightgreen)](https://github.com/tshim1zu/japhrase)

---

## なぜ japhrase か

テキスト分析ツールの大半は、単語カウンターか LLM ラッパーのどちらかだ。japhrase はその間にいる——**計量言語学の統計手法**（PMI、カイ二乗、エントロピー、JSD、Heaps則、圧縮理論）を、書き手が実際に困っている問題に適用する。

- **外部依存ゼロ** — numpy + scipy だけで動く。APIキーもネットも GPU も不要
- **確定的** — 同じ入力は常に同じ出力。ハルシネーションがない
- **日本語ネイティブ** — 文字 N-gram ベースだから形態素解析が不要。英語にも対応
- **高速** — 290件以上のテストが 4秒 で全パス

---

## 何ができるか

### 統計エンジン（コア）

| エンジン | 何を測るか | 主な指標 |
|---------|-----------|---------|
| **DistributionComparator** | 2つの文章の違い | 対数尤度比(G²)、JS距離、効果量、キーネス |
| **CollocationScorer** | 語の結びつきの強さ | PMI、MI³、t値、z値、Log-Dice、Delta-P |
| **StylometryAnalyzer** | 語彙の豊かさ | Hapax比、Brunet's W、Honoré's R、Simpson's D、MATTR、Heaps則 |
| **ComplexityAnalyzer** | 文章の難しさ・情報密度 | パープレキシティ、圧縮率、語彙密度、情報率 |
| **TemporalAnalyzer** | 連載を通しての変化 | バースト検出、語彙飽和度、JSD距離行列 |
| **StatisticalScorer** | フレーズの統計的有意性 | カイ二乗、相互情報量、Zipf異常、Wilson信頼区間 |
| **PhraseExtracter** | 頻出フレーズの抽出 | N-gram + PMI + エントロピーフィルタ |

### 応用機能（執筆ワークフロー直結）

統計エンジンの上に構築した、原稿の実問題を解くモジュール群：

| モジュール | 解決する問題 | 出力例 |
|-----------|-------------|--------|
| **PreflightChecker** | 「この原稿、公開して大丈夫？」 | GO / WARN / NOGO 判定 + 品質スコア (0-100) |
| **EPDashboard** | 「話数を追うごとに語彙が枯れてないか？」 | MATTR推移、エントロピー推移、語彙飽和度、伏線バースト検出 |
| **HabitDriftDetector** | 「書き癖が悪化してないか？」 | 悪化/改善の時系列追跡 + スパークライン可視化 |
| **JPENDivergenceChecker** | 「翻訳で品質が落ちてないか？」 | 話数ごとの翻訳ロス率、劣化アラート |
| **CharacterStylometry** | 「キャラの語彙が混ざってないか？」 | キャラ別文体指紋、JSD分離度マトリクス |
| **PartHealthReport** | 「このパート全体の品質は？」 | A〜E 5段階評価 + 6項目診断 + 改善優先度リスト |

---

## クイックスタート

```python
from japhrase import DistributionComparator, StylometryAnalyzer, ComplexityAnalyzer
from collections import Counter

# 2つのテキストの語彙分布を比較
comp = DistributionComparator()
freq_a = Counter({"騎士": 10, "剣": 8, "王城": 5})
freq_b = Counter({"研究": 12, "実験": 9, "データ": 7})
result = comp.compare(freq_a, freq_b)
print(f"JS距離: {result.jsd:.4f}")
print(comp.generate_report(freq_a, freq_b))

# 語彙の豊かさを測定
stylo = StylometryAnalyzer()
print(stylo.analyze_advanced_diversity(text))
# → hapax_ratio, brunets_w, honores_r, simpsons_d, ...

# 文章の複雑度を測定
cx = ComplexityAnalyzer()
print(cx.analyze(text))
# → perplexity, compression_ratio, lexical_density, information_rate
```

### 公開前プリフライトチェック

```python
from japhrase.applied import PreflightChecker

checker = PreflightChecker()
result = checker.check(原稿テキスト, lang='jp', platform='sh')
print(result.verdict)        # 'GO', 'WARN', or 'NOGO'
print(result.quality_score)  # 0-100
print(result.report())       # 全指標の内訳
```

### 話数間の品質推移を追跡

```python
from japhrase.applied import EPDashboard

dashboard = EPDashboard()
result = dashboard.analyze({
    "第1話": ep01_text, "第2話": ep02_text, "第3話": ep03_text,
})
print(f"語彙飽和度: {result.vocab_saturation:.2f}")
print(f"MATTRトレンド: {result.mattr_trend:+.6f}")
print(result.report())
```

### 書き癖のドリフトを検出

```python
from japhrase.applied import HabitDriftDetector

detector = HabitDriftDetector()
result = detector.analyze({"第1話": t1, "第2話": t2, "第3話": t3})
print(f"悪化中の癖: {result.worsening_count}")
print(result.report())  # スパークライン付き
```

### キャラクターの文体分離

```python
from japhrase.applied import CharacterStylometry

cs = CharacterStylometry()
fps = cs.build_fingerprints(話数テキスト群, ["エリス", "レティシア", "ソフィア"])
print(cs.full_report(fps))
# → キャラ別MATTR、固有語、JSD分離度マトリクス
```

### パート全体の健康診断

```python
from japhrase.applied import PartHealthReport

report = PartHealthReport()
grade = report.diagnose(
    話数テキスト群,
    characters=["エリス", "レティシア", "ソフィア"],
    part_label="第1部",
)
print(grade.report())
# → 総合: B (78.3/100)
# → 🟢 語彙健康度 A (95.2) | 🟡 テンポ C (62.1) | 🟢 書き癖 A (90.0) | ...
```

---

## インストール

```bash
pip install japhrase

# ソースから
git clone https://github.com/tshim1zu/japhrase.git
cd japhrase
pip install -e .
```

**必要環境**: Python 3.8+、numpy、pandas、scipy（標準的な科学計算ライブラリのみ）

## テスト

```bash
pytest                            # 全290件以上を実行
pytest tests/test_applied.py -v   # 応用機能のみ
pytest --cov=japhrase             # カバレッジ付き
```

---

## アーキテクチャ

```
japhrase/
├── extracter.py              # N-gram フレーズ抽出（コアエンジン）
├── statistical_scorer.py     # カイ二乗、MI、Zipf、信頼区間、p値
├── distribution_comparator.py # G²、JSD、対数比、Dice、キーネス
├── collocation_scorer.py     # PMI、MI³、t値、z値、Log-Dice、Delta-P
├── stylometry.py             # TTR、Yule's K、Hapax、Brunet、Honoré、MATTR、Heaps
├── complexity_metrics.py     # パープレキシティ、圧縮率、語彙密度
├── temporal_analyzer.py      # バースト検出、語彙飽和度、トレンド追跡
├── writing_habit_detector.py # 頻度×PMI逆数 による書き癖検出
├── entropy_pacing.py         # シャノンエントロピーによるペーシング分析
├── chekhov_gun_detector.py   # チェーホフの銃（伏線の設置/回収）検知
│
├── applied/                  # 執筆ワークフロー直結モジュール
│   ├── preflight_stats.py    # 公開前品質ゲート
│   ├── ep_dashboard.py       # 話数間ダッシュボード
│   ├── habit_drift.py        # 書き癖ドリフト追跡
│   ├── jpen_divergence.py    # 和英翻訳品質乖離
│   ├── character_stylometry.py # キャラ文体指紋
│   └── part_health.py        # パート健康診断（A〜E評価）
│
├── similarity.py             # レーベンシュタイン / Jaccard / コサイン
├── cooccurrence.py           # 共起語分析
├── document_vectorizer.py    # NMFベースの文書ベクトル化
└── ...                       # 計50モジュール以上
```

---

## English Summary

**japhrase** is a pure-math text intelligence engine built for Japanese, with English support. It measures vocabulary richness (Hapax, MATTR, Heaps' Law), text complexity (perplexity, compression ratio), document similarity (JSD, G²), collocation strength (PMI, t-score, Log-Dice), and writing habit drift — all without external AI services. The `applied/` package provides editorial workflow tools: pre-publish quality gates, chapter-over-chapter dashboards, character voice fingerprinting, JP↔EN translation quality tracking, and arc-level health grading (A-E). 290+ tests, zero external dependencies beyond numpy/scipy.

---

## ライセンス

MIT License — 清水健

---

**japhrase**: 良い文章は、文字数以上の測り方に値する。
