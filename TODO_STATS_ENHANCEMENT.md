# japhrase 統計エンジン強化計画（知能型極振り）

## 設計方針
- 外部LLM不要。純粋な数学・統計のみ
- 既存の pandas + numpy + scipy.stats 基盤に乗せる
- 既存パターン（score → dataclass → DataFrame → report）を踏襲
- 新規ファイルは最小限。既存モジュールの拡張を優先

---

## Phase 1: 分布比較エンジン（distribution_comparator.py）【新規】
**コスト: ~250行 | 依存: scipy.stats のみ**

既存の χ² 一本足を多角化。テキスト間・Part間・キャラ間の「違い」を精密測定。

| 指標 | 用途 | 実装量 |
|------|------|--------|
| Log-Likelihood Ratio (G²) | キーネス分析。χ²よりスパースに強い | 30行 |
| Jensen-Shannon Divergence | 2分布間の対称距離。Part間比較 | 25行 |
| Effect Size (Log Ratio) | 有意性ではなく「どれだけ違うか」 | 20行 |
| Dice Coefficient | コーパスサイズ非依存の結合度 | 15行 |
| Keyness Profiler | G² + Effect Size で「このテキスト固有の語」抽出 | 60行 |
| Comparative Report | 2テキスト比較レポート生成 | 50行 |

## Phase 2: 語彙多様性の深化（stylometry.py 拡張）
**コスト: ~200行 | 依存: なし（numpy のみ）**

TTR + Yule's K だけでは長文で崩壊。長文耐性のある指標群を追加。

| 指標 | 特性 | 実装量 |
|------|------|--------|
| Hapax Legomena Ratio | 1回語の割合。語彙の洗練度 | 15行 |
| Brunet's W | 対数ベース。長文に強い | 15行 |
| Honoré's R | Hapax + 総語数の統合指標 | 15行 |
| Sichel's S | 2回語の割合 | 10行 |
| Simpson's D (1-D) | 生態学由来の多様性指標 | 15行 |
| MATTR (Moving Average TTR) | スライディングウィンドウ TTR 推移 | 30行 |
| Vocabulary Growth Curve (Heaps' Law) | 語彙増加速度のフィッティング | 40行 |
| 統合レポート強化 | 全指標を一覧化 | 40行 |

## Phase 3: 結合度多角化（collocation_scorer.py）【新規】
**コスト: ~200行 | 依存: scipy.stats**

PMI 一本足からの脱却。書き癖検出・文法パターンの精度が上がる。

| 指標 | 特性 | 実装量 |
|------|------|--------|
| t-score | 高頻度コロケーション検出（PMIの弱点を補完） | 25行 |
| z-score | 期待値からの標準偏差 | 20行 |
| Log-Dice | コーパスサイズ非依存。最も安定 | 20行 |
| MI³ (Cubic MI) | PMIの低頻度バイアスを補正 | 15行 |
| Delta-P (方向性結合) | A→B と B→A の非対称結合度 | 25行 |
| Multi-metric Ranker | 複数指標の統合ランキング | 40行 |
| 既存統合 | writing_habit_detector / grammar_pattern_extractor への注入 | 30行 |

## Phase 4: 時系列・進行分析（temporal_analyzer.py）【新規】
**コスト: ~250行 | 依存: numpy, scipy**

EP間・Part間の「推移」を追跡。伏線語のバースト検出。

| 指標 | 用途 | 実装量 |
|------|------|--------|
| Burstiness (Kleinberg簡易版) | 語の突発的使用区間を検出 | 50行 |
| MATTR 推移プロット | EP進行に伴う語彙多様性の変動 | 30行 |
| Entropy Rate 推移 | 文単位エントロピーのEP間推移 | 30行 |
| Term Frequency Trend | 特定語の出現頻度トレンド（増加/減少/バースト） | 40行 |
| Vocabulary Accumulation | EP追加ごとの新規語彙数推移 | 30行 |
| Part Comparison Matrix | Part間の全指標マトリクス | 40行 |

## Phase 5: 予測可能性・情報密度（complexity_metrics.py）【新規】
**コスト: ~150行 | 依存: zlib (標準), numpy**

エントロピーの上位互換。

| 指標 | 用途 | 実装量 |
|------|------|--------|
| N-gram Perplexity | 文の予測困難度。退屈/難解の精密測定 | 40行 |
| Compression Ratio | zlib/lzma圧縮率 = 情報密度 | 20行 |
| Lexical Density | 内容語 / 全語 の比率 | 25行 |
| Information Rate | 文あたりの新情報量 | 30行 |
| Segment Complexity Profile | テキスト全体の複雑度推移 | 35行 |

---

## 総コスト見積もり

| Phase | 新規行数 | ファイル | 難度 |
|-------|---------|---------|------|
| 1. 分布比較 | ~250 | distribution_comparator.py (新規) | ★★☆ |
| 2. 語彙多様性 | ~200 | stylometry.py (拡張) | ★☆☆ |
| 3. 結合度 | ~200 | collocation_scorer.py (新規) | ★★☆ |
| 4. 時系列 | ~250 | temporal_analyzer.py (新規) | ★★★ |
| 5. 情報密度 | ~150 | complexity_metrics.py (新規) | ★★☆ |
| **合計** | **~1,050** | **4新規 + 1拡張** | |

依存追加: なし（scipy.stats + numpy + zlib で全部できる）

---

## 実装順序

```
Phase 1 → Phase 3 → Phase 2 → Phase 5 → Phase 4
(分布比較)  (結合度)   (語彙)    (密度)    (時系列)
```

理由:
- Phase 1 は statistical_scorer.py の直接強化。最もコスパ高い
- Phase 3 は Phase 1 の指標を結合度に転用できる
- Phase 2 は独立。Phase 1/3 と並行可能
- Phase 5 は entropy_pacing.py の上位互換
- Phase 4 は Phase 1-3 の指標を時系列に乗せるので最後

## __init__.py への統合

全Phase完了後に `__init__.py` と `__all__` を更新。CLIへの統合は別タスク。
