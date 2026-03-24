# japhrase 応用機能計画 — 執筆・原稿チェック・stores 直結

## 設計方針
- 今回追加した統計エンジン（Phase 1-5）を **実際のワークフローに接続** する
- novel_cli の既存 lint/audit コマンドの **精度向上 or 新コマンド追加** として実装
- stores の原稿管理（PF間比較・公開前チェック）に使える機能を追加
- japhrase 側にロジックを置き、novel_cli 側は薄いラッパーにする

---

## 全体構成（6機能群）

```
A. EP間比較ダッシュボード ← TemporalAnalyzer + DistributionComparator
B. 書き癖ドリフト検出    ← CollocationScorer + WritingHabitDetector 強化
C. キャラ文体指紋        ← Stylometry拡張 + DistributionComparator
D. 公開前プリフライト     ← ComplexityAnalyzer + audit_immersion 統合
E. JP↔EN 品質乖離検出   ← DistributionComparator + Stylometry
F. Part健康診断レポート   ← 全エンジン統合の1コマンド
```

---

## A. EP間比較ダッシュボード（ep_dashboard.py）【新規】
**コスト: ~200行 | 接続先: novel_cli audit / TemporalAnalyzer**

**課題**: 現状 EP 単体のチェックはあるが、EP 間の「推移」が見えない。
Part を通して書いてるうちに語彙が枯れたり、テンポが単調化しても気づけない。

| 機能 | 使う統計エンジン | 出力 |
|------|-----------------|------|
| 語彙多様性の推移（MATTR）| TemporalAnalyzer | EP ごとの MATTR 値 + トレンド |
| エントロピー推移 | TemporalAnalyzer | 退屈化/複雑化のトレンド |
| 新語出現率の減衰 | TemporalAnalyzer（語彙飽和度）| 「EP108以降、新語が出ていない」 |
| EP 間 JSD 距離行列 | DistributionComparator | 隣接 EP が離れすぎ/近すぎの検出 |
| 伏線語バースト検出 | TemporalAnalyzer（Burstiness）| 「EP110〜112 で『システム』がバースト」 |

**CLI統合案**:
```bash
python -m novel_cli audit ep-dashboard -p part10
python -m novel_cli audit ep-dashboard -p part10 --term システム,エリス
```

**最大の価値**: 「Part を書き終えた後のセルフレビュー」で、EP 単位では見えない Part レベルの問題を可視化する。

---

## B. 書き癖ドリフト検出（habit_drift.py）【新規】
**コスト: ~150行 | 接続先: novel_cli lint writing-habit 強化**

**課題**: 現在の `lint writing-habit` は 1EP 単体の癖しか見ない。
同じ癖が Part 全体で悪化しているか、改善しているかがわからない。

| 機能 | 使う統計エンジン | 出力 |
|------|-----------------|------|
| 癖スコアの時系列推移 | CollocationScorer + TemporalAnalyzer | 「〜だった」使用率の増減 |
| PMI vs t-score 乖離マップ | CollocationScorer | 書き癖 vs 意図的な固定表現の分類 |
| 新規癖の検出 | CollocationScorer (Delta-P) | 「Part10 後半から急に『しかし』が増えた」 |
| Part 間比較 | DistributionComparator | Part10 vs Part12 で癖がどう変わったか |

**CLI統合案**:
```bash
python -m novel_cli lint writing-habit -p part10 --drift   # ドリフトモード追加
python -m novel_cli lint writing-habit -p part10 --compare part12
```

---

## C. キャラ文体指紋（character_stylometry.py）【新規】
**コスト: ~250行 | 接続先: novel_cli lint character-consistency 強化**

**課題**: 現在の `lint character-consistency` は設定値（眼鏡の有無等）の一致しか見ない。
キャラの「文体」がブレているか — つまり台詞や地の文の語彙傾向が一貫しているか — は検出できない。

| 機能 | 使う統計エンジン | 出力 |
|------|-----------------|------|
| キャラ別文体ベクトル | Stylometry（文字種比率 + MATTR + Yule's K）| キャラごとの文体指紋 |
| キャラ間 JSD 距離 | DistributionComparator | 「エリスとソフィアの語彙が混ざっている」 |
| キャラ文体の EP 間一貫性 | TemporalAnalyzer | 「レティシアの語彙が EP108 で急変」 |
| 台詞の語彙 Keyness | DistributionComparator（Keyness）| 「このキャラだけに頻出する語」抽出 |
| 地の文 POV 漏洩検出 | CollocationScorer | POV キャラ以外の内面語が地の文に出現 |

**CLI統合案**:
```bash
python -m novel_cli lint character-voice -p part10 --char エリス
python -m novel_cli lint character-voice -p part10 --all-chars
python -m novel_cli lint character-voice -p part10 --compare エリス レティシア
```

**データソース**: `story/settings/characters/characters.yaml` + `story/part*/scenes/*.md`
キャラ名で grep → セリフ抽出 → 各キャラのコーパスを構築 → 比較

---

## D. 公開前プリフライト（preflight_stats.py）【新規】
**コスト: ~180行 | 接続先: stores の各PF公開前チェック**

**課題**: 公開前にやるべきチェックが散在している（audit_immersion, count_chars, en-names, etc）。
統計的な「品質スコア」が1つも出ないまま公開判断している。

| 機能 | 使う統計エンジン | 出力 |
|------|-----------------|------|
| 複雑度スコア | ComplexityAnalyzer | PP / CR / LD / IR の4指標 |
| 離脱リスクスコア | ComplexityAnalyzer + audit_immersion | SKIP/DRAG/NOISE 件数 + 複雑度の合成 |
| 語彙密度チェック | ComplexityAnalyzer（Lexical Density）| 機能語過多 = 冗長 の警告 |
| 圧縮率異常検出 | ComplexityAnalyzer（Compression Ratio）| 極端に低い = 同じことの繰り返し |
| PF別文字数バリデーション | count_chars 連携 | SH 上限 / Ci-en 推奨長 / Ream チャプター長 |
| 合否判定（Go/NoGo） | 全指標の閾値チェック | ✅ or ❌ + 理由 |

**CLI統合案**:
```bash
python -m novel_cli workflow preflight -p part10 -e 101
python -m novel_cli workflow preflight -p part10 -e 101 --platform sh
python -m novel_cli workflow preflight -p part10 --all-eps
```

**stores 連携**: `stores/{PF}/{pf}_playwright.py` の upload-chapter 前に自動実行できるフックとして設計。

---

## E. JP↔EN 品質乖離検出（jpen_divergence.py）【新規】
**コスト: ~200行 | 接続先: webai/scripts/jpen_consistency.py 強化**

**課題**: 現在の JP↔EN チェックは行数一致・キャラ名一致程度。
翻訳によって「語彙の豊かさが落ちている」「情報密度が変わっている」が見えない。

| 機能 | 使う統計エンジン | 出力 |
|------|-----------------|------|
| JP vs EN の語彙多様性比較 | Stylometry（Hapax Ratio, MATTR）| 「EN版の語彙がJPより30%貧弱」 |
| JP vs EN の複雑度比較 | ComplexityAnalyzer | 圧縮率・PP の差分 |
| JP vs EN のエントロピー比較 | ComplexityAnalyzer | 「EN版の方が単調」 |
| EP別 JP-EN 品質ギャップ推移 | TemporalAnalyzer | 「EP108以降、ENの品質が落ちている」 |
| 翻訳ロス率 | 全指標の差分 | 各指標の JP→EN 変化率を1つのスコアに |

**CLI統合案**:
```bash
python -m novel_cli lint jpen-quality -p part10 -e 101
python -m novel_cli lint jpen-quality -p part10 --all-eps
python -m novel_cli lint jpen-quality -p part10 --trend  # EP間推移
```

---

## F. Part 健康診断レポート（part_health.py）【新規】
**コスト: ~200行 | 全エンジン統合の1コマンド**

**課題**: `workflow check-all` は違反検出に特化。Part 全体の「健康状態」を定量的に示す統合レポートがない。

| セクション | 使うエンジン | 出力 |
|-----------|------------|------|
| 語彙健康度 | Stylometry（全指標）| Hapax / MATTR / Heaps β |
| テンポ健康度 | ComplexityAnalyzer + EntropyPacing | PP推移 / 退屈区間数 / 難解区間数 |
| 語彙成長 | TemporalAnalyzer | 飽和度 / 新語トレンド |
| 書き癖負債 | CollocationScorer | 上位10癖 + ドリフト方向 |
| キャラ分離度 | DistributionComparator | キャラ間 JSD 最小値（混ざりやすいペア） |
| JP↔EN ギャップ | 全指標 | 翻訳ロス率 |
| バースト警報 | TemporalAnalyzer | 伏線語の突発使用区間 |
| 総合スコア | 重み付き統合 | A〜E 5段階 + 改善優先度リスト |

**CLI統合案**:
```bash
python -m novel_cli audit part-health -p part10
python -m novel_cli audit part-health -p part10 --compare part12
```

---

## 総コスト見積もり

| 機能群 | 新規行数 | ファイル | 難度 | 依存 |
|-------|---------|---------|------|------|
| A. EP間ダッシュボード | ~200 | ep_dashboard.py | ★★☆ | TemporalAnalyzer, DistributionComparator |
| B. 書き癖ドリフト | ~150 | habit_drift.py | ★★☆ | CollocationScorer, TemporalAnalyzer |
| C. キャラ文体指紋 | ~250 | character_stylometry.py | ★★★ | Stylometry, DistributionComparator |
| D. 公開前プリフライト | ~180 | preflight_stats.py | ★★☆ | ComplexityAnalyzer, count_chars |
| E. JP↔EN 乖離検出 | ~200 | jpen_divergence.py | ★★☆ | Stylometry, ComplexityAnalyzer |
| F. Part 健康診断 | ~200 | part_health.py | ★★★ | 全エンジン |
| **novel_cli 統合** | ~300 | _lint_stats.py, _audit_stats.py | ★★☆ | japhrase_integration.py 拡張 |
| **合計** | **~1,480** | **6新規 + 2統合** | |

---

## 実装順序（依存関係順）

```
D (プリフライト) → A (ダッシュボード) → B (書き癖ドリフト)
                                        ↓
E (JP↔EN) → C (キャラ文体) → F (健康診断 = 全統合)
```

理由:
- D は ComplexityAnalyzer 単体で動く。最も独立性が高く、stores に即座に役立つ
- A は TemporalAnalyzer の直接応用。D の次に自然
- B は A の語彙推移に「癖」の軸を追加する形
- E は JP/EN 両方のシーンを読む必要がある。C と並行可能
- C はキャラ別コーパス構築が必要。やや重い
- F は全部を統合するので最後

## ファイル配置

```
phrase-project/japhrase/
  applied/                  # 新ディレクトリ: 応用機能群
    __init__.py
    ep_dashboard.py         # A
    habit_drift.py          # B
    character_stylometry.py # C
    preflight_stats.py      # D
    jpen_divergence.py      # E
    part_health.py          # F

system/cli/novel_cli/
  commands/
    _lint_stats.py          # B, C, E の lint コマンド統合
    _audit_stats.py         # A, D, F の audit コマンド統合
  japhrase_integration.py   # 拡張（新エンジンの import パス追加）
```

## japhrase_integration.py 拡張方針

現在の `japhrase_integration.py` は PhraseExtracter + SimilarityAnalyzer しか import していない。
新エンジン群を段階的に追加:

```python
# Phase 2 統計エンジン
from japhrase import (
    DistributionComparator, CollocationScorer,
    ComplexityAnalyzer, TemporalAnalyzer, StylometryAnalyzer,
)
# Phase 2 応用機能
from japhrase.applied import (
    EPDashboard, HabitDriftDetector, CharacterStylometry,
    PreflightChecker, JPENDivergenceChecker, PartHealthReport,
)
```
