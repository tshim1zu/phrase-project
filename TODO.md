# japhrase TODO

## 優先度: 高

### CLI を v0.3 全機能に対応させる

現状の CLI は v0.2 のコマンドのみ（extract, stats, analyze, check, detect-habits 等）。
v0.3 で追加した統計エンジン・contamination・applied への CLI 経路がない。

初利用者が `japhrase --help` で全機能を知れるようにする。
各コマンドの `--help` で使い方・引数・出力形式がわかるようにする。

追加すべきコマンド:

```
japhrase compare <file1> <file2>         # DistributionComparator（2テキストの分布比較）
japhrase similarity <file1> <file2> ...  # SimilarityAnalyzer（類似度行列）
japhrase collocation <file>              # CollocationScorer（結合度分析）
japhrase stylometry <file>               # StylometryAnalyzer（語彙多様性）
japhrase complexity <file>               # ComplexityAnalyzer（複雑度・情報密度）
japhrase temporal <file1> <file2> ...    # TemporalAnalyzer（時系列分析）
japhrase scan <file>                     # contamination scan（汚染検出）
japhrase scan --batch <dir>              # contamination batch_scan
japhrase scan --compare <f1> <f2>        # contamination compare
japhrase preflight <file>                # PreflightChecker
japhrase health <file1> <file2> ...      # PartHealthReport
```

設計方針:
- 全コマンドが `--help` で引数・オプション・出力形式を説明する
- デフォルト引数で即座に動く（初利用者がオプションを覚えなくていい）
- 出力形式は text（デフォルト）/ json / csv を共通オプションで選べる
- `japhrase --help` のトップレベルヘルプで全コマンドの一覧と一行説明が見える

### docs/USAGE.md を v0.3 に対応させる

現状は v0.2 のまま。以下が未記載:
- DistributionComparator / CollocationScorer の使い方
- StylometryAnalyzer の新指標（Hapax, MATTR, Heaps等）
- ComplexityAnalyzer / TemporalAnalyzer の使い方
- contamination パッケージ（scan, compare, batch_scan, quick_check）
- applied パッケージ（PreflightChecker, EPDashboard, PartHealthReport 等）

### docs/API_REFERENCE.md を v0.3 に対応させる

新クラス・新メソッドの記載がない。

## 優先度: 中

### 既存テストの fail を修正する

v0.3 の変更で既存テスト 13件が fail している。
stylometry.py の analyze_full() の返り値構造変更が主因。

### examples/ に v0.3 のデモを追加する

- contamination_demo.py
- distribution_compare_demo.py
- stylometry_demo.py
- temporal_demo.py

## 優先度: 低

### `python -m japhrase` を動くようにする

現在 `__main__.py` がないので `python -m japhrase` が動かない。
`japhrase` コマンド（entry_points）は動く。
