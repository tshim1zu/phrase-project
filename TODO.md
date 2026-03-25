# japhrase TODO

## UX 最大化（次回リリースで対応）

- [x] README 全コードブロックがコピペで即動く（3ラウンド検証済み v0.3.11）
- [x] GitHub/PyPI のフェンスドコードブロックにコピーボタン自動表示（マークダウン仕様で対応済み）
- [x] `AdaptiveTuner` — Optuna ベースの動的パラメータ最適化（v0.3.12 予定）
- [ ] `AdaptiveTuner` のテスト追加
- [ ] `AdaptiveTuner` を README に追加（コピペで動く使用例）

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

### docs/ を v0.3 に合わせて再構築

v0.2 の docs/ は削除済み（README に統合）。必要に応じて docs/CHANGELOG.md を更新。

## 優先度: 中

### 既存テストの fail を修正する

v0.3 の変更で既存テスト 13件が fail している。
stylometry.py の analyze_full() の返り値構造変更が主因。

### examples/ を復活させるか検討

examples/ は削除済み。各クラスに .demo() があるので不要かもしれない。

## 優先度: 低

### `python -m japhrase` を動くようにする

現在 `__main__.py` がないので `python -m japhrase` が動かない。
`japhrase` コマンド（entry_points）は動く。
