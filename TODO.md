# japhrase TODO

## 🔴 リリース直前: README 多視点レビュー（6ラウンド手順）

**なぜやるか:**
README はユーザーとの最初で最後の接点。PyPI で10秒見て去るか、pip install するかがここで決まる。
1視点だけのレビューでは必ず嘘コードや死んだ例が残る（v0.3.11で実証: 1ラウンドで12箇所の嘘が残存）。
6視点で回すことで「誰にとっても壊れていない」状態を作る。

**いつやるか:** PyPI publish の直前。コードが固まった後、build の前。

**手順:**

### ラウンド1: 自己完結性チェック
全コードブロックを上から順にコピペ実行する。1ブロック = 1回の実行で動くか？
- 未定義変数（`text`, `df`, `pe` が前のブロックに依存）
- 存在しないファイル（`input.txt` を要求する例）
- 出力例と実際の出力の不一致（数値・列名・行数）

### ラウンド2: Python初心者の目
プログラミング経験1年の人間として読む。
- 最初のコード例は3行で動くか？リスト内包表記や5引数チェーンがないか？
- f-string + dict アクセス + format指定子の組み合わせが初心者に読めるか？
- 「何をするツールか」が最初のコード例より先に書かれているか？

### ラウンド3: 二周目ユーザーの目
一度使って戻ってきた人として読む。
- 「次に何を試すべきか」の導線があるか？
- auto_tune の効果が見えるか（show_params の出力例）？
- PhraseExtractor の結果を他の機能に繋ぐ方法が書かれているか？（df → Counter、df["freq"] → score_phrase）

### ラウンド4: 10秒スキマー
見出しだけを上から読む。
- 見出しだけで「何ができるか」のストーリーが読めるか？
- デモが埋もれていないか？
- 寂しい見出し（テーブル3行で終わるセクション等）がないか？

### ラウンド5: 懐疑派
「MeCab + Counter で十分では？」と思っている人として読む。
- MeCab との違い（単語 vs 可変長フレーズ）が冒頭で説明されているか？
- 出力に助詞が付く理由（N-gram の特徴）が説明されているか？
- 「フレーズ」の定義が書かれているか？

### ラウンド6: コピペ派
散文を一切読まず、コードブロックだけをコピペする人として読む。
- 各ブロックの先頭に `# ↑ の df, pe を使う` 等の依存コメントがあるか？
- コメントアウト行にしか書かれていない重要情報がないか？
- プレースホルダー文字列（`"あなたのテキスト"`）がそのまま実行して意味のある結果を返すか？

### 完了条件
6ラウンド全てで「修正ゼロ」になったら publish してよい。
1件でも修正があったら、そのラウンドをもう一度回す。

---

## UX（次回リリースで対応）

- [x] README 全コードブロックがコピペで即動く（6ラウンド検証済み v0.3.12）
- [x] GitHub/PyPI のフェンスドコードブロックにコピーボタン自動表示（マークダウン仕様で対応済み）
- [x] `AdaptiveTuner` — Optuna ベースの動的パラメータ最適化（v0.3.12）
- [x] PhraseExtractor に `auto_tune` / `show_params` / `save_params` / `load_params` 統合
- [ ] `AdaptiveTuner` のテスト追加
- [ ] show_params() の出力例を README に追加

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
