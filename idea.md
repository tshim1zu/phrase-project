# japhrase 拡張アイデア集

原稿のバージョン管理や執筆支援という文脈で、`japhrase` を活用するアイデアを整理します。

現状の `japhrase` は「何が書いてあるか（What）」の分析に特化していますが、執筆支援ツールとして使うなら**「どこにあるか（Where）」**や**「どう変わったか（How）」**という視点が加わると非常に強力になります。

---

## 実装済み機能 ✅

以下の5つの機能は `japhrase/writing_assistant.py` で実装済みです：

### 1. ✅ 逆引き検索（KWIC: Key Word In Context）機能
**`KWICAnalyzer` クラス**
- フレーズが原稿のどこにあるかを検索
- 行番号、文字位置、前後の文脈を表示
- 複数フレーズの一括検索対応

### 2. ✅ 「推敲の偏り」ヒートマップ（修正箇所の可視化）
**`RevisionHeatmap` クラス**
- 複数バージョン間でセクションごとのフレーズ分布を分析
- 「推敲が集中している箇所」を検出
- バージョンラベルのカスタマイズに対応

### 3. ✅ 「あらすじ」vs「本文」の乖離アラート
**`AbstractBodyChecker` クラス**
- あらすじにあるが本文に無いフレーズを検出
- 本文で新規に出現するフレーズを特定
- 乖離スコア計算と自動レポート生成

### 4. ✅ 自分だけの「口癖」検出器
**`HabitDetector` クラス**
- 参照コーパスと比較して異常に頻度が高いフレーズを検出
- Z-scoreで統計的な有意性を判定
- 習癖スコアを計算

### 5. ✅ フレーズランキングの推移グラフ（River Plot）
**`RankingTrajectory` クラス**
- 複数バージョン間でフレーズのランク変化を追跡
- ランク変化が最大のフレーズ（risers/fallers）を抽出
- CSV形式でランキング推移データをエクスポート

---

## 実装完了した追加機能 ✅

さらに2つの強力な機能を実装しました：

### 1. ✅ エディタ用「表記ゆれ修正辞書」の自動生成
**`EditorConfigGenerator` クラス**
- SimilarityAnalyzerで類似フレーズペアを検出
- VS Code用のJSON設定に自動変換
- 置換ルールの自動生成機能
- エディタ連携で表記ゆれを効率的に修正

```python
from japhrase import PhraseExtracter, EditorConfigGenerator

extractor = PhraseExtracter()
phrases_df = extractor.get_dfphrase(texts)
generator = EditorConfigGenerator(phrases_df)
generator.export_vscode_config("settings.json")
```

### 2. ✅ 過去原稿からの「セルフ・リコメンデーション」
**`SelfRecommender` クラス**
- 今の原稿から過去のコーパスを検索
- 関連度が高い過去記事を自動発見
- 共通フレーズを検出（重複回避、再利用支援）
- 関連記事レポート自動生成

```python
from japhrase import SelfRecommender

recommender = SelfRecommender("past_articles/")
related = recommender.find_related_articles(current_text, top_n=5)
overlaps = recommender.find_overlapping_phrases(current_text)
report = recommender.generate_recommendation_report(current_text)
```

---

## 没にしたアイデア ❌

### 1. ❌ GitHub Actions で「用語の新人検査」を自動化
**判定理由**: japhrase の範疇外
- japhraseがサポートするのはフレーズ比較までで十分
- Git/GitHub API操作やCI/CDワークフローは、ユーザー側でカスタマイズ必要
- 外部ツール化すべき（例: GitHub Actionsの別スクリプト）

### 2. ❌ 「文体の指紋」をバージョン管理（最適化機能の逆利用）
**判定理由**: ドメイン特化的で一般性が低い
- `UnsupervisedOptimizer`があればユーザー側で実装可能
- パラメータ記録は簡単なスクリプトで十分
- 「文体診断」としての実用性が限定的

---

## 総括

`japhrase` を単なる「抽出ツール」として使うだけでなく、以下のように**「執筆サイクルのハブ」**として使うのが、バージョン管理における究極の活用法です。

1. **書く**（エディタ連携で支援）
2. **保存する**（Git連携で差分可視化）
3. **振り返る**（最適化機能で文体診断）

ここまでやれば、個人開発のツールとしてはかなり高度な執筆環境になるはずです。
