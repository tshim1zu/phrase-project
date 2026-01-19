# 執筆支援機能 — 実装ガイド

## 概要

japhrase に実装された 4 つの執筆支援機能を統合的に使用するガイドです。

**Phase 1（即効性）**
- ✅ てにをはLint: 助詞の異常検出
- ✅ 単語ゆれ統一: 推奨表記辞書モード
- ✅ 冗長語尾ヒートマップ: CLI化・改善提案機能

**Phase 2（連携効果）**
- ✅ 主語/視点ブレ検出: 統計情報・改善提案
- ✅ 同文反復バリエーション: JSON出力・自動修正

---

## 1. てにをはLint — 助詞の異常検出

### 検出項目
- **助詞の連続**: 「がが」「をを」などの不自然な連続
- **助詞の重複**: 「彼が私が好き」など格助詞の異常使用
- **過剰使用**: 1文内に格助詞が3回以上出現

### 使用例

```python
from japhrase import TeniwohaLinter

linter = TeniwohaLinter(strict_mode=False)  # False: 緩い, True: 厳格

# チェック実行
issues = linter.check("彼女が私が好きだと言った。")
print(linter.format_issues(issues))

# 出力例:
# 1. ❌ 助詞「が」が近接して繰り返されています
#    位置 4-5: ...好きだ...
#    💡 提案: 文を分割するか、「が」を使わない表現を検討してください
```

### 改善の手順

| 問題 | 対策 |
|------|------|
| 「がが」「をを」 | 文を分割、または省略形を使用 |
| 過剰使用 | 文末を「だ」「である」に統一、または文を短く |
| 不自然連接 | 格助詞を変更（「彼が私に好まれた」など） |

---

## 2. 単語ゆれ統一 — 推奨表記辞書機能

### 機能
- **自動検出**: テキスト内の表記ゆれを統計的に検出
- **推奨表記辞書**: ユーザー定義の統一基準を適用
- **JSON出力**: 修正案を構造化データで出力

### 使用例

```python
from japhrase import TextVariantDetector
import pandas as pd

# 初期化
detector = TextVariantDetector(similarity_threshold=0.7)

# 推奨表記辞書を読み込み
detector.load_preferred_dictionary("preferred_forms.json")

# またはプログラムで設定
detector.add_preferred_form(
    variants=['ひらがな', 'ひらかな'],
    preferred='ひらがな',
    reason='常用漢字表に準拠'
)

# 候補を検出
df_phrases = pd.DataFrame({
    'seqchar': ['iPhone', 'iphone', 'iOS', 'i-phone'],
    'freq': [12, 5, 8, 2]
})

candidates = detector.detect_variants(df_phrases)

# 推奨表記を適用
candidates = detector.apply_preferred_forms(candidates)

# JSON出力
detector.export_candidates_json(candidates, "variant_candidates.json")

# テキストへの修正案を生成
suggestions = detector.generate_correction_suggestions(text, candidates)
for sugg in suggestions:
    print(f"{sugg['original']} → {sugg['preferred']} ({sugg['reason']})")
```

### 推奨表記辞書フォーマット

```json
{
  "iPhone": {
    "preferred": "iPhone",
    "reason": "Apple公式表記"
  },
  "iphone": {
    "preferred": "iPhone",
    "reason": "大文字小文字の統一"
  }
}
```

---

## 3. 冗長語尾のヒートマップ

### 機能
- **密度可視化**: 文末表現の使用率をチャンク単位で表示
- **改善提案**: 多様化のための具体的な提案
- **JSON出力**: 分析結果を構造化データで出力

### 使用例

```python
from japhrase import EndingHeatmapGenerator

generator = EndingHeatmapGenerator(chunk_size=5)  # 5文ごと

# テキスト分析
analysis = generator.analyze(text)

# ヒートマップ表示
print(generator.format_heatmap(analysis))

# 出力例:
# === 文末表現ヒートマップ ===
# 総文数: 20
# 文末表現の多様性スコア: 0.50
#
# 【チャンク別分析】（5文ごと）
#
# チャンク |  過去  |  過去進 |  過去丁 | である |   だ   |  です  |  ます  |  継続
# ------
#    0    | 2 ▓   | 0     | 1 ░   | 0     | 2 ▓   | 0     | 0     | 0
#    1    | 1 ░   | 0     | 3 ▓   | 0     | 1 ░   | 0     | 0     | 0
#    2    | 0     | 2 ▒   | 1 ░   | 0     | 2 ▓   | 0     | 0     | 0
#    3    | 1 ░   | 0     | 2 ▒   | 0     | 2 ▓   | 0     | 0     | 0

# 問題検出
issues = generator.detect_issues(analysis, threshold=0.6)

# 改善提案
improvements = generator.suggest_improvements(analysis, top_n=3)
for imp in improvements:
    print(f"・{imp['description']} が{imp['usage_ratio']:.0%}使用されています")
    print(f"  → {imp['alternative_patterns'][0]}")
```

### チャンク単位での改善ポイント

| 内容 | アクション |
|------|-----------|
| 1パターン > 60% | バリエーション追加必須 |
| 最多パターン > 40% | 次点パターンの活用 |
| 多様性 < 0.4 | 大規模な文末表現改革 |

---

## 4. 主語/視点ブレ検出

### 検出項目
- **主語の変化**: N文内で主語が頻繁に切り替わる
- **視点の混在**: 一人称・二人称・三人称が混在
- **段落内の不統一**: 主語の安定性スコア算出

### 使用例

```python
from japhrase import SubjectPOVDetector

detector = SubjectPOVDetector(sensitivity='medium')

# チェック実行
issues = detector.check(text)
print(detector.format_issues(issues, text))

# 統計情報を取得
stats = detector.get_statistics(text)
print(f"視点の支配度スコア: {stats['dominant_pov_ratio']:.1%}")
print(f"主語の安定性スコア: {stats['subject_stability_score']:.1%}")

# 改善提案を取得
suggestions = detector.suggest_improvements(text)
for sugg in suggestions:
    print(f"\n問題: {sugg['problem']}")
    for i, suggestion in enumerate(sugg['suggestions'], 1):
        print(f"  {i}. {suggestion}")

# JSON出力
detector.export_issues_json(issues, "pov_issues.json")
```

### 感度設定

```python
# sensitivity = 'low': 明らかな問題のみ検出（主語変化 >= 4回）
# sensitivity = 'medium': バランス型（主語変化 >= 3回）
# sensitivity = 'high': 厳格型（主語変化 >= 2回）
```

---

## 5. 同文反復検出 — 自動バリエーション生成

### 機能
- **自動検出**: 同じ文の繰り返しを検出
- **バリエーション自動生成**: 語尾・語順・省略パターンを提案
- **自動修正**: テキストに直接適用可能

### 使用例

```python
from japhrase import SentenceVariationGenerator

generator = SentenceVariationGenerator(
    similarity_threshold=0.95,  # 95%以上の類似度
    min_repetitions=2           # 2回以上の繰り返し
)

# 繰り返しを検出
repetitions = generator.detect_repetitions(text)

# 修正テキストを生成（最初の候補を自動適用）
corrected = generator.generate_correction_text(
    text, 
    repetitions, 
    apply_all=True
)

# または、提案を確認してから適用
suggestions = generator.suggest_corrections(text, priority='high')
for sugg in suggestions:
    print(f"位置 {sugg['position']}: {sugg['original']} → {sugg['suggested_replacement']}")
    print(f"理由: {sugg['reasoning']}")

# 対話的に適用
def confirm(sugg):
    # ユーザー入力など
    return True

corrected, count = generator.apply_suggestions(text, suggestions, confirm)
print(f"{count}件の修正を適用しました")

# JSON出力
generator.export_repetitions_json(repetitions, "repetitions.json")
```

### バリエーション生成パターン

| 型 | 例 | 効果 |
|------|------|------|
| ending_variation | 「〜だった」→「〜である」 | 文体の多様化 |
| word_order | 時間表現を文末に移動 | リズムの変化 |
| omission | 冗長表現を削除 | 簡潔化 |
| conjunction | 接続詞を追加 | 流れの明確化 |

---

## 6. 統合ワークフロー例

```python
from japhrase import (
    TeniwohaLint,
    TextVariantDetector,
    EndingHeatmapGenerator,
    SubjectPOVDetector,
    SentenceVariationGenerator
)

# 1. テキストを読み込み
with open('manuscript.txt', 'r') as f:
    text = f.read()

# 2. 基本チェック
linter = TeniwohaLinter()
lint_issues = linter.check(text)

# 3. 表記ゆれを統一
variant_detector = TextVariantDetector()
variant_detector.load_preferred_dictionary('preferred.json')

# 4. 文末表現を多様化
heatmap = EndingHeatmapGenerator()
analysis = heatmap.analyze(text)
improvements = heatmap.suggest_improvements(analysis)

# 5. 主語/視点の一貫性を確認
pov = SubjectPOVDetector()
pov_issues = pov.check(text)

# 6. 繰り返しを検出・修正
var_gen = SentenceVariationGenerator()
repetitions = var_gen.detect_repetitions(text)
corrected = var_gen.generate_correction_text(text, repetitions, apply_all=True)

# 7. レポート出力
print(f"てにをはLint: {len(lint_issues)} 件")
print(f"主語/視点ブレ: {len(pov_issues)} 件")
print(f"同文反復: {len(repetitions)} 件")

# 8. JSON形式でエクスポート
variant_detector.export_candidates_json(candidates, "report.json")
heatmap.export_analysis_json(analysis, "heatmap.json")
pov.export_issues_json(pov_issues, "pov.json")
var_gen.export_repetitions_json(repetitions, "repetitions.json")
```

---

## FAQ

### Q: どの機能を優先して使うべき？

**推奨順序:**
1. **てにをはLint** — 最優先（実装価値高、即効性高）
2. **同文反復検出** — 繰り返しの削除（読みやすさ向上）
3. **主語/視点ブレ** — 文章の流れを改善
4. **文末表現ヒートマップ** — 文体の多様化（微調整）
5. **単語ゆれ統一** — 最後の仕上げ

### Q: JSON出力の用途は？

- **自動化**: CI/CD パイプラインでテキスト品質チェック
- **可視化**: 分析結果を BI ツールで処理
- **機械学習**: 執筆品質の定量評価学習データとして
- **エディタ連携**: VS Code プラグインなど外部ツール連携

### Q: リアルタイム処理に対応？

現在は単発チェック向け。ストリーミング対応は `StreamingPhraseExtracter` を参照。

---

## 参考リンク

- [API リファレンス](../docs/API_REFERENCE.md)
- [テスト例](../tests/)
- [統合デモ](./integrated_writing_review.py)
