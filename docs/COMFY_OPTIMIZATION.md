# ComfyUI プロンプト最適化分析ガイド

## 概要

`japhrase` に **ComfyUI プロンプト最適化用の比較分析機能** を追加しました。Good と Bad のプロンプト群を比較して、最適なプロンプト要素を自動抽出します。

## 🎯 何ができるか

### Good と Bad の差分から発見

```python
from japhrase import ComparisonAnalyzer

# Good プロンプト 10件、Bad プロンプト 10件を比較
analyzer = ComparisonAnalyzer()
result = analyzer.compare_corpora(
    good_prompts="good_prompts.txt",
    bad_prompts="bad_prompts.txt"
)

# 結果
result['winning_ranked']   # ✅ Good にだけある要素（スコア付き）
result['failure_ranked']   # ❌ Bad にだけある要素（スコア付き）
result['common']          # 🔗 両方にある共通要素
```

### ビジュアルレポート出力

```bash
python scripts/run_comfy_analysis.py \
  --good good_prompts.txt \
  --bad bad_prompts.txt \
  --comfy-format \
  --top-n 10 \
  --output result/
```

**出力例:**
```
【必ず使う】Must Use (TOP 10):
  1. masterpiece
  2. cinematic lighting
  3. 8k resolution
  4. best quality
  ... (TOP 10まで)

【避ける】Must Avoid (TOP 10):
  1. rough sketch
  2. low resolution
  3. blurry
  ... (TOP 10まで)

【ベースタグ】Base Tags:
  1girl, witch, hair colors, ...
```

## 📋 実装内容

### 1. ComparisonAnalyzer クラス

**ファイル:** `japhrase/comparison_analyzer.py`

```python
class ComparisonAnalyzer:
    """Good/Bad テキスト比較分析"""
    
    def compare_corpora(good_text, bad_text) -> Dict:
        """集合演算で Good/Bad の差分を計算"""
    
    def generate_report(result, top_n=10) -> str:
        """ビジュアルレポート生成"""
    
    def save_results(result, output_dir) -> None:
        """JSON + テキストレポート保存"""
```

**特徴:**
- ✅ 小規模データ（10-20件）最適化
- ✅ 集合演算で簡潔に実装
- ✅ TF-IDF スコアリング
- ✅ Chi-square 統計検定対応
- ✅ TOP-N パラメータで柔軟

### 2. 実行スクリプト

**ファイル:** `scripts/run_comfy_analysis.py`

```bash
# 基本実行
python scripts/run_comfy_analysis.py --good g.txt --bad b.txt

# ComfyUI フォーマット + TOP 10
python scripts/run_comfy_analysis.py --good g.txt --bad b.txt --comfy-format --top-n 10

# カスタム出力ディレクトリ
python scripts/run_comfy_analysis.py --good g.txt --bad b.txt -o result/
```

**パラメータ:**
- `--good`: Good プロンプトファイル
- `--bad`: Bad プロンプトファイル
- `--output (-o)`: 出力ディレクトリ
- `--top-n`: 表示件数（デフォルト: 10）
- `--comfy-format`: ComfyUI JSON フォーマット出力

## 📊 スコアリングロジック

### TF-IDF スコア

```
score = TF × IDF
where:
  TF = phrase の Good/Bad での出現回数
  IDF = 1.0 / (1.0 + opposite_count)
```

**例:**
- `masterpiece`: Good 10件中 10 回、Bad 0 回 → score = 10.0 ✅
- `rough sketch`: Good 0 回、Bad 10件中 10 回 → score = 1.0 ❌
- 共通タグ: Good/Bad 両方に出現 → スコア低い

### Chi-square 統計検定

各フレーズについて統計的有意性（p-value）を計算します。

- p < 0.05 → 統計的に有意（重要）
- p ≥ 0.05 → 有意でない（影響小）

**注:** 人工データの場合、p-value が 1.0 になることがあります（完全分離のため）

## 🔧 使用方法

### Python コードから使用

```python
from japhrase import ComparisonAnalyzer

analyzer = ComparisonAnalyzer(min_count=3, min_length=5)

# テキストファイルから
result = analyzer.compare_corpora("good.txt", "bad.txt")

# テキストリストから
good_list = ["prompt1", "prompt2", ...]
bad_list = ["prompt1", "prompt2", ...]
result = analyzer.compare_corpora(good_list, bad_list)

# レポート出力
report = analyzer.generate_report(result, top_n=15)
print(report)

# ファイルに保存
analyzer.save_results(result, output_dir="result/", filename_prefix="comfy")
```

### CLI から使用

```bash
# 基本的な実行
python scripts/run_comfy_analysis.py \
  --good good_prompts.txt \
  --bad bad_prompts.txt

# ComfyUI フォーマット + TOP 5
python scripts/run_comfy_analysis.py \
  --good good_prompts.txt \
  --bad bad_prompts.txt \
  --comfy-format \
  --top-n 5

# カスタム出力
python scripts/run_comfy_analysis.py \
  --good good_prompts.txt \
  --bad bad_prompts.txt \
  --output results/
```

## 📁 出力ファイル

実行後、指定の出力ディレクトリに以下が生成されます：

```
output_dir/
├── comfy_analysis_results.json       # 構造化データ
├── comfy_analysis_results_report.txt # ビジュアルレポート
└── comfy_format.json                 # ComfyUI フォーマット
```

### comfy_format.json の構造

```json
{
  "type": "comfy_ui_prompt_optimization",
  "recommendations": {
    "must_use": ["masterpiece", "cinematic lighting", ...],
    "must_avoid": ["rough sketch", "low resolution", ...],
    "base_tags": ["1girl", "witch", ...]
  },
  "usage": {
    "positive_prompt_template": "masterpiece, best quality, ..."
  }
}
```

## 🧪 検証用トイデータセット

### 生成コマンド

```bash
python scripts/generate_comfy_toy_dataset.py
```

### データセット特性

**Good プロンプト (10枚):**
- 共通品質タグ: `masterpiece, best quality, ultra detailed, 8k resolution, cinematic lighting, anime style, cel shading`
- アレンジ部: 髪色、背景、ポーズが異なる

**Bad プロンプト (10枚):**
- 品質タグ欠落
- ノイズ混入: `rough sketch, blurry, draft, bad anatomy, mutated hands`

## 💡 実装例：ユーザーデータでの活用

### ステップ 1: データ準備

```bash
# Good プロンプト 10-20件を用意
echo "prompt1
prompt2
..." > good_prompts.txt

# Bad プロンプト 10-20件を用意
echo "failed_prompt1
failed_prompt2
..." > bad_prompts.txt
```

### ステップ 2: 分析実行

```bash
python scripts/run_comfy_analysis.py \
  --good good_prompts.txt \
  --bad bad_prompts.txt \
  --comfy-format \
  --top-n 15
```

### ステップ 3: 結果確認

```bash
# レポートを表示
cat data/user_provided/comfy_analysis_results_report.txt

# JSON で構造化データを利用
python -m json.tool data/user_provided/comfy_format.json
```

## 📈 期待される結果

### 人工データ（完全分離）

```
Winning Factors (TOP 5):
  masterpiece      [██████████] 10.00
  cinematic lighting [██████████] 10.00
  cel shading      [██████████] 10.00
  ultra detailed   [██████████] 10.00
  best quality     [██████████] 10.00

Failure Factors (TOP 5):
  rough sketch     [█░░░░░░░░░] 1.00
  low resolution   [█░░░░░░░░░] 1.00
  blurry           [█░░░░░░░░░] 1.00
  draft            [█░░░░░░░░░] 1.00
  bad anatomy      [█░░░░░░░░░] 1.00
```

### 実データ（現実的な分布）

より自然なスコア分布（5.00 前後）が期待できます。

## 🔍 トラブルシューティング

### エラー: "No phrases found"

**原因:** min_count が大きすぎるか、データが少ない

**対策:**
```python
analyzer = ComparisonAnalyzer(min_count=2)  # 下げてみる
```

### p-value がすべて 1.0

**原因:** 人工データで Good/Bad が完全に分離している

**対策:** 実際のデータで試すと改善します

## 🚀 次のステップ

1. **実ユーザーデータでのテスト**: 実際のプロンプト 20-50 件で検証
2. **スコア分布の改善**: より細かいスコアリング（今は 1.0 or 10.0）
3. **ワークフロー統合**: ComfyUI ノード化や WebUI への組み込み

---

**詳細:** [API_REFERENCE.md](API_REFERENCE.md) の `ComparisonAnalyzer` を参照
