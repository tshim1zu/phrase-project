# テキストマイニング・計量文学モジュール ガイド

japhrase v0.2.0 で追加された 5つの新しいモジュールの使用ガイドです。

---

## 📋 目次

1. [DialogueAnalyzer](#dialogueanalyzer)
2. [OrthographyVariantDetector](#orthographyvariantdetector)
3. [StylometryAnalyzer](#stylometryanalyzer)
4. [CharacterNetworkGenerator](#characternetworkgenerator)
5. [PromptOptimizer](#promptoptimizer)
6. [統合例](#統合例)

---

## DialogueAnalyzer

**会話文と地の文（ナレーション）のバランスを分析**

### 概要

小説やラノベのテキストを「会話文」と「地の文」に分けて分析します。
カギ括弧（「」『』""''）に対応しています。

### 基本的な使い方

```python
from japhrase import DialogueAnalyzer

analyzer = DialogueAnalyzer()

text = """
「おはよう」と太郎が言った。
「おはよう」と花子が応じた。
二人は毎日顔を合わせる。
"""

result = analyzer.analyze(text)

print(f"会話比率: {result['dialogue_ratio']:.1%}")
print(f"地の文比率: {result['narrative_ratio']:.1%}")
print(f"会話数: {result['dialogue_count']}")
print(f"会話の平均長: {result['dialogue_avg_len']:.1f}字")
```

### 返り値

```python
{
    'total_characters': int,          # 総文字数（空白・改行除外）
    'dialogue_ratio': float,          # 会話文の比率 (0.0-1.0)
    'narrative_ratio': float,         # 地の文の比率 (0.0-1.0)
    'dialogue_count': int,            # 会話文の数
    'dialogue_avg_len': float,        # 会話文の平均長
    'dialogue_keywords': List[str],   # （extract_features=Trueの場合）
    'narrative_keywords': List[str]   # （extract_features=Trueの場合）
}
```

### 特徴語抽出付き分析

```python
result = analyzer.analyze(text, extract_features=True)

print("会話文の特徴語:", result.get('dialogue_keywords', []))
print("地の文の特徴語:", result.get('narrative_keywords', []))
```

### サマリー表示

```python
print(analyzer.get_summary(result))
```

**用途:**
- ラノベの執筆支援（会話と地の文のバランスチェック）
- 小説の「説明過多」の検出
- ジャンル別の会話比率の分析

---

## OrthographyVariantDetector

**表記ゆれ（同じ語の異なる表記）を検出**

### 概要

同一語彙で表記が割れているもの（「ゆれ」）を統計的に検出します。
外部辞書は不要で、編集距離とパターンマッチングで動作します。

### 基本的な使い方

```python
from japhrase import OrthographyVariantDetector

detector = OrthographyVariantDetector(similarity_threshold=0.75)

text = """
ComfyUIの使い方について。
ComfyUIで画像生成できる。
コンピューターで処理する。
コンピュータの性能は重要。
"""

issues = detector.check(text)

for issue in issues:
    print(issue['message'])
```

### 検出される表記ゆれの種類

1. **カタカナ長音のゆれ** （例：コンピューター vs コンピュータ）
2. **編集距離ベースの類似表記** （例：申し込み vs 申込み）

### 返り値

```python
[
    {
        'type': 'katakana_vowel',      # ゆれの種類
        'root': 'コンピュータ',
        'variants': ['コンピュータ', 'コンピューター'],
        'message': '...'
    },
    {
        'type': 'similar_spelling',
        'variants': ['申し込み', '申込み'],
        'similarity': 0.92,
        'message': '...'
    }
]
```

### サマリー表示

```python
summary = detector.get_summary(issues)
print(summary)
```

**用途:**
- テキスト品質チェック
- 自動修正の候補提案
- 執筆時の一貫性チェック

---

## StylometryAnalyzer

**文体の定量的な特徴を分析**

### 概要

語彙の豊かさ（TTR、Yule's K）や文字種の比率から、
文体を数値化・分類します。

### 基本的な使い方

```python
from japhrase import StylometryAnalyzer

analyzer = StylometryAnalyzer()

text = "機械学習は重要な技術です。..." * 10

# 語彙多様性分析
vocab = analyzer.analyze_vocabulary_richness(text)
print(f"TTR: {vocab['ttr']:.3f}")
print(f"Yule's K: {vocab['yules_k']:.2f}")
print(f"評価: {vocab['assessment']}")

# 文字種比率分析
char_type = analyzer.analyze_char_type_ratio(text)
print(f"漢字比率: {char_type['kanji_ratio']:.1%}")
print(f"ひらがな比率: {char_type['hiragana_ratio']:.1%}")
print(f"カタカナ比率: {char_type['katakana_ratio']:.1%}")
print(f"文体: {char_type['style_type']}")

# 文長分析
sent = analyzer.analyze_sentence_length(text)
print(f"平均文長: {sent['avg_length']:.1f}字")
print(f"文の数: {sent['sentence_count']}")
```

### 指標の意味

| 指標 | 意味 | 用途 |
|------|------|------|
| **TTR** | Type-Token Ratio（0-1） | 語彙多様性の基本指標 |
| **Yule's K** | 長文向け語彙多様性 | 大型コーパス分析に強い |
| **漢字比率** | 文章中の漢字の割合 | 文体の硬さ診断 |

### 全体分析

```python
full_result = analyzer.analyze_full(text)

# 包括的な分析結果
print(f"語彙: {full_result['vocabulary']}")
print(f"文字種: {full_result['character_type']}")
print(f"文長: {full_result['sentence']}")
```

### サマリー表示

```python
from japhrase import get_stylometry_summary

summary = get_stylometry_summary(text)
print(summary)
```

**用途:**
- 著者判定（スタイロメトリー分析）
- 文体の自動分類
- テキストの「読みやすさ」診断
- 執筆トーンの一貫性チェック

---

## CharacterNetworkGenerator

**テキスト中の重要語の関係ネットワークを構築**

### 概要

登場人物や重要な概念を「ノード」、
それらの共起を「エッジ」として、
グラフデータを生成します。

### 基本的な使い方

```python
from japhrase import CharacterNetworkGenerator

generator = CharacterNetworkGenerator(window_size=50)

text = """
太郎と花子が出会った。太郎は学生。花子も学生。
太郎と花子は一緒に勉強する。花子は図書館が好きだ。
"""

# エッジリスト（ネットワークデータ）の生成
df_edges = generator.generate_edgelist(text, top_n_nodes=10)

print(df_edges)
#     source target  weight
# 0      太郎      花子     3.0
# 1      花子      学生     2.0
```

### ネットワーク統計

```python
stats = generator.get_network_stats(df_edges)

print(f"ノード数: {stats['node_count']}")
print(f"エッジ数: {stats['edge_count']}")
print(f"ネットワーク密度: {stats['density_est']:.3f}")
```

### 重要ノードの検出

```python
important_nodes = generator.get_important_nodes(df_edges, top_n=5)

print(important_nodes)
# 接続度の高い（中心的な）ノードがランキング表示
```

### グラフ形式でのエクスポート

```python
# CSV形式（汎用）
generator.to_csv(df_edges, "network.csv")

# GEXF形式（Gephi互換）
generator.to_gexf(df_edges, "network.gexf")
```

**用途:**
- 登場人物の相関図の自動生成
- 概念のネットワーク分析
- テキストマイニング結果の可視化
- 物語構造の分析

---

## PromptOptimizer

**AI画像生成（ComfyUI/Stable Diffusion）のプロンプト最適化**

### 概要

過去の「良いプロンプト」から学習し、
新しいプロンプトの品質向上を支援します。

### 基本的な使い方

```python
from japhrase import PromptOptimizer

# コーパス（過去の良プロンプト）を準備
corpus = [
    "masterpiece, best quality, 1girl, solo, blue eyes, white hair",
    "best quality, high resolution, scenery, blue sky, cloud, day",
    "masterpiece, detailed, 1boy, smile, looking at viewer",
]

# オプティマイザーを初期化
optimizer = PromptOptimizer(corpus)

# プロンプトを分析
prompt = "1girl, blue eyes, standing, looking at viewer"
result = optimizer.analyze(prompt)

print(f"品質スコア: {result['quality_score']}/100")
print(f"トークン数: {result['token_count']}")
print(f"改善提案: {len(result['suggestions'])}件")
```

### 返り値

```python
{
    'syntax_issues': List[str],      # 構文エラー
    'redundancy_issues': List[Dict], # 冗長性
    'suggestions': List[Dict],       # 改善提案
    'weight_stats': Dict,            # 重み分布統計
    'token_count': int,              # トークン数
    'quality_score': float,          # 品質スコア (0-100)
    'analysis_summary': str          # サマリーテキスト
}
```

### 検出される問題

1. **構文エラー**
   - 括弧の不一致 `( が3個, ) が2個`
   - ダブルコンマ `,, `
   - 先頭・末尾のカンマ

2. **冗長性エラー**
   - 完全な重複 `blue, blue`
   - 包含関係 `blue eyes, eyes`

3. **重み分布の問題**
   - インフレ気味の重み（平均 > 1.3）
   - 極端に強い重み（> 2.0）

4. **改善提案**
   - コーパスから学習した欠落タグ

### レポート表示

```python
report = optimizer.get_report(prompt)
print(report)
```

**出力例:**
```
======================================================================
【プロンプト分析レポート】
======================================================================

品質スコア: 75.0/100 (良好) | トークン数: 5 | 改善提案: 2件

【改善提案】
  1. masterpiece (スコア: 0.333) - コーパスで頻出（出現率: 33.3%）
  2. best quality (スコア: 0.333) - コーパスで頻出（出現率: 33.3%）

======================================================================
```

**用途:**
- ComfyUI/Stable Diffusion のプロンプト品質向上
- 自分だけの「勝ちパターン」の学習と適用
- プロンプト作成時の自動チェック

---

## 統合例

### 小説の全体分析

```python
from japhrase import (
    DialogueAnalyzer,
    StylometryAnalyzer,
    CharacterNetworkGenerator,
    OrthographyVariantDetector
)

novel_text = open("my_novel.txt").read()

print("=" * 70)
print("【小説の全体分析】")
print("=" * 70)

# 1. 会話分析
print("\n【会話・地の文バランス】")
dia = DialogueAnalyzer()
result_dia = dia.analyze(novel_text)
print(dia.get_summary(result_dia))

# 2. 文体診断
print("\n【文体診断】")
stylo = StylometryAnalyzer()
vocab = stylo.analyze_vocabulary_richness(novel_text)
char_type = stylo.analyze_char_type_ratio(novel_text)
print(f"語彙豊かさ (Yule's K): {vocab['yules_k']}")
print(f"評価: {vocab['assessment']}")
print(f"文体: {char_type['style_type']}")

# 3. 表記ゆれチェック
print("\n【表記ゆれチェック】")
ortho = OrthographyVariantDetector()
issues = ortho.check(novel_text)
if issues:
    print(ortho.get_summary(issues))
else:
    print("表記ゆれは検出されませんでした")

# 4. 登場人物ネットワーク
print("\n【登場人物ネットワーク】")
net = CharacterNetworkGenerator()
df_edges = net.generate_edgelist(novel_text, top_n_nodes=15)
if not df_edges.empty:
    important = net.get_important_nodes(df_edges, top_n=5)
    print("重要な登場人物:")
    print(important[['node', 'weighted_degree']])
    # エクスポート
    net.to_csv(df_edges, "novel_network.csv")
    print("ネットワーク図データ: novel_network.csv に保存")
```

### プロンプト最適化ワークフロー

```python
from japhrase import PromptOptimizer

# Step 1: 過去の良い画像のプロンプトを収集
good_prompts = [
    "1/50 best quality, masterpiece, 1girl, looking at viewer, ...",
    # ... (50個以上推奨)
]

# Step 2: オプティマイザーを初期化
opt = PromptOptimizer(good_prompts)

# Step 3: 新しいプロンプトをチェック
while True:
    new_prompt = input("プロンプトを入力: ")
    if new_prompt.lower() == 'quit':
        break

    result = opt.analyze(new_prompt)
    print(opt.get_report(new_prompt))

    if result['suggestions']:
        print("\n推奨追加タグ:")
        for sug in result['suggestions']:
            print(f"  - {sug['tag']}")
```

---

## ⚠️ 注意事項

### 外部依存について

- **DialogueAnalyzer**: 括弧検出のみで完全に独立
- **StylometryAnalyzer, CharacterNetworkGenerator**: `PhraseExtracter` に依存（同一パッケージ）
- **OrthographyVariantDetector**: `PhraseExtracter` + `SimilarityAnalyzer` に依存
- **PromptOptimizer**: 完全に独立

### パフォーマンスについて

- テキストサイズ 10,000字以下: リアルタイム処理可能
- テキストサイズ 100,000字以上: 数秒～十秒程度の処理時間

### 精度について

統計ベースのため、100% の正確性は保証しません：
- 表記ゆれ検出は閾値調整で精度を変更可能
- ネットワーク分析は window_size の選択が重要
- コーパスサイズが大きいほど PromptOptimizer の提案精度が向上

---

## 参考資料

- **Stylometry（計量文学）**: 著者判定、文体分類の学問
- **TTR (Type-Token Ratio)**: 基本的な語彙多様性指標
- **Yule's K**: Gustav Yule が開発した長文向け語彙多様性指標
- **編集距離（Levenshtein Distance）**: 2つの文字列の「距離」

---

## FAQ

**Q: PromptOptimizer を使わずに PromptOptimizer クラスだけを使えますか？**
A: はい。`from japhrase import PromptOptimizer` で直接利用できます。

**Q: ネットワークを可視化したいのですが？**
A: GEXF 形式で出力して Gephi（無料ソフト）で可視化できます。

**Q: 日本語以外のテキストに対応していますか？**
A: PromptOptimizer は言語非依存です。その他は日本語向けです。

---

**最後に**: ご質問や改善提案がありましたら、GitHub Issues までお願いします！
