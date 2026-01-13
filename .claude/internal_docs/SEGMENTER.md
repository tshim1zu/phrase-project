# TextSegmenter - テキストセグメンテーション機能

統計的テキスト分割機能。分岐エントロピー（Branching Entropy）を活用して、意味の切れ目を自動検出し、長文を複数のセグメントに分割します。

## 概要

従来のテキスト分割（固定長チャンクなど）では、文の途中で不自然に切られることがあります。本機能は **統計の力だけで「自然な切れ目」を自動判定** します。

### 応用例

- **句読点なしテキストの復元**：音声認識テキスト、古文、OCRデータ
- **RAG用チャンク分割**：LLMに入力する際の長文分割（意味の切れ目で分割）
- **読みやすさ向上**：ベタ書きテキストに意味段落ごとの改行を挿入

## インストール

```bash
pip install japhrase
```

## 基本使用法

### 1. 単純な関数呼び出し

```python
from japhrase import segment_text

text = "人工知能は機械学習の一分野です深層学習は最近注目されています自然言語処理は複雑な課題です"

# 適応的に分割（自動で切れ目を判定）
segments = segment_text(text, method='adaptive')
for i, seg in enumerate(segments):
    print(f"[{i}] {seg}")

# 出力例:
# [0] 人工知能は機械学習の一分野です
# [1] 深層学習は最近注目されています
# [2] 自然言語処理は複雑な課題です
```

### 2. TextSegmenter クラスを直接使用

```python
from japhrase import TextSegmenter

segmenter = TextSegmenter(window_size=4)  # コンテキスト長

text = "あああああああいいいいいいあああああああ"

# 方法1: エントロピー閾値ベース
segments1 = segmenter.split_by_threshold(text, threshold=0.5)

# 方法2: 上位N個で分割
segments2 = segmenter.split_top_n(text, n=2)

# 方法3: スマート分割（目的に応じた選択）
segments3 = segmenter.smart_split(text, target_chunks=3)
```

## 分割方式の選択

### A. `split_by_threshold()` - 閾値ベース

**用途**: 「意味が切れる場所すべて」で切りたい

```python
segmenter = TextSegmenter()
segments = segmenter.split_by_threshold(
    text,
    threshold=0.5,           # エントロピー閾値（0.0-1.0）
    min_chunk_length=5       # 最小チャンク長
)
```

**特徴**:
- エントロピーが高い場所を検出して自動的に分割
- 分割数が事前に決まらない（成り行き任せ）
- **句読点復元や段落検出に最適**

**パラメータ**:
- `threshold` (float): 高いほど高いエントロピーの場所で切る
  - `0.3` : 細かく分割（多くの場所で切る）
  - `0.5` : バランス型
  - `0.7` : 大雑把（重要な切れ目だけ）
- `min_chunk_length` (int): 隣同士の切り目の最小距離

### B. `split_top_n()` - Top-N方式

**用途**: 「指定した数に分割したい」（例：3分割、5分割）

```python
segmenter = TextSegmenter()
segments = segmenter.split_top_n(
    text,
    n=3,                     # 分割数
    min_chunk_length=10      # 最小チャンク長
)
```

**特徴**:
- 分割数を明確に指定できる
- エントロピーが最も高い上位 n 個の位置で分割
- **RAG用チャンク分割に最適**

**返り値**: `n+1` 個のセグメント

### C. `smart_split()` - スマート分割

**用途**: 自動選択（目的に応じた柔軟な分割）

```python
segmenter = TextSegmenter()

# パターン1: チャンク数を指定
segments = segmenter.smart_split(text, target_chunks=3)

# パターン2: 閾値を指定
segments = segmenter.smart_split(text, threshold=0.6)

# パターン3: 自動適応（推奨）
segments = segmenter.smart_split(text)
```

**特徴**:
- 目的に応じて最適な方式を自動選択
- パラメータなしでも動作（デフォルト設定を使用）

## 応用例

### 例1: 句読点なしテキストに「。」を挿入

```python
from japhrase import TextSegmenter

text = "今日の天気は晴れです明日も晴れる予報です気温は25度まで上がるでしょう"
segmenter = TextSegmenter()

# 統計的に正しい位置に「。」を挿入
result = segmenter.add_punctuation(text, marker="。")
print(result)
# 出力: 今日の天気は晴れです。明日も晴れる予報です。気温は25度まで上がるでしょう。
```

### 例2: RAG用チャンク分割

```python
from japhrase import TextSegmenter

long_document = """
人工知能（AI）は急速に発展しています。
機械学習は数学的な最適化問題として定式化されます。
深層学習はニューラルネットワークの多層構造を利用します。
...（長いテキスト）
"""

segmenter = TextSegmenter()

# 3つのチャンクに分割して LLM に入力
chunks = segmenter.split_top_n(long_document, n=3)

for i, chunk in enumerate(chunks):
    # LLM の入力サイズに合わせて処理
    print(f"\n--- Chunk {i+1} ---")
    print(chunk)
```

### 例3: 読みやすさ向上

```python
from japhrase import segment_text

# ベタ書きの長文テキスト
poorly_formatted = "私たちはこのプロジェクトに取り組んでいます最初の段階では要件定義を行いました次に実装フェーズに進みました最後にテストを実施しました"

# 意味段落ごとに改行を挿入
formatted = segment_text(poorly_formatted, method='threshold', threshold=0.4)
readable_text = "\n".join(formatted)

print(readable_text)
# 出力:
# 私たちはこのプロジェクトに取り組んでいます
# 最初の段階では要件定義を行いました
# 次に実装フェーズに進みました
# 最後にテストを実施しました
```

## 分析機能

### エントロピープロファイルの分析

```python
from japhrase import TextSegmenter

text = "あああああああいいいいいいああああああああ"
segmenter = TextSegmenter()

# エントロピープロファイルの統計情報を取得
analysis = segmenter.analyze_entropy_profile(text)

print(f"平均エントロピー: {analysis['mean']:.3f}")
print(f"標準偏差: {analysis['std']:.3f}")
print(f"最小値: {analysis['min']:.3f}")
print(f"最大値: {analysis['max']:.3f}")
print(f"ピーク位置: {analysis['peaks']}")
```

## パラメータガイド

### window_size

コンテキスト長（直前何文字を見るか）

```python
segmenter = TextSegmenter(window_size=4)  # デフォルト: 4
```

- `2-3`: 短い文脈に基づく（敏感）
- `4-6`: バランス型（推奨）
- `8+`: 長い文脈に基づく（保守的）

### threshold（閾値ベース分割）

エントロピー閾値（0.0-1.0）

```python
# 細かく分割（低い閾値）
segments = segmenter.split_by_threshold(text, threshold=0.3)

# 標準的な分割（中程度）
segments = segmenter.split_by_threshold(text, threshold=0.5)

# 大雑把に分割（高い閾値）
segments = segmenter.split_by_threshold(text, threshold=0.7)
```

### min_chunk_length

最小チャンク長（これ以下の距離での分割を防ぐ）

```python
# デフォルト: 5（文字）
segments = segmenter.split_by_threshold(text, min_chunk_length=10)
```

## 理論的背景

### 分岐エントロピーとは

分岐エントロピー（Branching Entropy; BE）は、与えられたコンテキストの直後に現れる文字の多様性を測ります。

$$BE = -\sum_{x} P(x) \log P(x)$$

ここで $P(x)$ はコンテキスト直後の各文字の出現確率。

**意義**:
- **低いBE** = 「人工知」のように、次の文字がほぼ確定（「能」）→ 単語の途中
- **高いBE** = 「人工知能」のように、次に何が来るか多様（「は」「が」「を」など）→ **単語の境界**

### テキスト分割への応用

テキストを左から右にスキャンしながら、各位置のエントロピーを計算します。

- **高いエントロピー = 分割の候補地点**
- 峰（ピーク）を見つけて切る

```
テキスト: 人工知能は機械学習だ
位置    : 0 1 2 3 4 5 6 7 8 9...
エント  : 低低低[高] 低低低[高] 低
         ↑ここ    ↑ここで切る
```

## トラブルシューティング

### 分割が細かすぎる / 粗すぎる

閾値を調整してください：

```python
# 粗い分割に
segments = segmenter.split_by_threshold(text, threshold=0.7)

# 細かい分割に
segments = segmenter.split_by_threshold(text, threshold=0.3)
```

### 不自然な位置で切れている

`min_chunk_length` を増やしてください：

```python
segments = segmenter.split_by_threshold(text, min_chunk_length=15)
```

### Top-N方式で期待より少ない分割数になった

これは正常な動作です。`min_chunk_length` 制約により、相互に近い切り目は統合されます。

```python
# より多く分割したい場合
segments = segmenter.split_top_n(text, n=5, min_chunk_length=5)
```

## ベストプラクティス

1. **まずは `smart_split()` から始める**
   ```python
   segments = segmenter.smart_split(text)
   ```

2. **テキストのドメインに合わせて調整**
   - 小説など「自然な段落」：`threshold=0.5`
   - ニュース記事：`threshold=0.6`
   - 技術文書：`split_top_n` でチャンク数固定

3. **分析機能で動作を確認**
   ```python
   analysis = segmenter.analyze_entropy_profile(text)
   print(analysis['peaks'])  # ピーク位置を確認
   ```

## 参考文献

- Harris, Z. S. (1954). "Distributional structure". *Word*, 10(2-3), 146-162.
- Kenesei, I. (1986). "On defining the word". *Indiana University Linguistics Club*.
- Jin, Z., & Tanaka-Ishii, K. (2006). "Unsupervised segmentation of Chinese text using boundary entropy". ACL.

## 今後の拡張

- [ ] 言語別の最適パラメータプリセット
- [ ] マルチ言語サポート
- [ ] セグメント品質スコアの計算
- [ ] インタラクティブなパラメータ調整UI
