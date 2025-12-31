# パラメータ最適化ガイド

jphrase では、テキストに応じて最適なパラメータを自動的に見つけることができます。

## 🎯 概要

### 2つのアプローチ

1. **教師なし最適化（推奨）** - 正解データ不要
2. **教師あり最適化** - 正解データがある場合

---

## 📚 教師なし最適化

正解データなしで、内部指標を使ってパラメータを最適化します。

### 基本的な使い方

```python
from japhrase import PhraseExtracter, UnsupervisedOptimizer

# テキストデータを用意
texts = [
    "機械学習は人工知能の一分野です。",
    "深層学習はニューラルネットワークを用いた学習方法です。",
    # ... more texts
]

# 最適化実行
optimizer = UnsupervisedOptimizer(
    param_grid={
        'min_count': [3, 5, 10],
        'max_length': [10, 15, 20],
        'threshold_originality': [0.5, 0.7, 0.9]
    }
)

best_params, results = optimizer.optimize(texts)

print(f"最適パラメータ: {best_params}")

# 最適パラメータで実行
df = PhraseExtracter(**best_params).get_dfphrase(texts)
```

### データソースとの連携

```python
from japhrase import UnsupervisedOptimizer
from japhrase.datasource import WikipediaSource

# Wikipediaからデータ取得
source = WikipediaSource()
texts = source.fetch_random(100)

# 最適化
optimizer = UnsupervisedOptimizer()
best_params, results = optimizer.optimize(texts)
```

---

## 🎓 教師あり最適化

正解データがある場合、より正確な最適化が可能です。

```python
from japhrase import SupervisedOptimizer

# 正解フレーズを用意
gold_phrases = [
    "機械学習",
    "深層学習",
    "自然言語処理",
    "ニューラルネットワーク"
]

# テキストデータ
texts = [...]

# 最適化実行
optimizer = SupervisedOptimizer(
    gold_phrases=gold_phrases,
    metric='f1'  # 'precision', 'recall', 'f1' から選択
)

best_params, results = optimizer.optimize(texts)

print(f"F1スコア: {max(r['score'] for r in results):.4f}")
```

---

## ⚙️ 最適化手法

### 1. グリッドサーチ（デフォルト）

全てのパラメータの組み合わせを試す。

```python
optimizer = UnsupervisedOptimizer(param_grid={...})
best_params, results = optimizer.optimize(texts, method='grid')
```

**特徴:**
- ✅ 確実に最適解を見つける
- ⚠️ パラメータが多いと時間がかかる

### 2. ランダムサーチ

ランダムにパラメータを選んで試す。

```python
best_params, results = optimizer.optimize(
    texts,
    method='random',
    n_iterations=20  # 試行回数
)
```

**特徴:**
- ✅ 高速
- ⚠️ 最適解の保証なし

---

## 📊 評価指標

### 教師なし評価の指標

| 指標 | 説明 | 理想値 |
|------|------|--------|
| **diversity** | フレーズの多様性 | 高いほど良い |
| **coverage** | 元テキストのカバー率 | 適度な値 |
| **balance** | 頻度分布のバランス | 0.5前後 |
| **length** | 平均文字長の適切さ | 6文字前後 |

### 詳細スコアの取得

```python
from japhrase.evaluation import UnsupervisedEvaluator

evaluator = UnsupervisedEvaluator()
scores = evaluator.get_detailed_scores(phrases, texts, df)

for metric, score in scores.items():
    print(f"{metric}: {score:.4f}")
```

---

## 🎨 カスタマイズ

### パラメータグリッドのカスタマイズ

```python
optimizer = UnsupervisedOptimizer(
    param_grid={
        'min_count': [2, 5, 10, 20],           # 最小出現回数
        'max_length': [8, 12, 16, 20],         # 最大文字数
        'min_length': [3, 4, 5],               # 最小文字数
        'threshold_originality': [0.3, 0.5, 0.7, 0.9]  # 類似度閾値
    }
)
```

### 評価器のカスタマイズ

```python
from japhrase.evaluation import UnsupervisedEvaluator

# 評価指標の重みを調整
evaluator = UnsupervisedEvaluator(
    weight_diversity=2.0,    # 多様性を重視
    weight_coverage=1.0,
    weight_balance=1.0,
    weight_length=0.5
)

optimizer = UnsupervisedOptimizer(evaluator=evaluator)
```

---

## 💾 結果の保存と再利用

### 最適パラメータの保存

```python
import json

# 最適化実行
best_params, results = optimizer.optimize(texts)

# JSONで保存
with open('optimal_params.json', 'w') as f:
    json.dump(best_params, f, indent=2)

# 全結果も保存
with open('optimization_results.json', 'w') as f:
    json.dump(results, f, indent=2)
```

### 保存したパラメータの読み込み

```python
import json
from japhrase import PhraseExtracter

# パラメータ読み込み
with open('optimal_params.json', 'r') as f:
    params = json.load(f)

# 使用
extractor = PhraseExtracter(**params)
df = extractor.extract("new_data.txt")
```

---

## 📈 実践例

### 例1: SNSテキスト向け最適化

```python
from japhrase import UnsupervisedOptimizer
from japhrase.datasource import TextFileSource

# SNS投稿データ取得
source = TextFileSource(["tweets.txt"])
texts = source.fetch()

# SNS向けパラメータで最適化
optimizer = UnsupervisedOptimizer(
    param_grid={
        'min_count': [5, 10, 15],      # 頻出フレーズ
        'max_length': [10, 15, 20],    # 短めのフレーズ
        'threshold_originality': [0.7, 0.9]  # 類似語を厳しく除去
    }
)

best_params, _ = optimizer.optimize(texts)

# プリセットとして保存
with open('sns_preset.json', 'w') as f:
    json.dump(best_params, f)
```

### 例2: 学術論文向け最適化

```python
optimizer = UnsupervisedOptimizer(
    param_grid={
        'min_count': [3, 5, 8],        # 専門用語は少ない
        'max_length': [15, 20, 30],    # 長めの用語
        'threshold_originality': [0.5, 0.7]
    }
)

best_params, _ = optimizer.optimize(academic_texts)
```

### 例3: 複数ドメインで最適化

```python
domains = {
    'sns': sns_texts,
    'news': news_texts,
    'academic': academic_texts
}

optimal_params = {}

for domain, texts in domains.items():
    print(f"\n{domain} 最適化中...")
    optimizer = UnsupervisedOptimizer()
    best_params, _ = optimizer.optimize(texts)
    optimal_params[domain] = best_params

# 全ドメインの結果を保存
with open('all_domain_params.json', 'w') as f:
    json.dump(optimal_params, f, indent=2)
```

---

## 🚀 デモの実行

サンプルデモを用意しています：

```bash
# ローカルファイルで最適化デモ
python examples/optimization_demo.py local

# Wikipediaデータで最適化デモ
python examples/optimization_demo.py wikipedia

# 評価器のデモ
python examples/optimization_demo.py eval
```

---

## ⏱️ パフォーマンスのヒント

### 1. パラメータ数を減らす

```python
# 悪い例：81通り（3×3×3×3）
param_grid = {
    'min_count': [3, 5, 10],
    'max_length': [10, 15, 20],
    'min_length': [3, 4, 5],
    'threshold_originality': [0.5, 0.7, 0.9]
}

# 良い例：8通り（2×2×2）
param_grid = {
    'min_count': [5, 10],
    'max_length': [10, 20],
    'threshold_originality': [0.5, 0.9]
}
```

### 2. ランダムサーチを使う

```python
# グリッドサーチより高速
best_params, _ = optimizer.optimize(
    texts,
    method='random',
    n_iterations=20
)
```

### 3. テキスト量を調整

```python
# 大量のテキストがある場合はサンプリング
import random
sampled_texts = random.sample(texts, min(1000, len(texts)))

optimizer.optimize(sampled_texts)
```

---

## 🔧 トラブルシューティング

### エラー: No valid results found

**原因:** すべてのパラメータの組み合わせで抽出に失敗

**解決策:**
```python
# min_count を小さくする
param_grid = {
    'min_count': [2, 3, 5],  # より小さい値を試す
    ...
}
```

### 最適化に時間がかかる

**解決策:**
1. パラメータグリッドを小さくする
2. ランダムサーチを使う
3. テキスト量を減らす
4. `verbose=0` で進捗表示を無効化

---

## 📚 関連ドキュメント

- [USAGE.md](USAGE.md) - 基本的な使い方
- [DATA_SOURCES.md](DATA_SOURCES.md) - データ取得方法
- [README.md](README.md) - プロジェクト概要
