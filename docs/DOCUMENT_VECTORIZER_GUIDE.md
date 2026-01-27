# DocumentVectorizer ユーザーガイド

NMF（Non-negative Matrix Factorization）を使用した、複数ドキュメントのベクトル化と差分分析を行うモジュールです。

## 概要

複数のテキストドキュメントを自動的に「トピック」に分解し、文書間の意味的・文体的な差分を分析します。

### 主な特徴

- **NMFによるトピック抽出**: テキストを自動的に意味のあるトピックに分解
- **PMIフィルタリング**: 意味的な差だけでなく、著者の文体・手癖の差も検出可能
- **テキストベース分析**: グラフ出力なしにコンソール上で結果を理解できる
- **複数の出力形式**: CSV、JSON、Pickle など様々な形式で結果を保存

---

## 基本的な使い方

### 1. コマンドラインから使用（最も簡単）

```bash
# 基本的な使用例
japhrase vectorize doc1.txt doc2.txt doc3.txt -t 10 -o results/

# 意味的な差を検出（TF-IDFモード、デフォルト）
japhrase vectorize file1.txt file2.txt -t 5 -o output/ -m tfidf

# 文体・手癖の差を検出（Low-PMI モード）
japhrase vectorize author1.txt author2.txt -t 5 -o output/ \
  -m low_pmi --pmi-threshold 3.0
```

### 2. Python API から使用

```python
from japhrase import DocumentVectorizer

# ドキュメントをベクトル化
vectorizer = DocumentVectorizer(
    n_topics=10,
    feature_mode='tfidf',  # またはlow_pmi, high_pmi, hybrid
)

result = vectorizer.from_files([
    'doc1.txt',
    'doc2.txt',
    'doc3.txt'
])

# テキスト形式で結果を表示
text = vectorizer.format_document_profiles_as_text(result)
print(text)

# 差分を計算して表示
diffs = vectorizer.calculate_differences(result, 0, [1, 2])
diff_text = vectorizer.format_differences_as_text(diffs)
print(diff_text)
```

---

## 出力の見方

### 1. ドキュメント-トピック プロファイル

```
【doc1.txt】
  Topic 0: ████████████████░░░░░░░░░░░░░  52.3%
  Topic 1: ██████████░░░░░░░░░░░░░░░░░░░░  28.5%
  Topic 2: ███░░░░░░░░░░░░░░░░░░░░░░░░░░░  10.2%
  Topic 3: █░░░░░░░░░░░░░░░░░░░░░░░░░░░░░   9.0%
```

**意味**:
- 各トピックがドキュメントにどの程度含まれているかを表示
- バーの長さ = そのトピックの重要度
- 100%の合計になるよう正規化されている
- ドキュメントの「トピックシグネチャ」として機能

### 2. トピック詳細分析

```
【Topic 0】
   1. 学習              ██████████████████████████████ 0.996
   2. データ             ███████████████ 0.514
   3. 処理              ██████████████ 0.485
   4. ニューラル         ██████████ 0.362
   5. ネットワーク       ██████████ 0.354
```

**意味**:
- 各トピックを特徴付ける「キーターム」を表示
- バーの長さ = そのトピック内での重要度
- 最初のタームほど、そのトピックの中心的な要素

### 3. ドキュメント間の距離

```
          doc1.txt  doc2.txt  doc3.txt
doc1.txt       0.0       0.45       0.87
doc2.txt       0.45      0.0        0.62
doc3.txt       0.87      0.62       0.0
```

**意味**:
- コサイン距離（0～1）で表現
- **0に近い** = トピック分布が似ている（内容が類似）
- **1に近い** = トピック分布が異なっている（内容が相異）
- 対角線は常に 0（自身との距離）

例：
- doc1 と doc2: 距離 0.45 → やや異なる
- doc1 と doc3: 距離 0.87 → 大きく異なる

### 4. トピック差分分析

```
トピック         doc2.txt    |    doc3.txt
─────────────────────────────────────────
  T0    |  ▲ 0.234  |  ▼ -0.156
  T1    |  ▼ -0.089 |  ▲ 0.412
  T2    |  ▲ 0.145  |  ░ 0.001
```

**記号の意味**:
- **▲** = 比較ドキュメントが参照ドキュメントより高い
- **▼** = 参照ドキュメントが比較ドキュメントより高い
- **正の値** = 比較ドキュメントがより強く持つトピック
- **負の値** = 参照ドキュメントがより強く持つトピック

**読み方の例**:
- T0 で▲ 0.234 → doc2.txt は doc1.txt より Topic 0 をより多く含む
- T1 で▼ -0.089 → doc1.txt は doc2.txt より Topic 1 をより多く含む

---

## 3つの特徴抽出モード

### モード1: TF-IDF （デフォルト）

```bash
japhrase vectorize doc1.txt doc2.txt -m tfidf
```

**特徴**:
- 各ドキュメントで最も重要な「意味のある表現」を抽出
- 機械学習や自然言語処理など、**トピック的な差分**を検出

**用途**:
- 異なるジャンルのテキスト比較（技術文 vs ビジネス文 vs 文学作品）
- 論文の主要テーマの抽出
- 記事カテゴリの自動判定

**出力例**:
```
Topic 0: 機械学習、データ、アルゴリズム
Topic 1: ビジネス、戦略、マーケット
Topic 2: 創作、表現、文学
```

### モード2: Low-PMI （新機能）

```bash
japhrase vectorize author1.txt author2.txt -m low_pmi --pmi-threshold 3.0
```

**特徴**:
- **意味的には重要でないが頻繁に出現する表現**を抽出
- PMI（Pointwise Mutual Information）が低い = 組み合わせの意味が薄い
- 固定表現や著者の無意識的な癖が浮かび上がる

**用途**:
- **著者の文体・手癖を検出** ✨
- 複数著者の識別
- 執筆スタイルの分析
- 「口癖」の定量化

**出力例（著者A）**:
```
Topic 0: 本件、以下、まずもって、要するに
Topic 1: 念のため、ご確認、施策、推奨
Topic 2: 了解、承知、つきましては
```

**出力例（著者B）**:
```
Topic 0: 本当に、実は、実のところ、心の
Topic 1: とても、大切、姿勢、準備
Topic 2: 忘れ、がちで、思う
```

**分析**: 著者Aは実務的で定型表現が多く、著者Bは感情的で反復表現が多い

### モード3: High-PMI

```bash
japhrase vectorize doc1.txt doc2.txt -m high_pmi --pmi-threshold 3.0
```

**特徴**:
- 意味的に強く結びついた表現（専門用語、複合語）を抽出
- PMI が高い = 組み合わせに強い意味がある

**用途**:
- 専門用語の抽出と比較
- ドメイン特有の表現の検出

### モード4: Hybrid

```bash
japhrase vectorize doc1.txt doc2.txt -m hybrid
```

**特徴**:
- 複数のモードを組み合わせた分析

---

## 実践的な使用例

### 例1: 複数の学生レポートを比較

```bash
# 3つの学生レポートを分析
japhrase vectorize \
  student1_report.txt \
  student2_report.txt \
  student3_report.txt \
  -t 10 -o report_analysis/ \
  -m tfidf

# 結果の見方:
# - 各学生がどのトピックに焦点を当てているか
# - 学生間でのトピック分布の差異
# - 最初の学生を基準とした差分分析
```

### 例2: 著者の手癖を分析

```bash
# 3人の著者の文体を比較
japhrase vectorize \
  author1_book.txt \
  author2_book.txt \
  author3_book.txt \
  -t 5 -o author_analysis/ \
  -m low_pmi \
  --pmi-threshold 3.0 \
  --min-count 5

# 結果の見方:
# - 各著者の習慣的な表現パターン
# - 手癖の定量的な比較
# - 著者識別の可能性
```

### 例3: Python API で結果を再利用

```python
from japhrase import DocumentVectorizer

# 分析を実行
vectorizer = DocumentVectorizer(n_topics=5, feature_mode='low_pmi')
result = vectorizer.from_files(['text1.txt', 'text2.txt'])

# テキスト出力で理解する
print(vectorizer.format_document_profiles_as_text(result))

# 結果を保存（後で再利用可能）
result.save('analysis_result.pkl')

# 必要に応じて後から読み込む
loaded_result = result.load('analysis_result.pkl')
```

---

## パラメータ詳解

### DocumentVectorizer の主なパラメータ

```python
vectorizer = DocumentVectorizer(
    # トピック関連
    n_topics=10,              # トピック数（デフォルト: 10）
                              # 大きいほど細粒度、小さいほど荒粒度

    # 特徴抽出関連
    feature_mode='tfidf',     # 'tfidf'|'low_pmi'|'high_pmi'|'hybrid'
    max_features=1000,        # 最大特徴数
    ngram_range=(2, 3),       # N-gram の文字範囲（日本語向け）

    # PMI フィルタリング関連（low_pmi/high_pmi 使用時）
    pmi_threshold=3.0,        # PMI 閾値
    min_count=6,              # 最小出現回数

    # NMF 関連
    nmf_init='nndsvd',        # 初期化方法
    nmf_max_iter=1000,        # 最大反復回数
    random_state=42,          # ランダムシード（再現性用）
)
```

### パラメータの選び方

| パラメータ | 推奨値 | 効果 |
|----------|--------|------|
| `n_topics` | 5-20 | 少なすぎると大雑把、多すぎるとノイズ |
| `pmi_threshold` | 2.5-4.0 | 低いほど多くのフレーズを抽出 |
| `min_count` | 3-10 | 低いほど稀な表現も検出 |
| `max_features` | 500-2000 | テキスト量に応じて調整 |

---

## 出力ファイル一覧

```
results/
├── vectorization_result.pkl      # Python用の結果オブジェクト
├── document_topic_matrix.csv     # ドキュメント-トピック行列
├── pairwise_distances.csv        # ドキュメント間距離
├── topic_differences.csv         # トピック差分行列
└── top_terms.json               # 各トピックの上位ターム
```

### ファイルの内容

#### document_topic_matrix.csv
```csv
,topic_0,topic_1,topic_2,topic_3
doc1.txt,0.65,0.20,0.10,0.05
doc2.txt,0.10,0.50,0.30,0.10
```
→ 正規化されたトピック分布（各行の合計=1.0）

#### pairwise_distances.csv
```csv
,doc1.txt,doc2.txt,doc3.txt
doc1.txt,0.0,0.45,0.87
doc2.txt,0.45,0.0,0.62
doc3.txt,0.87,0.62,0.0
```
→ ドキュメント間のコサイン距離

#### top_terms.json
```json
{
  "topic_0": [
    {"term": "機械学習", "score": 0.996},
    {"term": "データ", "score": 0.514},
    ...
  ]
}
```
→ 各トピックの主要なターム

---

## よくある質問

### Q1: トピック数はいくつが最適?

**A:** データによって異なります。目安：
- **小規模** (< 5000文字): 3-5
- **中規模** (5000-50000文字): 5-15
- **大規模** (> 50000文字): 10-30

試行錯誤して、結果が意味あるものになるトピック数を選択してください。

### Q2: Low-PMI モードが「フレーズが抽出されない」と言う

**A:** パラメータを調整してください：
```bash
# pmi_threshold を下げる（基準を緩くする）
--pmi-threshold 2.0

# min_count を下げる（出現回数の最小値を下げる）
--min-count 3
```

### Q3: CSV の値が 1.0 または 0.0 ばかり

**A:** テキスト量に対してトピック数が多すぎます。`n_topics` を減らしてください：
```bash
-t 3  # 少なくしてから試す
```

### Q4: CLI 出力をファイルに保存したい

**A:** リダイレクトで保存：
```bash
japhrase vectorize doc1.txt doc2.txt -t 10 -o output/ > analysis_report.txt 2>&1
```

---

## 応用例

### 応用1: 執筆支援ツール

```python
# 他の著者と比較して自分の文体を分析
vectorizer = DocumentVectorizer(n_topics=3, feature_mode='low_pmi')
result = vectorizer.from_texts([
    my_writing,      # 自分の執筆
    model_author1,   # モデル著者1
    model_author2    # モデル著者2
])

# 自分の文体の特徴を理解
diffs = vectorizer.calculate_differences(result, 0, [1, 2])
```

### 応用2: 論文の重複検出

```python
# 複数の論文のトピック分布を比較
vectorizer = DocumentVectorizer(n_topics=10, feature_mode='tfidf')
result = vectorizer.from_files(paper_list)

# 距離が小さい論文ペアを検出
distances = vectorizer.calculate_pairwise_distances(result)
```

### 応用3: カテゴリ自動判定

```python
# 既知のカテゴリテンプレートとの比較
vectorizer = DocumentVectorizer(n_topics=10)

# 学習用データ
train_result = vectorizer.from_files(training_docs)

# テスト対象
test_result = vectorizer.from_texts([new_document])

# どのカテゴリに最も近いか判定
distances = vectorizer.calculate_pairwise_distances(
    # ... 比較処理
)
```

---

## トラブルシューティング

### エラー: "No phrases found with low_pmi filtering"

**原因**: テキストが短すぎるか、設定が厳しすぎる

**解決策**:
```bash
# テキストボリュームを増やすか、パラメータを緩和
--pmi-threshold 2.0 --min-count 3
```

### 警告: "nndsvd requires n_topics (10) <= min(samples=2, features=...)"

**原因**: 正常です。自動的に random initialization に切り替わります

**対策**: 不要。処理は継続します。

### 出力がすべて 0 または 1

**原因**: トピック数が多すぎるか、ドキュメント数が少なすぎる

**解決策**:
- `n_topics` を減らす
- テキスト量を増やす

---

## 参考資料

- **NMF について**: https://scikit-learn.org/stable/modules/decomposition.html#non-negative-matrix-factorization-nmf
- **PMI について**: https://en.wikipedia.org/wiki/Pointwise_mutual_information
- **TF-IDF について**: https://scikit-learn.org/stable/modules/feature_extraction.html#tfidf-term-weighting

---

## 謝辞

このモジュールは japhrase の WritingHabitDetector と PhraseExtracter をベースに構築されています。
