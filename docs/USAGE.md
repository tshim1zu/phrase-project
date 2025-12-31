# 使用ガイド

## 基本的な使い方

### 1. 最もシンプルな使い方

```python
from japhrase import PhraseExtracter

# テキストのリストから抽出
sentences = [
    "フォローありがとうございます",
    "フォローしてください",
    "プレゼントキャンペーン開催中"
]

extractor = PhraseExtracter()
df = extractor.get_dfphrase(sentences)
print(df)
```

### 2. ファイルから直接抽出（クラスメソッド）

```python
from japhrase import PhraseExtracter

# テキストファイルから直接抽出
df = PhraseExtracter.from_file("input.txt")

# CSVファイルから抽出（列を指定）
df = PhraseExtracter.from_file("data.csv", column="text")

# パラメータを指定して抽出
df = PhraseExtracter.from_file("input.txt", min_count=10, max_length=20)
```

### 3. ファイルから直接抽出（インスタンスメソッド）

```python
from japhrase import PhraseExtracter

# インスタンスを作成してパラメータを設定
extractor = PhraseExtracter(
    min_count=10,
    max_length=20,
    verbose=1
)

# ファイルから抽出
df = extractor.extract("input.txt")
```

### 4. 複数ファイルから抽出

```python
from japhrase import PhraseExtracter

# 複数のファイルをまとめて処理
files = ["file1.txt", "file2.txt", "file3.txt"]
df = PhraseExtracter.from_files(files, min_count=5)
```

### 5. 結果のエクスポート

```python
from japhrase import PhraseExtracter

extractor = PhraseExtracter()
df = extractor.extract("input.txt")

# CSVに出力（Excel対応のBOM付きUTF-8）
extractor.export_csv(df, "output.csv")

# JSONに出力
extractor.export_json(df, "output.json")

# Excelに出力
extractor.export_excel(df, "output.xlsx")

# ネストされたディレクトリも自動作成
extractor.export_csv(df, "results/2025/output.csv")
```

## ワンライナーで完結

```python
from japhrase import PhraseExtracter

# ファイル読み込み → 抽出 → CSV出力を一行で
extractor = PhraseExtracter()
extractor.export_csv(
    PhraseExtracter.from_file("input.txt", min_count=10),
    "output.csv"
)
```

## パラメータ詳細

### よく使うパラメータ

```python
extractor = PhraseExtracter(
    min_count=6,              # 最小出現回数（小さいと計算時間が増える）
    max_length=16,            # フレーズの最大文字数
    min_length=4,             # フレーズの最小文字数
    threshold_originality=0.5,# 類似フレーズ除去の閾値（0.0〜1.0）
    verbose=1,                # 進捗表示（0:非表示, 1:表示）
)
```

### すべてのパラメータ

```python
extractor = PhraseExtracter(
    min_count=6,                    # フレーズ出現回数の最小閾値
    max_length=16,                  # フレーズの最大文字数
    min_length=4,                   # フレーズの最小文字数
    weight_freq=1.0,                # 頻度への重み
    weight_len=1.0,                 # 長さへの重み
    removes="⠀ #�\n.：...",        # 除去する文字
    unnecesary=["http", "www"],     # 除外する文字列
    threshold_originality=0.5,      # 類似度閾値
    size_sentence=5000,             # 一度に処理する文数
    knowns=["既知語1", "既知語2"],  # 優先的に抽出したい既知語
    selection=1,                    # フィルタリング有効化（0:無効, 1:有効）
    verbose=1,                      # 進捗表示レベル
    positive=None,                  # カスタムポジティブフィルター
    negative=None,                  # カスタムネガティブフィルター
)
```

## 対応ファイル形式

- **テキストファイル**: `.txt`, `.text`
- **CSV**: `.csv` （列を指定可能）
- **TSV**: `.tsv` （列を指定可能）

```python
# テキストファイル
df = PhraseExtracter.from_file("data.txt")

# CSV（最初の列を使用）
df = PhraseExtracter.from_file("data.csv")

# CSV（特定の列を指定）
df = PhraseExtracter.from_file("data.csv", column="text_column")

# TSV
df = PhraseExtracter.from_file("data.tsv", column="content")
```

## 実践例

### SNSテキスト分析

```python
from japhrase import PhraseExtracter

# SNS投稿から頻出フレーズを抽出
extractor = PhraseExtracter(
    min_count=10,      # 10回以上出現
    max_length=20,     # 短めのフレーズ
    verbose=1
)

df = extractor.extract("tweets.csv")
extractor.export_excel(df, "sns_phrases.xlsx")

# 上位10件を表示
print(df.head(10))
```

### ニュース記事分析

```python
from japhrase import PhraseExtracter

# 複数の記事ファイルから専門用語を抽出
extractor = PhraseExtracter(
    min_count=5,
    max_length=30,     # 長めの用語も抽出
    min_length=3,
    threshold_originality=0.7  # 類似語を厳しく除去
)

files = ["news1.txt", "news2.txt", "news3.txt"]
df = PhraseExtracter.from_files(files)
extractor.export_csv(df, "news_terms.csv")
```

### 既知語を優先抽出

```python
from japhrase import PhraseExtracter

# 特定のキーワードを優先的に抽出
extractor = PhraseExtracter(
    knowns=["機械学習", "深層学習", "ニューラルネットワーク"],
    min_count=3
)

df = extractor.extract("academic_papers.txt")
```

## カスタムフィルター

```python
import re
from japhrase import PhraseExtracter, get_positive_patterns, get_negative_patterns

# デフォルトのパターンを取得して修正
positive = get_positive_patterns()
negative = get_negative_patterns()

# カスタムパターンを追加
positive["custom"] = re.compile(r'カスタムパターン')

# カスタムフィルターで抽出
extractor = PhraseExtracter(
    positive=positive,
    negative=negative
)
```

## トラブルシューティング

### メモリ不足の場合

```python
# size_sentenceを小さくする
extractor = PhraseExtracter(size_sentence=1000)
```

### 処理が遅い場合

```python
# min_countを大きくする（出現回数の少ないフレーズを無視）
extractor = PhraseExtracter(min_count=20)

# max_lengthを小さくする
extractor = PhraseExtracter(max_length=10)
```

### 結果が多すぎる場合

```python
# threshold_originalityを高くする（類似フレーズを削除）
extractor = PhraseExtracter(threshold_originality=0.8)

# min_countを高くする
extractor = PhraseExtracter(min_count=20)
```

## より詳しい情報

- [README.md](README.md) - プロジェクト概要
- [TESTING.md](TESTING.md) - テスト実行方法
- [example.ipynb](jphrase/example.ipynb) - Jupyter Notebookサンプル
