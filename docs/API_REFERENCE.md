# japhrase - API リファレンス

## クラス: PhraseExtracter

### クラスメソッド

#### `PhraseExtracter.preset(preset_name, **kwargs)`

エビデンスベースのプリセット設定で初期化します。

**パラメータ:**
- `preset_name` (str): `'sns'`, `'news'`, `'novel'`, `'report'`, `'default'`
- `**kwargs`: パラメータの上書き

**戻り値:** PhraseExtracter インスタンス

**例:**
```python
extractor = PhraseExtracter.preset('sns', min_count=10)
```

#### `PhraseExtracter.list_presets()`

利用可能なプリセット一覧を表示します。

#### `PhraseExtracter.from_file(filepath, **kwargs)`

ファイルから直接フレーズを抽出します。

**パラメータ:**
- `filepath` (str): ファイルパス
- `encoding` (str): エンコーディング（デフォルト: 'auto'）
- `**kwargs`: 抽出パラメータ

**戻り値:** pandas.DataFrame

#### `PhraseExtracter.from_files(filepaths, **kwargs)`

複数ファイルから抽出します。

#### `PhraseExtracter.demo()`

デモデータで動作確認します。

### インスタンスメソッド

#### `__init__(min_count=6, max_length=16, min_length=4, use_pmi=False, use_branching_entropy=False, ...)`

フレーズ抽出器を初期化します。

**主要パラメータ:**
- `min_count` (int): 最小出現回数
- `max_length` (int): 最大文字数
- `min_length` (int): 最小文字数
- `use_pmi` (bool): PMI スコア使用
- `use_branching_entropy` (bool): 分岐エントロピー使用

#### `extract(texts, **kwargs)`

テキスト（ファイルまたはリスト）からフレーズを抽出します。

**パラメータ:**
- `texts` (str or list): ファイルパスまたはテキストリスト
- `verbose` (int): 進捗表示（デフォルト: 1）

**戻り値:** pandas.DataFrame
- `phrase` (str): 抽出されたフレーズ
- `count` (int): 出現回数
- `score` (float): スコア（PMI、エントロピー等）

**例:**
```python
extractor = PhraseExtracter(min_count=5)
df = extractor.extract(["テキスト1", "テキスト2"])
print(df[['phrase', 'count', 'score']].head(10))
```

#### `export_csv(df, filepath)`

結果をCSV形式で出力します。

#### `export_json(df, filepath)`

結果をJSON形式で出力します。

#### `export_excel(df, filepath)`

結果をExcel形式で出力します。

---

## クラス: ComparisonAnalyzer

プロンプト分析用クラス。Good と Bad のプロンプト群を比較して、成功テンプレートと失敗パターンを抽出します。

### コンストラクタ

#### `__init__(min_count=2, min_length=10, max_length=100, use_pmi=True)`

**パラメータ:**
- `min_count` (int): フレーズの最小出現数
- `min_length` (int): フレーズの最小文字数（短いゴミを除外）
- `max_length` (int): フレーズの最大文字数（長いテンプレートを許容）
- `use_pmi` (bool): PMI スコアリングを使用

### メソッド

#### `compare_corpora(good_texts, bad_texts)`

Good と Bad のテキストリストを比較してテンプレートを抽出します。

生のテキストのまま（カンマで分割しない）N-gram エンジンに食わせることで、「長い呪文の塊」を抽出できます。

**パラメータ:**
- `good_texts` (List[str]): 良好なプロンプトのリスト
- `bad_texts` (List[str]): 不良なプロンプトのリスト

**戻り値:** Dict
```python
{
    "winning_templates": [("phrase", score), ...],  # Good-only
    "failure_patterns": [("phrase", score), ...],   # Bad-only
    "common_baseline": [("phrase", score), ...],    # Both
    "analysis": {
        "good_count": int,
        "bad_count": int,
        "only_in_good": int,
        "only_in_bad": int,
        "common_phrases": int,
        "min_count": int,
        "min_length": int,
        "max_length": int,
        "use_pmi": bool
    }
}
```

**例:**
```python
analyzer = ComparisonAnalyzer(
    min_count=2,
    min_length=10,
    max_length=100,
    use_pmi=True
)

result = analyzer.compare_corpora(
    ["プロンプト1", "プロンプト2"],
    ["プロンプト3", "プロンプト4"]
)
```

#### `compare_from_files(good_file, bad_file)`

ファイルから Good/Bad テキストを読み込んで比較します。

**パラメータ:**
- `good_file` (Path): Good プロンプトファイルパス
- `bad_file` (Path): Bad プロンプトファイルパス

**例:**
```python
from pathlib import Path

analyzer = ComparisonAnalyzer()
result = analyzer.compare_from_files(
    Path("good_prompts.txt"),
    Path("bad_prompts.txt")
)
```

#### `generate_report(result)`

分析結果をテキストレポートとして生成します。

**パラメータ:**
- `result` (Dict): `compare_corpora()` の戻り値

**戻り値:** str（レポートテキスト）

#### `save_results(result, output_file, include_report=True)`

結果をJSON + テキストレポートで保存します。

**パラメータ:**
- `result` (Dict): 比較結果
- `output_file` (Path): 出力JSONファイルパス
- `include_report` (bool): レポートも生成するか

**戻り値:** Tuple[Path, Path] (JSON ファイルパス, レポートファイルパス)

---

## クラス: SimilarityAnalyzer

複数テキスト間の類似度分析。コピペ検出に対応。

### メソッド

#### `__init__(method='auto')`

**パラメータ:**
- `method` (str): `'auto'`, `'levenshtein'`, `'jaccard'`, `'cosine'`

#### `compare_files(filepaths)`

複数ファイルの類似度行列を計算します。

**戻り値:** pandas.DataFrame（類似度行列）

#### `find_similar_pairs(matrix, threshold=0.7)`

閾値以上の類似ペアを抽出します。

#### `export_matrix(matrix, filepath)`

類似度行列をCSV出力します。

#### `export_heatmap(matrix, filepath)`

ヒートマップ画像を生成します（matplotlib必須）。
