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

#### `extract(texts, **kwargs)`

テキスト（ファイルまたはリスト）からフレーズを抽出します。

**パラメータ:**
- `texts` (str or list): ファイルパスまたはテキストリスト
- `min_count` (int): 最小出現回数（デフォルト: 6）
- `max_length` (int): 最大文字数（デフォルト: 16）
- `min_length` (int): 最小文字数（デフォルト: 4）
- `use_pmi` (bool): PMIスコア使用（デフォルト: False）
- `use_branching_entropy` (bool): 分岐エントロピー使用（デフォルト: False）
- `verbose` (int): 進捗表示（デフォルト: 1）

**戻り値:** pandas.DataFrame

#### `export_csv(df, filepath)`

結果をCSV形式で出力します。

#### `export_json(df, filepath)`

結果をJSON形式で出力します。

#### `export_excel(df, filepath)`

結果をExcel形式で出力します。

---

## クラス: ComparisonAnalyzer

ComfyUI プロンプト分析用クラス。Good と Bad プロンプトを比較します。

### メソッド

#### `compare_corpora(good_text, bad_text)`

Good と Bad テキストを比較して差分を抽出します。

**パラメータ:**
- `good_text` (str): 良好なプロンプト（複数行またはファイルパス）
- `bad_text` (str): 不良なプロンプト（複数行またはファイルパス）

**戻り値:** Dict
```python
{
    "winning_ranked": [("phrase", score), ...],  # Good-only
    "failure_ranked": [("phrase", score), ...],  # Bad-only
    "common": [...],                              # Both
    "analysis": {
        "good_phrases": int,
        "bad_phrases": int,
        "statistical_test": "chi-square"
    }
}
```

#### `generate_report(result, top_n=10)`

分析結果をテキストレポートとして生成します。

#### `save_results(result, output_dir, filename_prefix='analysis')`

結果をJSON + テキストレポートで保存します。

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
