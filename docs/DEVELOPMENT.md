# 開発者ガイド

開発環境のセットアップから、テスト実行、プルリクエストまでの流れを説明します。

## 🚀 開発環境セットアップ

### 1. リポジトリのクローン

```bash
git clone https://github.com/tshim1zu/japhrase.git
cd japhrase
```

### 2. 仮想環境の作成と有効化

```bash
# venv を使用
python -m venv venv
source venv/bin/activate  # Linux/macOS
# または
venv\Scripts\activate  # Windows
```

### 3. 開発依存関係のインストール

```bash
pip install -e ".[dev]"
```

このコマンドで以下がインストールされます：
- `japhrase` パッケージ（開発モード）
- `pytest`、`pytest-cov`：テストフレームワーク
- `black`、`flake8`：コードフォーマッタとリンター
- `sphinx`：ドキュメント生成
- その他開発ツール

## 🧪 テスト実行

### 全テストを実行

```bash
pytest
```

### カバレッジレポート付きで実行

```bash
pytest --cov=japhrase
```

### 特定のテストファイルを実行

```bash
pytest tests/test_checker.py -v
```

### 特定のテストケースを実行

```bash
pytest tests/test_checker.py::TestChecker::test_extract_ngrams -v
```

## 📝 コード品質チェック

### コードフォーマット（Black）

```bash
black japhrase/ tests/
```

### リント（Flake8）

```bash
flake8 japhrase/ tests/
```

### 全チェック

```bash
# Black + Flake8 + Tests
black japhrase/ tests/ && flake8 japhrase/ tests/ && pytest
```

## 📚 プロジェクト構造

```
japhrase/
├── __init__.py                 # パッケージ初期化
├── extracter.py                # PhraseExtracter メインクラス
├── checker.py                  # テキスト処理（分かち書き等）
├── comparison_analyzer.py       # ComfyUI 用プロンプト比較分析（NEW）
├── similarity.py               # 類似度分析（SimilarityAnalyzer）
├── patterns.py                 # 正規表現パターン定義
├── config.py                   # 設定管理
├── constants.py                # 定数定義（デフォルト値）
├── datasource.py               # データソース管理（辞書等）
├── utils.py                    # ユーティリティ関数
├── use_cases.py                # ユースケース定義
├── cli.py                      # CLIコマンド実装
├── workflow.py                 # ワークフロー管理
├── evaluation.py               # 評価指標計算
├── writing_assistant.py        # 執筆支援機能
└── writing_tools.py            # 執筆ツール

tests/
├── test_extracter.py           # 抽出機能のテスト
├── test_checker.py             # チェッカー機能のテスト
├── test_patterns.py            # パターン処理のテスト
├── test_similarity.py          # 類似度分析のテスト
├── test_presets.py             # プリセット機能のテスト
├── test_utils.py               # ユーティリティのテスト
├── test_workflow.py            # ワークフロー機能のテスト
├── test_cli.py                 # CLIのテスト
└── ... (その他テスト)

scripts/
├── run_comfy_analysis.py       # ComfyUI プロンプト比較実行スクリプト
├── generate_comfy_toy_dataset.py  # テストデータセット生成
└── ... (その他スクリプト)

docs/
├── README.md                   # メインドキュメント
├── COMFY_OPTIMIZATION.md       # ComfyUI ガイド（NEW!）
├── API_REFERENCE.md            # API リファレンス
├── USAGE.md                    # 詳細な使用ガイド
├── POSITIONING.md              # 設計思想と位置づけ
└── DEVELOPMENT.md              # このファイル
```

## 🔧 コア概念

### フレーズ抽出の流れ

1. **テキスト読み込み** (`extracter.py`)
   - 複数フォーマット対応（TXT、CSV、TSV、Excel）
   - エンコーディング自動検出

2. **テキスト前処理** (`checker.py`)
   - 日本語分かち書き（MeCab 使用）
   - 不要な記号・空白の除去

3. **N-gram生成**
   - 指定された最大N（デフォルト 3）までのN-gramを生成
   - 複数N-gramの組み合わせで精度向上

4. **スコアリング** (`evaluation.py`)
   - **PMI（自己相互情報量）**: フレーズの独立性を評価
   - **分岐エントロピー**: 文脈の一貫性を評価
   - **カイ二乗検定**: 統計的有意性を判定

5. **フィルタリングと出力**
   - スコアでランキング
   - 複数フォーマットで出力（CSV、JSON、Excel、DataFrameなど）

### ComfyUI プロンプト最適化（NEW!）

Good と Bad のテキストコーパスを比較し、最適化要素を抽出：

- **TF-IDF スコアリング**: 両コーパスでの出現頻度差を計算
- **カイ二乗検定**: 統計的有意差を確認
- **ComfyUI JSON出力**: prompt タグとして直接利用可能

詳細は [COMFY_OPTIMIZATION.md](COMFY_OPTIMIZATION.md) を参照

## 📦 プリセット開発

新しい用途別プリセットを追加する手順：

### 1. プリセット定義

`japhrase/use_cases.py` に新しいプリセット定義を追加：

```python
PRESETS = {
    "my_preset": {
        "min_count": 3,
        "max_ngram": 3,
        "topn": 20,
        "entropy_weight": 0.5,
        "pmi_weight": 0.5,
    }
}
```

### 2. テストケース追加

`tests/test_presets.py` でテストを追加：

```python
def test_my_preset():
    df = PhraseExtracter.preset('my_preset').extract(sample_texts)
    assert len(df) > 0
    assert 'score' in df.columns
```

### 3. ドキュメント更新

[USAGE.md](USAGE.md) に使用例を追加

## 🚀 新機能追加の流れ

1. **Issue を作成**（機能説明、実装方針）
2. **ブランチを切る** (`git checkout -b feature/your-feature`)
3. **実装＋テストを追加**
4. **テスト＆QAチェック** （`pytest --cov`, `black`, `flake8`）
5. **ドキュメント更新**
6. **プルリクエスト作成**

## 📊 パフォーマンス最適化

### メモリ効率

大規模テキスト処理時は `from_files()` で逐次処理：

```python
extractor = PhraseExtracter()
for df_chunk in extractor.from_files(["file1.txt", "file2.txt"]):
    # チャンク単位で処理
    print(df_chunk)
```

### 処理速度

- キャッシング機構を活用
- N-gram サイズを最小限に
- 不要なカイ二乗検定をスキップ（`with_chi2=False`）

## 🔗 関連リソース

- **GitHub Issues**: バグ報告・機能リクエスト
- **GitHub Discussions**: 質問・ディスカッション
- **PyPI**: パッケージ情報

## 📝 ライセンス

MIT License。詳細は [LICENSE](../LICENSE) を参照
