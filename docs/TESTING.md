# テストガイド

## セットアップ

### 依存パッケージのインストール

```bash
# 開発用パッケージのインストール
pip install -r requirements-dev.txt
```

または

```bash
# pytestのみインストール
pip install pytest pytest-cov
```

## テストの実行

### すべてのテストを実行

```bash
pytest
```

または

```bash
python -m pytest
```

### 詳細な出力で実行

```bash
pytest -v
```

### 特定のテストファイルのみ実行

```bash
pytest tests/test_extracter.py
```

### 特定のテストクラスのみ実行

```bash
pytest tests/test_extracter.py::TestPhraseExtracterInit
```

### 特定のテスト関数のみ実行

```bash
pytest tests/test_extracter.py::TestPhraseExtracterInit::test_create_instance_default
```

### カバレッジレポートを生成

```bash
pytest --cov=jphrase --cov-report=html
```

カバレッジレポートは `htmlcov/index.html` に生成されます。

## テストファイルの構成

```
tests/
├── __init__.py
├── test_constants.py      # 定数のテスト
├── test_patterns.py       # パターンマッチングのテスト
└── test_extracter.py      # メイン機能のテスト
```

## テストの種類

### test_constants.py
- `FIRST_KANJI`などの定数が正しく定義されているかを確認
- デフォルト値の検証

### test_patterns.py
- 正規表現パターンが正しく動作するかを確認
- ポジティブフィルター（抽出したいパターン）のテスト
- ネガティブフィルター（除外したいパターン）のテスト

### test_extracter.py
- `PhraseExtracter`クラスの各メソッドのテスト
- 統合テスト（実際のフレーズ抽出）
- エッジケースのテスト

## CI/CDへの組み込み

GitHub Actionsなどで自動テストを実行する場合：

```yaml
- name: Install dependencies
  run: |
    pip install -r requirements-dev.txt

- name: Run tests
  run: |
    pytest --cov=jphrase --cov-report=xml
```

## トラブルシューティング

### pytestが見つからない場合

```bash
pip install pytest
```

### インポートエラーが発生する場合

プロジェクトのルートディレクトリで実行していることを確認してください：

```bash
cd /path/to/phrase-project
pytest
```

または、パッケージをインストールしてください：

```bash
pip install -e .
```
