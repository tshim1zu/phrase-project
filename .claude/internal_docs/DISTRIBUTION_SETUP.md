# PyPI 配布整備完了リポート

## 実施日
2026-01-06

## 対応内容

### 1. pyproject.toml の整備
- ✅ setuptools_scm 削除（静的バージョン管理で十分）
- ✅ setuptools 最小バージョン更新（45 → 61.0）
- ✅ Python 最小バージョン更新（3.7 → 3.8）
- ✅ 依存関係にバージョン上限を設定
  - `numpy>=1.20.0,<2.0`
  - `pandas>=1.3.0,<3.0`
- ✅ Python 3.12 対応を明記
- ✅ ライセンス指定方式を改善（`{file}` → `{text}`）
- ✅ メタデータの充実化（maintainers, readme-content-type など）
- ✅ `include-package-data = true` を追加

### 2. requirements.txt の統一
- ✅ pyproject.toml との完全同期
- ✅ 依存関係の整理と分類
- ✅ オプション依存関係を明記（コメント化）
- ✅ 開発依存関係の明記

### 3. MANIFEST.in の最適化
- ✅ パッケージ含有物の完全な指定
- ✅ examples/workflows/\*.yaml を含有
- ✅ tests/ ディレクトリを含有
- ✅ CHANGELOG.md を含有
- ✅ 不要なファイルの除外ルール強化

### 4. setup.py の廃止
- ✅ 近代的な pyproject.toml へ一本化
- ✅ backward compatibility note のみ記載

### 5. バージョン管理の統一
- ✅ japhrase/__init__.py の `__version__` を 0.1.3 に統一
- ✅ __copyright__, __license__, __email__ を追加

### 6. 配布・ビルド自動化スクリプト作成
- ✅ `scripts/build.py` を実装
  - `--check`: 設定検証
  - `--build`: wheel + source distribution ビルド
  - `--test-local`: ローカルインストーステスト
  - `--clean`: ビルドアーティファクト削除
  - `--full`: 完全なビルドパイプライン

### 7. ドキュメント整備
- ✅ `CHANGELOG.md` を作成
- ✅ `DEVELOPMENT.md` を作成（開発者ガイド）
- ✅ PyPI 公開手順を文書化

### 8. CI/CD 設定
- ✅ `tox.ini` を作成
  - Python 3.8-3.12 マルチバージョンテスト
  - lint (black, flake8)
  - type checking (mypy)
  - ビルド検証

### 9. .gitignore の強化
- ✅ より完全な除外ルール
- ✅ .test-venv/ を追加
- ✅ wheel (.whl) を追加

## 別マシンインストール失敗の原因（修正済み）

### 原因1: 依存関係が不完全
- `chardet`, `click`, `PyYAML`, `networkx` が requirements.txt に未記載
- **修正**: すべての依存関係を明記

### 原因2: バージョン指定が曖昧
- numpy, pandas に上限がなく、新版のメジャー変更で破損可能
- **修正**: `<2.0`, `<3.0` で上限を設定

### 原因3: wheel パッケージングが不完全
- MANIFEST.in が wheel に含まれていない可能性
- **修正**: `include-package-data = true` を設定

## 使用方法

### ローカルでのビルド・テスト

```bash
# 全ステップを実行
python scripts/build.py --full

# または個別実行
python scripts/build.py --check      # 設定確認
python scripts/build.py --build      # ビルド
python scripts/build.py --test-local # テスト
```

### PyPI への公開

```bash
# テスト PyPI (推奨)
twine upload --repository testpypi dist/*

# 本番 PyPI
twine upload dist/*
```

### 開発環境でのテスト実行

```bash
# 個別テスト
pytest tests/

# 全テスト with tox (複数Python版)
tox

# 型チェック
mypy japhrase

# フォーマット
black japhrase tests
```

## 配布物の検証

wheel に含まれるファイルを確認：
```bash
unzip -l dist/japhrase-0.1.3-py3-none-any.whl
```

期待される含有物：
- japhrase/
  - __init__.py
  - checker.py
  - cli.py
  - ... (全モジュール)
- examples/
  - example.ipynb
  - optimization_demo.py
  - workflows/
  - data/
- README.md
- LICENSE
- etc.

## 次のステップ

1. GitHub リポジトリの確認
   - Actions workflow の設定（optional）
   - main branch protection rules

2. PyPI アカウント設定
   - トークン生成（~/.pypirc に設定）
   - 2FA の有効化

3. 初回公開
   ```bash
   python scripts/build.py --full
   twine upload --repository testpypi dist/*  # テスト
   twine upload dist/*                         # 本番
   ```

4. リリース管理
   - タグ付け: `git tag v0.1.3`
   - GitHub Releases の作成
   - CHANGELOG.md の定期更新

## 参考資料

- [Python Packaging Guide](https://packaging.python.org/)
- [pyproject.toml Configuration](https://setuptools.pypa.io/en/latest/userguide/pyproject_config.html)
- [Semantic Versioning](https://semver.org/)
- [PyPI Help](https://pypi.org/help/)
