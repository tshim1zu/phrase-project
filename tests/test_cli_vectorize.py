"""`japhrase vectorize` CLIコマンドの回帰テスト(2026-09-06新設)。

chatgptへのコードレビューで指摘: `tests/test_cli.py`は`extract`/`kwic`/
`version`/`presets-list`等をテストしていたが、`vectorize`コマンドを
一度も実行していなかった。CLI経由の`--pmi-threshold`/`--min-count`が
インスタンス設定を無視していた不具合(document_vectorizer.py側で修正済み)
は、このコマンドを実際に実行する専用テストが無かったために見逃されていた。
"""
from __future__ import annotations

import tempfile
from pathlib import Path

import pytest
from click.testing import CliRunner

from japhrase.cli import cli
from japhrase import DocumentVectorizer

pytestmark = pytest.mark.skipif(
    DocumentVectorizer is None,
    reason="scikit-learn が無いとvectorizeコマンドは無効化される",
)


@pytest.fixture
def runner():
    return CliRunner()


@pytest.fixture
def doc_files(tmp_path):
    contents = [
        "機械学習は重要な技術です。" * 5,
        "深層学習は機械学習の一部です。" * 5,
        "自然言語処理は機械学習の応用です。" * 5,
    ]
    paths = []
    for i, c in enumerate(contents):
        p = tmp_path / f"doc{i}.txt"
        p.write_text(c, encoding="utf-8")
        paths.append(str(p))
    return paths


def test_vectorizeは正常終了しoutputへ成果物を書く(runner, doc_files, tmp_path):
    out_dir = tmp_path / "out"
    result = runner.invoke(cli, [
        "vectorize", *doc_files,
        "-t", "2", "-o", str(out_dir),
    ])

    assert result.exit_code == 0, result.output
    assert (out_dir / "vectorization_result.pkl").is_file()
    assert (out_dir / "document_topic_matrix.csv").is_file()
    assert (out_dir / "topic_differences.csv").is_file()


def test_vectorizeはlow_pmiモードでも正常終了する(runner, tmp_path):
    """CLIの`--pmi-threshold`/`--min-count`がインスタンス設定として
    実際に伝わることの間接確認(伝わらなければ既定値のまま動くだけで
    差は見えないが、少なくともlow_pmiモード自体がクラッシュしないこと
    を固定する)。"""
    contents = [
        ("実際のところ、この方法は非常に効果的です。実際のところ、"
         "多くの人が使用しています。") * 5,
        ("以下の内容をご説明させていただきたく存じます。"
         "まずもって、本件につきましてはご報告申し上げます。") * 5,
    ]
    paths = []
    for i, c in enumerate(contents):
        p = tmp_path / f"style{i}.txt"
        p.write_text(c, encoding="utf-8")
        paths.append(str(p))

    out_dir = tmp_path / "out_low_pmi"
    result = runner.invoke(cli, [
        "vectorize", *paths,
        "-t", "2", "-o", str(out_dir),
        "--feature-mode", "low_pmi",
        "--pmi-threshold", "100.0",
        "--min-count", "1",
    ])

    assert result.exit_code == 0, result.output


def test_vectorizeのvisualizeは依存不足を分かりやすく報告する(runner, doc_files, tmp_path, monkeypatch):
    """2026-09-06、chatgptへのコードレビューで指摘・実コードで確認:
    matplotlib/seabornはdependenciesに含まれておらず、clean install後に
    `--visualize`を使うと失敗していた。以前は汎用の`except Exception`に
    まとめて拾われ、原因(要インストール)が伝わらないメッセージだった。

    このdev環境には実際にmatplotlib/seabornがインストール済みのため、
    `sys.modules`へ`None`を差し込んで`import matplotlib`自体を
    ImportErrorにする(標準的な「未インストールを装う」手法)——
    cli.py側の「事前にimportだけ試す」チェックを、実際に通す形で確認する。
    """
    import sys

    monkeypatch.setitem(sys.modules, "matplotlib", None)

    out_dir = tmp_path / "out_viz"
    result = runner.invoke(cli, [
        "vectorize", *doc_files,
        "-t", "2", "-o", str(out_dir),
        "--visualize",
    ])

    assert result.exit_code == 1
    assert "japhrase[viz]" in result.output, (
        "matplotlib/seaborn不足時に、pip install japhrase[viz]の案内が"
        f"出ていない: {result.output}"
    )


def test_pyproject_はviz_extraにmatplotlibとseabornを宣言している():
    """`--visualize`が実際にインストール可能な形になっていることの
    確認(パッケージ定義側)。"""
    import tomllib

    pyproject = Path(__file__).resolve().parent.parent / "pyproject.toml"
    data = tomllib.loads(pyproject.read_text(encoding="utf-8"))
    viz_deps = data["project"]["optional-dependencies"]["viz"]
    names = [d.split(">=")[0].split("==")[0].strip() for d in viz_deps]
    assert "matplotlib" in names
    assert "seaborn" in names
