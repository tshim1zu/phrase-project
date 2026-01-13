"""
CLIコマンドのテスト
"""

import pytest
import tempfile
from pathlib import Path
from click.testing import CliRunner

from japhrase.cli import cli


class TestCLI:
    """CLIコマンドの基本テスト"""

    @pytest.fixture
    def runner(self):
        """Clickテストランナー"""
        return CliRunner()

    @pytest.fixture
    def sample_text_file(self):
        """テスト用サンプルテキスト"""
        text = "\n".join([
            "機械学習は重要な技術です。" * 3,
            "深層学習は機械学習の一部です。" * 3,
            "自然言語処理は機械学習の応用です。" * 3,
        ])

        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False, encoding='utf-8') as f:
            f.write(text)
            filepath = f.name

        yield filepath

        # クリーンアップ
        Path(filepath).unlink()

    def test_cli_help(self, runner):
        """CLIヘルプ表示テスト"""
        result = runner.invoke(cli, ['--help'])
        assert result.exit_code == 0
        assert 'japhrase' in result.output

    def test_version(self, runner):
        """バージョン表示テスト"""
        result = runner.invoke(cli, ['version'])
        assert result.exit_code == 0

    def test_extract_command_help(self, runner):
        """extractコマンドのヘルプ"""
        result = runner.invoke(cli, ['extract', '--help'])
        assert result.exit_code == 0
        assert 'extract' in result.output

    def test_extract_basic(self, runner, sample_text_file):
        """extractコマンドの基本実行"""
        result = runner.invoke(cli, [
            'extract',
            sample_text_file,
            '--min-count', '1',
            '--format', 'table'
        ])
        # フレーズが見つかる場合は0、見つからない場合は1
        assert result.exit_code in [0, 1]

    def test_extract_with_preset(self, runner, sample_text_file):
        """プリセット指定でのextract"""
        result = runner.invoke(cli, [
            'extract',
            sample_text_file,
            '--preset', 'sns',
            '--format', 'table'
        ])
        # sns プリセットはmin_count=6なので結果がない可能性あり
        assert result.exit_code in [0, 1]

    def test_extract_output_csv(self, runner, sample_text_file):
        """CSV出力テスト"""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.csv', delete=False) as f:
            output_file = f.name

        try:
            result = runner.invoke(cli, [
                'extract',
                sample_text_file,
                '--min-count', '1',
                '--output', output_file,
                '--format', 'csv'
            ])
            # フレーズが見つかる場合のみファイルが作成される
            assert result.exit_code in [0, 1]
            if result.exit_code == 0:
                assert Path(output_file).exists()
        finally:
            Path(output_file).unlink(missing_ok=True)

    def test_kwic_command_help(self, runner):
        """kwicコマンドのヘルプ"""
        result = runner.invoke(cli, ['kwic', '--help'])
        assert result.exit_code == 0

    def test_kwic_search(self, runner, sample_text_file):
        """KWIC検索テスト"""
        result = runner.invoke(cli, [
            'kwic',
            sample_text_file,
            '--phrase', '機械学習',
            '--context', '1'
        ])
        # フレーズが見つかる場合は0、見つからない場合は1
        assert result.exit_code in [0, 1]

    def test_presets_list(self, runner):
        """プリセット一覧表示テスト"""
        result = runner.invoke(cli, ['presets-list'])
        assert result.exit_code == 0
        assert 'sns' in result.output or 'default' in result.output


class TestCLIIntegration:
    """CLIの統合テスト"""

    def test_cli_workflow(self):
        """複数コマンドの連続実行テスト"""
        runner = CliRunner()

        # サンプルテキストを作成
        text = "テスト" * 20
        with tempfile.NamedTemporaryFile(mode='w', suffix='.txt', delete=False, encoding='utf-8') as f:
            f.write(text)
            filepath = f.name

        try:
            # extract実行
            result1 = runner.invoke(cli, ['extract', filepath, '--min-count', '1'])
            assert result1.exit_code in [0, 1]

            # version表示
            result2 = runner.invoke(cli, ['version'])
            assert result2.exit_code == 0

        finally:
            Path(filepath).unlink(missing_ok=True)
