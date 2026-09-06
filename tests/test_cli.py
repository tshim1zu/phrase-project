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

    def test_extract_csv_without_output_errors(self, runner, sample_text_file):
        """--format csv を -o/--output なしで指定した場合はエラー終了すること

        回帰テスト: 以前は -o が無いと黙って table 表示にフォールバックし、
        CSVファイルを作らないままexit code 0で「成功」を報告していた。
        """
        result = runner.invoke(cli, [
            'extract',
            sample_text_file,
            '--min-count', '1',
            '--format', 'csv',
        ])
        assert result.exit_code == 1
        assert '--output' in result.output or '-o' in result.output

    def test_extract_json_without_output_errors(self, runner, sample_text_file):
        """--format json を -o/--output なしで指定した場合もエラー終了すること"""
        result = runner.invoke(cli, [
            'extract',
            sample_text_file,
            '--min-count', '1',
            '--format', 'json',
        ])
        assert result.exit_code == 1
        assert '--output' in result.output or '-o' in result.output

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


class TestWorkflowCommandExitCode:
    """`japhrase workflow` の終了コード回帰テスト"""

    def test_workflow_with_zero_tasks_fails(self):
        """tasks: [] のワークフローは検証エラーとしてexit code 1になること

        回帰テスト: 以前は0件のワークフローが検証を素通りし、「0/0 成功」を
        exit code 0・「すべてのタスクが正常に完了しました」として報告していた。
        """
        runner = CliRunner()
        with tempfile.TemporaryDirectory() as d:
            yaml_path = Path(d) / 'empty_workflow.yaml'
            yaml_path.write_text(
                "name: empty_workflow\ndescription: no tasks\ntasks: []\n",
                encoding='utf-8',
            )

            result = runner.invoke(cli, ['workflow', str(yaml_path)])

        assert result.exit_code == 1
        assert 'すべてのタスクが正常に完了しました' not in result.output

    def test_workflow_missing_tasks_key_fails(self):
        """tasks: キー自体が無いワークフローも同様にexit code 1になること"""
        runner = CliRunner()
        with tempfile.TemporaryDirectory() as d:
            yaml_path = Path(d) / 'no_tasks_key.yaml'
            yaml_path.write_text(
                "name: no_tasks_key\ndescription: tasks key omitted\n",
                encoding='utf-8',
            )

            result = runner.invoke(cli, ['workflow', str(yaml_path)])

        assert result.exit_code == 1


class TestCheckCommandExitCode:
    """`japhrase check` の終了コード回帰テスト"""

    def test_check_without_rules_fails(self):
        """[check]セクションが無い設定ファイルではexit code 1になること

        回帰テスト: 以前はチェックルールが1つも実行されなくても
        「✅ すべてのチェックに合格しました」exit code 0 を返していた
        (品質ゲートが何も検査していないのにgreenになるfail-open)。
        """
        runner = CliRunner()
        with tempfile.TemporaryDirectory() as d:
            doc_path = Path(d) / 'doc.txt'
            doc_path.write_text('本文です。', encoding='utf-8')

            cfg_path = Path(d) / '.japhrase.toml'
            # [check] 以外のセクションのみを持つ設定ファイル
            cfg_path.write_text("[extract]\nmin_count = 3\n", encoding='utf-8')

            result = runner.invoke(cli, ['check', str(doc_path), '--config', str(cfg_path)])

        assert result.exit_code == 1

    def test_check_with_enabled_false_succeeds(self):
        """check.enabled=falseを明示した場合は成功扱いになること(合法的なスキップ)"""
        runner = CliRunner()
        with tempfile.TemporaryDirectory() as d:
            doc_path = Path(d) / 'doc.txt'
            doc_path.write_text('本文です。', encoding='utf-8')

            cfg_path = Path(d) / '.japhrase.toml'
            cfg_path.write_text("[check]\nenabled = false\n", encoding='utf-8')

            result = runner.invoke(cli, ['check', str(doc_path), '--config', str(cfg_path)])

        assert result.exit_code == 0
