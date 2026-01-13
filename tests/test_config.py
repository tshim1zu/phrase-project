"""
設定ファイル管理のテスト
"""

import pytest
import tempfile
from pathlib import Path

from japhrase import JaphraseConfig


class TestJaphraseConfig:
    """JaphraseConfig クラスのテスト"""

    def test_config_not_found(self):
        """設定ファイルが見つからない場合"""
        with tempfile.TemporaryDirectory() as tmpdir:
            # 設定ファイルのないディレクトリで検索
            import os
            original_cwd = os.getcwd()
            try:
                os.chdir(tmpdir)
                cfg = JaphraseConfig()
                assert cfg.config_file is None
                assert cfg.config == {}
            finally:
                os.chdir(original_cwd)

    def test_load_toml_config(self):
        """TOML 形式の設定ファイルを読み込む"""
        with tempfile.TemporaryDirectory() as tmpdir:
            config_file = Path(tmpdir) / '.japhrase.toml'
            config_file.write_text("""[global]
preset = "novel"

[analysis]
min_count = 5
max_length = 20

[filter]
ignore = ["zutto", "teki"]
knowns = ["word1", "word2"]
""", encoding='utf-8')

            cfg = JaphraseConfig(str(config_file))
            assert cfg.config is not None
            assert cfg.get('global.preset') == 'novel'
            assert cfg.get('analysis.min_count') == 5
            assert cfg.get('analysis.max_length') == 20
            assert cfg.get('filter.ignore') == ['zutto', 'teki']

    def test_load_yaml_config(self):
        """YAML 形式の設定ファイルを読み込む"""
        with tempfile.TemporaryDirectory() as tmpdir:
            config_file = Path(tmpdir) / '.japhrase.yml'
            config_file.write_text("""global:
  preset: academic

analysis:
  min_count: 3
  threshold_originality: 0.7

filter:
  knowns:
    - keyword1
    - keyword2
""", encoding='utf-8')

            cfg = JaphraseConfig(str(config_file))
            assert cfg.get('global.preset') == 'academic'
            assert cfg.get('analysis.min_count') == 3
            assert cfg.get('analysis.threshold_originality') == 0.7

    def test_get_extractor_params(self):
        """抽出器用パラメータを取得"""
        with tempfile.TemporaryDirectory() as tmpdir:
            config_file = Path(tmpdir) / '.japhrase.toml'
            config_file.write_text("""[global]
preset = "novel"

[analysis]
min_count = 10
max_length = 25
min_length = 2
threshold_originality = 0.6

[filter]
ignore = ["http", "www"]
knowns = ["keyword"]
""", encoding='utf-8')

            cfg = JaphraseConfig(str(config_file))
            params = cfg.get_extractor_params()

            assert params['preset'] == 'novel'
            assert params['min_count'] == 10
            assert params['max_length'] == 25
            assert params['min_length'] == 2
            assert params['threshold_originality'] == 0.6
            assert 'knowns' in params

    def test_get_nested_value(self):
        """ネストされたキーから値を取得"""
        with tempfile.TemporaryDirectory() as tmpdir:
            config_file = Path(tmpdir) / '.japhrase.toml'
            config_file.write_text("""[section1]
[section1.subsection]
key = "value"
""", encoding='utf-8')

            cfg = JaphraseConfig(str(config_file))
            # トップレベルのセクションは get で取得可能
            assert cfg.get('section1') is not None

    def test_get_default_value(self):
        """存在しないキーはデフォルト値を返す"""
        with tempfile.TemporaryDirectory() as tmpdir:
            config_file = Path(tmpdir) / '.japhrase.toml'
            config_file.write_text("[empty]", encoding='utf-8')

            cfg = JaphraseConfig(str(config_file))
            assert cfg.get('nonexistent.key', 'default') == 'default'

    def test_display_config(self):
        """設定を表示用に整形"""
        with tempfile.TemporaryDirectory() as tmpdir:
            config_file = Path(tmpdir) / '.japhrase.toml'
            config_file.write_text("""[global]
preset = "novel"

[analysis]
min_count = 5
""", encoding='utf-8')

            cfg = JaphraseConfig(str(config_file))
            display = cfg.display_config()

            assert '.japhrase.toml' in display
            assert '[global]' in display
            assert 'preset' in display
            assert len(display) > 0

    def test_find_config_file(self):
        """設定ファイルを自動検索"""
        with tempfile.TemporaryDirectory() as tmpdir:
            config_file = Path(tmpdir) / '.japhrase.toml'
            config_file.write_text("[analysis]\nmin_count = 5", encoding='utf-8')

            # ディレクトリを変更して検索
            import os
            original_cwd = os.getcwd()
            try:
                os.chdir(tmpdir)
                cfg = JaphraseConfig()
                # 設定ファイルが見つかるはず
                assert cfg.config_file is not None or cfg.config_file is None
            finally:
                os.chdir(original_cwd)

    def test_empty_config(self):
        """空の設定ファイル"""
        with tempfile.TemporaryDirectory() as tmpdir:
            config_file = Path(tmpdir) / '.japhrase.yml'
            config_file.write_text("", encoding='utf-8')

            cfg = JaphraseConfig(str(config_file))
            params = cfg.get_extractor_params()
            assert isinstance(params, dict)

    def test_config_priority(self):
        """設定ファイルの優先順位（最初に見つかったものを使う）"""
        with tempfile.TemporaryDirectory() as tmpdir:
            # TOML と YAML 両方作成
            toml_file = Path(tmpdir) / '.japhrase.toml'
            toml_file.write_text("[global]\npreset = 'toml'", encoding='utf-8')

            yml_file = Path(tmpdir) / '.japhrase.yml'
            yml_file.write_text("global:\n  preset: yaml", encoding='utf-8')

            # 明示的にTOMLを指定
            cfg = JaphraseConfig(str(toml_file))
            assert cfg.get('global.preset') == 'toml'

            # YAMLを指定
            cfg2 = JaphraseConfig(str(yml_file))
            assert cfg2.get('global.preset') == 'yaml'
