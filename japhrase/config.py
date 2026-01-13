"""
設定ファイル管理モジュール

.japhrase.toml / .japhrase.yml から設定を読み込む
"""

import os
from pathlib import Path
from typing import Dict, Any, Optional
import logging

logger = logging.getLogger(__name__)


class JaphraseConfig:
    """japhrase 設定ファイル管理クラス"""

    # デフォルト設定ファイル名
    CONFIG_FILES = ['.japhrase.toml', '.japhrase.yml', 'japhrase.toml', 'japhrase.yml']

    def __init__(self, config_file: Optional[str] = None):
        """
        設定を初期化

        Args:
            config_file: 明示的に指定する設定ファイルパス
        """
        self.config_file = config_file or self._find_config_file()
        self.config: Dict[str, Any] = {}

        if self.config_file:
            self._load_config()
        else:
            logger.debug("設定ファイルが見つかりません")

    @staticmethod
    def _find_config_file() -> Optional[str]:
        """現在のディレクトリと親ディレクトリから設定ファイルを探索"""
        current_dir = Path.cwd()

        # 現在のディレクトリから5階層上まで探索
        for parent in [current_dir, *current_dir.parents[:5]]:
            for config_name in JaphraseConfig.CONFIG_FILES:
                config_path = parent / config_name
                if config_path.exists():
                    logger.debug(f"設定ファイルが見つかりました: {config_path}")
                    return str(config_path)

        return None

    def _load_config(self):
        """設定ファイルから設定を読み込む"""
        if not self.config_file:
            return

        try:
            if self.config_file.endswith('.toml'):
                self._load_toml()
            elif self.config_file.endswith('.yml') or self.config_file.endswith('.yaml'):
                self._load_yaml()
            else:
                logger.warning(f"不正な設定ファイル形式: {self.config_file}")
        except Exception as e:
            logger.error(f"設定ファイルの読み込みエラー: {e}")

    def _load_toml(self):
        """TOML形式の設定ファイルを読み込む"""
        try:
            import tomllib
        except ImportError:
            try:
                import tomli as tomllib
            except ImportError:
                logger.error("toml ライブラリがインストールされていません")
                return

        try:
            with open(self.config_file, 'rb') as f:
                self.config = tomllib.load(f)
                logger.info(f"設定ファイルを読み込みました: {self.config_file}")
        except Exception as e:
            logger.error(f"TOML ファイルの読み込みエラー: {e}")

    def _load_yaml(self):
        """YAML形式の設定ファイルを読み込む"""
        try:
            import yaml
        except ImportError:
            logger.error("PyYAML がインストールされていません")
            return

        try:
            with open(self.config_file, 'r', encoding='utf-8') as f:
                self.config = yaml.safe_load(f) or {}
                logger.info(f"設定ファイルを読み込みました: {self.config_file}")
        except Exception as e:
            logger.error(f"YAML ファイルの読み込みエラー: {e}")

    def get(self, key: str, default: Any = None) -> Any:
        """ネストされたキーから値を取得"""
        keys = key.split('.')
        value = self.config

        for k in keys:
            if isinstance(value, dict):
                value = value.get(k)
                if value is None:
                    return default
            else:
                return default

        return value if value is not None else default

    def get_extractor_params(self) -> Dict[str, Any]:
        """PhraseExtracter 用のパラメータを取得"""
        params = {}

        # グローバル設定
        if 'global' in self.config:
            if 'preset' in self.config['global']:
                params['preset'] = self.config['global']['preset']

        # 分析設定
        if 'analysis' in self.config:
            analysis = self.config['analysis']
            if 'min_count' in analysis:
                params['min_count'] = analysis['min_count']
            if 'max_length' in analysis:
                params['max_length'] = analysis['max_length']
            if 'min_length' in analysis:
                params['min_length'] = analysis['min_length']
            if 'threshold_originality' in analysis:
                params['threshold_originality'] = analysis['threshold_originality']
            if 'weight_freq' in analysis:
                params['weight_freq'] = analysis['weight_freq']
            if 'weight_len' in analysis:
                params['weight_len'] = analysis['weight_len']

        # フィルタ設定
        if 'filter' in self.config:
            filter_cfg = self.config['filter']
            if 'ignore' in filter_cfg:
                # ignore リストを文字列に変換
                ignore_list = filter_cfg['ignore']
                if isinstance(ignore_list, list):
                    params['removes'] = ''.join(ignore_list)
            if 'knowns' in filter_cfg:
                params['knowns'] = filter_cfg['knowns']
            if 'unnecessary' in filter_cfg:
                params['unnecesary'] = filter_cfg['unnecessary']

        return params

    def get_workflow_params(self) -> Dict[str, Any]:
        """ワークフロー用のパラメータを取得"""
        return self.config.get('workflow', {})

    def get_check_rules(self) -> Dict[str, Any]:
        """check コマンド用のルール設定を取得"""
        return self.config.get('check', {})

    def get_output_config(self) -> Dict[str, Any]:
        """出力設定を取得"""
        return self.config.get('output', {})

    def display_config(self) -> str:
        """現在の設定を表示用に整形"""
        if not self.config:
            return "設定ファイルが読み込まれていません"

        lines = []
        lines.append(f"設定ファイル: {self.config_file}")
        lines.append("=" * 70)

        for section, content in self.config.items():
            lines.append(f"\n[{section}]")
            if isinstance(content, dict):
                for key, value in content.items():
                    lines.append(f"  {key} = {value}")
            else:
                lines.append(f"  {content}")

        return "\n".join(lines)
