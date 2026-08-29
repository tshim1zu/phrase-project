"""
汎用ファイルフォーマットサポート

テキスト、JSON、CSV、HTML、XMLなど複数のファイルフォーマットから
統一的にテキストを抽出するモジュール

各フォーマッターはメタデータとともにテキストリストを返す
"""

import sys
from abc import ABC, abstractmethod
from pathlib import Path
from typing import List, Dict, Tuple, Optional, Any
import json
import csv
from io import StringIO
import logging

logger = logging.getLogger(__name__)


class BaseFormatter(ABC):
    """全フォーマッターの基底クラス"""

    def __init__(self):
        """抽出結果に付随するメタデータ辞書を初期化する。"""
        self.metadata = {}

    @staticmethod
    @abstractmethod
    def detect(file_path: Path) -> bool:
        """ファイル形式を判定"""
        pass

    @abstractmethod
    def extract(self, file_path: Path, **kwargs) -> Tuple[List[str], Dict]:
        """テキストとメタデータを抽出

        Returns:
            (テキストリスト, メタデータ辞書)
        """
        pass

    def get_metadata(self) -> Dict:
        """メタデータを取得"""
        return self.metadata.copy()


class TextFormatter(BaseFormatter):
    """プレーンテキスト形式（.txt）"""

    @staticmethod
    def detect(file_path: Path) -> bool:
        """テキストファイルかどうか判定"""
        return file_path.suffix in ['.txt', '.text', '']

    def extract(self, file_path: Path, encoding: str = 'utf-8', **kwargs) -> Tuple[List[str], Dict]:
        """テキストファイルからテキストを抽出"""
        from .encoding import EncodingDetector

        try:
            # Pathオブジェクトに変換
            file_path = Path(file_path)

            # ファイルサイズを取得
            file_size = file_path.stat().st_size

            # エンコーディング検出
            detector = EncodingDetector()
            with open(file_path, 'rb') as f:
                raw_data = f.read()

            if encoding == 'auto':
                detected_encoding, confidence = detector.detect_with_fallback(raw_data)
            else:
                detected_encoding = encoding
                confidence = 1.0

            # テキストをデコード
            try:
                text = raw_data.decode(detected_encoding)
            except UnicodeDecodeError:
                logger.warning(f"Fallback: {detected_encoding} failed, trying utf-8 with errors='replace'")
                text = raw_data.decode(detected_encoding, errors='replace')

            # 行に分割
            lines = [line.strip() for line in text.split('\n') if line.strip()]

            # メタデータ
            self.metadata = {
                'format': 'text',
                'encoding': detected_encoding,
                'encoding_confidence': confidence,
                'file_size': file_size,
                'num_lines': len(lines),
                'encoding_errors': []
            }

            return lines, self.metadata

        except Exception as e:
            logger.error(f"TextFormatter error: {e}")
            return [], {'format': 'text', 'error': str(e)}


class JSONFormatter(BaseFormatter):
    """JSON形式（.json, .jsonl）"""

    @staticmethod
    def detect(file_path: Path) -> bool:
        """JSONファイルかどうか判定"""
        return file_path.suffix in ['.json', '.jsonl']

    def extract(
        self,
        file_path: Path,
        encoding: str = 'utf-8',
        field_paths: Optional[List[str]] = None,
        **kwargs
    ) -> Tuple[List[str], Dict]:
        """JSON形式からテキストを抽出

        Args:
            file_path: JSONファイルのパス
            encoding: エンコーディング（'auto'で自動検出）
            field_paths: 抽出する フィールドパス
                例: ["prompt", "data.items[*].description"]
        """
        from .encoding import EncodingDetector

        # Pathオブジェクトに変換
        file_path = Path(file_path)

        texts = []
        metadata = {
            'format': 'json',
            'encoding': encoding,
            'total_lines': 0,
            'extracted_lines': 0,
            'errors': []
        }

        try:
            # ファイルサイズを取得
            file_size = file_path.stat().st_size

            # エンコーディング検出
            detector = EncodingDetector()
            with open(file_path, 'rb') as f:
                raw_data = f.read()

            if encoding == 'auto':
                detected_encoding, confidence = detector.detect_with_fallback(raw_data)
            else:
                detected_encoding = encoding
                confidence = 1.0

            # デコード
            try:
                content = raw_data.decode(detected_encoding)
            except UnicodeDecodeError:
                logger.warning(f"Fallback: {detected_encoding} failed, trying utf-8")
                content = raw_data.decode(detected_encoding, errors='replace')

            # ファイル形式を判定（JSONL vs JSON）
            is_jsonl = file_path.suffix == '.jsonl'

            if is_jsonl:
                # JSONL形式（1行1JSON）
                for line_num, line in enumerate(content.split('\n'), 1):
                    line = line.strip()
                    if not line:
                        continue

                    metadata['total_lines'] += 1

                    try:
                        data = json.loads(line)
                        extracted = self._extract_fields(data, field_paths)
                        texts.extend(extracted)
                        metadata['extracted_lines'] += len(extracted)

                    except json.JSONDecodeError as e:
                        metadata['errors'].append(f"Line {line_num}: {str(e)}")
                        logger.warning(f"JSON decode error at line {line_num}: {e}")
                        continue

            else:
                # JSON形式（複数行JSON）
                try:
                    data = json.loads(content)
                    metadata['total_lines'] = 1
                    extracted = self._extract_fields(data, field_paths)
                    texts.extend(extracted)
                    metadata['extracted_lines'] = len(extracted)

                except json.JSONDecodeError as e:
                    metadata['errors'].append(str(e))
                    logger.error(f"JSON decode error: {e}")

            metadata['encoding'] = detected_encoding
            metadata['encoding_confidence'] = confidence
            metadata['file_size'] = file_size

            self.metadata = metadata
            return texts, metadata

        except Exception as e:
            logger.error(f"JSONFormatter error: {e}")
            metadata['errors'].append(str(e))
            return texts, metadata

    @staticmethod
    def _extract_fields(data: Any, field_paths: Optional[List[str]]) -> List[str]:
        """JSONデータからフィールドを抽出

        Args:
            data: JSONオブジェクト
            field_paths: パスリスト
                None の場合は全テキストフィールドを抽出
                例: ["prompt", "data.description", "data.items[*].name"]
        """
        extracted = []

        if field_paths is None:
            # デフォルト：よくあるフィールド名を試す
            field_paths = ['prompt', 'text', 'content', 'message', 'description', 'title']

        for path in field_paths:
            # [*] ワイルドカード対応：さらなる抽出が必要な場合
            if '[*]' in path:
                # パスを分割： "data.items[*].name" -> base="data.items", remaining="name"
                parts = path.split('[*]')
                base_path = parts[0].rstrip('.')
                remaining = parts[1].lstrip('.')

                # ベースパスから配列を取得
                array = JSONFormatter._get_nested_value(data, base_path)
                if isinstance(array, list):
                    # 配列の各要素から remaining フィールドを抽出
                    for item in array:
                        if remaining:
                            value = JSONFormatter._get_nested_value(item, remaining)
                        else:
                            value = item

                        if value:
                            if isinstance(value, str):
                                extracted.append(value)
                            elif isinstance(value, list):
                                extracted.extend([str(v) for v in value if v])
                            else:
                                extracted.append(str(value))
            else:
                # 通常のパス処理
                value = JSONFormatter._get_nested_value(data, path)
                if value:
                    if isinstance(value, str):
                        extracted.append(value)
                    elif isinstance(value, list):
                        extracted.extend([str(v) for v in value if v])
                    else:
                        extracted.append(str(value))

        return extracted

    @staticmethod
    def _get_nested_value(data: Any, path: str) -> Any:
        """ネストされたJSONパスから値を取得

        Args:
            data: JSONオブジェクト
            path: パス文字列（"data.items[0].name"や"data.items[*].name"など）
        """
        import re

        # パスをセグメントに分割（[*]対応）
        # "data.items[*].name" -> ["data", "items[*]", "name"]
        segments = re.split(r'\.(?=(?:[^\[\]]*\[[^\[\]]*\])*[^\[\]]*$)', path)

        current = data

        for segment in segments:
            if not segment:
                continue

            # [*] ワイルドカード処理
            if '[*]' in segment:
                field_name = segment.replace('[*]', '')

                if isinstance(current, dict) and field_name in current:
                    items = current[field_name]
                    if isinstance(items, list):
                        # リストの各要素を返す
                        return items
                continue

            # 通常のインデックスアクセス処理
            if isinstance(current, dict):
                # "field[0]" の形式に対応
                if '[' in segment:
                    field_name, index_part = segment.split('[', 1)
                    index = int(index_part.rstrip(']'))
                    current = current.get(field_name)
                    if isinstance(current, list) and index < len(current):
                        current = current[index]
                    else:
                        return None
                else:
                    current = current.get(segment)
            elif isinstance(current, list):
                try:
                    idx = int(segment)
                    if idx < len(current):
                        current = current[idx]
                    else:
                        return None
                except (ValueError, IndexError):
                    return None
            else:
                return None

            if current is None:
                return None

        return current


class CSVFormatter(BaseFormatter):
    """CSV/TSV形式"""

    @staticmethod
    def detect(file_path: Path) -> bool:
        """CSVファイルかどうか判定"""
        return file_path.suffix in ['.csv', '.tsv']

    def extract(
        self,
        file_path: Path,
        encoding: str = 'utf-8',
        column: Optional[str] = None,
        delimiter: Optional[str] = None,
        **kwargs
    ) -> Tuple[List[str], Dict]:
        """CSV形式からテキストを抽出

        Args:
            file_path: CSVファイルのパス
            encoding: エンコーディング
            column: 抽出するカラム名（Noneの場合は全列結合）
            delimiter: 区切り文字（Noneの場合は自動判定）
        """
        from .encoding import EncodingDetector

        # Pathオブジェクトに変換
        file_path = Path(file_path)

        texts = []
        metadata = {
            'format': 'csv' if file_path.suffix == '.csv' else 'tsv',
            'encoding': encoding,
            'num_rows': 0,
            'columns': [],
            'errors': []
        }

        try:
            # エンコーディング検出
            detector = EncodingDetector()
            with open(file_path, 'rb') as f:
                raw_data = f.read()

            if encoding == 'auto':
                detected_encoding, confidence = detector.detect_with_fallback(raw_data)
            else:
                detected_encoding = encoding
                confidence = 1.0

            # デコード
            try:
                content = raw_data.decode(detected_encoding)
            except UnicodeDecodeError:
                logger.warning(f"Fallback: {detected_encoding} failed, trying utf-8")
                content = raw_data.decode(detected_encoding, errors='replace')

            # 区切り文字を自動判定または使用指定
            if delimiter is None:
                delimiter = '\t' if file_path.suffix == '.tsv' else ','

            # CSV読み込み
            reader = csv.DictReader(StringIO(content), delimiter=delimiter)

            if reader.fieldnames:
                metadata['columns'] = list(reader.fieldnames)

            for row_num, row in enumerate(reader, 1):
                if column and column in row:
                    # 特定カラム抽出
                    value = row[column]
                    if value:
                        texts.append(value)
                elif not column:
                    # 全カラムを結合
                    values = [str(v) for v in row.values() if v]
                    if values:
                        texts.append(' '.join(values))

                metadata['num_rows'] = row_num

            metadata['encoding'] = detected_encoding
            metadata['encoding_confidence'] = confidence

            self.metadata = metadata
            return texts, metadata

        except Exception as e:
            logger.error(f"CSVFormatter error: {e}")
            metadata['errors'].append(str(e))
            return texts, metadata


class UniversalFormatter:
    """複数フォーマットの統合フォーマッター"""

    FORMATTERS = {
        'text': TextFormatter,
        'json': JSONFormatter,
        'csv': CSVFormatter,
    }

    @staticmethod
    def auto_detect(file_path: Path) -> Optional[BaseFormatter]:
        """ファイル形式を自動判定してフォーマッターを返す"""
        file_path = Path(file_path)

        for formatter_class in UniversalFormatter.FORMATTERS.values():
            if formatter_class.detect(file_path):
                return formatter_class()

        # デフォルトはテキスト扱い
        logger.warning(f"Format not recognized for {file_path}, treating as text")
        return TextFormatter()

    @staticmethod
    def extract(
        file_path: Path,
        format_hint: Optional[str] = None,
        **kwargs
    ) -> Tuple[List[str], Dict]:
        """ファイルから統一的にテキストを抽出

        Args:
            file_path: ファイルパス
            format_hint: フォーマットのヒント（'json', 'csv', 'text'等）
            **kwargs: フォーマッター固有のオプション

        Returns:
            (テキストリスト, メタデータ辞書)
        """
        file_path = Path(file_path)

        # フォーマッターを決定
        if format_hint and format_hint.lower() in UniversalFormatter.FORMATTERS:
            formatter = UniversalFormatter.FORMATTERS[format_hint.lower()]()
        else:
            formatter = UniversalFormatter.auto_detect(file_path)

        if formatter is None:
            logger.error(f"No suitable formatter found for {file_path}")
            return [], {'error': 'No suitable formatter found'}

        return formatter.extract(file_path, **kwargs)

    @staticmethod
    def register_custom(format_name: str, formatter_class: type) -> None:
        """カスタムフォーマッターを登録

        Args:
            format_name: フォーマット名（キー）
            formatter_class: BaseFormatterの継承クラス
        """
        if not issubclass(formatter_class, BaseFormatter):
            raise TypeError(f"{formatter_class} must be subclass of BaseFormatter")

        UniversalFormatter.FORMATTERS[format_name.lower()] = formatter_class
        logger.info(f"Registered custom formatter: {format_name}")
