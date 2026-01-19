# coding: utf-8
"""
頑健性強化用の共通utility層

エラーハンドリング、スコア検証、設定バリデーションを一元化
"""

import logging
import math
from typing import Any, Dict, Optional, Union, Tuple
from dataclasses import dataclass

logger = logging.getLogger(__name__)


@dataclass
class ValidationError:
    """バリデーション結果"""
    is_valid: bool
    message: str = ""
    suggestion: str = ""


class ScoreValidator:
    """スコア値の検証・正規化"""

    SCORE_MIN = 0.0
    SCORE_MAX = 1.0
    EPSILON = 1e-10  # ゼロ除算対策

    @staticmethod
    def validate_score(value: float, 
                      allow_none: bool = False,
                      name: str = "score") -> Tuple[bool, float, str]:
        """
        スコア値を検証・正規化

        Args:
            value: スコア値
            allow_none: None を許可するか
            name: 変数名（エラーメッセージ用）

        Returns:
            (is_valid, normalized_value, error_message)
        """
        if value is None:
            if allow_none:
                return True, None, ""
            return False, 0.0, f"{name} は None です"

        # NaN/Inf チェック
        if not isinstance(value, (int, float)):
            return False, 0.0, f"{name} は数値ではありません: {type(value)}"

        if math.isnan(value):
            return False, 0.0, f"{name} は NaN です"

        if math.isinf(value):
            return False, 0.0, f"{name} は ∞ です"

        # 範囲外の値を修正
        if value < ScoreValidator.SCORE_MIN:
            logger.warning(f"{name} が 0.0 未満です: {value} → 0.0 に修正")
            return True, 0.0, f"{name} を 0.0 に修正"

        if value > ScoreValidator.SCORE_MAX:
            logger.warning(f"{name} が 1.0 を超えています: {value} → 1.0 に修正")
            return True, 1.0, f"{name} を 1.0 に修正"

        return True, value, ""

    @staticmethod
    def safe_divide(numerator: float, denominator: float, 
                   default: float = 0.0) -> float:
        """安全な除算（ゼロ除算対策）"""
        if denominator is None or abs(denominator) < ScoreValidator.EPSILON:
            logger.debug(f"ゼロ除算を検出、デフォルト値 {default} を使用")
            return default

        try:
            result = numerator / denominator
            is_valid, normalized, msg = ScoreValidator.validate_score(result)
            return normalized if is_valid else default
        except (TypeError, ValueError) as e:
            logger.warning(f"除算エラー: {e}、デフォルト値 {default} を使用")
            return default

    @staticmethod
    def normalize_scores(scores: Dict[str, float]) -> Dict[str, float]:
        """複数スコアを一括正規化"""
        normalized = {}
        for key, value in scores.items():
            is_valid, norm_value, msg = ScoreValidator.validate_score(value, name=key)
            normalized[key] = norm_value
            if msg:
                logger.debug(f"スコア正規化: {msg}")

        return normalized


class ConfigValidator:
    """設定値のバリデーション"""

    @staticmethod
    def validate_sensitivity(sensitivity: str) -> ValidationError:
        """感度設定のバリデーション"""
        valid_values = ['low', 'medium', 'high']

        if sensitivity not in valid_values:
            return ValidationError(
                is_valid=False,
                message=f"sensitivity は {valid_values} のいずれかである必要があります",
                suggestion=f"'medium' を使用してください"
            )

        return ValidationError(is_valid=True)

    @staticmethod
    def validate_threshold(threshold: float, 
                          name: str = "threshold") -> ValidationError:
        """閾値のバリデーション"""
        is_valid, norm_value, msg = ScoreValidator.validate_score(threshold, name=name)

        if not is_valid:
            return ValidationError(
                is_valid=False,
                message=msg,
                suggestion="0.0 ～ 1.0 の値を指定してください"
            )

        return ValidationError(is_valid=True)

    @staticmethod
    def validate_chunk_size(chunk_size: int, 
                           min_size: int = 1) -> ValidationError:
        """チャンク サイズのバリデーション"""
        if not isinstance(chunk_size, int):
            return ValidationError(
                is_valid=False,
                message=f"chunk_size は整数である必要があります（{type(chunk_size)}）",
                suggestion="正の整数を指定してください"
            )

        if chunk_size < min_size:
            return ValidationError(
                is_valid=False,
                message=f"chunk_size は {min_size} 以上である必要があります",
                suggestion=f"chunk_size >= {min_size}"
            )

        return ValidationError(is_valid=True)

    @staticmethod
    def validate_position(position: Tuple[int, int], 
                         text_length: int) -> ValidationError:
        """位置情報のバリデーション"""
        if not isinstance(position, tuple) or len(position) != 2:
            return ValidationError(
                is_valid=False,
                message="position は (start, end) のタプルである必要があります"
            )

        start, end = position

        if not isinstance(start, int) or not isinstance(end, int):
            return ValidationError(
                is_valid=False,
                message="position の要素は整数である必要があります"
            )

        if start < 0 or end < 0:
            return ValidationError(
                is_valid=False,
                message="position は非負の整数である必要があります"
            )

        if start >= end:
            return ValidationError(
                is_valid=False,
                message="start < end である必要があります"
            )

        if end > text_length:
            return ValidationError(
                is_valid=False,
                message=f"position が テキスト長（{text_length}）を超えています"
            )

        return ValidationError(is_valid=True)


class TextProcessor:
    """テキスト処理ユーティリティ"""

    @staticmethod
    def is_valid_text(text: Optional[str], 
                     allow_empty: bool = False) -> ValidationError:
        """テキストのバリデーション"""
        if text is None:
            return ValidationError(
                is_valid=False,
                message="テキストは None です"
            )

        if not isinstance(text, str):
            return ValidationError(
                is_valid=False,
                message=f"テキストは文字列である必要があります（{type(text)}）"
            )

        if len(text) == 0 and not allow_empty:
            return ValidationError(
                is_valid=False,
                message="テキストが空です"
            )

        return ValidationError(is_valid=True)

    @staticmethod
    def safe_substring(text: str, start: int, end: int) -> Tuple[bool, str]:
        """安全な部分文字列抽出"""
        try:
            # 境界チェック
            actual_start = max(0, min(start, len(text)))
            actual_end = max(actual_start, min(end, len(text)))

            if actual_start != start or actual_end != end:
                logger.warning(
                    f"部分文字列の境界を調整: ({start}, {end}) → ({actual_start}, {actual_end})"
                )

            return True, text[actual_start:actual_end]
        except (TypeError, IndexError) as e:
            logger.error(f"部分文字列抽出エラー: {e}")
            return False, ""

    @staticmethod
    def normalize_whitespace(text: str, 
                           remove_newlines: bool = False) -> str:
        """空白・改行の正規化"""
        if not isinstance(text, str):
            return ""

        # 複数の空白を1つに
        text = " ".join(text.split())

        if remove_newlines:
            text = text.replace("\n", "").replace("\r", "")

        return text

    @staticmethod
    def calculate_byte_position(text: str, char_position: int) -> int:
        """文字位置からバイト位置を計算"""
        try:
            return len(text[:char_position].encode('utf-8'))
        except (UnicodeEncodeError, IndexError):
            return -1

    @staticmethod
    def calculate_char_position(text: str, byte_position: int) -> int:
        """バイト位置から文字位置を計算"""
        try:
            byte_count = 0
            for i, char in enumerate(text):
                if byte_count >= byte_position:
                    return i
                byte_count += len(char.encode('utf-8'))
            return len(text)
        except (UnicodeEncodeError, IndexError):
            return -1


class ErrorHandler:
    """統一エラーハンドリング"""

    @staticmethod
    def safe_json_export(data: Any, 
                        filepath: str,
                        fallback_format: str = 'csv') -> Tuple[bool, str]:
        """
        JSON出力を試行し、失敗時にフォールバック

        Args:
            data: 出力データ
            filepath: 出力ファイルパス
            fallback_format: フォールバック形式 ('csv', 'txt')

        Returns:
            (success, output_filepath)
        """
        import json
        import csv

        # JSON出力を試行
        try:
            with open(filepath, 'w', encoding='utf-8') as f:
                json.dump(data, f, ensure_ascii=False, indent=2, default=str)
            logger.info(f"JSON出力成功: {filepath}")
            return True, filepath
        except (IOError, json.JSONEncodeError) as e:
            logger.warning(f"JSON出力失敗: {e}、フォールバック形式 {fallback_format} を使用")

        # フォールバック処理
        fallback_path = filepath.replace('.json', f'.{fallback_format}')

        if fallback_format == 'csv' and isinstance(data, list) and len(data) > 0:
            try:
                # リストの場合、最初の要素をキーにする
                first_item = data[0]
                if isinstance(first_item, dict):
                    keys = first_item.keys()
                    with open(fallback_path, 'w', newline='', encoding='utf-8') as f:
                        writer = csv.DictWriter(f, fieldnames=keys)
                        writer.writeheader()
                        writer.writerows(data)
                    logger.info(f"CSV出力成功（フォールバック）: {fallback_path}")
                    return True, fallback_path
            except Exception as e:
                logger.error(f"CSV出力も失敗: {e}")

        # テキスト出力
        try:
            with open(fallback_path.replace('.csv', '.txt'), 'w', encoding='utf-8') as f:
                f.write(str(data))
            logger.info(f"テキスト出力成功（フォールバック）: {fallback_path}")
            return True, fallback_path.replace('.csv', '.txt')
        except Exception as e:
            logger.error(f"すべての出力が失敗しました: {e}")
            return False, ""

    @staticmethod
    def catch_and_log(func, *args, default=None, **kwargs):
        """関数実行時の例外をキャッチしてログ"""
        try:
            return func(*args, **kwargs)
        except Exception as e:
            logger.error(f"関数実行エラー: {func.__name__} - {e}")
            return default
