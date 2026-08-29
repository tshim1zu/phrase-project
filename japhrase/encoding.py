"""
高度なエンコーディング検出と修復

複雑な文字化けへの対応、BOMの処理、複数エンコーディング候補の試行
文字化けに対する堅牢な対応を提供
"""

import sys
from typing import Tuple, List, Dict, Optional
import logging
import re

try:
    import chardet
    HAS_CHARDET = True
except ImportError:
    HAS_CHARDET = False

logger = logging.getLogger(__name__)


class EncodingDetector:
    """高度なエンコーディング検出と修復"""

    # よく使われるエンコーディング候補（優先順位順）
    ENCODING_CANDIDATES = [
        'utf-8',
        'utf-8-sig',  # BOM付きUTF-8
        'cp932',  # Shift-JIS（Windows標準）
        'shift_jis',
        'euc-jp',
        'iso-2022-jp',
        'ascii',
    ]

    # BOMシグネチャ
    BOM_SIGNATURES = {
        b'\xff\xfe\x00\x00': 'utf-32-le',
        b'\x00\x00\xfe\xff': 'utf-32-be',
        b'\xff\xfe': 'utf-16-le',
        b'\xfe\xff': 'utf-16-be',
        b'\xef\xbb\xbf': 'utf-8-sig',
    }

    # 文字化けパターン（修復パターン）
    GARBLED_PATTERNS = {
        # UTF-8をShift-JISで読んだ場合
        'utf8_as_sjis': {
            'pattern': r'[\u0080-\u009f\u00a0-\u00ff]{2,}',  # 制御文字が多い
            'source_enc': 'cp932',
            'target_enc': 'utf-8'
        },
        # Shift-JISをUTF-8で読んだ場合
        'sjis_as_utf8': {
            'pattern': r'[\ufffd]+',  # Replacement character
            'source_enc': 'utf-8',
            'target_enc': 'cp932'
        },
    }

    def __init__(self):
        """直近の検出結果とエンコーディング試行履歴を初期化する。"""
        self.last_detection = None
        self.last_confidence = 0.0
        self.trials = []  # 試行ログ

    @staticmethod
    def detect_bom(data: bytes) -> Optional[str]:
        """BOM（Byte Order Mark）を検出

        Returns:
            BOMが見つかった場合はエンコーディング名、なければNone
        """
        for bom, encoding in EncodingDetector.BOM_SIGNATURES.items():
            if data.startswith(bom):
                logger.debug(f"BOM detected: {encoding}")
                return encoding
        return None

    @staticmethod
    def detect_with_fallback(data: bytes, try_repair: bool = True) -> Tuple[str, float]:
        """複数の候補からエンコーディングを検出（文字化け対応）

        Args:
            data: バイト列
            try_repair: 文字化けの修復を試みるかどうか

        Returns:
            (エンコーディング名, 信頼度)
        """
        # BOMを確認
        bom_encoding = EncodingDetector.detect_bom(data)
        if bom_encoding:
            return bom_encoding, 1.0

        # chardet が利用可能なら使用
        if HAS_CHARDET:
            result = chardet.detect(data)
            if result and result.get('encoding') and result.get('confidence', 0) >= 0.7:
                encoding = result['encoding']
                confidence = result['confidence']

                # Shift-JIS系を正規化
                if encoding and ('shift' in encoding.lower() or 'cp932' in encoding.lower() or 'ms932' in encoding.lower()):
                    encoding = 'cp932'

                logger.debug(f"chardet detected: {encoding} (confidence: {confidence})")
                return encoding, confidence

        # 候補を順番に試す（文字化けのシグナルをチェック）
        best_encoding = None
        best_score = -1

        for encoding in EncodingDetector.ENCODING_CANDIDATES:
            try:
                text = data.decode(encoding)

                # デコード成功時、文字化けスコアを計算
                garble_score = EncodingDetector._calculate_garble_score(text)
                logger.debug(f"{encoding}: garble_score={garble_score:.2f}")

                if garble_score > best_score:
                    best_score = garble_score
                    best_encoding = encoding

            except (UnicodeDecodeError, LookupError) as e:
                logger.debug(f"{encoding}: decode failed - {e}")
                continue

        if best_encoding:
            confidence = max(0.5, 1.0 - best_score / 10.0)  # スコアを信頼度に変換
            logger.debug(f"Best match: {best_encoding} (score: {best_score:.2f})")
            return best_encoding, confidence

        # 最終手段：UTF-8 with errors='replace'
        logger.warning("All encoding attempts failed, using UTF-8 with error handling")
        return 'utf-8', 0.1

    @staticmethod
    def _calculate_garble_score(text: str) -> float:
        """テキストの文字化けスコアを計算

        Returns:
            スコア（低いほど文字化けの可能性が低い）
        """
        if not text:
            return 0.0

        score = 0.0

        # 置換文字（U+FFFD）の出現
        replacement_count = text.count('\ufffd')
        score += replacement_count * 5.0

        # 制御文字の出現
        control_chars = sum(1 for c in text if ord(c) < 0x20 and c not in '\n\r\t')
        score += control_chars * 2.0

        # 不正なUnicode範囲
        invalid_chars = sum(1 for c in text if ord(c) in range(0xD800, 0xE000))
        score += invalid_chars * 3.0

        # 疑わしい文字パターン（例：化けたカナ）
        suspicious_patterns = len(re.findall(r'[\u0080-\u009f]{2,}', text))
        score += suspicious_patterns * 1.5

        return score

    @staticmethod
    def try_repair_garbled_text(data: bytes, assumed_encoding: str = 'utf-8') -> Tuple[str, str, bool]:
        """文字化けを修復

        Args:
            data: バイト列
            assumed_encoding: 仮定されるエンコーディング

        Returns:
            (テキスト, 使用エンコーディング, 修復したかどうか)
        """
        # 1. 仮定されるエンコーディングで試す
        try:
            text = data.decode(assumed_encoding)
            garble_score = EncodingDetector._calculate_garble_score(text)

            if garble_score < 1.0:  # 文字化けしていない
                return text, assumed_encoding, False
        except UnicodeDecodeError:
            pass

        # 2. 文字化け修復パターンを試す
        for pattern_name, pattern_info in EncodingDetector.GARBLED_PATTERNS.items():
            try:
                # 元のバイト列を別のエンコーディングで読み直す
                text_attempt = data.decode(
                    pattern_info['source_enc'],
                    errors='replace'
                )

                # パターンにマッチするかチェック
                if re.search(pattern_info['pattern'], text_attempt):
                    # 修復を試みる
                    repaired = EncodingDetector.repair_encoding(
                        text_attempt,
                        pattern_info['source_enc'],
                        pattern_info['target_enc']
                    )

                    garble_score = EncodingDetector._calculate_garble_score(repaired)
                    logger.info(f"Garble repair attempt '{pattern_name}': score={garble_score:.2f}")

                    if garble_score < 1.0:
                        logger.info(f"Successfully repaired using pattern: {pattern_name}")
                        return repaired, pattern_info['target_enc'], True

            except Exception as e:
                logger.debug(f"Repair pattern '{pattern_name}' failed: {e}")
                continue

        # 3. すべて失敗時は、最もスコアの低いエンコーディングを選ぶ
        best_encoding = assumed_encoding
        best_score = float('inf')
        best_text = None

        for encoding in EncodingDetector.ENCODING_CANDIDATES:
            try:
                text = data.decode(encoding, errors='replace')
                score = EncodingDetector._calculate_garble_score(text)

                if score < best_score:
                    best_score = score
                    best_encoding = encoding
                    best_text = text

            except Exception:
                continue

        if best_text is not None:
            logger.info(f"Fallback to {best_encoding} (score: {best_score:.2f})")
            return best_text, best_encoding, True

        # 最後の手段
        text = data.decode('utf-8', errors='replace')
        return text, 'utf-8', True

    @staticmethod
    def repair_encoding(
        text: str,
        original_encoding: str,
        target_encoding: str = 'utf-8'
    ) -> str:
        """誤ったエンコーディングで読まれたテキストを修復

        例: "あいう"が"ぁぃぅ"に化けた場合を修復

        Args:
            text: 化けているテキスト
            original_encoding: 誤ったエンコーディング
            target_encoding: 目標エンコーディング
        """
        if original_encoding == target_encoding:
            return text

        try:
            # 元のバイト列に戻す
            bytes_data = text.encode(original_encoding, errors='replace')

            # 正しいエンコーディングで再度デコード
            repaired = bytes_data.decode(target_encoding, errors='replace')
            logger.info(f"Repaired encoding: {original_encoding} -> {target_encoding}")
            return repaired

        except Exception as e:
            logger.warning(f"Encoding repair failed: {e}")
            return text

    @staticmethod
    def detect_encoding_errors(text: str) -> List[Dict]:
        """疑わしい文字セットを検出

        Returns:
            文字化けの可能性がある位置と修復提案のリスト
        """
        errors = []
        suspicious_patterns = [
            # UTF-8の一部をcp932で読んだ場合のパターン
            (r'[\ufffd]+', 'Replacement character detected (encoding mismatch)'),
            # 化けたカナのパターン（半角カナが混在）
            (r'[ァ-ヴー]{1,}[^\w\n ァ-ヴー，。！？]', 'Suspicious katakana sequence'),
        ]

        for i, char in enumerate(text):
            code = ord(char)

            # 制御文字（0x00-0x1F, 0x7F-0x9F）を検出
            if (0x00 <= code <= 0x1F) or (0x7F <= code <= 0x9F):
                if char not in '\n\r\t':
                    errors.append({
                        'position': i,
                        'char': repr(char),
                        'code': code,
                        'type': 'control_character'
                    })

            # 不正なUnicode範囲
            elif code == 0xFFFD:  # Replacement character
                errors.append({
                    'position': i,
                    'char': char,
                    'type': 'replacement_character'
                })

        return errors

    @staticmethod
    def remove_control_characters(text: str) -> str:
        """制御文字を除去

        Args:
            text: テキスト

        Returns:
            制御文字が除去されたテキスト
        """
        return ''.join(
            char for char in text
            if (ord(char) >= 0x20 and ord(char) != 0x7F) or char in '\n\r\t'
        )


class BOMHandler:
    """BOM（Byte Order Mark）の処理"""

    @staticmethod
    def remove_bom(text: str, encoding: str) -> str:
        """テキストからBOMを除去

        Args:
            text: BOM付きテキスト
            encoding: エンコーディング

        Returns:
            BOMが除去されたテキスト
        """
        if encoding == 'utf-8-sig' and text.startswith('\ufeff'):
            return text[1:]
        elif text.startswith('\ufeff'):
            return text[1:]
        return text

    @staticmethod
    def add_bom(text: str, encoding: str) -> bytes:
        """テキストにBOMを付加してバイト列を返す

        Args:
            text: テキスト
            encoding: エンコーディング

        Returns:
            BOM付きバイト列
        """
        if encoding == 'utf-8-sig':
            return b'\xef\xbb\xbf' + text.encode('utf-8')
        elif encoding == 'utf-16':
            # UTF-16はデフォルトでBOM付き
            return text.encode('utf-16')
        else:
            return text.encode(encoding)

    @staticmethod
    def normalize_encoding(encoding: str) -> str:
        """エンコーディング名を正規化

        Args:
            encoding: エンコーディング名

        Returns:
            正規化されたエンコーディング名
        """
        encoding = encoding.lower().replace('-', '_').replace('_', '-')

        # Shift-JIS系を統一
        if 'shift' in encoding or 'sjis' in encoding or 'cp932' in encoding or 'ms932' in encoding:
            return 'cp932'

        # EUC-JP系を統一
        if 'euc' in encoding and 'jp' in encoding:
            return 'euc-jp'

        # ISO-2022-JP系を統一
        if 'iso' in encoding and '2022' in encoding and 'jp' in encoding:
            return 'iso-2022-jp'

        return encoding


class EncodingValidator:
    """エンコーディングの妥当性チェック"""

    @staticmethod
    def is_valid_utf8(data: bytes) -> bool:
        """データがUTF-8として有効かチェック"""
        try:
            data.decode('utf-8')
            return True
        except UnicodeDecodeError:
            return False

    @staticmethod
    def is_valid_encoding(data: bytes, encoding: str) -> bool:
        """データがspecified encodingとして有効かチェック"""
        try:
            data.decode(encoding)
            return True
        except (UnicodeDecodeError, LookupError):
            return False

    @staticmethod
    def validate_and_repair(data: bytes, encoding: str) -> Tuple[str, str]:
        """エンコーディングを検証し、失敗した場合は修復を試みる

        Args:
            data: バイト列
            encoding: 指定エンコーディング

        Returns:
            (デコードされたテキスト, 使用されたエンコーディング)
        """
        # 指定のエンコーディングで試す
        try:
            text = data.decode(encoding)
            logger.debug(f"Decoded successfully with {encoding}")
            return text, encoding
        except (UnicodeDecodeError, LookupError):
            logger.warning(f"Failed to decode with {encoding}, trying alternatives...")

        # 自動検出にフォールバック
        detected, _ = EncodingDetector.detect_with_fallback(data)
        try:
            text = data.decode(detected)
            logger.info(f"Successfully decoded with detected encoding: {detected}")
            return text, detected
        except UnicodeDecodeError:
            # 最終手段
            text = data.decode(detected, errors='replace')
            logger.warning(f"Using error handling with {detected}")
            return text, detected
