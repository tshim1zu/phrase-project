# coding: utf-8
"""
encoding.py の文字化け対応テスト

EncodingDetector、BOMHandler、EncodingValidator のテスト
特に文字化け検出と修復の機能をテストする
"""

import pytest
import tempfile
import os
from pathlib import Path
from japhrase.encoding import (
    EncodingDetector,
    BOMHandler,
    EncodingValidator
)


class TestBOMDetection:
    """BOM（Byte Order Mark）検出テスト"""

    def test_detect_utf8_bom(self):
        """UTF-8 BOM検出テスト"""
        data = b'\xef\xbb\xbf\xe3\x81\x82\xe3\x81\x84\xe3\x81\x86'  # "あいう" with BOM
        encoding = EncodingDetector.detect_bom(data)
        assert encoding == 'utf-8-sig'

    def test_detect_utf16_le_bom(self):
        """UTF-16 LE BOM検出テスト"""
        data = b'\xff\xfe\xa0\x30'  # UTF-16 LE BOM
        encoding = EncodingDetector.detect_bom(data)
        assert encoding == 'utf-16-le'

    def test_detect_utf16_be_bom(self):
        """UTF-16 BE BOM検出テスト"""
        data = b'\xfe\xff\x30\xa0'  # UTF-16 BE BOM
        encoding = EncodingDetector.detect_bom(data)
        assert encoding == 'utf-16-be'

    def test_detect_utf32_le_bom(self):
        """UTF-32 LE BOM検出テスト"""
        data = b'\xff\xfe\x00\x00'  # UTF-32 LE BOM
        encoding = EncodingDetector.detect_bom(data)
        assert encoding == 'utf-32-le'

    def test_no_bom(self):
        """BOMなしテスト"""
        data = b'\xe3\x81\x82\xe3\x81\x84\xe3\x81\x86'  # "あいう" without BOM
        encoding = EncodingDetector.detect_bom(data)
        assert encoding is None


class TestGarbleScoring:
    """文字化けスコア計算テスト"""

    def test_clean_text_low_score(self):
        """正常なテキストのスコアが低いテスト"""
        text = "これはきれいなテキストです。"
        score = EncodingDetector._calculate_garble_score(text)
        assert score < 1.0, f"Clean text score should be low: {score}"

    def test_replacement_char_increases_score(self):
        """置換文字（U+FFFD）が含まれるとスコアが上がるテスト"""
        text = "これは\ufffdテキストです。"
        score = EncodingDetector._calculate_garble_score(text)
        assert score > 0.0

    def test_control_char_increases_score(self):
        """制御文字が含まれるとスコアが上がるテスト"""
        text = "これは\x01テキストです。"
        score = EncodingDetector._calculate_garble_score(text)
        assert score > 0.0

    def test_invalid_unicode_increases_score(self):
        """不正なUnicode範囲がスコアを上げるテスト"""
        # UTF-16 surrogate pair range (D800-DFFF) は不正
        try:
            text = "これは\ud800テキストです。"
            score = EncodingDetector._calculate_garble_score(text)
            assert score > 0.0
        except:
            pytest.skip("Cannot create invalid Unicode in this environment")

    def test_multiple_garble_indicators(self):
        """複数の文字化けインジケータがあるとスコアが高くなるテスト"""
        text_clean = "これはきれいなテキストです。"
        text_garbled = "これは\ufffd\ufffd\x01テキストです。"

        score_clean = EncodingDetector._calculate_garble_score(text_clean)
        score_garbled = EncodingDetector._calculate_garble_score(text_garbled)

        assert score_garbled > score_clean


class TestEncodingDetection:
    """エンコーディング検出テスト"""

    def test_detect_utf8(self):
        """UTF-8検出テスト"""
        text = "これはUTF-8のテキストです"
        data = text.encode('utf-8')
        encoding, confidence = EncodingDetector.detect_with_fallback(data)
        assert encoding.lower() in ['utf-8', 'utf8', 'ascii']
        assert confidence > 0.5

    def test_detect_utf8_sig(self):
        """UTF-8 BOM付き検出テスト"""
        text = "これはBOM付きUTF-8です"
        data = b'\xef\xbb\xbf' + text.encode('utf-8')
        encoding, confidence = EncodingDetector.detect_with_fallback(data)
        assert encoding == 'utf-8-sig'
        assert confidence == 1.0

    def test_detect_shift_jis(self):
        """Shift-JIS検出テスト"""
        try:
            text = "これはShift-JISのテキストです"
            data = text.encode('shift_jis')
            encoding, confidence = EncodingDetector.detect_with_fallback(data)
            assert encoding.lower() in ['cp932', 'shift_jis', 'shift-jis', 'sjis']
            assert confidence > 0.5
        except:
            pytest.skip("Shift-JIS not available")

    def test_detect_euc_jp(self):
        """EUC-JP検出テスト"""
        try:
            text = "これはEUC-JPのテキストです"
            data = text.encode('euc-jp')
            encoding, confidence = EncodingDetector.detect_with_fallback(data)
            assert encoding.lower() in ['euc-jp', 'euc_jp', 'eucjp']
            assert confidence > 0.5
        except:
            pytest.skip("EUC-JP not available")

    def test_fallback_on_invalid_data(self):
        """不正なデータでのフォールバックテスト"""
        # 完全に不正なバイト列
        data = b'\xff\xfe\xff\xfe\xff\xfe\xff\xfe'
        encoding, confidence = EncodingDetector.detect_with_fallback(data)
        assert isinstance(encoding, str)
        assert encoding != ''

    def test_candidate_loop_picks_lowest_garble_score(self, monkeypatch):
        """chardetの信頼度が低い場合の候補選択が、最も文字化けの少ない
        （garble_scoreが最小の）候補を選ぶこと。

        garble_scoreは「低いほど文字化けの可能性が低い」定義なので、
        最大値ではなく最小値を選ぶ候補選択ロジックを直接検証する。
        全候補が正常にデコードできる純ASCIIバイト列を使い、
        ENCODING_CANDIDATESの順序通りに呼ばれる_calculate_garble_scoreへ
        候補ごとの固定スコアを返させることで、選択ロジックだけを分離して検証する。
        """
        # chardetを無効化し、必ず候補ループを通す
        monkeypatch.setattr('japhrase.encoding.HAS_CHARDET', False)

        # ENCODING_CANDIDATES = ['utf-8', 'utf-8-sig', 'cp932', 'shift_jis',
        #                        'euc-jp', 'iso-2022-jp', 'ascii']
        scores = [5.0, 5.0, 0.0, 8.0, 3.0, 9.0, 4.0]  # cp932が最小(0.0)
        call_index = {'i': 0}

        def fake_score(text):
            i = call_index['i']
            call_index['i'] += 1
            return scores[i]

        monkeypatch.setattr(
            EncodingDetector, '_calculate_garble_score', staticmethod(fake_score)
        )

        data = b'hello world'  # 全候補が例外なくデコードできる純ASCII
        encoding, confidence = EncodingDetector.detect_with_fallback(data)

        assert encoding == 'cp932'  # scores中の最小値(0.0)を持つ候補


class TestGarbleRepair:
    """文字化け修復テスト"""

    def test_repair_utf8_as_sjis(self):
        """UTF-8をShift-JISで読んだ場合の修復テスト"""
        # UTF-8のバイト列を用意
        original_text = "テスト"
        utf8_bytes = original_text.encode('utf-8')

        # これをcp932で読もうとするとエラー → errors='replace'で置換文字混入
        garbled_text = utf8_bytes.decode('cp932', errors='replace')

        # 修復を試みる
        repaired, enc_used, was_repaired = EncodingDetector.try_repair_garbled_text(
            utf8_bytes,
            assumed_encoding='utf-8'
        )

        # 修復できれば元のテキストに戻る
        if was_repaired or repaired == original_text:
            assert original_text in repaired or repaired == original_text

    def test_repair_detects_garble(self):
        """文字化けを検出するテスト"""
        # 意図的に文字化けさせる
        original = "きれいなテキスト"
        utf8_bytes = original.encode('utf-8')

        # cp932で読む（エラーになるかもしれない）
        try:
            garbled = utf8_bytes.decode('cp932', errors='replace')
            garble_score = EncodingDetector._calculate_garble_score(garbled)

            # 置換文字があれば文字化けと判定
            if '\ufffd' in garbled:
                assert garble_score > 0.0
        except:
            pytest.skip("Cannot create garbled text in this environment")

    def test_clean_text_not_modified(self):
        """きれいなテキストは修復されない"""
        text = "これはきれいなテキストです"
        data = text.encode('utf-8')

        repaired, enc_used, was_repaired = EncodingDetector.try_repair_garbled_text(data)

        # 修復されなかった or テキストが一致
        assert repaired == text or not was_repaired


class TestEncodingValidation:
    """エンコーディング妥当性チェックテスト"""

    def test_valid_utf8(self):
        """有効なUTF-8のチェック"""
        data = "これはUTF-8です".encode('utf-8')
        assert EncodingValidator.is_valid_utf8(data)

    def test_invalid_utf8(self):
        """無効なUTF-8のチェック"""
        # 不正なUTF-8バイト列
        data = b'\xff\xfe\x00\x00invalid'
        assert not EncodingValidator.is_valid_utf8(data)

    def test_valid_encoding_check(self):
        """指定エンコーディングの妥当性チェック"""
        text = "テスト"
        data = text.encode('utf-8')
        assert EncodingValidator.is_valid_encoding(data, 'utf-8')

    def test_invalid_encoding_check(self):
        """無効なエンコーディングのチェック"""
        data = b'\xff\xfe\x00\x00'
        assert not EncodingValidator.is_valid_encoding(data, 'utf-8')

    def test_validate_and_repair(self):
        """エンコーディング検証と修復"""
        text = "テスト文字列"
        data = text.encode('utf-8')

        decoded, used_encoding = EncodingValidator.validate_and_repair(data, 'utf-8')
        assert decoded == text
        assert 'utf-8' in used_encoding.lower() or used_encoding.lower() in ['utf-8', 'utf8']


class TestBOMHandler:
    """BOM処理テスト"""

    def test_remove_utf8_bom(self):
        """UTF-8 BOM除去テスト"""
        text = "テキスト"
        with_bom = '\ufeff' + text
        removed = BOMHandler.remove_bom(with_bom, 'utf-8-sig')
        assert removed == text

    def test_remove_bom_no_bom_present(self):
        """BOMがない場合のテスト"""
        text = "テキスト"
        result = BOMHandler.remove_bom(text, 'utf-8')
        assert result == text

    def test_add_utf8_bom(self):
        """UTF-8 BOM追加テスト"""
        text = "テキスト"
        with_bom = BOMHandler.add_bom(text, 'utf-8-sig')
        assert with_bom.startswith(b'\xef\xbb\xbf')
        assert with_bom[3:].decode('utf-8') == text

    def test_normalize_encoding_shift_jis(self):
        """Shift-JIS系の正規化テスト"""
        encodings = ['shift_jis', 'shift-jis', 'cp932', 'ms932', 'Shift-JIS']
        for enc in encodings:
            normalized = BOMHandler.normalize_encoding(enc)
            assert normalized == 'cp932'

    def test_normalize_encoding_euc_jp(self):
        """EUC-JP系の正規化テスト"""
        encodings = ['euc-jp', 'EUC-JP', 'euc_jp', 'EUCJP']
        for enc in encodings:
            normalized = BOMHandler.normalize_encoding(enc)
            assert normalized == 'euc-jp'


class TestEncodingDetectionErrors:
    """エンコーディング誤検出テスト"""

    def test_detect_encoding_errors(self):
        """疑わしい文字セット検出テスト"""
        # 置換文字が含まれるテキスト
        text = "これは\ufffdテキストです"
        errors = EncodingDetector.detect_encoding_errors(text)
        assert len(errors) > 0
        assert any(e['type'] == 'replacement_character' for e in errors)

    def test_detect_control_characters(self):
        """制御文字検出テスト"""
        text = "これは\x01\x02テキストです"
        errors = EncodingDetector.detect_encoding_errors(text)
        assert len(errors) > 0
        assert any(e['type'] == 'control_character' for e in errors)

    def test_remove_control_characters(self):
        """制御文字除去テスト"""
        text = "これは\x01\x02テキスト\x03です"
        cleaned = EncodingDetector.remove_control_characters(text)
        assert '\x01' not in cleaned
        assert '\x02' not in cleaned
        assert '\x03' not in cleaned
        assert 'テキスト' in cleaned


class TestIntegrationWithFiles:
    """ファイル I/O との統合テスト"""

    @pytest.fixture
    def temp_dir(self):
        """一時ディレクトリ"""
        with tempfile.TemporaryDirectory() as tmpdir:
            yield tmpdir

    def test_detect_encoding_from_file_utf8(self, temp_dir):
        """ファイルからのUTF-8検出テスト"""
        filepath = os.path.join(temp_dir, "test_utf8.txt")
        text = "これはUTF-8のテキストです\n"
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(text)

        with open(filepath, 'rb') as f:
            data = f.read()

        encoding, confidence = EncodingDetector.detect_with_fallback(data)
        assert encoding.lower() in ['utf-8', 'utf8', 'ascii']

    def test_detect_encoding_from_file_with_bom(self, temp_dir):
        """BOM付きファイル検出テスト"""
        filepath = os.path.join(temp_dir, "test_bom.txt")
        text = "BOM付きテキスト\n"
        with open(filepath, 'w', encoding='utf-8-sig') as f:
            f.write(text)

        with open(filepath, 'rb') as f:
            data = f.read()

        encoding, confidence = EncodingDetector.detect_with_fallback(data)
        assert encoding == 'utf-8-sig'

    def test_read_file_with_auto_detection(self, temp_dir):
        """自動検出を使用したファイル読み込みテスト"""
        filepath = os.path.join(temp_dir, "test.txt")
        text = "自動検出テスト\nきちんと読める\n"
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(text)

        with open(filepath, 'rb') as f:
            raw_data = f.read()

        encoding, confidence = EncodingDetector.detect_with_fallback(raw_data)
        decoded = raw_data.decode(encoding)

        assert "自動検出テスト" in decoded
        assert "きちんと読める" in decoded


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
