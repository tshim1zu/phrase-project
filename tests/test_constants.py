# coding:utf-8
"""
constants.pyのテスト
"""

import pytest
from japhrase.constants import FIRST_KANJI, DEFAULT_REMOVES, DEFAULT_UNNECESSARY


class TestConstants:
    """定数のテスト"""

    def test_first_kanji_exists(self):
        """FIRST_KANJIが定義されていることを確認"""
        assert FIRST_KANJI is not None
        assert isinstance(FIRST_KANJI, str)
        assert len(FIRST_KANJI) > 0

    def test_first_kanji_contains_common_kanji(self):
        """FIRST_KANJIに一般的な漢字が含まれていることを確認"""
        assert '日' in FIRST_KANJI
        assert '本' in FIRST_KANJI
        assert '語' in FIRST_KANJI
        assert '人' in FIRST_KANJI

    def test_default_removes_is_string(self):
        """DEFAULT_REMOVESが文字列であることを確認"""
        assert isinstance(DEFAULT_REMOVES, str)

    def test_default_unnecessary_is_list(self):
        """DEFAULT_UNNECESSARYがリストであることを確認"""
        assert isinstance(DEFAULT_UNNECESSARY, list)
        assert len(DEFAULT_UNNECESSARY) > 0

    def test_default_unnecessary_contains_http(self):
        """DEFAULT_UNNECESSARYにhttpが含まれることを確認"""
        assert "http" in DEFAULT_UNNECESSARY
        assert "www" in DEFAULT_UNNECESSARY
