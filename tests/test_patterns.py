# coding:utf-8
"""
patterns.pyのテスト
"""

import pytest
import re
from japhrase.patterns import get_positive_patterns, get_negative_patterns


class TestPatterns:
    """パターンマッチングのテスト"""

    def test_get_positive_patterns_returns_dict(self):
        """get_positive_patterns()が辞書を返すことを確認"""
        patterns = get_positive_patterns()
        assert isinstance(patterns, dict)
        assert len(patterns) > 0

    def test_get_negative_patterns_returns_dict(self):
        """get_negative_patterns()が辞書を返すことを確認"""
        patterns = get_negative_patterns()
        assert isinstance(patterns, dict)
        assert len(patterns) > 0

    def test_positive_patterns_are_regex(self):
        """ポジティブパターンが正規表現オブジェクトであることを確認"""
        patterns = get_positive_patterns()
        for key, pattern in patterns.items():
            assert hasattr(pattern, 'match'), f"{key} is not a regex pattern"

    def test_negative_patterns_are_regex(self):
        """ネガティブパターンが正規表現オブジェクトであることを確認"""
        patterns = get_negative_patterns()
        for key, pattern in patterns.items():
            assert hasattr(pattern, 'match'), f"{key} is not a regex pattern"

    def test_kana_pattern_matches_katakana(self):
        """カタカナパターンがカタカナにマッチすることを確認"""
        patterns = get_positive_patterns()
        kana_pattern = patterns['Kana']

        assert kana_pattern.search('フォロー') is not None
        assert kana_pattern.search('プレゼント') is not None
        assert kana_pattern.search('キャンペーン') is not None

    def test_han_pattern_matches_kanji(self):
        """漢字パターンが漢字にマッチすることを確認"""
        patterns = get_positive_patterns()
        han_pattern = patterns['HAN']

        assert han_pattern.search('日本語') is not None
        assert han_pattern.search('漢字') is not None
        assert han_pattern.search('検出') is not None

    def test_alpha_pattern_matches_alphabet(self):
        """アルファベットパターンが英字にマッチすることを確認"""
        patterns = get_positive_patterns()
        alpha_pattern = patterns['alpha']

        assert alpha_pattern.search('test') is not None
        assert alpha_pattern.search('YouTube') is not None
        assert alpha_pattern.search('ABC') is not None

    def test_negative_gana_pattern_matches_hiragana(self):
        """ひらがなネガティブパターンがひらがなにマッチすることを確認"""
        patterns = get_negative_patterns()
        gana_pattern = patterns['x_Gana']

        assert gana_pattern.search('ひらがな') is not None
        assert gana_pattern.search('あいうえお') is not None

    def test_negative_url_pattern_matches_url(self):
        """URLネガティブパターンがURLにマッチすることを確認"""
        patterns = get_negative_patterns()
        url_pattern = patterns['x_url']

        assert url_pattern.search('https://example.com') is not None
        assert url_pattern.search('http://test.jp') is not None

    def test_negative_mail_pattern_matches_email(self):
        """メールネガティブパターンがメールアドレスにマッチすることを確認"""
        patterns = get_negative_patterns()
        mail_pattern = patterns['x_mail']

        assert mail_pattern.search('test@example.com') is not None
        assert mail_pattern.search('user@domain.co.jp') is not None

    def test_negative_yen_pattern_matches_currency(self):
        """通貨ネガティブパターンが通貨表記にマッチすることを確認"""
        patterns = get_negative_patterns()
        yen_pattern = patterns['x_Yen']

        assert yen_pattern.search('1000円') is not None
        assert yen_pattern.search('100万円') is not None
