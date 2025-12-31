# coding:utf-8
"""
extracter.pyのテスト
"""

import pytest
import pandas as pd
import numpy as np
from japhrase import PhraseExtracter, extracter


class TestPhraseExtracterInit:
    """PhraseExtracterの初期化テスト"""

    def test_create_instance_default(self):
        """デフォルトパラメータでインスタンス作成できることを確認"""
        extractor = PhraseExtracter()
        assert extractor is not None
        assert extractor.min_count == 6
        assert extractor.max_length == 17  # 16 + 1
        assert extractor.min_length == 4

    def test_create_instance_with_params(self):
        """カスタムパラメータでインスタンス作成できることを確認"""
        extractor = PhraseExtracter(
            min_count=10,
            max_length=20,
            min_length=3,
            verbose=0
        )
        assert extractor.min_count == 10
        assert extractor.max_length == 21  # 20 + 1
        assert extractor.min_length == 3
        assert extractor.verbose == 0

    def test_backward_compatibility(self):
        """後方互換性のためのエイリアスが存在することを確認"""
        assert extracter == PhraseExtracter


class TestPhraseExtracterMethods:
    """PhraseExtracterのメソッドテスト"""

    @pytest.fixture
    def extractor(self):
        """テスト用のエクストラクタインスタンスを提供"""
        return PhraseExtracter(min_count=2, verbose=0)

    def test_make_ngrampieces(self, extractor):
        """N-gram生成のテスト"""
        sentences = ["こんにちは", "ありがとう"]
        ngrams = extractor.make_ngrampieces(sentences)

        assert isinstance(ngrams, list)
        assert len(ngrams) > 0
        # 最小長さ以上のフレーズのみが生成される
        for ngram in ngrams:
            assert len(ngram) >= extractor.min_length

    def test_count_characters(self, extractor):
        """文字カウントのテスト"""
        phrases = ["テスト", "テスト", "テスト", "サンプル", "サンプル", "サンプル"]
        df = extractor.count_characters(phrases)

        assert isinstance(df, pd.DataFrame)
        assert 'seqchar' in df.columns
        assert 'freq' in df.columns
        assert 'length' in df.columns
        assert len(df) > 0

    def test_count_knowns(self, extractor):
        """既知語カウントのテスト"""
        extractor.knowns = ["こんにちは", "ありがとう"]
        sentences = ["こんにちは、今日はいい天気ですね", "ありがとうございます"]

        df = extractor.count_knowns(sentences)

        assert isinstance(df, pd.DataFrame)
        assert len(df) == 2
        assert 'seqchar' in df.columns
        assert 'freq' in df.columns

    def test_levenshtein(self, extractor):
        """レーベンシュタイン距離の計算テスト"""
        # 完全一致
        assert extractor.levenshtein("test", "test") == 0

        # 1文字違い
        assert extractor.levenshtein("test", "text") == 1

        # 長さが違う
        distance = extractor.levenshtein("test", "testing")
        assert distance > 0

    def test_similarity(self, extractor):
        """類似度計算のテスト"""
        # 完全一致は類似度1.0
        sim = extractor.similarity("test", "test")
        assert sim == 1.0

        # 完全不一致は類似度が低い
        sim = extractor.similarity("abc", "xyz")
        assert sim < 1.0

    def test_doubt_periodic_letter(self, extractor):
        """周期的パターン検出のテスト"""
        # 周期的なパターン
        assert extractor.doubt_periodic_letter("ゴロゴロ") == True
        assert extractor.doubt_periodic_letter("あああ") == True

        # 非周期的なパターン
        assert extractor.doubt_periodic_letter("こんにちは") == False
        assert extractor.doubt_periodic_letter("テスト") == False

    def test_exclude_unnecessary(self, extractor):
        """不要文字列除外のテスト"""
        extractor.unnecessary = ["http", "www"]
        df = pd.DataFrame({
            'seqchar': ['テスト', 'http://example.com', 'サンプル', 'www.test.com'],
            'freq': [5, 3, 4, 2],
            'length': [3, 18, 4, 13]
        })

        result = extractor.exclude_unnecessary(df)

        assert len(result) == 2
        assert 'http://example.com' not in result['seqchar'].values
        assert 'www.test.com' not in result['seqchar'].values


class TestPhraseExtracterIntegration:
    """統合テスト"""

    def test_get_dfphrase_simple(self):
        """簡単なテキストからフレーズを抽出"""
        extractor = PhraseExtracter(min_count=2, verbose=0)
        sentences = [
            "フォローありがとうございます",
            "フォローありがとうございます",
            "フォローしてください",
            "プレゼントキャンペーン開催中",
            "プレゼントキャンペーン実施中",
            "プレゼントキャンペーン応募受付中"
        ]

        df = extractor.get_dfphrase(sentences)

        assert isinstance(df, pd.DataFrame)
        # フレーズが抽出されているはず
        if len(df) > 0:
            assert 'seqchar' in df.columns
            assert 'freq' in df.columns

    def test_get_dfphrase_empty(self):
        """空のテキストの場合はValueErrorを発生させる"""
        extractor = PhraseExtracter(verbose=0)
        sentences = []

        # 空のリストに対してはValueErrorが発生することを確認
        with pytest.raises(ValueError) as exc_info:
            extractor.get_dfphrase(sentences)

        assert "入力テキストが空です" in str(exc_info.value)

    def test_demo_method(self):
        """demo()メソッドが正しく動作することを確認"""
        df = PhraseExtracter.demo(verbose=0)

        # 結果が返されることを確認
        assert isinstance(df, pd.DataFrame)
        # サンプルデータから何かしらのフレーズが抽出されることを期待
        assert len(df) > 0
        # 必要なカラムが存在することを確認
        assert 'seqchar' in df.columns
        assert 'freq' in df.columns

    def test_get_dfphrase_with_knowns(self):
        """既知語を指定してフレーズ抽出"""
        extractor = PhraseExtracter(
            min_count=1,
            verbose=0,
            knowns=["テスト"]
        )
        sentences = [
            "テストを実行します",
            "テストが完了しました"
        ]

        df = extractor.get_dfphrase(sentences)

        assert isinstance(df, pd.DataFrame)
        # 既知語が含まれているはず
        if len(df) > 0:
            assert 'knowns' in df.columns


class TestPhraseExtracterEdgeCases:
    """エッジケースのテスト"""

    def test_very_short_sentences(self):
        """非常に短い文章のテスト"""
        extractor = PhraseExtracter(min_count=1, min_length=2, verbose=0)
        sentences = ["ab", "cd", "ef"]

        df = extractor.get_dfphrase(sentences)

        assert isinstance(df, pd.DataFrame)

    def test_single_sentence(self):
        """単一の文章のテスト"""
        extractor = PhraseExtracter(min_count=1, verbose=0)
        sentences = ["これは単一の文章です"]

        df = extractor.get_dfphrase(sentences)

        assert isinstance(df, pd.DataFrame)

    def test_repeated_phrase(self):
        """同じフレーズが繰り返される場合"""
        extractor = PhraseExtracter(min_count=3, verbose=0)
        sentences = ["テストテストテストテスト"] * 5

        df = extractor.get_dfphrase(sentences)

        assert isinstance(df, pd.DataFrame)
