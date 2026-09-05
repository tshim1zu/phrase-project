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
        assert extractor.min_count == 2
        assert extractor.max_length == 16
        assert extractor.min_length == 2

    def test_create_instance_with_params(self):
        """カスタムパラメータでインスタンス作成できることを確認"""
        extractor = PhraseExtracter(
            min_count=10,
            max_length=20,
            min_length=3,
            verbose=0
        )
        assert extractor.min_count == 10
        assert extractor.max_length == 20
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


class TestBoundaryBugRegressions:
    """境界値バグの回帰テスト（レビューで指摘された問題の再発防止）"""

    def test_max_length_not_exceeded(self):
        """make_ngrampiecesが指定したmax_lengthより長いN-gramを生成しないこと"""
        extractor = PhraseExtracter(min_count=1, max_length=5, min_length=1, verbose=0)
        sentences = ["あ" * 20]
        ngrams = extractor.make_ngrampieces(sentences)
        assert all(len(ng) <= 5 for ng in ngrams)
        assert any(len(ng) == 5 for ng in ngrams)

    def test_max_length_minus_one_uses_half_sentence_count(self):
        """max_length=-1のとき、sentences件数の半分をmax_lengthとして使う特殊ケースが機能すること
        （従来はコンストラクタの+1補正のせいでこの分岐に絶対到達しなかった）"""
        extractor = PhraseExtracter(min_count=1, max_length=-1, min_length=1, verbose=0)
        sentences = ["あいうえおかきくけこ"] * 6  # 6件 -> 内部max_lengthは3
        ngrams = extractor.make_ngrampieces(sentences)
        assert max(len(ng) for ng in ngrams) == 3

    def test_min_count_boundary_is_inclusive(self):
        """min_countちょうどの出現回数のフレーズも残ること（count_charactersとdf_from_countsで挙動を統一）"""
        extractor = PhraseExtracter(min_count=3, verbose=0)
        phrases = ["テスト"] * 3 + ["サンプル"] * 2
        df = extractor.count_characters(phrases)
        assert "テスト" in df["seqchar"].values
        assert "サンプル" not in df["seqchar"].values

    def test_save_load_roundtrip_preserves_weights(self):
        """save_params/load_paramsでweight_freq等のチューニング可能パラメータが失われないこと"""
        import tempfile, os
        extractor = PhraseExtracter(
            weight_freq=0.4, weight_len=1.6,
            pmi_weight=2.5, entropy_weight=0.3,
            verbose=0,
        )
        path = os.path.join(tempfile.gettempdir(), "test_roundtrip_params.json")
        try:
            extractor.save_params(path)
            restored = PhraseExtracter.load_params(path)
            assert restored.params == extractor.params
        finally:
            if os.path.exists(path):
                os.remove(path)

    def test_branching_entropy_no_cross_sentence_leak(self):
        """分岐エントロピーが文境界をまたいだ偽の隣接文字を作らないこと"""
        extractor = PhraseExtracter(min_count=1, verbose=0)
        # "abc" の直後に本来存在しない "d" が続くように見えないことを確認
        sentences = ["xyzabc", "dabc"]
        scores = extractor.calculate_branching_entropy(sentences, ["abc"])
        left_entropy, right_entropy, _ = scores["abc"]
        # "abc"の右には常に何も続かない（各文の末尾）ため右エントロピーは0のはず
        assert right_entropy == 0.0
