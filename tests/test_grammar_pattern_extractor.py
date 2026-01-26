"""
GrammarPatternExtractor のテスト
"""

import pytest
import pandas as pd
from japhrase.grammar_pattern_extractor import (
    GrammarPatternExtractor,
    DEFAULT_GRAMMAR_PATTERNS
)


class TestGrammarPatternExtractor:
    """GrammarPatternExtractorのテストクラス"""

    def test_init_default(self):
        """デフォルト初期化のテスト"""
        extractor = GrammarPatternExtractor()
        assert extractor.use_pmi is True
        assert extractor.use_frequency is True
        assert extractor.min_count == 3
        assert len(extractor.patterns) > 0

    def test_init_custom_patterns(self):
        """カスタムパターンのテスト"""
        custom_patterns = {
            'test_pattern': r'(\S+だった)',
        }
        extractor = GrammarPatternExtractor(patterns=custom_patterns)
        assert 'test_pattern' in extractor.patterns
        assert len(extractor.patterns) == 1

    def test_extract_basic(self):
        """基本的な抽出のテスト"""
        text = """
        彼は走っていた。彼女も走っていた。
        天気が良かった。昨日も良かった。
        それは面白いである。本当に面白いである。
        """
        extractor = GrammarPatternExtractor(min_count=2)
        df = extractor.extract(text)

        assert isinstance(df, pd.DataFrame)
        assert not df.empty
        assert 'phrase' in df.columns
        assert 'pattern' in df.columns
        assert 'count' in df.columns
        assert 'pmi' in df.columns
        assert 'score' in df.columns

    def test_extract_past_pattern(self):
        """過去形パターンの抽出テスト"""
        text = "彼は走っていた。" * 5 + "彼女も走っていた。" * 5
        extractor = GrammarPatternExtractor(min_count=3)
        df = extractor.extract(text, pattern_names=['progressive_teita'])

        assert not df.empty
        assert df['pattern'].iloc[0] == 'progressive_teita'
        assert '走っていた' in df['phrase'].values

    def test_extract_with_min_count(self):
        """min_countフィルタリングのテスト"""
        text = "走っていた。" * 2 + "歩いていた。" * 5
        extractor = GrammarPatternExtractor(min_count=3)
        df = extractor.extract(text)

        # min_count未満のフレーズは除外される
        assert all(df['count'] >= 3)

    def test_extract_low_pmi(self):
        """低PMIフレーズ抽出のテスト"""
        text = """
        それは良いである。それは悪いである。
        これは正しいである。あれは間違いである。
        """ * 3
        extractor = GrammarPatternExtractor(min_count=2)
        df = extractor.extract_low_pmi(text, max_pmi=2.0)

        assert isinstance(df, pd.DataFrame)
        if not df.empty:
            assert all(df['pmi'] <= 2.0)

    def test_extract_high_pmi(self):
        """高PMIフレーズ抽出のテスト"""
        text = "彼は走っていた。" * 5 + "速く走っていた。" * 5
        extractor = GrammarPatternExtractor(min_count=3)
        df = extractor.extract_high_pmi(text, min_pmi=1.0)

        assert isinstance(df, pd.DataFrame)
        if not df.empty:
            assert all(df['pmi'] >= 1.0)

    def test_rank_by_pmi(self):
        """PMIランキングのテスト"""
        text = "走っていた。" * 5 + "歩いていた。" * 3
        extractor = GrammarPatternExtractor(min_count=2)
        df = extractor.extract(text)
        ranked_df = extractor.rank_by_pmi(df)

        assert isinstance(ranked_df, pd.DataFrame)
        if not ranked_df.empty and len(ranked_df) > 1:
            # PMIの降順になっているか確認
            assert all(ranked_df['pmi'].iloc[i] >= ranked_df['pmi'].iloc[i+1]
                      for i in range(len(ranked_df)-1))

    def test_rank_by_frequency(self):
        """頻度ランキングのテスト"""
        text = "走っていた。" * 5 + "歩いていた。" * 3
        extractor = GrammarPatternExtractor(min_count=2)
        df = extractor.extract(text)
        ranked_df = extractor.rank_by_frequency(df)

        assert isinstance(ranked_df, pd.DataFrame)
        if not ranked_df.empty and len(ranked_df) > 1:
            # 頻度の降順になっているか確認
            assert all(ranked_df['count'].iloc[i] >= ranked_df['count'].iloc[i+1]
                      for i in range(len(ranked_df)-1))

    def test_group_by_pattern(self):
        """パターン別グループ化のテスト"""
        text = """
        走っていた。歩いていた。
        良かった。悪かった。
        """ * 3
        extractor = GrammarPatternExtractor(min_count=2)
        df = extractor.extract(text)
        grouped = extractor.group_by_pattern(df)

        assert isinstance(grouped, dict)
        for pattern_name, group_df in grouped.items():
            assert isinstance(group_df, pd.DataFrame)
            assert all(group_df['pattern'] == pattern_name)

    def test_get_pattern_summary(self):
        """パターンサマリーのテスト"""
        text = """
        走っていた。歩いていた。
        良かった。悪かった。
        """ * 5
        extractor = GrammarPatternExtractor(min_count=3)
        df = extractor.extract(text)
        summary = extractor.get_pattern_summary(df)

        assert isinstance(summary, pd.DataFrame)
        if not summary.empty:
            assert 'pattern' in summary.columns
            assert 'unique_phrases' in summary.columns
            assert 'total_count' in summary.columns
            assert 'avg_pmi' in summary.columns
            assert 'max_pmi' in summary.columns

    def test_empty_text(self):
        """空のテキストのテスト"""
        extractor = GrammarPatternExtractor()
        df = extractor.extract("")

        assert isinstance(df, pd.DataFrame)
        assert df.empty

    def test_no_match(self):
        """マッチしないテキストのテスト"""
        text = "abcdefg 123456"
        extractor = GrammarPatternExtractor()
        df = extractor.extract(text)

        assert isinstance(df, pd.DataFrame)
        # マッチしないので空になる可能性が高い

    def test_multiple_patterns(self):
        """複数パターンのテスト"""
        text = """
        走っていた。走っていた。
        良かった。良かった。
        面白いである。面白いである。
        """ * 3
        extractor = GrammarPatternExtractor(min_count=3)
        df = extractor.extract(text)

        if not df.empty:
            unique_patterns = df['pattern'].unique()
            assert len(unique_patterns) >= 1

    def test_without_pmi(self):
        """PMI無効のテスト"""
        text = "走っていた。" * 5
        extractor = GrammarPatternExtractor(use_pmi=False, min_count=2)
        df = extractor.extract(text)

        assert isinstance(df, pd.DataFrame)
        if not df.empty:
            # PMIは0になる
            assert all(df['pmi'] == 0.0)

    def test_list_input(self):
        """リスト入力のテスト"""
        sentences = [
            "走っていた。",
            "走っていた。",
            "歩いていた。",
            "歩いていた。",
            "歩いていた。",
        ]
        extractor = GrammarPatternExtractor(min_count=2)
        df = extractor.extract(sentences)

        assert isinstance(df, pd.DataFrame)
        assert not df.empty

    def test_custom_min_count(self):
        """カスタムmin_countのテスト"""
        text = "走っていた。" * 10
        extractor_low = GrammarPatternExtractor(min_count=5)
        extractor_high = GrammarPatternExtractor(min_count=15)

        df_low = extractor_low.extract(text)
        df_high = extractor_high.extract(text)

        assert not df_low.empty  # 5回以上出現するのでマッチ
        assert df_high.empty  # 15回未満なのでマッチしない

    def test_default_patterns_exist(self):
        """デフォルトパターンが存在するかのテスト"""
        assert isinstance(DEFAULT_GRAMMAR_PATTERNS, dict)
        assert len(DEFAULT_GRAMMAR_PATTERNS) > 0
        assert 'past_datta' in DEFAULT_GRAMMAR_PATTERNS
        assert 'progressive_teita' in DEFAULT_GRAMMAR_PATTERNS

    def test_score_calculation(self):
        """スコア計算のテスト"""
        text = "走っていた。" * 5 + "歩いていた。" * 3
        extractor = GrammarPatternExtractor(min_count=2)
        df = extractor.extract(text)

        assert 'score' in df.columns
        if not df.empty:
            # スコアは0-1の範囲
            assert all(df['score'] >= 0.0)
            assert all(df['score'] <= 1.0)
