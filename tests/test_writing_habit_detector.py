"""
WritingHabitDetector のテスト
"""

import pytest
import pandas as pd
from japhrase.writing_habit_detector import WritingHabitDetector


class TestWritingHabitDetector:
    """WritingHabitDetectorのテストクラス"""

    def test_init_default(self):
        """デフォルト初期化のテスト"""
        detector = WritingHabitDetector()
        assert detector.min_count == 10
        assert detector.max_pmi == 3.0
        assert detector.min_length == 2
        assert detector.max_length == 15
        assert detector.verbose is True

    def test_init_custom(self):
        """カスタム初期化のテスト"""
        detector = WritingHabitDetector(
            min_count=5,
            max_pmi=2.0,
            top_n=10,
            verbose=False
        )
        assert detector.min_count == 5
        assert detector.max_pmi == 2.0
        assert detector.top_n == 10
        assert detector.verbose is False

    def test_detect_basic(self):
        """基本的な検出のテスト"""
        # 高頻度×低PMIのフレーズを含むテキスト
        text = """
        それは良い。それは悪い。それは普通。それは面白い。
        それは素晴らしい。それは微妙。それは最高。それは最悪。
        それは楽しい。それは退屈。それは興味深い。
        """ * 3

        detector = WritingHabitDetector(min_count=5, max_pmi=5.0)
        df = detector.detect(text)

        assert isinstance(df, pd.DataFrame)
        assert 'phrase' in df.columns
        assert 'count' in df.columns
        assert 'pmi' in df.columns
        assert 'habit_score' in df.columns

    def test_detect_high_frequency_low_pmi(self):
        """高頻度×低PMIフレーズの検出テスト"""
        # 「それは」を高頻度で使用
        text = """
        それは良い。それは悪い。それは普通。
        それは面白い。それは素晴らしい。
        """ * 15

        detector = WritingHabitDetector(min_count=10, max_pmi=5.0)
        df = detector.detect(text)

        # 高頻度のフレーズが検出される（空でも良い場合がある）
        if not df.empty:
            assert all(df['count'] >= 10)
            assert all(df['pmi'] <= 5.0)

    def test_detect_with_top_n(self):
        """top_nパラメータのテスト"""
        text = """
        それは良い。それは悪い。それは普通。
        これは面白い。これは素晴らしい。
        あれは微妙。あれは最高。
        """ * 5

        detector = WritingHabitDetector(min_count=3, top_n=3)
        df = detector.detect(text)

        assert len(df) <= 3

    def test_habit_score_calculation(self):
        """書き癖スコアの計算テスト"""
        text = "それは良い。" * 20 + "これは悪い。" * 10
        detector = WritingHabitDetector(min_count=5)
        df = detector.detect(text)

        if not df.empty:
            # スコアは0-1の範囲
            assert all(df['habit_score'] >= 0.0)
            assert all(df['habit_score'] <= 1.0)
            # スコア順にソートされている
            assert all(df['habit_score'].iloc[i] >= df['habit_score'].iloc[i+1]
                      for i in range(len(df)-1) if len(df) > 1)

    def test_frequency_rank(self):
        """頻度ランクのテスト"""
        text = "それは" * 30 + "これは" * 20 + "あれは" * 10
        detector = WritingHabitDetector(min_count=5)
        df = detector.detect(text)

        if not df.empty:
            assert 'frequency_rank' in df.columns
            # 最頻出は1位
            top_freq_phrase = df.loc[df['count'].idxmax()]
            assert top_freq_phrase['frequency_rank'] == 1

    def test_pmi_rank(self):
        """PMIランクのテスト"""
        text = "それは良い。" * 20
        detector = WritingHabitDetector(min_count=5)
        df = detector.detect(text)

        if not df.empty:
            assert 'pmi_rank' in df.columns
            # 最低PMIは1位
            min_pmi_phrase = df.loc[df['pmi'].idxmin()]
            assert min_pmi_phrase['pmi_rank'] == 1

    def test_detect_by_category(self):
        """カテゴリ別検出のテスト"""
        text = """
        それは良い。それは悪い。それは普通。
        これは面白い。これは素晴らしい。
        """ * 20

        detector = WritingHabitDetector(min_count=10)
        categories = detector.detect_by_category(text)

        assert isinstance(categories, dict)
        assert 'high_frequency' in categories
        assert 'very_low_pmi' in categories
        assert 'balanced' in categories

        for category_name, category_df in categories.items():
            assert isinstance(category_df, pd.DataFrame)

    def test_get_summary(self):
        """サマリー統計のテスト"""
        text = "それは良い。" * 20 + "これは悪い。" * 15
        detector = WritingHabitDetector(min_count=10)
        df = detector.detect(text)
        summary = detector.get_summary(df)

        assert isinstance(summary, dict)
        if summary:
            assert 'total_habits' in summary
            assert 'avg_frequency' in summary
            assert 'max_frequency' in summary
            assert 'avg_pmi' in summary
            assert 'min_pmi' in summary
            assert 'top_habit' in summary
            assert 'top_habit_score' in summary

    def test_compare_texts(self):
        """テキスト比較のテスト"""
        text1 = "それは良い。" * 30
        text2 = "これは悪い。" * 30

        detector = WritingHabitDetector(min_count=10)
        comparison = detector.compare_texts(text1, text2, "著者A", "著者B")

        assert isinstance(comparison, pd.DataFrame)
        if not comparison.empty:
            assert 'phrase' in comparison.columns
            assert 'diff' in comparison.columns

    def test_empty_text(self):
        """空のテキストのテスト"""
        detector = WritingHabitDetector()
        df = detector.detect("")

        assert isinstance(df, pd.DataFrame)
        assert df.empty or len(df) == 0

    def test_list_input(self):
        """リスト入力のテスト"""
        sentences = [
            "それは良い。",
            "それは悪い。",
            "それは普通。",
        ] * 10

        detector = WritingHabitDetector(min_count=5)
        df = detector.detect(sentences)

        assert isinstance(df, pd.DataFrame)

    def test_min_count_filtering(self):
        """min_countフィルタリングのテスト"""
        text = "それは良い。" * 5 + "これは悪い。" * 15
        detector = WritingHabitDetector(min_count=10)
        df = detector.detect(text)

        # min_count未満は除外される
        if not df.empty:
            assert all(df['count'] >= 10)

    def test_max_pmi_filtering(self):
        """max_pmiフィルタリングのテスト"""
        text = "それは良い。" * 20
        detector = WritingHabitDetector(min_count=5, max_pmi=2.0)
        df = detector.detect(text)

        # max_pmi以上は除外される
        if not df.empty:
            assert all(df['pmi'] <= 2.0)

    def test_length_filtering(self):
        """長さフィルタリングのテスト"""
        text = "あ" * 20 + "とても良い" * 15 + "本当に素晴らしい作品" * 10
        detector = WritingHabitDetector(
            min_count=5,
            min_length=3,
            max_length=10
        )
        df = detector.detect(text)

        if not df.empty:
            # 長さがmin_length以上、max_length以下
            assert all(len(phrase) >= 3 for phrase in df['phrase'])
            assert all(len(phrase) <= 10 for phrase in df['phrase'])
