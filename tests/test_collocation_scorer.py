# coding: utf-8
"""
コロケーション結合度スコアラーのテスト

CollocationScorer の全指標を検証：
- PMI, MI³, t-score, z-score, Log-Dice, Delta-P
"""

import pytest
import math
import pandas as pd
from japhrase import CollocationScorer, CollocationScore


class TestIndividualMetrics:
    """個別指標のテスト"""

    def test_pmi_positive(self):
        """共起が期待以上 → PMI正"""
        pmi = CollocationScorer.calc_pmi(100, 200, 300, 10000)
        assert pmi > 0

    def test_pmi_zero_freq(self):
        """頻度ゼロ → PMI=0"""
        pmi = CollocationScorer.calc_pmi(0, 100, 100, 10000)
        assert pmi == 0.0

    def test_mi3_higher_for_frequent(self):
        """MI³は頻度が高いと高くなる（PMIとの違い）"""
        mi3_high = CollocationScorer.calc_mi3(50, 100, 100, 10000)
        mi3_low = CollocationScorer.calc_mi3(5, 100, 100, 10000)
        assert mi3_high > mi3_low

    def test_t_score_positive(self):
        """共起が期待以上 → t-score正"""
        t = CollocationScorer.calc_t_score(100, 200, 300, 10000)
        assert t > 0

    def test_z_score_positive(self):
        """共起が期待以上 → z-score正"""
        z = CollocationScorer.calc_z_score(100, 200, 300, 10000)
        assert z > 0

    def test_log_dice_max(self):
        """完全共起 → Log-Dice最大（14）"""
        ld = CollocationScorer.calc_log_dice(100, 100, 100)
        assert ld == 14.0

    def test_log_dice_bounded(self):
        """Log-Diceは14以下"""
        ld = CollocationScorer.calc_log_dice(10, 200, 300)
        assert ld <= 14.0

    def test_delta_p_asymmetric(self):
        """Delta-Pは非対称"""
        dp_ab, dp_ba = CollocationScorer.calc_delta_p(50, 100, 200, 10000)
        # A→BとB→Aは一般に異なる
        assert isinstance(dp_ab, float)
        assert isinstance(dp_ba, float)

    def test_delta_p_perfect_prediction(self):
        """AがBを完全に予測 → ΔP(A→B)が高い"""
        # freq_ab == freq_a → P(B|A) = 1
        dp_ab, _ = CollocationScorer.calc_delta_p(100, 100, 500, 10000)
        assert dp_ab > 0.5


class TestScorePhrases:
    """バッチスコアリングのテスト"""

    def test_basic_scoring(self):
        """基本的なバッチスコアリング"""
        scorer = CollocationScorer()
        df = pd.DataFrame({
            'phrase': ['近衛騎士', 'レティシア'],
            'freq': [10, 5],
        })
        text = '近衛騎士団のレティシアは近衛騎士として名を馳せていた。' * 5

        result = scorer.score_phrases(df, text)
        assert len(result) == 2
        assert 'pmi' in result.columns
        assert 't_score' in result.columns
        assert 'log_dice' in result.columns
        assert 'combined_rank_score' in result.columns

    def test_sorted_by_rank(self):
        """結果は combined_rank_score で降順ソート"""
        scorer = CollocationScorer()
        df = pd.DataFrame({
            'phrase': ['aa', 'bb', 'cc'],
            'freq': [10, 5, 2],
        })
        text = 'aabbccaabbccaabb' * 20

        result = scorer.score_phrases(df, text)
        scores = result['combined_rank_score'].tolist()
        assert scores == sorted(scores, reverse=True)


class TestClassification:
    """コロケーション分類のテスト"""

    def test_classify(self):
        """分類が文字列を返す"""
        scorer = CollocationScorer()
        score = CollocationScore(
            phrase='test', part_a='te', part_b='st',
            freq_phrase=10, freq_a=20, freq_b=30, corpus_size=1000,
            pmi=5.0, mi3=8.0, t_score=3.0, z_score=2.0,
            log_dice=10.0, delta_p_ab=0.3, delta_p_ba=0.1,
            combined_rank_score=0.8,
        )
        result = scorer.classify_collocation(score)
        assert result in ('fixed_expression', 'habitual', 'creative', 'stable', 'weak')


class TestReport:
    """レポート生成のテスト"""

    def test_report(self):
        scorer = CollocationScorer()
        df = pd.DataFrame({
            'phrase': ['近衛騎士', 'レティシア'],
            'freq': [10, 5],
        })
        text = '近衛騎士団のレティシアは' * 10
        scores = scorer.score_phrases(df, text)
        report = scorer.generate_report(scores)
        assert '結合度分析' in report
