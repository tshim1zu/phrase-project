# coding: utf-8
"""
分布比較エンジンのテスト

DistributionComparator の全機能を検証：
- Log-Likelihood Ratio (G²)
- Jensen-Shannon Divergence
- Log Ratio (Effect Size)
- Dice Coefficient
- Keyness Profiler
"""

import pytest
import numpy as np
from collections import Counter
from japhrase import DistributionComparator, DistributionComparison, KeynessResult


class TestLogLikelihoodRatio:
    """G² テスト"""

    def test_perfect_association(self):
        """完全一致の場合、G²は大きい"""
        g2 = DistributionComparator.log_likelihood_ratio(100, 0, 0, 100)
        assert g2 > 50, "完全一致は高い G²"

    def test_independent(self):
        """独立分布はG²が小さい"""
        g2 = DistributionComparator.log_likelihood_ratio(50, 50, 50, 50)
        assert abs(g2) < 1.0, "均等分布は低い G²"

    def test_non_negative(self):
        """G²は非負"""
        g2 = DistributionComparator.log_likelihood_ratio(10, 20, 30, 40)
        assert g2 >= 0


class TestJensenShannonDivergence:
    """JSD テスト"""

    def test_identical_distributions(self):
        """同一分布のJSDは0"""
        p = np.array([0.3, 0.3, 0.4])
        jsd = DistributionComparator.jensen_shannon_divergence(p, p)
        assert jsd < 0.001, f"同一分布のJSDは0に近い: {jsd}"

    def test_completely_different(self):
        """完全に異なる分布のJSDは高い"""
        p = np.array([1.0, 0.0, 0.0])
        q = np.array([0.0, 0.0, 1.0])
        jsd = DistributionComparator.jensen_shannon_divergence(p, q)
        assert jsd > 0.5, f"完全に異なる分布のJSDは高い: {jsd}"

    def test_symmetric(self):
        """JSDは対称"""
        p = np.array([0.2, 0.5, 0.3])
        q = np.array([0.4, 0.1, 0.5])
        jsd_pq = DistributionComparator.jensen_shannon_divergence(p, q)
        jsd_qp = DistributionComparator.jensen_shannon_divergence(q, p)
        assert abs(jsd_pq - jsd_qp) < 1e-10, "JSD is symmetric"

    def test_bounded(self):
        """JSDは0-1の範囲"""
        p = np.array([0.1, 0.9])
        q = np.array([0.8, 0.2])
        jsd = DistributionComparator.jensen_shannon_divergence(p, q)
        assert 0 <= jsd <= 1


class TestLogRatio:
    """Effect Size テスト"""

    def test_overuse(self):
        """ターゲットで頻度が高い → 正"""
        lr = DistributionComparator.log_ratio(100, 10, 1000, 1000)
        assert lr > 0

    def test_underuse(self):
        """ターゲットで頻度が低い → 負"""
        lr = DistributionComparator.log_ratio(10, 100, 1000, 1000)
        assert lr < 0

    def test_equal_use(self):
        """頻度が同じ → 0付近"""
        lr = DistributionComparator.log_ratio(50, 50, 1000, 1000)
        assert abs(lr) < 0.1


class TestDiceCoefficient:
    """Dice 係数テスト"""

    def test_identical(self):
        """同じ正規化頻度 → 1.0"""
        dice = DistributionComparator.dice_coefficient(10, 10, 100, 100)
        assert abs(dice - 1.0) < 0.01

    def test_zero(self):
        """片方がゼロ → 0.0"""
        dice = DistributionComparator.dice_coefficient(10, 0, 100, 100)
        assert dice == 0.0

    def test_bounded(self):
        """0-1の範囲"""
        dice = DistributionComparator.dice_coefficient(30, 10, 200, 500)
        assert 0.0 <= dice <= 1.0


class TestCompare:
    """compare() 統合テスト"""

    def test_basic_comparison(self):
        """基本的な比較が動く"""
        comp = DistributionComparator()
        freq_a = Counter({'a': 10, 'b': 5, 'c': 3})
        freq_b = Counter({'a': 3, 'b': 8, 'd': 6})
        result = comp.compare(freq_a, freq_b)

        assert isinstance(result, DistributionComparison)
        assert result.jsd >= 0
        assert result.shared_terms == 2  # a, b
        assert len(result.keyness_results) > 0

    def test_identical_texts(self):
        """同一テキストはJSDが低い"""
        comp = DistributionComparator()
        freq = Counter({'x': 10, 'y': 20, 'z': 30})
        result = comp.compare(freq, freq)
        assert result.jsd < 0.01

    def test_keyness_direction(self):
        """キーネスの方向が正しい"""
        comp = DistributionComparator(min_freq=1)
        freq_a = Counter({'only_a': 50})
        freq_b = Counter({'only_b': 50})
        result = comp.compare(freq_a, freq_b)

        for kr in result.keyness_results:
            if kr.term == 'only_a':
                assert kr.direction == 'overuse'
            elif kr.term == 'only_b':
                assert kr.direction == 'underuse'

    def test_dataframe_output(self):
        """DataFrame 出力が正しい"""
        comp = DistributionComparator()
        freq_a = Counter({'a': 10, 'b': 5})
        freq_b = Counter({'a': 3, 'c': 8})
        df = comp.compare_to_dataframe(freq_a, freq_b)

        assert 'term' in df.columns
        assert 'log_likelihood' in df.columns
        assert 'keyness_score' in df.columns

    def test_report_generation(self):
        """レポート生成が動く"""
        comp = DistributionComparator()
        freq_a = Counter({'a': 10, 'b': 5, 'c': 3})
        freq_b = Counter({'a': 3, 'b': 8, 'd': 6})
        report = comp.generate_report(freq_a, freq_b)
        assert '分布比較レポート' in report
