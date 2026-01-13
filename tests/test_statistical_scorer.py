# coding: utf-8
"""
統計スコアリングのテスト

StatisticalScorer の全機能を検証：
- カイ二乗検定
- 相互情報量
- ジップ法則異常検出
- 信頼区間計算
- 複合スコア
- 信頼度メトリクス
"""

import pytest
import pandas as pd
import numpy as np
import math
from japhrase import StatisticalScorer, StatisticalScore, PhraseTrustMetric


class TestChiSquareTest:
    """カイ二乗検定のテスト"""

    def test_chi_square_expected_frequency(self):
        """期待度数の計算が正確か"""
        scorer = StatisticalScorer(alpha=0.05)

        # フレーズが期待度数通りに分布
        chi_score = scorer._chi_square_test(
            phrase_freq=100,      # 観測度数
            total_texts=1000,     # テキスト総数
            total_freq=10000      # 総フレーズ数
        )

        # 期待度数 = 10000 / 1000 = 10
        # 観測度数 = 100 は期待度数の10倍 → 有意
        assert chi_score > 0.5, "高頻度フレーズは高スコア"
        assert 0.0 <= chi_score <= 1.0, "スコアは0-1の範囲"

    def test_chi_square_rare_phrase(self):
        """稀なフレーズのスコア"""
        scorer = StatisticalScorer(alpha=0.05)

        chi_score = scorer._chi_square_test(
            phrase_freq=1,        # 観測度数
            total_texts=1000,
            total_freq=10000
        )

        # 期待度数 = 10000/1000 = 10, 観測度数 = 1
        # chi_square = (1-10)^2 / 10 = 8.1
        # このような大きな乖離は有意 → スコアが高い（これは正しい）
        assert chi_score > 0.8, "稀なフレーズでも顕著な乖離は高スコア"

    def test_chi_square_zero_frequency(self):
        """度数がゼロの場合"""
        scorer = StatisticalScorer(alpha=0.05)

        chi_score = scorer._chi_square_test(
            phrase_freq=0,
            total_texts=100,
            total_freq=1000
        )

        # 期待度数 = 1000/100 = 10, 観測度数 = 0
        # chi_square = (0-10)^2 / 10 = 10
        # これも大きな乖離 → スコアが高い（フレーズが出現しない = 統計的に有意な非出現）
        assert chi_score > 0.8, "度数ゼロでも顕著な乖離は有意"


class TestMutualInformation:
    """相互情報量のテスト"""

    def test_mutual_information_independent(self):
        """独立したコンテキスト（スコア低い）"""
        scorer = StatisticalScorer()

        corpus_stats = {
            'total_phrases': 1000,
            'total_texts': 100,
        }

        context_freq = {
            'テスト': {
                'text_count': 50,      # 半分のテキストに出現
                'frequency': 100,
            }
        }

        mi = scorer._mutual_information(
            phrase='テスト',
            phrase_freq=100,
            corpus_stats=corpus_stats,
            context_freq=context_freq
        )

        # 独立的な出現 → スコアは中程度
        assert 0.0 <= mi <= 1.0, "スコアは0-1の範囲"

    def test_mutual_information_no_context(self):
        """コンテキスト情報なし"""
        scorer = StatisticalScorer()

        mi = scorer._mutual_information(
            phrase='テスト',
            phrase_freq=100,
            corpus_stats={'total_phrases': 1000, 'total_texts': 100},
            context_freq=None
        )

        assert mi == 0.5, "データなしは中立的スコア"

    def test_mutual_information_zero_context(self):
        """テキストカウントがゼロ"""
        scorer = StatisticalScorer()

        context_freq = {
            'テスト': {'text_count': 0, 'frequency': 0}
        }

        mi = scorer._mutual_information(
            phrase='テスト',
            phrase_freq=100,
            corpus_stats={'total_phrases': 1000, 'total_texts': 100},
            context_freq=context_freq
        )

        assert mi == 0.0, "テキストカウント0はスコア0"


class TestZipfAnomaly:
    """ジップ法則異常検出のテスト"""

    def test_zipf_anomaly_normal(self):
        """正常な分布（ジップ法則に従う）"""
        scorer = StatisticalScorer()

        anomaly = scorer._zipf_anomaly(
            freq=100,
            rank=1,
            total_freq=1000
        )

        # rank 1 で freq 100 → zipf_constant = 100
        # 期待度数 = 100 / 1 = 100 → 乖離なし
        assert anomaly >= 0.0, "異常スコアは非負"
        assert anomaly <= 1.0, "異常スコアは1以下"

    def test_zipf_anomaly_high_rank_high_freq(self):
        """ランクが高いのに頻度が低い（期待値通り）"""
        scorer = StatisticalScorer()

        # rank が高い（20）のに freq が低い場合
        anomaly = scorer._zipf_anomaly(
            freq=5,             # 低頻度
            rank=20,            # 高ランク
            total_freq=1000
        )

        # zipf_constant = 5 * 20 = 100
        # 期待度数 = 100 / 20 = 5
        # 観測度数 = 5 → 乖離なし
        assert anomaly < 0.1, "期待値通りは異常スコア低い"

    def test_zipf_anomaly_zero_frequency(self):
        """度数ゼロ"""
        scorer = StatisticalScorer()

        anomaly = scorer._zipf_anomaly(
            freq=0,
            rank=1,
            total_freq=100
        )

        assert anomaly == 0.0, "度数ゼロはスコア0"


class TestConfidenceInterval:
    """信頼区間のテスト"""

    def test_confidence_interval_bounds(self):
        """信頼区間の境界チェック"""
        scorer = StatisticalScorer(alpha=0.05)

        lower, upper = scorer._confidence_interval(
            count=500,
            total=1000,
            alpha=0.05
        )

        # 割合 = 0.5 → 信頼区間は0.5周辺
        assert 0.0 <= lower <= 0.5, "下限は0-0.5の範囲"
        assert 0.5 <= upper <= 1.0, "上限は0.5-1の範囲"
        assert lower <= upper, "下限 <= 上限"
        assert lower < upper, "区間は非ゼロ幅"

    def test_confidence_interval_narrow(self):
        """サンプル数大きい → 区間狭い"""
        scorer = StatisticalScorer(alpha=0.05)

        lower_small, upper_small = scorer._confidence_interval(
            count=5,
            total=10,
            alpha=0.05
        )

        lower_large, upper_large = scorer._confidence_interval(
            count=5000,
            total=10000,
            alpha=0.05
        )

        width_small = upper_small - lower_small
        width_large = upper_large - lower_large

        # サンプル大きい → 区間狭い
        assert width_large < width_small, "大サンプルは区間が狭い"

    def test_confidence_interval_zero_total(self):
        """合計がゼロ"""
        scorer = StatisticalScorer(alpha=0.05)

        lower, upper = scorer._confidence_interval(
            count=0,
            total=0,
            alpha=0.05
        )

        assert lower == 0.0 and upper == 0.0, "合計0は(0,0)を返す"


class TestPValue:
    """p値計算のテスト"""

    def test_p_value_significant(self):
        """有意なフレーズ（p < 0.05）"""
        scorer = StatisticalScorer(alpha=0.05)

        p_value = scorer._calculate_p_value(
            phrase_freq=500,       # 期待値の10倍
            total_texts=100,
            total_freq=5000
        )

        # 期待度数 = 5000 / 100 = 50
        # 観測度数 = 500 は期待値の10倍 → 有意
        assert p_value < 0.05, "高度に異なる頻度は有意"

    def test_p_value_not_significant(self):
        """有意でないフレーズ（p >= 0.05）"""
        scorer = StatisticalScorer(alpha=0.05)

        p_value = scorer._calculate_p_value(
            phrase_freq=50,        # 期待値通り
            total_texts=100,
            total_freq=5000
        )

        # 期待度数 = 5000 / 100 = 50
        # 観測度数 = 50 は期待値と同じ → 非有意
        assert p_value > 0.05, "期待値通りの頻度は非有意"


class TestSignificanceLevel:
    """有意水準判定のテスト"""

    def test_significance_level_highly_significant(self):
        """p < 0.001"""
        scorer = StatisticalScorer()

        level = scorer._get_significance_level(p_value=0.0001)
        assert level == "***", "p < 0.001 は '***'"

    def test_significance_level_very_significant(self):
        """p < 0.01"""
        scorer = StatisticalScorer()

        level = scorer._get_significance_level(p_value=0.001)
        assert level == "**", "p < 0.01 は '**'"

    def test_significance_level_significant(self):
        """p < 0.05"""
        scorer = StatisticalScorer()

        level = scorer._get_significance_level(p_value=0.01)
        assert level == "*", "p < 0.05 は '*'"

    def test_significance_level_not_significant(self):
        """p >= 0.05"""
        scorer = StatisticalScorer()

        level = scorer._get_significance_level(p_value=0.1)
        assert level == "ns", "p >= 0.05 は 'ns'"


class TestCombinedScore:
    """複合スコア統合のテスト"""

    def test_combined_score_all_high(self):
        """全指標が高い"""
        scorer = StatisticalScorer()

        combined = scorer._combined_score(
            chi_square=1.0,
            mutual_info=1.0,
            zipf_anomaly=1.0,
            is_significant=True
        )

        assert combined > 0.8, "全指標高い → スコア高い"
        assert combined <= 1.0, "スコアは1以下"

    def test_combined_score_all_low(self):
        """全指標が低い"""
        scorer = StatisticalScorer()

        combined = scorer._combined_score(
            chi_square=0.0,
            mutual_info=0.0,
            zipf_anomaly=0.0,
            is_significant=False
        )

        assert combined == 0.0, "全指標低い → スコア0"

    def test_combined_score_weighted(self):
        """重み付けが反映される"""
        scorer = StatisticalScorer()

        # カイ二乗が40%の重み
        combined_high_chi = scorer._combined_score(
            chi_square=1.0,
            mutual_info=0.0,
            zipf_anomaly=0.0,
            is_significant=False
        )

        # カイ二乗が0、相互情報量が30%の重み
        combined_high_mi = scorer._combined_score(
            chi_square=0.0,
            mutual_info=1.0,
            zipf_anomaly=0.0,
            is_significant=False
        )

        # カイ二乗の重みが大きいため
        assert combined_high_chi > combined_high_mi, "カイ二乗の重みが最大"


class TestScorePhrase:
    """フレーズスコアリングのエンドツーエンドテスト"""

    def test_score_phrase_basic(self):
        """基本的なスコアリング"""
        scorer = StatisticalScorer()

        score = scorer.score_phrase(
            phrase='テスト',
            phrase_freq=100,
            corpus_stats={
                'total_texts': 100,
                'unique_phrases': 50,
                'total_phrases': 1000,
            }
        )

        assert isinstance(score, StatisticalScore), "StatisticalScore を返す"
        assert score.phrase == 'テスト', "フレーズ名が保存される"
        assert score.frequency == 100, "頻度が保存される"
        assert 0.0 <= score.chi_square_score <= 1.0, "カイ二乗スコアは0-1"
        assert 0.0 <= score.mutual_information <= 1.0, "MI スコアは0-1"
        assert 0.0 <= score.zipf_anomaly_score <= 1.0, "Zipf スコアは0-1"
        assert 0.0 <= score.combined_score <= 1.0, "複合スコアは0-1"

    def test_score_phrase_with_context(self):
        """コンテキスト付きスコアリング"""
        scorer = StatisticalScorer()

        context_freq = {
            'テスト': {'text_count': 50, 'frequency': 100}
        }

        score = scorer.score_phrase(
            phrase='テスト',
            phrase_freq=100,
            corpus_stats={
                'total_texts': 100,
                'unique_phrases': 50,
                'total_phrases': 1000,
            },
            context_freq=context_freq
        )

        # MI が計算される
        assert score.mutual_information > 0.0, "MI が計算される"


class TestScorePhrases:
    """複数フレーズのバッチスコアリング"""

    def test_score_phrases_dataframe(self):
        """DataFrame 入力"""
        scorer = StatisticalScorer()

        df_phrases = pd.DataFrame({
            'seqchar': ['テスト', 'テスト', '実装', '実装', '実装'],
            'freq': [100, 50, 80, 30, 20],
        })

        texts = ['テストを実装', 'テストは重要', '実装は難しい']

        df_scores = scorer.score_phrases(df_phrases, texts)

        assert len(df_scores) == len(df_phrases), "全フレーズがスコアされる"
        assert 'chi_square_score' in df_scores.columns, "カイ二乗スコア列あり"
        assert 'mutual_information' in df_scores.columns, "MI スコア列あり"
        assert 'combined_score' in df_scores.columns, "複合スコア列あり"
        assert df_scores['combined_score'].is_monotonic_decreasing, "スコアでソート"

    def test_score_phrases_empty_dataframe(self):
        """空の DataFrame"""
        scorer = StatisticalScorer()

        df_phrases = pd.DataFrame(columns=['seqchar', 'freq'])

        df_scores = scorer.score_phrases(df_phrases)

        assert len(df_scores) == 0, "空のデータフレームは空を返す"


class TestPhraseTrustMetric:
    """フレーズ信頼度メトリクス"""

    def test_calculate_trust_score_basic(self):
        """基本的な信頼度計算"""
        scorer = StatisticalScorer()
        trust_metric = PhraseTrustMetric(scorer)

        trust = trust_metric.calculate_trust_score(
            phrase='テスト',
            phrase_freq=100,
            corpus_stats={
                'total_texts': 100,
                'unique_phrases': 50,
                'total_phrases': 1000,
            }
        )

        assert 0.0 <= trust <= 1.0, "信頼度は0-1の範囲"

    def test_calculate_trust_score_with_significance(self):
        """有意なフレーズは信頼度が高い"""
        scorer = StatisticalScorer()
        trust_metric = PhraseTrustMetric(scorer)

        # 極端に有意なフレーズ（高頻度）
        trust_significant = trust_metric.calculate_trust_score(
            phrase='テスト',
            phrase_freq=900,     # 極端に高頻度 → 強く有意
            corpus_stats={
                'total_texts': 100,
                'unique_phrases': 50,
                'total_phrases': 1000,
            }
        )

        # 極端に非有意なフレーズ（低頻度）
        trust_not_significant = trust_metric.calculate_trust_score(
            phrase='の',
            phrase_freq=5,       # 極端に低頻度 → 非有意
            corpus_stats={
                'total_texts': 100,
                'unique_phrases': 50,
                'total_phrases': 1000,
            }
        )

        # 有意なフレーズの信頼度が高い
        assert trust_significant > trust_not_significant, "有意度の高いフレーズは信頼度高い"


class TestReportGeneration:
    """レポート生成のテスト"""

    def test_generate_report(self):
        """レポート生成"""
        scorer = StatisticalScorer()

        df_phrases = pd.DataFrame({
            'seqchar': ['テスト', 'テスト', '実装'],
            'freq': [100, 50, 80],
        })

        df_scores = scorer.score_phrases(df_phrases, ['テストを実装', 'テストは重要'])

        report = scorer.generate_report(df_scores, top_k=2)

        assert isinstance(report, str), "文字列を返す"
        assert '統計的有意性分析レポート' in report, "タイトルを含む"
        assert 'TOP' in report, "トップ結果を含む"
        assert '複合スコア分布' in report, "統計情報を含む"


class TestIntegration:
    """統合テスト"""

    def test_full_pipeline(self):
        """フレーズ抽出 → スコアリング → レポート の全フロー"""
        scorer = StatisticalScorer()

        # テストデータ
        df_phrases = pd.DataFrame({
            'seqchar': ['AI', 'AI', 'AI', '機械学習', '機械学習', 'テスト', 'テスト'],
            'freq': [200, 150, 100, 80, 70, 30, 20],
        })

        texts = [
            'AIと機械学習は重要',
            'AI技術は発展中',
            'テストが必要',
            'AIの性能',
        ]

        # スコアリング
        df_scores = scorer.score_phrases(df_phrases, texts)

        # 検証
        assert len(df_scores) == len(df_phrases), "全フレーズ処理"
        assert df_scores['combined_score'].sum() > 0, "スコアが計算"

        # レポート生成
        report = scorer.generate_report(df_scores, top_k=5)
        assert len(report) > 0, "レポート生成成功"
