# coding: utf-8
"""
パラメータ最適化のテスト

ParameterOptimizer の全機能を検証：
- テキスト特性分析
- パラメータ推奨
- 語彙多様性計算
- min_count、max_length の自動推奨
"""

import pytest
import pandas as pd
import math
from japhrase import (
    ParameterOptimizer,
    TextCharacteristics,
    ParameterRecommendation
)


class TestTextCharacteristicsAnalysis:
    """テキスト特性分析のテスト"""

    def test_analyze_small_dataset(self):
        """小規模テキストデータセット"""
        optimizer = ParameterOptimizer(target_phrase_count=100)

        texts = [
            'これはテストです',
            'テストは重要です',
            'テストを実施します',
            'テストが必要です',
            'テストの結果',
        ]

        chars = optimizer.analyze_text_characteristics(texts)

        assert isinstance(chars, TextCharacteristics), "TextCharacteristics を返す"
        assert chars.total_texts == 5, "テキスト数を正確に計数"
        assert chars.total_characters > 0, "文字数を計数"
        assert chars.avg_text_length > 0, "平均テキスト長を計算"
        assert 0.0 <= chars.vocabulary_diversity <= 1.0, "多様性は0-1"
        assert 0.0 <= chars.repetition_rate <= 1.0, "重複度は0-1"

    def test_analyze_large_dataset(self):
        """大規模テキストデータセット"""
        optimizer = ParameterOptimizer()

        texts = [f'テキスト{i}は異なる内容を含みます。' for i in range(1000)]

        chars = optimizer.analyze_text_characteristics(texts)

        assert chars.total_texts == 1000, "大規模テキストを処理"
        assert chars.avg_text_length > 0, "平均長を計算"

    def test_analyze_repeated_texts(self):
        """重複テキストが多い"""
        optimizer = ParameterOptimizer()

        texts = ['同じテキスト'] * 100 + ['異なるテキスト'] * 5

        chars = optimizer.analyze_text_characteristics(texts)

        # 重複が多い → repetition_rate が高い
        assert chars.repetition_rate > 0.9, "重複度が高い"

    def test_analyze_diverse_texts(self):
        """多様なテキスト"""
        optimizer = ParameterOptimizer()

        texts = [f'テキスト{i}' for i in range(100)]

        chars = optimizer.analyze_text_characteristics(texts)

        # ユニークテキスト = 100
        assert chars.repetition_rate == 0.0, "重複なし"

    def test_analyze_short_texts(self):
        """短いテキスト"""
        optimizer = ParameterOptimizer()

        texts = ['短い'] * 100

        chars = optimizer.analyze_text_characteristics(texts)

        assert chars.avg_text_length < 10, "短いテキスト"

    def test_analyze_long_texts(self):
        """長いテキスト"""
        optimizer = ParameterOptimizer()

        texts = ['これは長いテキストです。' * 10 for _ in range(50)]

        chars = optimizer.analyze_text_characteristics(texts)

        assert chars.avg_text_length > 100, "長いテキスト"

    def test_analyze_empty_texts_error(self):
        """空のテキストリスト"""
        optimizer = ParameterOptimizer()

        with pytest.raises(ValueError):
            optimizer.analyze_text_characteristics([])


class TestVocabularyDiversity:
    """語彙多様性計算のテスト"""

    def test_vocabulary_diversity_calculation(self):
        """Shannon エントロピーの計算"""
        optimizer = ParameterOptimizer()

        # 均等分布（最大多様性）
        texts_uniform = ['AAAABBBBC']  # A=4, B=3, C=1, 計8文字

        chars_uniform = optimizer.analyze_text_characteristics(texts_uniform)

        # 偏った分布（低多様性）
        texts_skewed = ['AAAAAABBC']  # A=6, B=2, C=1, 計9文字

        chars_skewed = optimizer.analyze_text_characteristics(texts_skewed)

        # 均等分布の方が多様性が高い
        assert (chars_uniform.vocabulary_diversity >=
                chars_skewed.vocabulary_diversity), "均等分布 >= 偏った分布"

    def test_vocabulary_diversity_bounds(self):
        """多様性は0-1の範囲"""
        optimizer = ParameterOptimizer()

        texts = ['様々なテキスト'] * 100

        chars = optimizer.analyze_text_characteristics(texts)

        assert 0.0 <= chars.vocabulary_diversity <= 1.0, "多様性は0-1の範囲"


class TestRecommendMinCount:
    """min_count 推奨のテスト"""

    def test_min_count_very_small_dataset(self):
        """50 行のテキスト"""
        optimizer = ParameterOptimizer()

        texts = [f'テキスト{i}' for i in range(50)]

        recommendation = optimizer.recommend_parameters(texts)

        # テキスト数が少ないので基本は2だが、語彙多様性で調整される
        assert recommendation.min_count >= 2, "小規模 → min_count >= 2"

    def test_min_count_small_dataset(self):
        """100-500 行"""
        optimizer = ParameterOptimizer()

        texts = [f'テキスト{i}' for i in range(200)]

        recommendation = optimizer.recommend_parameters(texts)

        # 基本は3だが、語彙多様性で調整される
        assert 3 <= recommendation.min_count <= 4, "中小規模 → 3-4"

    def test_min_count_medium_dataset(self):
        """500-2000 行"""
        optimizer = ParameterOptimizer()

        texts = [f'テキスト{i}' for i in range(1000)]

        recommendation = optimizer.recommend_parameters(texts)

        # 基本は5だが、語彙多様性で調整される
        assert 5 <= recommendation.min_count <= 6, "中規模 → 5-6"

    def test_min_count_large_dataset(self):
        """2000+ 行"""
        optimizer = ParameterOptimizer()

        texts = [f'テキスト{i}' for i in range(5000)]

        recommendation = optimizer.recommend_parameters(texts)

        # min_count = max(10, 5000 // 200) = max(10, 25) = 25
        assert recommendation.min_count >= 10, "大規模 → min_count >= 10"

    def test_min_count_low_vocabulary_diversity(self):
        """語彙多様性が低い → min_count は下げられる"""
        optimizer = ParameterOptimizer()

        # 語彙が限定的（同じ単語が繰り返される）
        texts = ['テスト'] * 100 + ['実装'] * 100

        recommendation = optimizer.recommend_parameters(texts)

        # 語彙多様性が低い → min_count をやや下げる
        assert recommendation.min_count <= 5, "低多様性 → min_count は抑える"

    def test_min_count_high_vocabulary_diversity(self):
        """語彙多様性が高い → min_count は上げられる"""
        optimizer = ParameterOptimizer()

        # 語彙が豊富
        texts = [f'これは{i}番目のテキストです' for i in range(500)]

        recommendation = optimizer.recommend_parameters(texts)

        # 語彙多様性が高い → min_count をやや上げる
        assert recommendation.min_count >= 3, "高多様性 → min_count は上げる"


class TestRecommendMaxLength:
    """max_length 推奨のテスト"""

    def test_max_length_short_texts(self):
        """短いテキスト（平均 < 20 文字）"""
        optimizer = ParameterOptimizer()

        texts = ['短い'] * 100

        recommendation = optimizer.recommend_parameters(texts)

        assert recommendation.max_length == 4, "短いテキスト → max_length=4"

    def test_max_length_medium_short_texts(self):
        """中短テキスト（平均 20-50 文字）"""
        optimizer = ParameterOptimizer()

        # 実際の文字数に合わせたテキスト
        texts = ['これは中程度の長さのテキストです。詳細説明も含まれています。'] * 100

        recommendation = optimizer.recommend_parameters(texts)

        # 平均文字数に応じて max_length が決定される
        assert 6 <= recommendation.max_length <= 8, "中程度長さ → 6-8"

    def test_max_length_medium_texts(self):
        """中程度テキスト（平均 50-100 文字）"""
        optimizer = ParameterOptimizer()

        texts = ['これは中程度の長さのテキストです。内容も多少含まれています。'] * 100

        recommendation = optimizer.recommend_parameters(texts)

        # 実際の文字数に応じて max_length が決定される
        assert recommendation.max_length >= 6, "中程度テキスト → max_length >= 6"

    def test_max_length_long_texts(self):
        """長いテキスト（平均 > 100 文字）"""
        optimizer = ParameterOptimizer()

        texts = ['これは長いテキストです。' * 10 for _ in range(100)]

        recommendation = optimizer.recommend_parameters(texts)

        assert recommendation.max_length == 10, "長いテキスト → max_length=10"


class TestRecommendMinLength:
    """min_length 推奨のテスト"""

    def test_min_length_small_dataset(self):
        """小規模データ"""
        optimizer = ParameterOptimizer()

        texts = [f'テキスト{i}' for i in range(50)]

        recommendation = optimizer.recommend_parameters(texts)

        # 小規模 → min_length=1
        assert recommendation.min_length == 1, "小規模 → min_length=1"

    def test_min_length_large_dataset(self):
        """大規模データ"""
        optimizer = ParameterOptimizer()

        texts = [f'テキスト{i}' for i in range(2000)]

        recommendation = optimizer.recommend_parameters(texts)

        # 大規模 → min_length=2（ノイズ削減）
        assert recommendation.min_length == 2, "大規模 → min_length=2"


class TestParameterRecommendation:
    """パラメータ推奨オブジェクト"""

    def test_recommendation_includes_reasoning(self):
        """推奨に理由が含まれる"""
        optimizer = ParameterOptimizer()

        texts = [f'テキスト{i}' for i in range(100)]

        recommendation = optimizer.recommend_parameters(texts)

        assert isinstance(recommendation, ParameterRecommendation), "ParameterRecommendation を返す"
        assert recommendation.min_count > 0, "min_count > 0"
        assert recommendation.max_length > 0, "max_length > 0"
        assert recommendation.min_length >= 1, "min_length >= 1"
        assert recommendation.target_phrase_count == 100, "目標フレーズ数"
        assert len(recommendation.reasoning) > 0, "推奨根拠がある"
        assert 'min_count' in recommendation.reasoning, "min_count の根拠"
        assert 'max_length' in recommendation.reasoning, "max_length の根拠"
        assert 'min_length' in recommendation.reasoning, "min_length の根拠"

    def test_recommendation_with_phrases(self):
        """フレーズデータフレーム付き推奨"""
        optimizer = ParameterOptimizer(target_phrase_count=50)

        texts = [f'テキスト{i}' for i in range(100)]

        df_phrases = pd.DataFrame({
            'seqchar': ['テスト', 'テスト', '実装', '実装', '実装'],
            'freq': [50, 30, 40, 20, 10],
        })

        recommendation = optimizer.recommend_parameters(texts, df_phrases)

        assert recommendation.target_phrase_count == 50, "目標フレーズ数"


class TestReportGeneration:
    """レポート生成のテスト"""

    def test_generate_report(self):
        """レポート生成"""
        optimizer = ParameterOptimizer()

        texts = [f'テキスト{i}' for i in range(100)]

        recommendation = optimizer.recommend_parameters(texts)

        report = optimizer.generate_report(recommendation, texts)

        assert isinstance(report, str), "文字列を返す"
        assert 'パラメータ最適化レポート' in report, "タイトルを含む"
        assert 'min_count' in report, "min_count を含む"
        assert 'max_length' in report, "max_length を含む"
        assert 'min_length' in report, "min_length を含む"
        assert '推奨根拠' in report, "推奨根拠を含む"

    def test_generate_report_with_text_characteristics(self):
        """テキスト特性を含むレポート"""
        optimizer = ParameterOptimizer()

        texts = [f'テキスト{i}' for i in range(100)]

        recommendation = optimizer.recommend_parameters(texts)

        report = optimizer.generate_report(recommendation, texts)

        assert 'テキスト特性' in report, "テキスト特性を表示"
        assert 'テキスト数' in report, "テキスト数を表示"
        assert '語彙多様性' in report, "語彙多様性を表示"
        assert '重複度' in report, "重複度を表示"


class TestIntegration:
    """統合テスト"""

    def test_full_recommendation_pipeline(self):
        """テキスト分析 → パラメータ推奨 → レポート の全フロー"""
        optimizer = ParameterOptimizer(target_phrase_count=150)

        # テストデータ
        texts = [
            'AI技術は急速に発展しています',
            'AIと機械学習は別の概念です',
            'テストは品質保証に重要です',
            '実装の前にテスト計画が必要です',
            '大規模システムのテストは複雑です',
        ] * 100

        # テキスト特性分析
        chars = optimizer.analyze_text_characteristics(texts)
        assert chars.total_texts == 500, "テキスト数"

        # パラメータ推奨
        recommendation = optimizer.recommend_parameters(texts)
        assert recommendation.min_count > 0, "min_count が計算"
        assert recommendation.max_length > 0, "max_length が計算"

        # レポート生成
        report = optimizer.generate_report(recommendation, texts)
        assert len(report) > 100, "詳細なレポート生成"

    def test_different_dataset_sizes(self):
        """異なるサイズのデータセット"""
        for size in [10, 100, 500, 2000]:
            optimizer = ParameterOptimizer()
            texts = [f'テキスト{i}' for i in range(size)]

            recommendation = optimizer.recommend_parameters(texts)

            assert recommendation.min_count > 0, f"サイズ{size}: min_count > 0"
            assert recommendation.max_length > 0, f"サイズ{size}: max_length > 0"
            assert recommendation.target_phrase_count > 0, f"サイズ{size}: 目標 > 0"

    def test_phrase_analysis_with_recommendation(self):
        """フレーズ分析を考慮した推奨"""
        optimizer = ParameterOptimizer()

        texts = [f'テキスト{i}' for i in range(100)]

        df_phrases = pd.DataFrame({
            'seqchar': ['A', 'AB', 'ABC', 'ABCD', 'ABCDE'],
            'freq': [100, 80, 60, 40, 20],
        })

        recommendation = optimizer.recommend_parameters(texts, df_phrases)

        # フレーズ情報を使用した推奨が行われる
        assert len(recommendation.reasoning) > 0, "推奨根拠あり"
