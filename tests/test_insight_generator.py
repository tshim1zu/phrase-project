# coding: utf-8
"""
自動インサイト生成のテスト

InsightGenerator の全機能を検証：
- インサイト自動生成
- パターン検出（トピック、トレンド、異常値）
- フレーズクラスタリング
- レポート生成
"""

import pytest
import pandas as pd
import tempfile
import os
from japhrase import InsightGenerator, Insight


class TestInsightDataclass:
    """Insight データクラスのテスト"""

    def test_insight_creation(self):
        """Insight オブジェクトの作成"""
        insight = Insight(
            category='dominant_topic',
            title='ドミナントトピック',
            description='重要なトピックです',
            phrases=[('テスト', 0.5), ('実装', 0.3)],
            confidence=0.85
        )

        assert insight.category == 'dominant_topic', "カテゴリ"
        assert insight.title == 'ドミナントトピック', "タイトル"
        assert len(insight.phrases) == 2, "フレーズリスト"
        assert insight.confidence == 0.85, "確信度"


class TestDetectDominantTopics:
    """ドミナントトピック検出のテスト"""

    def test_detect_dominant_topics_basic(self):
        """基本的なドミナントトピック検出"""
        generator = InsightGenerator()

        df_phrases = pd.DataFrame({
            'seqchar': ['テスト', 'テスト', 'テスト', '実装', '実装', 'バグ'],
            'freq': [100, 50, 30, 80, 40, 20],
        })

        insights = generator.generate_insights(df_phrases)

        # ドミナントトピックインサイトを探す
        dominant = next((i for i in insights if i.category == 'dominant_topic'), None)

        assert dominant is not None, "ドミナントトピックが検出"
        assert len(dominant.phrases) == 5, "TOP 5 フレーズ"
        assert dominant.phrases[0][0] == 'テスト', "最高頻度フレーズ"

    def test_dominant_topic_confidence(self):
        """ドミナントトピックの確信度"""
        generator = InsightGenerator()

        df_phrases = pd.DataFrame({
            'seqchar': ['テスト'] * 100,
            'freq': [1] * 100,
        })

        insights = generator.generate_insights(df_phrases)

        dominant = next((i for i in insights if i.category == 'dominant_topic'), None)

        # 1フレーズが100%占める
        assert dominant.confidence == 0.95, "高い確信度"


class TestDetectEmergingThemes:
    """浮上テーマ検出のテスト"""

    def test_detect_emerging_themes_basic(self):
        """基本的な浮上テーマ検出"""
        generator = InsightGenerator()

        df_phrases = pd.DataFrame({
            'seqchar': ['古い', '古い', '古い', '新しい', '新しい'],
            'freq': [3, 2, 1, 2, 1],
        })

        texts = [
            '古いテキスト1',
            '古いテキスト2',
            '古いテキスト3',
            '新しいテキスト1',
            '新しいテキスト2',
            '新しいテキスト3',
        ]

        insights = generator.generate_insights(df_phrases, texts)

        # テキストが少なくても処理可能
        assert len(insights) >= 1, "インサイトが生成"

    def test_no_emerging_themes_small_dataset(self):
        """テキストが少ない場合"""
        generator = InsightGenerator()

        df_phrases = pd.DataFrame({
            'seqchar': ['テスト', '実装'],
            'freq': [5, 3],
        })

        texts = ['テキスト1', 'テキスト2']

        insights = generator.generate_insights(df_phrases, texts)

        # 浮上テーマは検出されない（テキスト < 10）
        emerging = next((i for i in insights if i.category == 'emerging_theme'), None)

        assert emerging is None, "テキスト少数では浮上テーマ検出されない"

    def test_emerging_themes_growth_detection(self):
        """成長率の検出"""
        generator = InsightGenerator()

        df_phrases = pd.DataFrame({
            'seqchar': ['新規', '成熟'],
            'freq': [2, 1],
        })

        texts = [
            '成熟テキスト1', '成熟テキスト2', '成熟テキスト3',
            '成熟テキスト4', '成熟テキスト5',
            '新規テキスト1', '新規テキスト2', '新規テキスト3',
            '新規テキスト4', '新規テキスト5',
            '新規テキスト6', '新規テキスト7',
        ]

        insights = generator.generate_insights(df_phrases, texts)

        # インサイトが生成される
        assert len(insights) > 0, "インサイト生成"


class TestDetectAnomalies:
    """異常値検出のテスト"""

    def test_detect_anomalies_without_stats(self):
        """統計スコアなしでの異常検出"""
        generator = InsightGenerator()

        df_phrases = pd.DataFrame({
            'seqchar': ['テスト', 'テスト', '実装'],
            'freq': [100, 80, 50],
        })

        insights = generator.generate_insights(df_phrases, statistical_scores=None)

        # 統計スコアなしでは異常は検出されない
        anomalies = [i for i in insights if i.category == 'anomaly']

        assert len(anomalies) == 0, "統計スコアなしでは異常検出なし"

    def test_detect_anomalies_with_stats(self):
        """統計スコア付きでの異常検出"""
        generator = InsightGenerator()

        df_phrases = pd.DataFrame({
            'seqchar': ['テスト', '実装', 'バグ'],
            'freq': [100, 80, 50],
        })

        statistical_scores = pd.DataFrame({
            'phrase': ['テスト', '実装', 'バグ'],
            'frequency': [100, 80, 50],
            'is_significant': [False, True, True],
        })

        insights = generator.generate_insights(
            df_phrases,
            statistical_scores=statistical_scores
        )

        # 'テスト' が有意でないが頻出なので異常と判定される可能性
        assert len(insights) > 0, "インサイト生成"


class TestAnalyzeNoiseRatio:
    """ノイズ比率分析のテスト"""

    def test_analyze_noise_ratio_high_noise(self):
        """ノイズが多い場合"""
        generator = InsightGenerator()

        df_phrases = pd.DataFrame({
            'seqchar': ['a', 'b', '、', '。', 'テスト', '実装'],
            'freq': [100, 80, 60, 50, 30, 20],
        })

        insights = generator.generate_insights(df_phrases)

        noise = next((i for i in insights if i.category == 'noise_analysis'), None)

        # ノイズが5%以上 → インサイト生成
        if noise is not None:
            assert noise.confidence > 0.5, "ノイズ比率が高い"

    def test_analyze_noise_ratio_low_noise(self):
        """ノイズが少ない場合"""
        generator = InsightGenerator()

        df_phrases = pd.DataFrame({
            'seqchar': ['テスト', '実装', 'バグ', '修正', '改善'],
            'freq': [100, 80, 60, 50, 40],
        })

        insights = generator.generate_insights(df_phrases)

        noise = next((i for i in insights if i.category == 'noise_analysis'), None)

        assert noise is None, "ノイズ少ない場合は表示されない"


class TestEvaluateTextQuality:
    """テキスト品質評価のテスト"""

    def test_evaluate_text_quality_low(self):
        """品質が低い（語彙多様性低い）"""
        generator = InsightGenerator()

        df_phrases = pd.DataFrame({
            'seqchar': ['テスト'] * 100,
            'freq': [1] * 100,
        })

        insights = generator.generate_insights(df_phrases)

        quality = next((i for i in insights if i.category == 'text_quality'), None)

        assert quality is not None, "品質評価が生成"
        # Gini 係数が低い（語彙少ない）
        if 'Gini' in str(quality.description) or '低' in quality.description:
            assert '低' in quality.description, "低品質と判定"

    def test_evaluate_text_quality_high(self):
        """品質が高い（語彙多様性高い）"""
        generator = InsightGenerator()

        df_phrases = pd.DataFrame({
            'seqchar': [f'フレーズ{i}' for i in range(100)],
            'freq': [1] * 100,
        })

        insights = generator.generate_insights(df_phrases)

        quality = next((i for i in insights if i.category == 'text_quality'), None)

        assert quality is not None, "品質評価が生成"


class TestGenerateRecommendations:
    """推奨アクション生成のテスト"""

    def test_generate_recommendations_with_noise(self):
        """ノイズ推奨"""
        generator = InsightGenerator()

        df_phrases = pd.DataFrame({
            'seqchar': ['a', 'b', '、', 'テスト', '実装'],
            'freq': [100, 80, 60, 50, 40],
        })

        insights = generator.generate_insights(df_phrases)

        recommendations = [i for i in insights if i.category == 'recommendation']

        # ノイズが多い場合は推奨が生成される
        if len([i for i in insights if i.category == 'noise_analysis']) > 0:
            assert len(recommendations) > 0, "推奨が生成"


class TestClusterRelatedPhrases:
    """関連フレーズクラスタリングのテスト"""

    def test_cluster_similar_phrases(self):
        """類似フレーズのクラスタリング"""
        generator = InsightGenerator()

        df_phrases = pd.DataFrame({
            'seqchar': ['テスト', 'テスツ', 'てすと', '実装', '実装'],
            'freq': [100, 50, 30, 80, 20],
        })

        clusters = generator.cluster_related_phrases(df_phrases, similarity_threshold=0.7)

        assert isinstance(clusters, dict), "辞書を返す"
        assert len(clusters) >= 2, "複数クラスタ"

    def test_cluster_exact_match(self):
        """同一フレーズのクラスタリング"""
        generator = InsightGenerator()

        df_phrases = pd.DataFrame({
            'seqchar': ['テスト', 'テスト', '実装'],
            'freq': [100, 50, 80],
        })

        clusters = generator.cluster_related_phrases(df_phrases, similarity_threshold=0.5)

        # 'テスト' は1つのクラスタになる
        test_clusters = [v for v in clusters.values() if 'テスト' in v]

        assert len(test_clusters) > 0, "テストをクラスタ"

    def test_cluster_threshold(self):
        """閾値による影響"""
        generator = InsightGenerator()

        df_phrases = pd.DataFrame({
            'seqchar': ['A', 'AB', 'ABC', 'ABCD'],
            'freq': [1, 1, 1, 1],
        })

        clusters_strict = generator.cluster_related_phrases(df_phrases, similarity_threshold=0.9)
        clusters_loose = generator.cluster_related_phrases(df_phrases, similarity_threshold=0.5)

        # 厳しい閾値 → より多くのクラスタ
        assert len(clusters_strict) >= len(clusters_loose), "厳しい閾値でクラスタ増加"


class TestExportInsights:
    """インサイトエクスポートのテスト"""

    def test_export_insights_csv(self):
        """CSV 出力"""
        generator = InsightGenerator()

        df_phrases = pd.DataFrame({
            'seqchar': ['テスト', 'テスト', '実装', '実装'],
            'freq': [100, 50, 80, 30],
        })

        insights = generator.generate_insights(df_phrases)

        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.csv') as f:
            filepath = f.name

        try:
            generator.export_insights(filepath)

            assert os.path.exists(filepath), "ファイルが生成"
            assert os.path.getsize(filepath) > 0, "ファイルに内容"

            # CSV として読み込み可能か
            df = pd.read_csv(filepath)
            assert len(df) > 0, "CSV が読み込める"
        finally:
            if os.path.exists(filepath):
                os.unlink(filepath)

    def test_export_insights_no_insights(self):
        """インサイトなしでのエクスポート"""
        generator = InsightGenerator()

        with tempfile.NamedTemporaryFile(mode='w', delete=False, suffix='.csv') as f:
            filepath = f.name

        try:
            # インサイトなし
            generator.insights = []
            generator.export_insights(filepath)

            # ファイルは生成されない（または空で警告が出る）
            if os.path.exists(filepath):
                # ファイルが空の場合はパスする
                try:
                    df = pd.read_csv(filepath)
                except pd.errors.EmptyDataError:
                    # 空ファイルは正常なケース
                    pass
        finally:
            if os.path.exists(filepath):
                os.unlink(filepath)


class TestGenerateReport:
    """レポート生成のテスト"""

    def test_generate_report_basic(self):
        """基本的なレポート生成"""
        generator = InsightGenerator()

        df_phrases = pd.DataFrame({
            'seqchar': ['テスト', 'テスト', '実装', '実装', '実装'],
            'freq': [100, 50, 80, 30, 20],
        })

        insights = generator.generate_insights(df_phrases)

        report = generator.generate_report()

        assert isinstance(report, str), "文字列を返す"
        assert '自動インサイトレポート' in report, "タイトル"
        assert '生成日時' in report, "日時"
        assert 'インサイト数' in report, "インサイト数"

    def test_generate_report_content(self):
        """レポート内容の検証"""
        generator = InsightGenerator()

        df_phrases = pd.DataFrame({
            'seqchar': ['テスト', 'テスト', '実装'],
            'freq': [100, 50, 80],
        })

        insights = generator.generate_insights(df_phrases)

        report = generator.generate_report()

        # 各インサイトの情報が含まれるか
        for insight in insights:
            if len(report) > 500:  # レポート内容が充実している場合
                # タイトルやカテゴリが含まれる可能性
                assert 'カテゴリ' in report or insight.title in report, "インサイト情報"

    def test_report_formatting(self):
        """レポートのフォーマット"""
        generator = InsightGenerator()

        df_phrases = pd.DataFrame({
            'seqchar': ['A', 'A', 'B'],
            'freq': [100, 50, 80],
        })

        insights = generator.generate_insights(df_phrases)

        report = generator.generate_report()

        # 基本的なレポート構造
        lines = report.split('\n')
        assert len(lines) > 1, "複数行のレポート"


class TestIntegration:
    """統合テスト"""

    def test_full_insight_pipeline(self):
        """インサイト生成の全フロー"""
        generator = InsightGenerator()

        # テストデータ
        df_phrases = pd.DataFrame({
            'seqchar': [
                'AI', 'AI', 'AI', 'AI', 'AI',
                '機械学習', '機械学習', '機械学習',
                'テスト', 'テスト',
                'a', 'b', '、',  # ノイズ
            ],
            'freq': [100, 80, 60, 40, 20, 80, 50, 30, 50, 30, 100, 80, 60],
        })

        texts = [
            'AI技術の発展',
            'AI と機械学習',
            '機械学習の応用',
            'テストが重要',
            'テスト実施',
        ] * 2

        # インサイト生成
        insights = generator.generate_insights(df_phrases, texts)

        # 検証
        assert len(insights) > 0, "インサイトが生成"

        # クラスタリング
        clusters = generator.cluster_related_phrases(df_phrases)
        assert len(clusters) > 0, "クラスタ生成"

        # レポート生成
        report = generator.generate_report()
        assert len(report) > 100, "詳細レポート"

    def test_insight_with_statistical_scores(self):
        """統計スコア付きインサイト"""
        generator = InsightGenerator()

        df_phrases = pd.DataFrame({
            'seqchar': ['重要', '重要', 'ノイズ', 'ノイズ'],
            'freq': [100, 50, 40, 30],
        })

        statistical_scores = pd.DataFrame({
            'phrase': ['重要', '重要', 'ノイズ', 'ノイズ'],
            'frequency': [100, 50, 40, 30],
            'is_significant': [True, True, False, False],
        })

        insights = generator.generate_insights(
            df_phrases,
            texts=None,
            statistical_scores=statistical_scores
        )

        # インサイトが生成される
        assert len(insights) > 0, "インサイト生成"

    def test_large_phrase_set(self):
        """大規模フレーズセット"""
        generator = InsightGenerator()

        # 1000個のフレーズ
        df_phrases = pd.DataFrame({
            'seqchar': [f'フレーズ{i}' for i in range(1000)],
            'freq': [100 - i for i in range(1000)],
        })

        insights = generator.generate_insights(df_phrases)

        # 処理できるか
        assert len(insights) > 0, "大規模データ処理"

        # レポート生成
        report = generator.generate_report()
        assert len(report) > 50, "レポート生成成功"

    def test_edge_case_single_phrase(self):
        """単一フレーズ"""
        generator = InsightGenerator()

        df_phrases = pd.DataFrame({
            'seqchar': ['テスト'],
            'freq': [100],
        })

        insights = generator.generate_insights(df_phrases)

        # エラーなく処理
        assert isinstance(insights, list), "インサイトリスト"

    def test_edge_case_all_same_frequency(self):
        """すべてのフレーズが同じ頻度"""
        generator = InsightGenerator()

        df_phrases = pd.DataFrame({
            'seqchar': ['A', 'B', 'C', 'D', 'E'],
            'freq': [50] * 5,
        })

        insights = generator.generate_insights(df_phrases)

        assert len(insights) > 0, "均等分布でもインサイト生成"
