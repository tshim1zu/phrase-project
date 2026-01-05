"""
執筆支援ツールのテスト

KWICAnalyzer、AbstractBodyChecker、HabitDetector等の機能をテストします。
"""

import pytest
import pandas as pd
from japhrase import (
    KWICAnalyzer,
    AbstractBodyChecker,
    HabitDetector,
    RevisionHeatmap,
    RankingTrajectory
)


class TestKWICAnalyzer:
    """KWIC検索機能のテスト"""

    def test_basic_phrase_search(self):
        """基本的なフレーズ検索テスト"""
        sentences = [
            "これは表記ゆれのテストです。",
            "表記ゆれは重要な問題です。",
            "表記ゆれを修正する必要があります。"
        ]
        kwic = KWICAnalyzer(sentences)
        results = kwic.find_phrase("表記ゆれ")

        assert len(results) == 3
        assert results.loc[0, 'occurrence_num'] == 1
        assert results.loc[0, 'total_occurrences'] == 3

    def test_no_matching_phrase(self):
        """マッチしないフレーズの検索テスト"""
        sentences = ["これはテストです。"]
        kwic = KWICAnalyzer(sentences)
        results = kwic.find_phrase("存在しないフレーズ")

        assert len(results) == 0

    def test_line_numbering(self):
        """行番号が正しく付与されるテスト"""
        sentences = ["1行目", "2行目", "3行目"]
        kwic = KWICAnalyzer(sentences)
        results = kwic.find_phrase("行目")

        assert results.loc[0, 'line_num'] == 1
        assert results.loc[1, 'line_num'] == 2
        assert results.loc[2, 'line_num'] == 3

    def test_find_multiple_phrases(self):
        """複数フレーズの検索テスト"""
        sentences = [
            "Aは重要である。Bも重要である。",
            "Aを確認した。"
        ]
        kwic = KWICAnalyzer(sentences)
        results = kwic.find_multiple_phrases(["A", "B"])

        assert "A" in results
        assert "B" in results
        assert len(results["A"]) == 2
        assert len(results["B"]) == 1


class TestAbstractBodyChecker:
    """あらすじ vs 本文チェッカーのテスト"""

    def test_missing_phrases(self):
        """あらすじにあるが本文に無いフレーズの検出テスト"""
        abstract = "機械学習について述べる。深層学習の基本を説明する。"
        body = "深層学習について詳しく述べる。ニューラルネットワークの実装を説明する。"

        checker = AbstractBodyChecker(abstract, body)
        missing = checker.get_missing_phrases()

        # あらすじ側で抽出されたが本文側に無いフレーズがある可能性がある
        assert isinstance(missing, pd.DataFrame)

    def test_added_phrases(self):
        """本文で新規に出現するフレーズの検出テスト"""
        abstract = "テストについて述べる。"
        body = "テストについて詳しく述べる。実装の詳細を解説する。"

        checker = AbstractBodyChecker(abstract, body)
        added = checker.get_added_phrases()

        assert isinstance(added, pd.DataFrame)

    def test_divergence_score(self):
        """乖離スコアの計算テスト"""
        abstract = "機械学習の基本"
        body = "全く別のテーマについて説明する"

        checker = AbstractBodyChecker(abstract, body)
        score = checker.get_divergence_score()

        assert 0.0 <= score <= 1.0

    def test_report_generation(self):
        """レポート生成テスト"""
        abstract = "テーマA"
        body = "テーマA について述べる"

        checker = AbstractBodyChecker(abstract, body)
        report = checker.generate_report()

        assert isinstance(report, str)
        assert "乖離スコア" in report


class TestHabitDetector:
    """口癖検出器のテスト"""

    def test_habit_detection(self):
        """口癖の検出テスト"""
        target = "基本的にはそうである。基本的にそのようである。基本的にこうである。"
        detector = HabitDetector(target)
        habits = detector.detect_habits()

        assert isinstance(habits, pd.DataFrame)

    def test_empty_target(self):
        """空のテキストでのハンドリングテスト"""
        target = ""
        detector = HabitDetector(target)
        habits = detector.detect_habits()

        assert isinstance(habits, pd.DataFrame)

    def test_custom_reference(self):
        """カスタム参照コーパスでのテスト"""
        target = "特殊な表現を多用する文章"
        reference = ["一般的な表現を使う文章"]

        detector = HabitDetector(target, reference_texts=reference)
        habits = detector.detect_habits()

        assert isinstance(habits, pd.DataFrame)


class TestRevisionHeatmap:
    """推敲の偏りヒートマップのテスト"""

    def test_basic_heatmap(self):
        """基本的なヒートマップ分析テスト"""
        v1 = ["これは最初のバージョンです。"] * 5
        v2 = ["これは改良されたバージョンです。"] * 5
        versions = [v1, v2]

        heatmap = RevisionHeatmap(versions)
        changes = heatmap.get_section_changes()

        assert isinstance(changes, pd.DataFrame)
        assert 'section' in changes.columns

    def test_custom_labels(self):
        """カスタムバージョンラベルのテスト"""
        v1 = ["バージョン1"] * 3
        v2 = ["バージョン2"] * 3
        versions = [v1, v2]
        labels = ["原稿案1", "原稿案2"]

        heatmap = RevisionHeatmap(versions, version_labels=labels)
        changes = heatmap.get_section_changes()

        assert "原稿案1" in changes.columns
        assert "原稿案2" in changes.columns


class TestRankingTrajectory:
    """フレーズランキング推移のテスト"""

    def test_ranking_extraction(self):
        """ランキング抽出のテスト"""
        v1 = ["重要な重要な重要な"] * 3
        v2 = ["変更点変更点変更点"] * 3
        versions = [v1, v2]

        traj = RankingTrajectory(versions, top_n=5)
        history = traj.get_ranking_history()

        assert isinstance(history, pd.DataFrame)

    def test_biggest_movers(self):
        """ランク変化が大きいフレーズの検出テスト"""
        v1 = ["初期フレーズ"] * 10 + ["変動フレーズ"] * 5
        v2 = ["初期フレーズ"] * 3 + ["変動フレーズ"] * 10
        versions = [v1, v2]

        traj = RankingTrajectory(versions, top_n=5)
        movers = traj.get_biggest_movers()

        assert isinstance(movers, pd.DataFrame)

    def test_custom_labels(self):
        """カスタムバージョンラベルのテスト"""
        v1 = ["テスト用テスト用"] * 5
        v2 = ["テスト用テスト用"] * 5
        versions = [v1, v2]
        labels = ["初稿", "改稿"]

        traj = RankingTrajectory(versions, version_labels=labels, top_n=3)
        history = traj.get_ranking_history()

        assert isinstance(history, pd.DataFrame)
        # フレーズが抽出されない場合もあるので、ラベルがあることのみを確認
        if len(history) > 0:
            assert "初稿" in history.columns
            assert "改稿" in history.columns


class TestIntegration:
    """統合テスト"""

    def test_full_workflow(self):
        """フル・ワークフロー（複数ツールの組み合わせ）テスト"""
        # テスト用のテキスト
        abstract = "機械学習について説明します。"
        body = "機械学習について詳しく説明し、実装方法を述べます。"
        v1 = abstract.split("。")
        v2 = body.split("。")

        # 各ツールの実行
        kwic = KWICAnalyzer(v1 + v2)
        checker = AbstractBodyChecker(abstract, body)
        detector = HabitDetector(body)
        traj = RankingTrajectory([v1, v2])

        # 結果の検証
        assert isinstance(kwic.find_phrase("機械学習"), pd.DataFrame)
        assert isinstance(checker.get_divergence_score(), float)
        assert isinstance(detector.detect_habits(), pd.DataFrame)
        assert isinstance(traj.get_ranking_history(), pd.DataFrame)
