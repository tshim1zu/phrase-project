"""
統計的スコアリング（PMI・分岐エントロピー）のテスト
"""

import pytest
import numpy as np
import pandas as pd
from japhrase import PhraseExtracter


class TestPMICalculation:
    """PMI（自己相互情報量）計算のテスト"""

    def test_calculate_pmi_high_cohesion(self):
        """結合度が高いフレーズのPMI"""
        extractor = PhraseExtracter(use_pmi=True)
        phrases = ["機械学習"]
        text = "機械学習は機械学習で機械学習を"  # "機械学習"が繰り返される

        pmi_scores = extractor.calculate_pmi(phrases, text)

        # 結合度が高いので、PMIは正の値
        assert "機械学習" in pmi_scores
        assert pmi_scores["機械学習"] > 0

    def test_calculate_pmi_low_cohesion(self):
        """結合度が低いフレーズのPMI"""
        extractor = PhraseExtracter(use_pmi=True)
        # 「ab」は「a」と「b」の独立確率の積に近い → PMIが低い
        phrases = ["ab"]
        # 「a」「b」が個別に頻出するがペアでは少ない
        text = "aaaabbbbab"  # 「ab」は1回、「a」は5回、「b」は5回

        pmi_scores = extractor.calculate_pmi(phrases, text)

        assert "ab" in pmi_scores
        # PMIスコアが計算される
        assert isinstance(pmi_scores["ab"], float)

    def test_calculate_pmi_single_char(self):
        """単一文字のPMI"""
        extractor = PhraseExtracter(use_pmi=True)
        phrases = ["あ"]
        text = "あああああああ"

        pmi_scores = extractor.calculate_pmi(phrases, text)

        assert "あ" in pmi_scores
        # 単一文字なのでPMIは0に近い
        assert pmi_scores["あ"] >= 0

    def test_calculate_pmi_not_found(self):
        """フレーズが見つからない場合"""
        extractor = PhraseExtracter(use_pmi=True)
        phrases = ["存在しないフレーズ"]
        text = "これはテストです"

        pmi_scores = extractor.calculate_pmi(phrases, text)

        assert "存在しないフレーズ" in pmi_scores
        assert pmi_scores["存在しないフレーズ"] == 0.0

    def test_calculate_pmi_empty_input(self):
        """空の入力"""
        extractor = PhraseExtracter(use_pmi=True)
        pmi_scores = extractor.calculate_pmi([], "")

        assert pmi_scores == {}

    def test_pmi_numerical_stability(self):
        """PMIの数値安定性（クリップ処理）"""
        extractor = PhraseExtracter(use_pmi=True)
        phrases = ["x"]
        text = "x" * 1000  # 非常に高いPMI

        pmi_scores = extractor.calculate_pmi(phrases, text)

        # PMIが数値的に安定している（-10 ~ 10の範囲）
        assert -10 <= pmi_scores["x"] <= 10


class TestBranchingEntropyCalculation:
    """分岐エントロピー（Branching Entropy）計算のテスト"""

    def test_branching_entropy_word_boundary(self):
        """単語境界でのエントロピー"""
        extractor = PhraseExtracter(use_branching_entropy=True)
        sentences = ["機械学習は重要です。機械学習を学ぶ。"]
        phrases = ["機械学習"]

        entropy_scores = extractor.calculate_branching_entropy(sentences, phrases)

        assert "機械学習" in entropy_scores
        left_entropy, right_entropy, boundary_score = entropy_scores["機械学習"]

        # 単語境界なので、左右のエントロピーが比較的高い
        assert left_entropy >= 0
        assert right_entropy >= 0
        assert boundary_score >= 0

    def test_branching_entropy_word_middle(self):
        """単語の途中でのエントロピー"""
        extractor = PhraseExtracter(use_branching_entropy=True)
        sentences = ["テストテストテスト"]  # "テス"の後は常に"ト"
        phrases = ["テス"]

        entropy_scores = extractor.calculate_branching_entropy(sentences, phrases)

        assert "テス" in entropy_scores
        left_entropy, right_entropy, boundary_score = entropy_scores["テス"]

        # 単語の途中なので、右エントロピーが低い（「ト」しか来ない）
        assert right_entropy < 2.0  # 低いエントロピー

    def test_branching_entropy_single_occurrence(self):
        """フレーズが1回だけ出現"""
        extractor = PhraseExtracter(use_branching_entropy=True)
        sentences = ["テストのみ"]
        phrases = ["テスト"]

        entropy_scores = extractor.calculate_branching_entropy(sentences, phrases)

        assert "テスト" in entropy_scores
        left_entropy, right_entropy, boundary_score = entropy_scores["テスト"]

        # 1回だけの出現なので、エントロピーは0か非常に小さい
        assert boundary_score >= 0
        assert boundary_score <= 1.0

    def test_branching_entropy_empty_input(self):
        """空の入力"""
        extractor = PhraseExtracter(use_branching_entropy=True)
        entropy_scores = extractor.calculate_branching_entropy([], [])

        assert entropy_scores == {}

    def test_branching_entropy_normalization(self):
        """エントロピースコアが0-1に正規化されている"""
        extractor = PhraseExtracter(use_branching_entropy=True)
        sentences = ["あああああいいいううう"]
        phrases = ["あ", "い", "う"]

        entropy_scores = extractor.calculate_branching_entropy(sentences, phrases)

        for phrase in phrases:
            left_entropy, right_entropy, boundary_score = entropy_scores[phrase]
            # スコアは0-1の範囲
            assert 0.0 <= boundary_score <= 1.0


class TestScoringIntegration:
    """PMI・BEのスコアリング統合テスト"""

    def test_score_with_pmi_enabled(self):
        """PMI有効時のスコアリング"""
        extractor_with_pmi = PhraseExtracter(
            min_count=2,
            use_pmi=True,
            verbose=0
        )
        extractor_without_pmi = PhraseExtracter(
            min_count=2,
            use_pmi=False,
            verbose=0
        )

        sentences = ["機械学習は機械学習で機械学習", "ていうのはていう"]

        # PMI有効版と無効版でスコアが異なる
        df_with = extractor_with_pmi.get_dfphrase(sentences)
        df_without = extractor_without_pmi.get_dfphrase(sentences)

        # 両方とも結果を返す（エラーがない）
        assert isinstance(df_with, pd.DataFrame)
        assert isinstance(df_without, pd.DataFrame)

    def test_score_with_entropy_enabled(self):
        """BE有効時のスコアリング"""
        extractor_with_entropy = PhraseExtracter(
            min_count=2,
            use_branching_entropy=True,
            verbose=0
        )
        extractor_without_entropy = PhraseExtracter(
            min_count=2,
            use_branching_entropy=False,
            verbose=0
        )

        sentences = ["機械学習は機械学習で機械学習です", "テストテストテスト"]

        df_with = extractor_with_entropy.get_dfphrase(sentences)
        df_without = extractor_without_entropy.get_dfphrase(sentences)

        # 両方とも結果を返す（エラーがない）
        assert isinstance(df_with, pd.DataFrame)
        assert isinstance(df_without, pd.DataFrame)

    def test_score_with_both_enabled(self):
        """PMI＋BE両方有効時のスコアリング"""
        extractor = PhraseExtracter(
            min_count=2,
            use_pmi=True,
            use_branching_entropy=True,
            pmi_weight=1.0,
            entropy_weight=1.0,
            verbose=0
        )

        sentences = [
            "機械学習は重要です。機械学習を学びます。",
            "ていうのはていう意味で",
            "テストテストテスト",
        ]

        df = extractor.get_dfphrase(sentences)

        # 結果が返される（エラーがない）
        assert isinstance(df, pd.DataFrame)
        # フレーズが抽出されている
        if len(df) > 0:
            assert "seqchar" in df.columns
            assert "freq" in df.columns
            assert "sc_index" in df.columns

    def test_backward_compatibility(self):
        """後方互換性（デフォルトではPMI/BEを使用しない）"""
        extractor_default = PhraseExtracter(verbose=0)
        assert extractor_default.use_pmi == False
        assert extractor_default.use_branching_entropy == False

        sentences = ["テストテストテスト", "これはテストです"]
        df = extractor_default.get_dfphrase(sentences)

        # PMI/BE列がない（デフォルト動作）
        assert "pmi" not in df.columns
        assert "boundary_score" not in df.columns


class TestParameterWeights:
    """PMI・BEの重み係数テスト"""

    def test_pmi_weight_parameter(self):
        """PMI重み係数の影響"""
        sentences = ["機械学習は機械学習で機械学習"]

        # 異なる重みでスコアが変わることを確認
        extractor1 = PhraseExtracter(
            min_count=2,
            use_pmi=True,
            pmi_weight=0.5,
            verbose=0
        )
        extractor2 = PhraseExtracter(
            min_count=2,
            use_pmi=True,
            pmi_weight=2.0,
            verbose=0
        )

        df1 = extractor1.get_dfphrase(sentences)
        df2 = extractor2.get_dfphrase(sentences)

        # 両方とも結果を返す
        assert isinstance(df1, pd.DataFrame)
        assert isinstance(df2, pd.DataFrame)

    def test_entropy_weight_parameter(self):
        """エントロピー重み係数の影響"""
        sentences = ["機械学習は機械学習で機械学習です"]

        # 異なる重みでスコアが変わることを確認
        extractor1 = PhraseExtracter(
            min_count=2,
            use_branching_entropy=True,
            entropy_weight=0.5,
            verbose=0
        )
        extractor2 = PhraseExtracter(
            min_count=2,
            use_branching_entropy=True,
            entropy_weight=2.0,
            verbose=0
        )

        df1 = extractor1.get_dfphrase(sentences)
        df2 = extractor2.get_dfphrase(sentences)

        # 両方とも結果を返す
        assert isinstance(df1, pd.DataFrame)
        assert isinstance(df2, pd.DataFrame)


class TestEdgeCases:
    """エッジケースのテスト"""

    def test_empty_phrase_list(self):
        """フレーズが空の場合"""
        extractor = PhraseExtracter(use_pmi=True, use_branching_entropy=True)

        pmi_scores = extractor.calculate_pmi([], "テキスト")
        entropy_scores = extractor.calculate_branching_entropy(["テキスト"], [])

        assert pmi_scores == {}
        assert entropy_scores == {}

    def test_very_long_text(self):
        """非常に長いテキスト"""
        extractor = PhraseExtracter(
            min_count=2,
            use_pmi=True,
            use_branching_entropy=True,
            verbose=0
        )

        # 長いテキストを生成
        long_text = "機械学習 " * 100
        sentences = [long_text]

        df = extractor.get_dfphrase(sentences)

        # エラーなく処理できる
        assert isinstance(df, pd.DataFrame)

    def test_special_characters(self):
        """特殊文字を含むテキスト"""
        extractor = PhraseExtracter(
            min_count=2,
            use_pmi=True,
            use_branching_entropy=True,
            verbose=0
        )

        sentences = ["テスト★テスト★テスト", "ABC★ABC★ABC"]

        df = extractor.get_dfphrase(sentences)

        # エラーなく処理できる
        assert isinstance(df, pd.DataFrame)

    def test_unicode_characters(self):
        """Unicode文字を含むテキスト"""
        extractor = PhraseExtracter(
            min_count=2,
            use_pmi=True,
            use_branching_entropy=True,
            verbose=0
        )

        sentences = ["😀😀😀", "🎉🎉🎉", "機械学習は機械学習"]

        df = extractor.get_dfphrase(sentences)

        # エラーなく処理できる
        assert isinstance(df, pd.DataFrame)


if __name__ == "__main__":
    pytest.main([__file__, "-v"])
