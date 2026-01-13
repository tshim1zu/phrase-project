# coding:utf-8
"""
similarity.pyのテスト
"""

import pytest
import pandas as pd
import numpy as np
import os
import tempfile
from japhrase import SimilarityAnalyzer


class TestSimilarityAnalyzer:
    """SimilarityAnalyzerクラスのテスト"""

    def test_create_instance_default(self):
        """デフォルト設定でインスタンス作成"""
        analyzer = SimilarityAnalyzer()
        assert analyzer.method == 'auto'
        assert analyzer.ngram_size == 3

    def test_create_instance_with_method(self):
        """手法を指定してインスタンス作成"""
        analyzer = SimilarityAnalyzer(method='jaccard')
        assert analyzer.method == 'jaccard'

    def test_invalid_method(self):
        """無効な手法でエラー"""
        with pytest.raises(ValueError):
            SimilarityAnalyzer(method='invalid_method')

    def test_select_method_short_text(self):
        """短文の場合はlevenshteinを選択"""
        analyzer = SimilarityAnalyzer(method='auto')
        method = analyzer._select_method(400)
        assert method == 'levenshtein'

    def test_select_method_medium_text(self):
        """中文の場合はjaccardを選択"""
        analyzer = SimilarityAnalyzer(method='auto')
        method = analyzer._select_method(2000)
        assert method == 'jaccard'

    def test_select_method_long_text(self):
        """長文の場合はcosineを選択"""
        analyzer = SimilarityAnalyzer(method='auto')
        method = analyzer._select_method(10000)
        assert method == 'cosine'


class TestSimilarityMethods:
    """各類似度計算手法のテスト"""

    def test_similarity_jaccard_identical(self):
        """Jaccard: 同一テキストの類似度は1.0"""
        analyzer = SimilarityAnalyzer(method='jaccard')
        text = "これはテストです"
        similarity = analyzer.similarity_jaccard(text, text)
        assert similarity == 1.0

    def test_similarity_jaccard_different(self):
        """Jaccard: 異なるテキストの類似度は1.0未満"""
        analyzer = SimilarityAnalyzer(method='jaccard')
        text1 = "これはテストです"
        text2 = "それは確認です"
        similarity = analyzer.similarity_jaccard(text1, text2)
        assert 0.0 <= similarity < 1.0

    def test_similarity_jaccard_empty(self):
        """Jaccard: 両方空文字列の場合は1.0"""
        analyzer = SimilarityAnalyzer(method='jaccard')
        similarity = analyzer.similarity_jaccard("", "")
        assert similarity == 1.0

    def test_similarity_levenshtein_identical(self):
        """Levenshtein: 同一テキストの類似度は1.0"""
        analyzer = SimilarityAnalyzer(method='levenshtein')
        text = "これはテストです"
        similarity = analyzer.similarity_levenshtein(text, text)
        assert similarity == 1.0

    def test_similarity_levenshtein_similar(self):
        """Levenshtein: 類似テキストの類似度"""
        analyzer = SimilarityAnalyzer(method='levenshtein')
        text1 = "これはテストです"
        text2 = "これはテスト"
        similarity = analyzer.similarity_levenshtein(text1, text2)
        assert 0.5 < similarity < 1.0

    def test_get_ngrams(self):
        """N-gram抽出のテスト"""
        analyzer = SimilarityAnalyzer(ngram_size=2)
        text = "テスト"
        ngrams = analyzer._get_ngrams(text, 2)
        expected = {"テス", "スト"}
        assert ngrams == expected

    def test_get_ngrams_short_text(self):
        """N-gramサイズより短いテキスト"""
        analyzer = SimilarityAnalyzer(ngram_size=5)
        text = "短い"
        ngrams = analyzer._get_ngrams(text, 5)
        assert ngrams == {text}


class TestCompareTexts:
    """compare_textsメソッドのテスト"""

    def test_compare_texts_basic(self):
        """基本的なテキスト比較"""
        analyzer = SimilarityAnalyzer(method='jaccard')
        texts = [
            "これはテストです",
            "これはテスト",
            "全く違う内容"
        ]
        matrix = analyzer.compare_texts(texts)

        assert isinstance(matrix, pd.DataFrame)
        assert matrix.shape == (3, 3)

        # 対角要素は1.0
        assert matrix.iloc[0, 0] == 1.0
        assert matrix.iloc[1, 1] == 1.0
        assert matrix.iloc[2, 2] == 1.0

        # 対称行列
        assert matrix.iloc[0, 1] == matrix.iloc[1, 0]

    def test_compare_texts_with_labels(self):
        """ラベル指定付きテキスト比較"""
        analyzer = SimilarityAnalyzer(method='jaccard')
        texts = ["テキスト1", "テキスト2"]
        labels = ["A", "B"]
        matrix = analyzer.compare_texts(texts, labels)

        assert list(matrix.index) == labels
        assert list(matrix.columns) == labels

    def test_compare_texts_invalid_labels(self):
        """ラベル数が不正な場合"""
        analyzer = SimilarityAnalyzer(method='jaccard')
        texts = ["テキスト1", "テキスト2"]
        labels = ["A"]  # 数が合わない

        with pytest.raises(ValueError):
            analyzer.compare_texts(texts, labels)


class TestCompareFiles:
    """compare_filesメソッドのテスト"""

    @pytest.fixture
    def temp_dir(self):
        """一時ディレクトリを提供"""
        with tempfile.TemporaryDirectory() as tmpdir:
            yield tmpdir

    def test_compare_files(self, temp_dir):
        """複数ファイルの類似度比較"""
        analyzer = SimilarityAnalyzer(method='jaccard')

        # テストファイルを作成
        file1 = os.path.join(temp_dir, "file1.txt")
        file2 = os.path.join(temp_dir, "file2.txt")
        file3 = os.path.join(temp_dir, "file3.txt")

        with open(file1, 'w', encoding='utf-8') as f:
            f.write("これはファイル1です\n")

        with open(file2, 'w', encoding='utf-8') as f:
            f.write("これはファイル1です\n")  # file1と同じ

        with open(file3, 'w', encoding='utf-8') as f:
            f.write("全く違う内容\n")

        # 類似度行列を計算
        matrix = analyzer.compare_files([file1, file2, file3])

        assert isinstance(matrix, pd.DataFrame)
        assert matrix.shape == (3, 3)

        # file1とfile2は類似度が高い
        assert matrix.loc['file1.txt', 'file2.txt'] > 0.9


class TestFindSimilarPairs:
    """find_similar_pairsメソッドのテスト"""

    def test_find_similar_pairs_basic(self):
        """基本的な類似ペア抽出"""
        analyzer = SimilarityAnalyzer(method='jaccard')
        texts = [
            "これはテストです",
            "これはテスト",  # text_1と類似
            "全く違う内容"
        ]
        matrix = analyzer.compare_texts(texts)
        pairs = analyzer.find_similar_pairs(matrix, threshold=0.5)

        assert isinstance(pairs, list)
        assert len(pairs) > 0

        # 最初のペアは最も類似度が高いはず
        assert pairs[0]['similarity'] >= 0.5
        assert 'item1' in pairs[0]
        assert 'item2' in pairs[0]

    def test_find_similar_pairs_high_threshold(self):
        """高い閾値で類似ペア抽出"""
        analyzer = SimilarityAnalyzer(method='jaccard')
        texts = [
            "テキスト1",
            "テキスト2",
            "テキスト3"
        ]
        matrix = analyzer.compare_texts(texts)
        pairs = analyzer.find_similar_pairs(matrix, threshold=0.99)

        # 類似度が非常に高いペアはないはず
        assert len(pairs) == 0

    def test_find_similar_pairs_top_n(self):
        """上位N件のみ取得"""
        analyzer = SimilarityAnalyzer(method='jaccard')
        texts = ["A", "B", "C", "D"]
        matrix = analyzer.compare_texts(texts)
        pairs = analyzer.find_similar_pairs(matrix, threshold=0.0, top_n=2)

        assert len(pairs) == 2


class TestExportMethods:
    """エクスポートメソッドのテスト"""

    @pytest.fixture
    def temp_dir(self):
        """一時ディレクトリを提供"""
        with tempfile.TemporaryDirectory() as tmpdir:
            yield tmpdir

    @pytest.fixture
    def sample_matrix(self):
        """サンプル類似度行列を提供"""
        analyzer = SimilarityAnalyzer(method='jaccard')
        texts = ["テキスト1", "テキスト2", "テキスト3"]
        return analyzer.compare_texts(texts)

    def test_export_matrix_csv(self, temp_dir, sample_matrix):
        """CSV形式でエクスポート"""
        analyzer = SimilarityAnalyzer()
        filepath = os.path.join(temp_dir, "matrix.csv")
        analyzer.export_matrix(sample_matrix, filepath, format='csv')

        assert os.path.exists(filepath)

        # 読み込んで確認
        df_loaded = pd.read_csv(filepath, index_col=0, encoding='utf-8-sig')
        assert df_loaded.shape == sample_matrix.shape

    def test_export_matrix_excel(self, temp_dir, sample_matrix):
        """Excel形式でエクスポート"""
        analyzer = SimilarityAnalyzer()
        filepath = os.path.join(temp_dir, "matrix.xlsx")

        try:
            analyzer.export_matrix(sample_matrix, filepath, format='excel')
            assert os.path.exists(filepath)
        except ImportError:
            pytest.skip("openpyxl not installed")

    def test_export_matrix_json(self, temp_dir, sample_matrix):
        """JSON形式でエクスポート"""
        analyzer = SimilarityAnalyzer()
        filepath = os.path.join(temp_dir, "matrix.json")
        analyzer.export_matrix(sample_matrix, filepath, format='json')

        assert os.path.exists(filepath)

    def test_export_matrix_invalid_format(self, temp_dir, sample_matrix):
        """無効な形式でエラー"""
        analyzer = SimilarityAnalyzer()
        filepath = os.path.join(temp_dir, "matrix.txt")

        with pytest.raises(ValueError):
            analyzer.export_matrix(sample_matrix, filepath, format='invalid')
