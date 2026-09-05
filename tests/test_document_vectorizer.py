"""DocumentVectorizer / vectorization_utils の回帰テスト

コードレビューで指摘された以下のバグの再発防止:
- from_files/from_texts がインスタンス設定（n_topics等）を無視していた
- low_pmi/high_pmi の特徴行列がngram_range不一致でほぼゼロになっていた
- hybridモードが実際にはlow_pmi語彙に制限されるだけだった
- 同名ファイル（別ディレクトリ）を比較するとラベルが衝突し結果が消えていた
"""
import os
import tempfile

import pytest
import numpy as np

from japhrase import DocumentVectorizer
from japhrase.document_vectorizer import DocumentVectorizer as DV
from japhrase.vectorization_utils import build_document_term_matrix


TEXT_CAT = "猫が好きだ。猫はかわいい。今日も猫を見た。猫と遊んだ。" * 8
TEXT_DOG = "犬が好きだ。犬は元気だ。今日も犬を見た。犬と遊んだ。" * 8
TEXT_FISH = "魚が好きだ。魚は静かだ。今日も魚を見た。魚と泳いだ。" * 8


class TestInstanceConfigHonored:
    """from_files/from_textsが自身のコンストラクタ引数を使うこと"""

    def test_from_texts_honors_n_topics(self):
        vectorizer = DocumentVectorizer(n_topics=2, feature_mode='tfidf', verbose=0)
        result = vectorizer.from_texts(
            [TEXT_CAT, TEXT_DOG, TEXT_FISH], labels=['A', 'B', 'C']
        )
        assert result.document_topic_matrix.shape[1] == 2

    def test_from_files_honors_n_topics_and_feature_mode(self, tmp_path):
        f1 = tmp_path / "cat.txt"
        f2 = tmp_path / "dog.txt"
        f1.write_text(TEXT_CAT, encoding='utf-8')
        f2.write_text(TEXT_DOG, encoding='utf-8')

        vectorizer = DocumentVectorizer(
            n_topics=2, feature_mode='low_pmi', min_count=3, pmi_threshold=5.0, verbose=0
        )
        result = vectorizer.from_files([str(f1), str(f2)])
        assert result.document_topic_matrix.shape[1] == 2
        assert result.metadata['feature_mode'] == 'low_pmi'


class TestUniqueLabels:
    def test_unique_basenames_kept_as_is(self):
        labels = DV._make_unique_labels(['a/one.txt', 'b/two.txt'])
        assert labels == ['one.txt', 'two.txt']

    def test_duplicate_basenames_disambiguated(self):
        labels = DV._make_unique_labels([
            'authorA/chapter1.txt', 'authorB/chapter1.txt', 'authorC/chapter1.txt'
        ])
        assert len(labels) == len(set(labels))

    def test_from_files_with_duplicate_basenames_keeps_all_comparisons(self, tmp_path):
        dir_a = tmp_path / "authorA"
        dir_b = tmp_path / "authorB"
        dir_a.mkdir()
        dir_b.mkdir()
        (dir_a / "chapter1.txt").write_text(TEXT_CAT, encoding='utf-8')
        (dir_b / "chapter1.txt").write_text(TEXT_DOG, encoding='utf-8')

        vectorizer = DocumentVectorizer(n_topics=2, feature_mode='tfidf', verbose=0)
        result = vectorizer.from_files([
            str(dir_a / "chapter1.txt"), str(dir_b / "chapter1.txt")
        ])
        assert len(set(result.labels)) == 2

        diffs = vectorizer.calculate_differences(result, 0, [1])
        # 同名ファイルでもラベル衝突で比較列が消えないこと
        assert len(diffs.columns) == 1


class TestVocabularyRestrictedModes:
    """low_pmi/high_pmiのvocabularyフレーズが実際にカウントされること"""

    def test_low_pmi_matrix_is_not_all_zero(self):
        matrix, _, feature_names, metadata = build_document_term_matrix(
            [TEXT_CAT, TEXT_DOG],
            feature_mode='low_pmi',
            min_count=3,
            pmi_threshold=5.0,
            verbose=0,
        )
        assert len(feature_names) > 0
        assert (matrix != 0).sum() > 0

    def test_high_pmi_matrix_is_not_all_zero(self):
        matrix, _, feature_names, metadata = build_document_term_matrix(
            [TEXT_CAT, TEXT_DOG],
            feature_mode='high_pmi',
            min_count=3,
            pmi_threshold=0.0,
            verbose=0,
        )
        assert len(feature_names) > 0
        assert (matrix != 0).sum() > 0

    def test_hybrid_combines_tfidf_and_low_pmi_features(self):
        tfidf_matrix, _, tfidf_names, _ = build_document_term_matrix(
            [TEXT_CAT, TEXT_DOG],
            feature_mode='tfidf',
            ngram_range=(2, 3),
            verbose=0,
        )
        hybrid_matrix, _, hybrid_names, hybrid_meta = build_document_term_matrix(
            [TEXT_CAT, TEXT_DOG],
            feature_mode='hybrid',
            min_count=3,
            pmi_threshold=5.0,
            ngram_range=(2, 3),
            verbose=0,
        )
        # hybridの特徴数は通常tfidf単体より多い（low_pmi語彙が追加されるため）
        assert hybrid_matrix.shape[1] > tfidf_matrix.shape[1]
        # tfidf側の特徴が失われていないこと（低PMI語彙だけに制限されていないこと）
        assert set(tfidf_names).issubset(set(hybrid_names))
        assert (hybrid_matrix != 0).sum() > 0
