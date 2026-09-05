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
from japhrase.vectorization_utils import (
    build_document_term_matrix, _PhraseAnalyzer, _CharRemover, HybridVectorizer,
    extract_pmi_filtered_phrases,
)


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
        assert set(f"tfidf:{name}" for name in tfidf_names).issubset(set(hybrid_names))
        assert (hybrid_matrix != 0).sum() > 0

    def test_hybrid_feature_names_are_namespaced(self):
        """tfidf空間とlow_pmi空間に同じ語（例:「猫が」）が入っても、
        特徴名がprefixで区別でき、衝突しないこと"""
        _, _, hybrid_names, _ = build_document_term_matrix(
            [TEXT_CAT, TEXT_DOG],
            feature_mode='hybrid',
            min_count=3,
            pmi_threshold=5.0,
            ngram_range=(2, 3),
            verbose=0,
        )
        assert len(hybrid_names) == len(set(hybrid_names))  # 重複なし
        assert all(
            name.startswith('tfidf:') or name.startswith('low_pmi:')
            for name in hybrid_names
        )


class TestPhraseAnalyzerOverlapCounting:
    """_PhraseAnalyzerの頻度定義がPhraseExtracterの重複カウントと一致すること"""

    def test_counts_overlapping_occurrences(self):
        # PhraseExtracter.make_ngrampieces()は1文字ずつ位置をずらすため、
        # "aaaa"中の"aa"は3回（位置0,1,2）。str.count()は2回しか数えない。
        analyzer = _PhraseAnalyzer(['aa'])
        tokens = analyzer('aaaa')
        assert tokens == ['aa', 'aa', 'aa']

    def test_matches_phrase_extracter_frequency(self):
        from japhrase import PhraseExtracter
        extractor = PhraseExtracter(min_count=1, min_length=2, max_length=2, verbose=0)
        df = extractor.get_dfphrase(["aaaa"])
        expected_freq = int(df.loc[df['seqchar'] == 'aa', 'freq'].iloc[0])

        analyzer = _PhraseAnalyzer(['aa'])
        assert len(analyzer('aaaa')) == expected_freq


class TestHighPmiPreprocessingMatchesVocabularySource:
    """high_pmiのvocabularyはPhraseExtracterがDEFAULT_REMOVES文字を除去した上で
    作られるため、_PhraseAnalyzerも同じ除去を検索前に行う必要がある"""

    def test_char_remover_strips_configured_chars(self):
        remover = _CharRemover('.')
        assert remover('猫.犬.猫.犬.') == '猫犬猫犬'

    def test_analyzer_without_preprocess_misses_phrase_across_removed_char(self):
        # 除去文字を挟むフレーズは、前処理なしでは生テキストから見つからない
        analyzer = _PhraseAnalyzer(['猫犬'])
        assert analyzer('猫.犬.') == []

    def test_analyzer_with_preprocess_finds_phrase_across_removed_char(self):
        analyzer = _PhraseAnalyzer(['猫犬'], preprocess=_CharRemover('.'))
        assert analyzer('猫.犬.') == ['猫犬']

    def test_high_pmi_matrix_not_zero_when_phrase_crosses_removed_chars(self):
        """レビュー指摘の再現ケース: '.'を挟んだ繰り返しテキストでも
        high_pmiの特徴行列がゼロにならないこと"""
        text = "猫.犬." * 10
        texts = [text, text]
        matrix, _, feature_names, _ = build_document_term_matrix(
            texts,
            feature_mode='high_pmi',
            min_count=3,
            pmi_threshold=0.0,
            verbose=0,
        )
        assert len(feature_names) > 0
        assert (matrix != 0).sum() > 0


class TestHybridVectorizerIsReusable:
    """hybridの result.vectorizer がhybrid特徴空間全体をtransformできること
    （tfidf部分だけを保持していると、nmf_model.transform()と列数が合わなくなる）"""

    def test_transform_matches_nmf_input_dimension(self):
        vectorizer = DocumentVectorizer(
            n_topics=2, feature_mode='hybrid', min_count=3, pmi_threshold=5.0, verbose=0
        )
        result = vectorizer.from_texts([TEXT_CAT, TEXT_DOG], labels=['cat', 'dog'])

        new_matrix = result.vectorizer.transform(["猫と犬が好きだ。"])
        assert new_matrix.shape[1] == result.topic_term_matrix.shape[1]

        # nmf_model.transform()にそのまま渡せること（列数不一致でエラーにならない）
        topic_vec = result.nmf_model.transform(new_matrix.toarray())
        assert topic_vec.shape == (1, 2)

    def test_hybrid_vectorizer_is_picklable(self):
        import pickle

        vectorizer = DocumentVectorizer(
            n_topics=2, feature_mode='hybrid', min_count=3, pmi_threshold=5.0, verbose=0
        )
        result = vectorizer.from_texts([TEXT_CAT, TEXT_DOG], labels=['cat', 'dog'])

        restored = pickle.loads(pickle.dumps(result))
        restored_matrix = restored.vectorizer.transform(["猫と犬が好きだ。"])
        assert restored_matrix.shape[1] == restored.topic_term_matrix.shape[1]

    def test_hybrid_vectorizer_falls_back_to_tfidf_only(self):
        # low_pmi語彙が見つからない場合でも、result.vectorizerはtfidf部分のみを
        # 正しくtransformできること
        from sklearn.feature_extraction.text import TfidfVectorizer

        vectorizer = HybridVectorizer(
            tfidf_vectorizer=TfidfVectorizer(analyzer='char', ngram_range=(2, 3)),
            pmi_vectorizer=None,
        )
        matrix = vectorizer.fit_transform([TEXT_CAT, TEXT_DOG])
        assert matrix.shape[1] == len(vectorizer.get_feature_names_out())
        transformed = vectorizer.transform(["猫が好きだ。"])
        assert transformed.shape[1] == matrix.shape[1]
