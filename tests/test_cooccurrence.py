"""CooccurrenceAnalyzer の専用テスト"""
import pytest
import pandas as pd
from japhrase import CooccurrenceAnalyzer, PhraseExtracter


# ターゲット語の周辺に同じフレーズが繰り返し出現するテキスト
SAMPLE_TEXT = (
    "太郎は走っていた。太郎が笑っていた。"
    "花子も走っていた。花子が笑っていた。"
    "太郎は笑っていた。花子は走っていた。"
) * 15


@pytest.fixture
def analyzer():
    extractor = PhraseExtracter(min_count=2)
    return CooccurrenceAnalyzer(extractor=extractor, window_size=200, min_cooccurrence=2)


class TestInit:
    def test_default_init(self):
        ca = CooccurrenceAnalyzer()
        assert ca.window_size == 50
        assert ca.min_cooccurrence == 3
        assert ca.extractor is not None

    def test_custom_window_size(self):
        ca = CooccurrenceAnalyzer(window_size=100)
        assert ca.window_size == 100

    def test_custom_extractor(self):
        e = PhraseExtracter(min_count=2)
        ca = CooccurrenceAnalyzer(extractor=e)
        assert ca.extractor is e


class TestExtractContext:
    def test_returns_list(self, analyzer):
        contexts = analyzer.extract_context(SAMPLE_TEXT, '太郎')
        assert isinstance(contexts, list)

    def test_finds_occurrences(self, analyzer):
        contexts = analyzer.extract_context(SAMPLE_TEXT, '太郎')
        assert len(contexts) > 0

    def test_unknown_word_returns_empty(self, analyzer):
        contexts = analyzer.extract_context(SAMPLE_TEXT, '存在しない語')
        assert len(contexts) == 0

    def test_context_count_matches_occurrences(self, analyzer):
        text = "Xが登場する。Xが活動する。Xが消える。" * 10
        contexts = analyzer.extract_context(text, 'X')
        assert len(contexts) == 30  # 3 occurrences × 10 repetitions


class TestAnalyze:
    def test_returns_dataframe(self, analyzer):
        df = analyzer.analyze(SAMPLE_TEXT, '太郎', include_target=True)
        assert isinstance(df, pd.DataFrame)

    def test_result_has_required_columns(self, analyzer):
        df = analyzer.analyze(SAMPLE_TEXT, '太郎', include_target=True)
        if not df.empty:
            for col in ('phrase', 'freq', 'lift', 'score'):
                assert col in df.columns

    def test_unknown_target_returns_empty(self, analyzer):
        df = analyzer.analyze(SAMPLE_TEXT, '存在しない語')
        assert df.empty

    def test_top_n_respected(self, analyzer):
        df = analyzer.analyze(SAMPLE_TEXT, '太郎', top_n=3, include_target=True)
        assert len(df) <= 3

    def test_lift_values_positive(self, analyzer):
        df = analyzer.analyze(SAMPLE_TEXT, '太郎', include_target=True)
        if not df.empty:
            assert (df['lift'] > 0).all()

    def test_freq_values_positive(self, analyzer):
        df = analyzer.analyze(SAMPLE_TEXT, '太郎', include_target=True)
        if not df.empty:
            assert (df['freq'] > 0).all()

    def test_include_target_false_excludes_target(self, analyzer):
        df = analyzer.analyze(SAMPLE_TEXT, '太郎', include_target=False)
        if not df.empty:
            assert '太郎' not in df['phrase'].values
            assert not df['phrase'].str.contains('太郎').any()

    def test_empty_text_returns_empty(self, analyzer):
        df = analyzer.analyze("", '太郎')
        assert df.empty
