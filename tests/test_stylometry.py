"""StylometryAnalyzer の専用テスト"""
import pytest
from japhrase import StylometryAnalyzer


SAMPLE_JP = (
    "機械学習は現代の重要な技術です。"
    "深層学習もまた同様に重要な研究分野です。"
    "自然言語処理は人工知能の一分野として発展しています。"
) * 5

SAMPLE_KATAKANA = "コンピュータはデータを処理します。ネットワークが接続されています。" * 5
SAMPLE_MIXED = "Pythonは人気のプログラミング言語です。機械学習ライブラリが豊富です。" * 5


@pytest.fixture
def analyzer():
    return StylometryAnalyzer()


class TestVocabularyRichness:
    def test_returns_required_keys(self, analyzer):
        result = analyzer.analyze_vocabulary_richness(SAMPLE_JP)
        for key in ('total_tokens_est', 'unique_types_est', 'ttr', 'yules_k', 'assessment'):
            assert key in result

    def test_ttr_range(self, analyzer):
        result = analyzer.analyze_vocabulary_richness(SAMPLE_JP)
        assert 0.0 <= result['ttr'] <= 1.0

    def test_yules_k_positive(self, analyzer):
        result = analyzer.analyze_vocabulary_richness(SAMPLE_JP)
        assert result['yules_k'] >= 0.0

    def test_unique_le_total(self, analyzer):
        result = analyzer.analyze_vocabulary_richness(SAMPLE_JP)
        assert result['unique_types_est'] <= result['total_tokens_est']

    def test_empty_text(self, analyzer):
        result = analyzer.analyze_vocabulary_richness("")
        assert result['ttr'] == 0.0
        assert result['yules_k'] == 0.0


class TestCharTypeRatio:
    def test_returns_required_keys(self, analyzer):
        result = analyzer.analyze_char_type_ratio(SAMPLE_JP)
        for key in ('kanji_ratio', 'hiragana_ratio', 'katakana_ratio', 'ascii_ratio', 'other_ratio', 'style_type'):
            assert key in result

    def test_ratios_sum_to_one(self, analyzer):
        result = analyzer.analyze_char_type_ratio(SAMPLE_JP)
        total = sum(result[k] for k in ('kanji_ratio', 'hiragana_ratio', 'katakana_ratio', 'ascii_ratio', 'other_ratio'))
        assert 0.99 <= total <= 1.01

    def test_kanji_heavy_text(self, analyzer):
        text = "機械学習研究論文概要" * 10
        result = analyzer.analyze_char_type_ratio(text)
        assert result['kanji_ratio'] > 0.5
        assert 'style_type' in result

    def test_hiragana_heavy_text(self, analyzer):
        text = "あいうえおかきくけこさしすせそ" * 10
        result = analyzer.analyze_char_type_ratio(text)
        assert result['hiragana_ratio'] > 0.8

    def test_katakana_detected(self, analyzer):
        result = analyzer.analyze_char_type_ratio(SAMPLE_KATAKANA)
        assert result['katakana_ratio'] > 0.0

    def test_ascii_detected(self, analyzer):
        result = analyzer.analyze_char_type_ratio(SAMPLE_MIXED)
        assert result['ascii_ratio'] > 0.0

    def test_empty_text(self, analyzer):
        result = analyzer.analyze_char_type_ratio("")
        assert result['kanji_ratio'] == 0.0

    def test_style_type_is_string(self, analyzer):
        result = analyzer.analyze_char_type_ratio(SAMPLE_JP)
        assert isinstance(result['style_type'], str)
        assert len(result['style_type']) > 0


class TestSentenceLength:
    def test_returns_required_keys(self, analyzer):
        result = analyzer.analyze_sentence_length(SAMPLE_JP)
        for key in ('avg_length', 'std_length', 'min_length', 'max_length', 'sentence_count'):
            assert key in result

    def test_avg_positive(self, analyzer):
        result = analyzer.analyze_sentence_length(SAMPLE_JP)
        assert result['avg_length'] > 0
        assert result['sentence_count'] > 0

    def test_min_le_avg_le_max(self, analyzer):
        result = analyzer.analyze_sentence_length(SAMPLE_JP)
        assert result['min_length'] <= result['avg_length'] <= result['max_length']


class TestAnalyzeFull:
    def test_returns_all_sections(self, analyzer):
        result = analyzer.analyze_full(SAMPLE_JP)
        for key in ('vocabulary', 'character_type', 'sentence'):
            assert key in result

    def test_vocabulary_section_structure(self, analyzer):
        result = analyzer.analyze_full(SAMPLE_JP)
        vocab = result['vocabulary']
        assert 'ttr' in vocab
        assert 'yules_k' in vocab

    def test_short_text_does_not_crash(self, analyzer):
        result = analyzer.analyze_full("短い文。")
        assert isinstance(result, dict)
