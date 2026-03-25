"""英語対応 (lang='en') のテスト"""
import pytest
from japhrase import PhraseExtractor
from japhrase.tokenizers import get_tokenizer, EnglishTokenizer, JapaneseTokenizer


# --- Tokenizer ---

class TestGetTokenizer:
    def test_ja_default(self):
        tok = get_tokenizer()
        assert isinstance(tok, JapaneseTokenizer)

    def test_en(self):
        tok = get_tokenizer("en")
        assert isinstance(tok, EnglishTokenizer)

    def test_unknown_raises(self):
        with pytest.raises(ValueError):
            get_tokenizer("klingon")


class TestEnglishTokenizer:
    def test_tokenize_basic(self):
        tok = EnglishTokenizer()
        tokens = tok.tokenize("Machine Learning is great.")
        assert tokens == ["machine", "learning", "is", "great"]

    def test_tokenize_punctuation(self):
        tok = EnglishTokenizer()
        tokens = tok.tokenize("Hello, world! How's it going?")
        assert "hello" in tokens
        assert "how's" in tokens

    def test_stopword_removal(self):
        tok = EnglishTokenizer(remove_stopwords=True)
        tokens = tok.tokenize("The cat is on the mat")
        assert "the" not in tokens
        assert "cat" in tokens
        assert "mat" in tokens

    def test_join(self):
        tok = EnglishTokenizer()
        assert tok.join(["machine", "learning"]) == "machine learning"


class TestJapaneseTokenizer:
    def test_tokenize_basic(self):
        tok = JapaneseTokenizer()
        tokens = tok.tokenize("機械学習")
        assert tokens == ["機", "械", "学", "習"]

    def test_join(self):
        tok = JapaneseTokenizer()
        assert tok.join(["機", "械"]) == "機械"


# --- PhraseExtractor with lang='en' ---

EN_TEXT = """Machine learning is transforming natural language processing.
Natural language processing relies on machine learning models.
Deep learning is a subset of machine learning.
Machine learning engineers are in high demand.
The field of machine learning continues to grow rapidly.
Natural language processing has made significant advances.
Deep learning requires large computational resources.
Machine learning applications span many industries.""" * 3


class TestEnglishExtraction:
    def test_basic_extraction(self):
        pe = PhraseExtractor(lang='en', min_count=2, verbose=0)
        df = pe.extract(EN_TEXT)
        phrases = df["seqchar"].tolist()
        assert "machine learning" in phrases

    def test_nlp_found(self):
        pe = PhraseExtractor(lang='en', min_count=2, verbose=0)
        df = pe.extract(EN_TEXT)
        phrases = df["seqchar"].tolist()
        assert "natural language processing" in phrases

    def test_preset_en_default(self):
        pe = PhraseExtractor.preset('en_default')
        pe.verbose = 0
        df = pe.extract(EN_TEXT)
        assert len(df) > 0

    def test_preset_en_academic(self):
        pe = PhraseExtractor.preset('en_academic')
        pe.verbose = 0
        df = pe.extract(EN_TEXT)
        phrases = df["seqchar"].tolist()
        assert "machine learning" in phrases

    def test_pmi_filters_noise(self):
        pe = PhraseExtractor(lang='en', min_count=2, use_pmi=True, verbose=0)
        df = pe.extract(EN_TEXT)
        phrases = df["seqchar"].tolist()
        # "machine learning" should survive PMI
        assert "machine learning" in phrases

    def test_auto_tune(self):
        pe = PhraseExtractor(lang='en', verbose=0)
        df = pe.extract(EN_TEXT, auto_tune=True)
        assert len(df) > 0

    def test_lang_stored(self):
        pe = PhraseExtractor(lang='en')
        assert pe.lang == 'en'

    def test_save_load_preserves_lang(self):
        import tempfile, os
        pe = PhraseExtractor(lang='en', min_count=5, use_pmi=True, verbose=0)
        path = os.path.join(tempfile.gettempdir(), 'test_en_save.json')
        pe.save_params(path)
        pe2 = PhraseExtractor.load_params(path)
        assert pe2.lang == 'en'
        assert pe2.use_pmi is True
        os.remove(path)

    def test_save_load_ja_no_lang_key(self):
        import tempfile, os, json
        pe = PhraseExtractor(verbose=0)
        path = os.path.join(tempfile.gettempdir(), 'test_ja_save.json')
        pe.save_params(path)
        with open(path) as f:
            data = json.load(f)
        assert 'lang' not in data['params']
        pe2 = PhraseExtractor.load_params(path)
        assert pe2.lang == 'ja'
        os.remove(path)


# --- Japanese not broken ---

JA_TEXT = """機械学習の応用が広がっている。
自然言語処理は機械学習の重要な分野だ。
深層学習は機械学習の手法の一つである。
機械学習エンジニアの需要が高まっている。""" * 5


class TestJapaneseNotBroken:
    def test_ja_still_works(self):
        pe = PhraseExtractor(verbose=0)
        df = pe.extract(JA_TEXT)
        assert len(df) > 0

    def test_ja_default_lang(self):
        pe = PhraseExtractor()
        assert pe.lang == 'ja'
