# coding: utf-8
"""
テキスト複雑度メトリクスのテスト

ComplexityAnalyzer の全機能を検証：
- N-gram Perplexity
- Compression Ratio
- Lexical Density
- Information Rate
- Segment Complexity Profile
"""

import pytest
from japhrase import ComplexityAnalyzer, ComplexityProfile


class TestPerplexity:
    """N-gram Perplexity テスト"""

    def test_repetitive_text_low_pp(self):
        """繰り返しテキストはPerplexityが低い"""
        analyzer = ComplexityAnalyzer()
        text = 'あいうえお' * 100
        pp = analyzer.calc_ngram_perplexity(text)
        assert pp > 0
        assert pp < 20, "繰り返しテキストはPPが低い"

    def test_varied_text_higher_pp(self):
        """多様なテキストはPerplexityが高い"""
        analyzer = ComplexityAnalyzer(ngram_order=2)  # 短いN-gramで差を出しやすく
        repetitive = 'あいうえおかきくけこ' * 200
        varied = (
            '吾輩は猫である。名前はまだ無い。どこで生れたかとんと見当がつかぬ。'
            '何でも薄暗いじめじめした所でニャーニャー泣いていた事だけは記憶している。'
            '書生というのは時々我々を捕まえて煮て食うという話である。'
        ) * 20

        pp_rep = analyzer.calc_ngram_perplexity(repetitive)
        pp_var = analyzer.calc_ngram_perplexity(varied)
        assert pp_var >= pp_rep

    def test_short_text(self):
        """短いテキストでもエラーにならない"""
        analyzer = ComplexityAnalyzer()
        pp = analyzer.calc_ngram_perplexity('ab')
        assert pp == 0.0


class TestCompressionRatio:
    """圧縮率テスト"""

    def test_repetitive_low_ratio(self):
        """繰り返しは低い圧縮率（よく圧縮される）"""
        analyzer = ComplexityAnalyzer()
        cr = analyzer.calc_compression_ratio('あ' * 10000)
        assert cr < 0.1, "繰り返しは高圧縮"

    def test_varied_higher_ratio(self):
        """多様なテキストは圧縮率が高い"""
        analyzer = ComplexityAnalyzer()
        import random, string
        varied = ''.join(random.choices(string.ascii_letters, k=5000))
        cr = analyzer.calc_compression_ratio(varied)
        assert cr > 0.5

    def test_empty(self):
        analyzer = ComplexityAnalyzer()
        assert analyzer.calc_compression_ratio('') == 0.0


class TestLexicalDensity:
    """語彙密度テスト"""

    def test_bounded(self):
        """0-1の範囲"""
        analyzer = ComplexityAnalyzer()
        ld = analyzer.calc_lexical_density('エリスは剣を抜いた。レティシアが微笑んだ。')
        assert 0.0 <= ld <= 1.0

    def test_empty(self):
        analyzer = ComplexityAnalyzer()
        assert analyzer.calc_lexical_density('') == 0.0


class TestInformationRate:
    """情報率テスト"""

    def test_repetitive_low(self):
        """同じ文の繰り返しは情報率が低い"""
        analyzer = ComplexityAnalyzer()
        text = '。'.join(['エリスは歩いた'] * 20)
        ir = analyzer.calc_information_rate(text)
        assert ir < 0.3

    def test_varied_higher(self):
        """多様な文は情報率が高い"""
        analyzer = ComplexityAnalyzer()
        text = '。'.join([
            'エリスは目を覚ました',
            'レティシアが入ってきた',
            'ソフィアは端末を操作していた',
            'ノアが窓の外を眺めていた',
            'シルヴィのシステムが起動した',
        ])
        ir = analyzer.calc_information_rate(text)
        assert ir > 0.5


class TestAnalyze:
    """統合分析テスト"""

    def test_full_analysis(self):
        """analyze()が全指標を返す"""
        analyzer = ComplexityAnalyzer()
        text = 'エリスは近衛騎士団の訓練場を歩いていた。レティシアが剣を構えている。ソフィアの研究室からデータが送られてきた。'
        result = analyzer.analyze(text)

        assert 'perplexity' in result
        assert 'compression_ratio' in result
        assert 'lexical_density' in result
        assert 'information_rate' in result
        assert 'profile' in result

    def test_report_generation(self):
        """レポート生成"""
        analyzer = ComplexityAnalyzer()
        text = 'テスト文章。' * 50
        report = analyzer.generate_report(text)
        assert '複雑度分析' in report
