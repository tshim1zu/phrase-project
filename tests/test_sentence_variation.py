"""
同文反復検出の自動バリエーション生成のテスト
"""

import pytest
from japhrase.sentence_variation import SentenceVariationGenerator


class TestSentenceVariationGenerator:
    """SentenceVariationGeneratorクラスのテスト"""

    def test_no_repetitions(self):
        """繰り返しのないテキスト"""
        generator = SentenceVariationGenerator()
        text = "今日は晴れです。明日は雨でしょう。昨日は曇りでした。"
        repetitions = generator.detect_repetitions(text)
        assert len(repetitions) == 0

    def test_exact_repetition(self):
        """完全に同じ文の繰り返し"""
        generator = SentenceVariationGenerator()
        text = "彼は走った。彼は走った。"
        repetitions = generator.detect_repetitions(text)

        assert len(repetitions) == 1
        assert repetitions[0]['count'] == 2
        assert '彼は走った' in repetitions[0]['sentence']

    def test_multiple_repetitions(self):
        """3回以上の繰り返し"""
        generator = SentenceVariationGenerator()
        text = "これは良い。これは良い。これは良い。"
        repetitions = generator.detect_repetitions(text)

        assert len(repetitions) == 1
        assert repetitions[0]['count'] == 3

    def test_variation_generation(self):
        """バリエーション生成のテスト"""
        generator = SentenceVariationGenerator()
        text = "彼は走った。彼は走った。"
        repetitions = generator.detect_repetitions(text)

        assert len(repetitions) == 1
        assert 'variations' in repetitions[0]
        assert len(repetitions[0]['variations']) > 0

    def test_ending_variation(self):
        """文末表現のバリエーション"""
        generator = SentenceVariationGenerator()
        variations = generator._vary_sentence_ending("彼は走った。")

        assert len(variations) > 0
        # 「た」→「ていた」「ました」などの変換
        texts = [v['text'] for v in variations]
        assert any('ていた' in t or 'ました' in t for t in texts)

    def test_word_order_variation(self):
        """語順変更のバリエーション"""
        generator = SentenceVariationGenerator()
        variations = generator._vary_word_order("昨日、彼は走った。")

        # 時間表現が文末に移動
        assert len(variations) > 0
        assert any('走った昨日' in v['text'] for v in variations)

    def test_omission_variation(self):
        """省略形のバリエーション"""
        generator = SentenceVariationGenerator()
        variations = generator._suggest_omissions("これは良いということだ。")

        assert len(variations) > 0
        assert any('ということ' not in v['text'] for v in variations)

    def test_conjunction_variation(self):
        """接続詞追加のバリエーション"""
        generator = SentenceVariationGenerator()
        variations = generator._add_conjunctions("彼は走った。")

        assert len(variations) > 0
        # しかし、また、そして、ただし などが追加される
        texts = [v['text'] for v in variations]
        assert any(t.startswith('しかし、') or t.startswith('また、') for t in texts)

    def test_min_repetitions_threshold(self):
        """最小繰り返し回数の閾値テスト"""
        generator = SentenceVariationGenerator(min_repetitions=3)
        text = "彼は走った。彼は走った。"  # 2回のみ
        repetitions = generator.detect_repetitions(text)

        # 3回未満なので検出されない
        assert len(repetitions) == 0

    def test_position_tracking(self):
        """位置情報の追跡テスト"""
        generator = SentenceVariationGenerator()
        text = "彼は走った。彼は走った。"
        repetitions = generator.detect_repetitions(text)

        assert len(repetitions) == 1
        assert 'positions' in repetitions[0]
        assert len(repetitions[0]['positions']) == 2

    def test_format_report_no_repetitions(self):
        """繰り返しなしのレポートフォーマット"""
        generator = SentenceVariationGenerator()
        repetitions = []
        report = generator.format_report(repetitions)

        assert "繰り返しは見つかりませんでした" in report

    def test_format_report_with_repetitions(self):
        """繰り返しありのレポートフォーマット"""
        generator = SentenceVariationGenerator()
        text = "彼は走った。彼は走った。"
        repetitions = generator.detect_repetitions(text)
        report = generator.format_report(repetitions, text)

        assert "件の繰り返しが見つかりました" in report
        assert "バリエーション候補" in report

    def test_detect_and_report(self):
        """統合メソッドのテスト"""
        generator = SentenceVariationGenerator()
        text = "彼は走った。彼は走った。"
        report = generator.detect_and_report(text)

        assert isinstance(report, str)
        assert len(report) > 0

    def test_complex_text(self):
        """複雑なテキストのテスト"""
        generator = SentenceVariationGenerator()
        text = """
        彼は学校に行った。彼は学校に行った。
        彼女は本を読んだ。
        彼は学校に行った。
        """
        repetitions = generator.detect_repetitions(text)

        # 「彼は学校に行った」が3回検出される
        assert len(repetitions) >= 1
        assert any(rep['count'] == 3 for rep in repetitions)

    def test_sentence_normalization(self):
        """文の正規化テスト"""
        generator = SentenceVariationGenerator()

        # 空白があっても同一とみなす
        text = "彼は走った。彼 は 走 っ た。"
        normalized1 = generator._normalize_sentence("彼は走った。")
        normalized2 = generator._normalize_sentence("彼 は 走 っ た。")

        assert normalized1 == normalized2

    def test_different_endings_same_base(self):
        """文末が異なる類似文"""
        generator = SentenceVariationGenerator()
        text = "彼は走った。彼は走った！彼は走った？"
        repetitions = generator.detect_repetitions(text)

        # 文末記号が違っても同一とみなす
        assert len(repetitions) >= 1
        assert repetitions[0]['count'] == 3

    def test_variation_types(self):
        """バリエーションの種類テスト"""
        generator = SentenceVariationGenerator()
        text = "昨日、彼は走った。昨日、彼は走った。"
        repetitions = generator.detect_repetitions(text)

        assert len(repetitions) == 1
        variations = repetitions[0]['variations']

        # 複数の種類のバリエーションが生成される
        types = set(v['type'] for v in variations)
        assert len(types) > 1  # ending_variation, word_order, conjunction など
