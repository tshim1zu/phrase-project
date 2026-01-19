"""
冗長語尾のヒートマップのテスト
"""

import pytest
from japhrase.ending_heatmap import EndingHeatmapGenerator


class TestEndingHeatmapGenerator:
    """EndingHeatmapGeneratorクラスのテスト"""

    def test_basic_analysis(self):
        """基本的な分析"""
        generator = EndingHeatmapGenerator(chunk_size=3)
        text = "私は走った。彼は歩いた。彼女は飛んだ。"
        analysis = generator.analyze(text)

        assert 'chunk_analysis' in analysis
        assert 'overall_stats' in analysis
        assert analysis['total_sentences'] == 3

    def test_past_plain_detection(self):
        """過去形の検出"""
        generator = EndingHeatmapGenerator()
        text = "私は走った。彼は歩いた。彼女は飛んだ。"
        analysis = generator.analyze(text)

        stats = analysis['overall_stats']
        assert 'past_plain' in stats['pattern_counts']
        assert stats['pattern_counts']['past_plain'] == 3

    def test_past_progressive_detection(self):
        """過去進行形の検出"""
        generator = EndingHeatmapGenerator()
        text = "私は走っていた。彼は歩いていた。彼女は飛んでいた。"
        analysis = generator.analyze(text)

        stats = analysis['overall_stats']
        assert 'past_progressive' in stats['pattern_counts']
        assert stats['pattern_counts']['past_progressive'] == 3

    def test_dearu_detection(self):
        """である調の検出"""
        generator = EndingHeatmapGenerator()
        text = "これは本である。それは車である。あれは家である。"
        analysis = generator.analyze(text)

        stats = analysis['overall_stats']
        assert 'present_dearu' in stats['pattern_counts']
        assert stats['pattern_counts']['present_dearu'] == 3

    def test_chunk_creation(self):
        """チャンク作成のテスト"""
        generator = EndingHeatmapGenerator(chunk_size=2)
        sentences = ["文1。", "文2。", "文3。", "文4。", "文5。"]
        chunks = generator._create_chunks(sentences)

        assert len(chunks) == 3
        assert len(chunks[0]) == 2
        assert len(chunks[1]) == 2
        assert len(chunks[2]) == 1

    def test_diversity_score(self):
        """多様性スコアの計算"""
        generator = EndingHeatmapGenerator()
        # 多様な文末表現
        text = "私は走った。彼は歩いている。これは本である。それは良いです。"
        analysis = generator.analyze(text)

        stats = analysis['overall_stats']
        assert stats['diversity_score'] > 0
        # 複数のパターンが使用されているので多様性が高い
        assert len(stats['pattern_counts']) >= 3

    def test_diversity_score_low(self):
        """多様性スコアが低い場合"""
        generator = EndingHeatmapGenerator()
        # 同じ文末表現ばかり
        text = "私は走った。彼は歩いた。彼女は飛んだ。"
        analysis = generator.analyze(text)

        stats = analysis['overall_stats']
        # 1種類のパターンのみ
        assert len(stats['pattern_counts']) == 1

    def test_detect_issues_chunk_overuse(self):
        """チャンク内での過剰使用検出"""
        generator = EndingHeatmapGenerator(chunk_size=3)
        # 同じパターンが集中
        text = "私は走った。彼は歩いた。彼女は飛んだ。"
        analysis = generator.analyze(text)
        issues = generator.detect_issues(analysis, threshold=0.6)

        # 閾値を超えているので問題が検出される
        assert len(issues) >= 1
        assert any(issue['type'] == 'pattern_overuse_in_chunk' for issue in issues)

    def test_detect_issues_overall_overuse(self):
        """全体での過剰使用検出"""
        generator = EndingHeatmapGenerator()
        # 全体で同じパターン
        text = "私は走った。彼は歩いた。彼女は飛んだ。犬は寝た。猫は遊んだ。"
        analysis = generator.analyze(text)
        issues = generator.detect_issues(analysis, threshold=0.6)

        # 全体で過剰使用
        overall_issues = [i for i in issues if i['type'] == 'pattern_overuse_overall']
        assert len(overall_issues) >= 1

    def test_detect_issues_no_issues(self):
        """問題なしの場合"""
        generator = EndingHeatmapGenerator()
        # 多様な文末表現
        text = "私は走った。彼は歩いている。これは本である。それは良いです。"
        analysis = generator.analyze(text)
        issues = generator.detect_issues(analysis, threshold=0.8)

        # 閾値が高いので検出されない
        assert len(issues) == 0

    def test_format_heatmap(self):
        """ヒートマップのフォーマット"""
        generator = EndingHeatmapGenerator()
        text = "私は走った。彼は歩いた。彼女は飛んだ。"
        analysis = generator.analyze(text)
        heatmap = generator.format_heatmap(analysis)

        assert isinstance(heatmap, str)
        assert "文末表現ヒートマップ" in heatmap
        assert "全体統計" in heatmap
        assert "チャンク別分析" in heatmap

    def test_analyze_and_report(self):
        """統合メソッドのテスト"""
        generator = EndingHeatmapGenerator()
        text = "私は走った。彼は歩いた。彼女は飛んだ。犬は寝た。猫は遊んだ。"
        report = generator.analyze_and_report(text)

        assert isinstance(report, str)
        assert len(report) > 0

    def test_multiple_chunks(self):
        """複数チャンクの分析"""
        generator = EndingHeatmapGenerator(chunk_size=2)
        text = "私は走った。彼は歩いた。彼女は飛んだ。犬は寝た。猫は遊んだ。鳥は鳴いた。"
        analysis = generator.analyze(text)

        assert analysis['total_chunks'] == 3
        assert len(analysis['chunk_analysis']) == 3

    def test_mixed_patterns(self):
        """混在するパターンの分析"""
        generator = EndingHeatmapGenerator()
        text = """
        私は走った。彼は歩いている。これは本である。
        それは良いです。彼女は飛んでいた。
        """
        analysis = generator.analyze(text)

        stats = analysis['overall_stats']
        # 複数のパターンが検出される
        assert len(stats['pattern_counts']) >= 4

    def test_dominant_pattern(self):
        """支配的なパターンの検出"""
        generator = EndingHeatmapGenerator()
        text = "私は走った。彼は歩いた。彼女は飛んだ。犬は寝た。"
        analysis = generator.analyze(text)

        stats = analysis['overall_stats']
        assert stats['dominant_pattern'] is not None
        assert stats['dominant_pattern'][0] == 'past_plain'
        assert stats['dominant_pattern'][1] == 4

    def test_empty_text(self):
        """空のテキスト"""
        generator = EndingHeatmapGenerator()
        text = ""
        analysis = generator.analyze(text)

        assert analysis['total_sentences'] == 0
        assert analysis['total_chunks'] == 0

    def test_heatmap_bar_generation(self):
        """ヒートマップバーの生成"""
        generator = EndingHeatmapGenerator(chunk_size=5)

        assert generator._get_heatmap_bar(0, 5) == "    "
        assert generator._get_heatmap_bar(1, 5) == " ░  "
        assert generator._get_heatmap_bar(2, 5) == " ▒  "
        assert generator._get_heatmap_bar(4, 5) == " ▓  "
        assert generator._get_heatmap_bar(5, 5) == " █  "

    def test_sentence_splitting(self):
        """文の分割テスト"""
        generator = EndingHeatmapGenerator()
        text = "私は走った。彼は歩いた！彼女は飛んだ？"
        sentences = generator._split_sentences(text)

        assert len(sentences) == 3
        assert "走った。" in sentences[0]
        assert "歩いた！" in sentences[1]
        assert "飛んだ？" in sentences[2]
