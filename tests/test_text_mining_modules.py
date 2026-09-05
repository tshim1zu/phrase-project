"""
テキストマイニング・計量文学モジュールのテストスイート
"""

import pytest
import pandas as pd
from japhrase import (
    DialogueAnalyzer,
    OrthographyVariantDetector,
    StylometryAnalyzer,
    CharacterNetworkGenerator,
)


class TestDialogueAnalyzer:
    """DialogueAnalyzerのテスト"""

    @pytest.fixture
    def analyzer(self):
        return DialogueAnalyzer()

    def test_basic_analysis(self, analyzer):
        """基本的な分析テスト"""
        text = """
        「おはよう」と太郎が言った。
        「おはよう」と花子が応じた。
        二人は毎日顔を合わせる。
        """
        result = analyzer.analyze(text)

        assert 'dialogue_ratio' in result
        assert 'narrative_ratio' in result
        assert result['dialogue_count'] > 0
        assert 0.0 <= result['dialogue_ratio'] <= 1.0

    def test_empty_text(self, analyzer):
        """空のテキストの処理"""
        result = analyzer.analyze("")
        assert result['total_characters'] == 0
        assert result['dialogue_ratio'] == 0.0

    def test_no_dialogue(self, analyzer):
        """会話がないテキスト"""
        text = "これは地の文だけのテキストです。会話がありません。"
        result = analyzer.analyze(text)

        assert result['dialogue_count'] == 0
        assert result['dialogue_ratio'] == 0.0

    def test_dialogue_only(self, analyzer):
        """会話のみのテキスト"""
        text = '「これは会話です」「そうですね」'
        result = analyzer.analyze(text)

        assert result['dialogue_count'] > 0
        assert result['dialogue_ratio'] > 0.0

    def test_get_summary(self, analyzer):
        """サマリー出力テスト"""
        text = '「おはよう」と言った。朝だ。'
        result = analyzer.analyze(text)
        summary = analyzer.get_summary(result)

        assert '会話・地の文バランス分析結果' in summary
        assert '会話文比率' in summary


class TestOrthographyVariantDetector:
    """OrthographyVariantDetectorのテスト"""

    @pytest.fixture
    def detector(self):
        return OrthographyVariantDetector(similarity_threshold=0.75)

    def test_init(self, detector):
        """初期化テスト"""
        assert detector.threshold == 0.75

    def test_katakana_variant_detection(self):
        """カタカナ長音のゆれ検出"""
        # このテストはPhraseExtracterに依存するため、
        # 簡単な構文チェックのみ
        detector = OrthographyVariantDetector()
        text = "コンピューターとコンピュータについて。"
        result = detector.check(text)

        # 結果がリストであることを確認
        assert isinstance(result, list)

    def test_empty_text(self):
        """空のテキスト処理"""
        detector = OrthographyVariantDetector()
        result = detector.check("")

        assert isinstance(result, list)
        # 空のテキストではゆれが検出されない
        assert len(result) == 0

    def test_similarity_threshold(self):
        """閾値設定テスト"""
        detector1 = OrthographyVariantDetector(similarity_threshold=0.9)
        detector2 = OrthographyVariantDetector(similarity_threshold=0.5)

        assert detector1.threshold == 0.9
        assert detector2.threshold == 0.5


class TestStylometryAnalyzer:
    """StylometryAnalyzerのテスト"""

    @pytest.fixture
    def analyzer(self):
        return StylometryAnalyzer()

    def test_char_type_ratio_basic(self, analyzer):
        """文字種比率の基本テスト"""
        text = "これは日本語のテキストです。"
        result = analyzer.analyze_char_type_ratio(text)

        assert 'kanji_ratio' in result
        assert 'hiragana_ratio' in result
        assert 'katakana_ratio' in result
        assert 'style_type' in result

        # 合計が1に近い
        total_ratio = (
            result['kanji_ratio']
            + result['hiragana_ratio']
            + result['katakana_ratio']
            + result.get('ascii_ratio', 0)
            + result.get('other_ratio', 0)
        )
        assert 0.99 <= total_ratio <= 1.01

    def test_char_type_empty_text(self, analyzer):
        """空のテキスト"""
        result = analyzer.analyze_char_type_ratio("")
        assert result['kanji_ratio'] == 0.0

    def test_kanji_heavy_text(self, analyzer):
        """漢字が多いテキスト"""
        text = "機械学習は重要です。" * 5
        result = analyzer.analyze_char_type_ratio(text)

        assert result['kanji_ratio'] > 0.3
        assert "論文調" in result['style_type']

    def test_hiragana_heavy_text(self, analyzer):
        """ひらがなが多いテキスト"""
        text = "あああああああああああああああ"
        result = analyzer.analyze_char_type_ratio(text)

        assert result['hiragana_ratio'] > 0.8
        assert "軟質" in result['style_type']

    def test_sentence_length_analysis(self, analyzer):
        """文長分析テスト"""
        text = "短い。これは短い文です。もっと長い文を書いてみます。それは難しい。"
        result = analyzer.analyze_sentence_length(text)

        assert 'avg_length' in result
        assert 'std_length' in result
        assert 'sentence_count' in result
        assert result['sentence_count'] > 0

    def test_vocabulary_richness(self, analyzer):
        """語彙多様性分析テスト"""
        text = "りんごとみかん。りんごはおいしい。みかんもおいしい。"
        result = analyzer.analyze_vocabulary_richness(text)

        assert 'ttr' in result
        assert 'yules_k' in result
        assert 'assessment' in result


class TestCharacterNetworkGenerator:
    """CharacterNetworkGeneratorのテスト"""

    @pytest.fixture
    def generator(self):
        return CharacterNetworkGenerator(window_size=50)

    def test_init(self, generator):
        """初期化テスト"""
        assert generator.window_size == 50

    def test_find_all_positions(self, generator):
        """出現位置検索テスト"""
        text = "abcabcabc"
        positions = generator._find_all_positions(text, "abc")

        assert len(positions) == 3
        assert positions == [0, 3, 6]

    def test_generate_edgelist_basic(self, generator):
        """基本的なネットワーク生成テスト"""
        text = "太郎と花子が会った。太郎は元気だ。花子も元気だ。"
        df_edges = generator.generate_edgelist(text, top_n_nodes=5)

        # DataFrameの構造確認
        assert isinstance(df_edges, pd.DataFrame)
        if not df_edges.empty:
            assert 'source' in df_edges.columns
            assert 'target' in df_edges.columns
            assert 'weight' in df_edges.columns

    def test_empty_text(self, generator):
        """空のテキスト処理"""
        df_edges = generator.generate_edgelist("")

        assert isinstance(df_edges, pd.DataFrame)
        assert df_edges.empty

    def test_network_stats(self, generator):
        """ネットワーク統計計算テスト"""
        # ダミーエッジリスト
        df_edges = pd.DataFrame(
            {
                'source': ['A', 'B', 'C'],
                'target': ['B', 'C', 'A'],
                'weight': [1.0, 2.0, 1.5],
            }
        )
        stats = generator.get_network_stats(df_edges)

        assert stats['node_count'] == 3
        assert stats['edge_count'] == 3
        assert 'avg_weight' in stats
        assert 'density_est' in stats

    def test_important_nodes(self, generator):
        """重要ノード検出テスト"""
        df_edges = pd.DataFrame(
            {
                'source': ['A', 'A', 'B', 'C'],
                'target': ['B', 'C', 'C', 'D'],
                'weight': [1.0, 1.0, 2.0, 1.0],
            }
        )
        df_nodes = generator.get_important_nodes(df_edges, top_n=3)

        assert isinstance(df_nodes, pd.DataFrame)
        # Aは2つのエッジを持つ
        if not df_nodes.empty:
            assert 'node' in df_nodes.columns
            assert 'degree' in df_nodes.columns


class TestIntegration:
    """統合テスト"""

    def test_full_analysis_workflow(self):
        """全モジュール統合テスト"""
        text = """
        「おはよう」と太郎が言った。
        「おはよう」と花子が応じた。
        二人は毎日のように会う。
        太郎と花子の関係は良好である。
        """

        # 1. DialogueAnalyzer
        analyzer = DialogueAnalyzer()
        result1 = analyzer.analyze(text)
        assert result1['dialogue_count'] > 0

        # 2. StylometryAnalyzer
        stylo = StylometryAnalyzer()
        result2 = stylo.analyze_char_type_ratio(text)
        assert result2['kanji_ratio'] > 0

        # 3. CharacterNetworkGenerator
        net = CharacterNetworkGenerator()
        df_edges = net.generate_edgelist(text, top_n_nodes=5)
        assert isinstance(df_edges, pd.DataFrame)

    def test_error_handling(self):
        """エラーハンドリングテスト"""
        # 各モジュールが異常系に対応しているか確認

        # None をテキストとして渡す試みをしないようにする
        analyzer = DialogueAnalyzer()
        # 空文字列は処理される
        result = analyzer.analyze("")
        assert 'total_characters' in result
