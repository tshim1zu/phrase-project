"""
PromptOptimizer のテストスイート
"""

import pytest
from japhrase.prompt_optimizer import PromptOptimizer, optimize_prompt


class TestPromptOptimizer:
    """PromptOptimizerのテスト"""

    @pytest.fixture
    def sample_corpus(self):
        """テスト用コーパス"""
        return [
            "masterpiece, best quality, 1girl, solo, blue eyes, white hair, standing",
            "best quality, high resolution, 1girl, smile, looking at viewer",
            "masterpiece, ultra detailed, scenery, blue sky, cloud, day",
        ]

    @pytest.fixture
    def optimizer_with_corpus(self, sample_corpus):
        return PromptOptimizer(sample_corpus)

    @pytest.fixture
    def optimizer_without_corpus(self):
        return PromptOptimizer()

    def test_init_with_corpus(self, optimizer_with_corpus):
        """コーパス付き初期化テスト"""
        assert optimizer_with_corpus.corpus_prompts is not None
        assert len(optimizer_with_corpus.corpus_prompts) > 0
        assert optimizer_with_corpus.corpus_stats is not None

    def test_init_without_corpus(self, optimizer_without_corpus):
        """コーパスなし初期化テスト"""
        assert optimizer_without_corpus.corpus_prompts == []
        assert optimizer_without_corpus.corpus_stats is None

    def test_analyze_basic(self, optimizer_with_corpus):
        """基本的な分析テスト"""
        prompt = "1girl, blue eyes, standing"
        result = optimizer_with_corpus.analyze(prompt)

        assert 'syntax_issues' in result
        assert 'redundancy_issues' in result
        assert 'suggestions' in result
        assert 'weight_stats' in result
        assert 'token_count' in result
        assert 'quality_score' in result

        assert isinstance(result['syntax_issues'], list)
        assert isinstance(result['suggestions'], list)
        assert 0 <= result['quality_score'] <= 100

    def test_syntax_check_bracket_mismatch(self, optimizer_without_corpus):
        """括弧のバランスチェック"""
        prompt = "1girl, (blue eyes, ((white hair)), standing"
        result = optimizer_without_corpus.analyze(prompt)

        assert len(result['syntax_issues']) > 0
        assert any('括弧' in issue for issue in result['syntax_issues'])

    def test_syntax_check_double_comma(self, optimizer_without_corpus):
        """ダブルコンマチェック"""
        prompt = "1girl, , blue eyes, standing"
        result = optimizer_without_corpus.analyze(prompt)

        assert len(result['syntax_issues']) > 0

    def test_redundancy_detection(self, optimizer_without_corpus):
        """冗長性検出テスト"""
        prompt = "blue eyes, blue, standing, stand"
        result = optimizer_without_corpus.analyze(prompt)

        # 冗長性が検出されるはず
        assert len(result['redundancy_issues']) > 0

    def test_weight_analysis(self, optimizer_without_corpus):
        """重み分析テスト"""
        prompt = "1girl, (blue eyes:1.5), (white hair:1.2), standing"
        result = optimizer_without_corpus.analyze(prompt)

        weight_stats = result['weight_stats']
        assert weight_stats['has_weights'] is True
        assert weight_stats['count'] == 2
        assert 1.0 <= weight_stats['average'] <= 2.0

    def test_weight_warnings(self, optimizer_without_corpus):
        """重み過多の警告"""
        prompt = "1girl, (blue eyes:2.5), (white hair:2.8), standing"
        result = optimizer_without_corpus.analyze(prompt)

        weight_stats = result['weight_stats']
        assert len(weight_stats.get('warnings', [])) > 0

    def test_normalize_tag(self, optimizer_without_corpus):
        """タグ正規化テスト"""
        assert (
            optimizer_without_corpus._normalize_tag("(blue eyes:1.5)")
            == "blue eyes"
        )
        assert (
            optimizer_without_corpus._normalize_tag("(((White Hair)))")
            == "white hair"
        )
        assert optimizer_without_corpus._normalize_tag("  STANDING  ") == "standing"

    def test_quality_score_perfect(self, optimizer_without_corpus):
        """品質スコア（完璧）"""
        prompt = "masterpiece, best quality, 1girl, solo, blue eyes"
        result = optimizer_without_corpus.analyze(prompt)

        # エラーがなければスコアが高い
        assert result['quality_score'] > 70

    def test_quality_score_poor(self, optimizer_without_corpus):
        """品質スコア（低品質）"""
        prompt = "1girl, (((wrong:10)), , blue eyes, blue, eyes"
        result = optimizer_without_corpus.analyze(prompt)

        # エラーが多ければスコアが低い
        assert result['quality_score'] < 70

    def test_suggestions_with_corpus(self, optimizer_with_corpus):
        """コーパスベースの提案"""
        prompt = "1girl, blue eyes"
        result = optimizer_with_corpus.analyze(prompt)

        # コーパスがあれば提案が生成される可能性がある
        suggestions = result['suggestions']
        # 提案がある場合、フォーマットをチェック
        for sug in suggestions:
            assert 'tag' in sug
            assert 'score' in sug
            assert 'reason' in sug

    def test_get_report(self, optimizer_without_corpus):
        """レポート生成テスト"""
        prompt = "1girl, (blue eyes:1.5), standing"
        report = optimizer_without_corpus.get_report(prompt)

        assert isinstance(report, str)
        assert 'プロンプト分析レポート' in report or '品質スコア' in report

    def test_empty_prompt(self, optimizer_without_corpus):
        """空のプロンプト処理"""
        result = optimizer_without_corpus.analyze("")

        assert 'token_count' in result
        assert result['token_count'] == 0

    def test_convenience_function(self, sample_corpus):
        """便利関数テスト"""
        result = optimize_prompt("1girl, blue eyes", sample_corpus)

        assert 'quality_score' in result
        assert isinstance(result, dict)

    def test_corpus_stats(self, optimizer_with_corpus):
        """コーパス統計テスト"""
        stats = optimizer_with_corpus.corpus_stats

        assert stats is not None
        assert 'freq' in stats
        assert 'total_tags' in stats
        assert 'unique_tags' in stats
        assert stats['total_tags'] > 0


class TestPromptQualityScore:
    """品質スコア計算のテスト"""

    def test_score_calculation_no_errors(self):
        """エラーなしの場合"""
        optimizer = PromptOptimizer()
        prompt = "masterpiece, best quality, 1girl, blue eyes, white hair"
        result = optimizer.analyze(prompt)

        # エラーがなければスコアは高い
        assert result['quality_score'] > 80

    def test_score_calculation_with_errors(self):
        """複数のエラーがある場合"""
        optimizer = PromptOptimizer()
        # 括弧ミス + ダブルコンマ + 冗長性
        prompt = "1girl, (blue eyes, , blue, white hair)), standing"
        result = optimizer.analyze(prompt)

        # エラーが多いのでスコアは低い
        assert result['quality_score'] < 80

    def test_score_token_count_bonus(self):
        """トークン数ボーナス"""
        optimizer = PromptOptimizer()
        # 50～150トークン（理想範囲）
        good_tags = ", ".join([f"tag{i}" for i in range(80)])
        result1 = optimizer.analyze(good_tags)

        # 少なすぎる
        few_tags = "tag1, tag2"
        result2 = optimizer.analyze(few_tags)

        # 理想範囲のほうがスコアが高い
        assert result1['quality_score'] > result2['quality_score']


class TestEdgeCases:
    """エッジケーステスト"""

    def test_very_long_prompt(self):
        """非常に長いプロンプト"""
        optimizer = PromptOptimizer()
        long_prompt = ", ".join([f"tag{i}" for i in range(500)])
        result = optimizer.analyze(long_prompt)

        # 処理されるはず
        assert 'quality_score' in result
        # 長すぎるので品質スコアは低い
        assert result['quality_score'] < 80

    def test_special_characters(self):
        """特殊文字を含むプロンプト"""
        optimizer = PromptOptimizer()
        prompt = "1girl, (blue eyes:1.5), [white hair], {standing}"
        result = optimizer.analyze(prompt)

        # 処理されるはず
        assert 'quality_score' in result

    def test_unicode_characters(self):
        """Unicode文字を含むプロンプト"""
        optimizer = PromptOptimizer()
        prompt = "1girl, 青い瞳, white hair, 立っている"
        result = optimizer.analyze(prompt)

        # 処理されるはず
        assert 'token_count' in result
