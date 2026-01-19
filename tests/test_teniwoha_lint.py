"""
てにをはLintのテスト
"""

import pytest
from japhrase.teniwoha_lint import TeniwohaLinter


class TestTeniwohaLinter:
    """TeniwohaLinterクラスのテスト"""

    def test_no_issues(self):
        """問題のないテキストのテスト"""
        linter = TeniwohaLinter()
        text = "私は学校に行きました。彼は本を読んでいます。"
        issues = linter.check(text)
        assert len(issues) == 0

    def test_particle_repetition_ga(self):
        """「が」の繰り返し検出"""
        linter = TeniwohaLinter()
        text = "彼女が私が好きだと言った。"
        issues = linter.check(text)

        assert len(issues) >= 1
        assert any(issue['type'] == 'particle_repetition' for issue in issues)
        assert any('が' in issue.get('particle', '') for issue in issues)

    def test_particle_repetition_wo(self):
        """「を」の繰り返し検出"""
        linter = TeniwohaLinter()
        text = "リンゴを彼女を見ながら食べた。"
        issues = linter.check(text)

        assert len(issues) >= 1
        assert any(issue['type'] == 'particle_repetition' for issue in issues)

    def test_particle_sequence_wo_wo(self):
        """「をを」の連続検出"""
        linter = TeniwohaLinter()
        text = "本をを読みました。"  # 誤字の例
        issues = linter.check(text)

        assert len(issues) >= 1
        assert any(issue['type'] == 'particle_sequence' for issue in issues)
        assert any(issue['severity'] == 'error' for issue in issues)

    def test_particle_overuse(self):
        """助詞の過剰使用検出"""
        linter = TeniwohaLinter()
        # 「が」を4回使用
        text = "私が彼が彼女が誰かが言っていたことを聞いた。"
        issues = linter.check(text)

        overuse_issues = [i for i in issues if i['type'] == 'particle_overuse']
        assert len(overuse_issues) >= 1
        assert any(issue.get('particle') == 'が' for issue in overuse_issues)

    def test_particle_overuse_strict_mode(self):
        """厳格モードでの助詞過剰使用検出"""
        linter = TeniwohaLinter(strict_mode=True)
        # 「が」を3回使用（20文字以上にする）
        text = "私が昨日に彼が知っている彼女が言っていた話を聞いた。"
        issues = linter.check(text)

        overuse_issues = [i for i in issues if i['type'] == 'particle_overuse']
        assert len(overuse_issues) >= 1

    def test_multiple_sentences(self):
        """複数文のテスト"""
        linter = TeniwohaLinter()
        text = "彼が私が好きです。彼女は優しいです。"
        issues = linter.check(text)

        # 最初の文で「が」の繰り返しが検出される
        assert len(issues) >= 1

    def test_format_issues_empty(self):
        """問題なしの場合のフォーマット"""
        linter = TeniwohaLinter()
        issues = []
        result = linter.format_issues(issues)

        assert "問題は見つかりませんでした" in result

    def test_format_issues_with_text(self):
        """問題ありの場合のフォーマット（テキスト付き）"""
        linter = TeniwohaLinter()
        text = "彼女が私が好きだと言った。"
        issues = linter.check(text)
        result = linter.format_issues(issues, text)

        assert "件の問題が見つかりました" in result
        assert "が" in result

    def test_check_and_report(self):
        """check_and_reportの統合テスト"""
        linter = TeniwohaLinter()
        text = "本をを読みました。"
        result = linter.check_and_report(text)

        assert isinstance(result, str)
        assert len(result) > 0

    def test_short_sentence_skip(self):
        """短い文は過剰使用チェックをスキップ"""
        linter = TeniwohaLinter()
        text = "私が行く。"  # 短い文
        issues = linter.check(text)

        # 短い文なので過剰使用の警告は出ない
        overuse_issues = [i for i in issues if i['type'] == 'particle_overuse']
        assert len(overuse_issues) == 0

    def test_position_tracking(self):
        """位置情報の正確性テスト"""
        linter = TeniwohaLinter()
        text = "彼女が私が好きだ。"
        issues = linter.check(text)

        assert len(issues) >= 1
        for issue in issues:
            assert 'position' in issue
            start, end = issue['position']
            assert 0 <= start < len(text)
            assert start < end <= len(text)

    def test_complex_text(self):
        """複雑なテキストのテスト"""
        linter = TeniwohaLinter()
        text = """
        私が昨日が行った店が美味しかった。
        彼は本を読んでいます。
        彼女が私が彼が好きだと言った話を聞いた。
        """
        issues = linter.check(text)

        # 複数の問題が検出されるはず
        assert len(issues) >= 2

    def test_particle_with_spacing(self):
        """助詞の間に文字がある場合の検出"""
        linter = TeniwohaLinter()
        text = "私が昨日が好きだった。"
        issues = linter.check(text)

        # 「が」が近接して使用されている
        repetition_issues = [i for i in issues if i['type'] == 'particle_repetition']
        assert len(repetition_issues) >= 1
