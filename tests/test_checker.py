"""
品質チェッカーのテスト
"""

import pytest
from japhrase import QualityChecker


class TestQualityChecker:
    """QualityChecker クラスのテスト"""

    def test_check_forbidden_phrases_found(self):
        """禁止ワードが見つかる"""
        text = "これは悪い表現です。とても悪いです。"
        checker = QualityChecker(text)
        result = checker.check_forbidden_phrases(['悪い', '最悪'])

        assert result is False  # エラーがある
        assert len(checker.errors) >= 1
        assert checker.errors[0]['type'] == 'forbidden_phrase'

    def test_check_forbidden_phrases_not_found(self):
        """禁止ワードが見つからない"""
        text = "これは良い表現です。とても良いです。"
        checker = QualityChecker(text)
        result = checker.check_forbidden_phrases(['悪い', '最悪'])

        assert result is True  # エラーなし
        assert len(checker.errors) == 0

    def test_check_required_keywords_sufficient(self):
        """必須キーワードが十分"""
        text = "機械学習は重要です。機械学習は有用です。機械学習は発展中です。"
        checker = QualityChecker(text)
        result = checker.check_required_keywords({'機械学習': 2})

        assert result is True  # 警告なし
        assert len(checker.warnings) == 0

    def test_check_required_keywords_insufficient(self):
        """必須キーワードが不足"""
        text = "機械学習は重要です。"
        checker = QualityChecker(text)
        result = checker.check_required_keywords({'機械学習': 3})

        assert result is False  # 警告あり
        assert len(checker.warnings) == 1
        assert checker.warnings[0]['type'] == 'missing_keyword'

    def test_check_spelling_consistency_found(self):
        """表記ゆれが見つかる"""
        text = "ユーザー登録とユーザ管理があります。"
        checker = QualityChecker(text)
        rules = {'ユーザー': ['ユーザ']}
        result = checker.check_spelling_consistency(rules)

        assert result is False  # エラーあり
        assert len(checker.errors) >= 1
        assert checker.errors[0]['type'] == 'spelling_inconsistency'

    def test_check_spelling_consistency_not_found(self):
        """表記ゆれがない"""
        text = "アルゴリズムの実装があります。"
        checker = QualityChecker(text)
        # algo は見つからない
        rules = {'アルゴリズム': ['algo']}
        result = checker.check_spelling_consistency(rules)

        assert result is True  # エラーなし
        assert len(checker.errors) == 0

    def test_check_length_limits_too_short(self):
        """文書が短すぎる"""
        text = "短い"
        checker = QualityChecker(text)
        result = checker.check_length_limits(min_length=100)

        assert result is False
        assert len(checker.errors) == 1
        assert checker.errors[0]['type'] == 'too_short'

    def test_check_length_limits_too_long(self):
        """文書が長すぎる"""
        text = "a" * 1000
        checker = QualityChecker(text)
        result = checker.check_length_limits(max_length=100)

        assert result is False
        assert len(checker.errors) == 1
        assert checker.errors[0]['type'] == 'too_long'

    def test_check_length_limits_ok(self):
        """文書の長さが適切"""
        text = "a" * 100
        checker = QualityChecker(text)
        result = checker.check_length_limits(min_length=50, max_length=150)

        assert result is True
        assert len(checker.errors) == 0

    def test_check_paragraph_structure_sufficient(self):
        """段落数が十分"""
        text = "段落1\n\n段落2\n\n段落3"
        checker = QualityChecker(text)
        result = checker.check_paragraph_structure(min_paragraphs=2)

        assert result is True
        assert len(checker.warnings) == 0

    def test_check_paragraph_structure_insufficient(self):
        """段落数が不足"""
        text = "段落1"
        checker = QualityChecker(text)
        result = checker.check_paragraph_structure(min_paragraphs=3)

        assert result is False
        assert len(checker.warnings) == 1
        assert checker.warnings[0]['type'] == 'few_paragraphs'

    def test_run_all_checks_success(self):
        """すべてのチェックに合格"""
        text = "これは良い文書です。\n\nとても良い文書です。\n\n本当に良い文書です。"
        config = {
            'check': {
                'forbidden_phrases': ['悪い'],
                'required_keywords': {'良い': 1},
                'spelling_rules': {'文書': ['ぶんしょ']},
                'min_length': 10,
                'max_length': 1000,
                'min_paragraphs': 2
            }
        }
        checker = QualityChecker(text, config)
        success, errors, warnings = checker.run_all_checks()

        assert success is True
        assert len(errors) == 0

    def test_run_all_checks_with_errors(self):
        """チェック失敗"""
        text = "これは悪い文書です。"
        config = {
            'check': {
                'forbidden_phrases': ['悪い'],
                'required_keywords': {'必須': 1},
            }
        }
        checker = QualityChecker(text, config)
        success, errors, warnings = checker.run_all_checks()

        assert success is False
        assert len(errors) >= 1

    def test_run_all_checks_no_rules_configured_is_failure(self):
        """[check]セクションはあるがルールキーが1つも無い場合は失敗扱いにすること

        回帰テスト: 以前はルールが1つも実行されない(=self.errorsが空のまま)ため
        無条件で success=True になり、「何もチェックしていないのに合格」という
        fail-openバグになっていた。
        """
        text = "何が書いてあってもチェックされないはずのテキスト"
        config = {'check': {}}
        checker = QualityChecker(text, config)
        success, errors, warnings = checker.run_all_checks()

        assert success is False
        assert len(errors) >= 1

    def test_run_all_checks_missing_check_section_is_failure(self):
        """設定ファイル自体は非空だが[check]セクションが存在しない場合も失敗扱いにすること"""
        text = "テキスト"
        config = {'extract': {'min_count': 3}}  # [check]以外のセクションのみ
        checker = QualityChecker(text, config)
        success, errors, warnings = checker.run_all_checks()

        assert success is False
        assert len(errors) >= 1

    def test_run_all_checks_explicitly_disabled_is_success(self):
        """check.enabled=falseで明示的に無効化した場合は合法的なスキップとして成功扱いにすること

        暗黙の0件ルール(エラー)と、明示的な無効化(成功だが警告で明示)を区別する。
        """
        text = "テキスト"
        config = {'check': {'enabled': False}}
        checker = QualityChecker(text, config)
        success, errors, warnings = checker.run_all_checks()

        assert success is True
        assert len(errors) == 0
        assert len(warnings) >= 1

    def test_get_report(self):
        """レポート生成"""
        text = "テキスト"
        checker = QualityChecker(text)
        checker.errors.append({'message': 'エラーです'})
        checker.warnings.append({'message': '警告です'})

        report = checker.get_report()
        assert '品質チェック' in report
        assert 'エラーです' in report
        assert '警告です' in report
        assert len(report) > 0

    def test_case_insensitive_matching(self):
        """大文字小文字を区別しない検索"""
        text = "User and USER and user"
        checker = QualityChecker(text)
        result = checker.check_required_keywords({'user': 3})

        assert result is True  # すべてマッチ

    def test_empty_text(self):
        """空のテキスト"""
        text = ""
        checker = QualityChecker(text)
        result = checker.check_forbidden_phrases(['test'])

        assert result is True  # エラーなし
        assert len(checker.errors) == 0
