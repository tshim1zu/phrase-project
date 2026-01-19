"""
主語/視点ブレ検出のテスト
"""

import pytest
from japhrase.subject_pov_detector import SubjectPOVDetector


class TestSubjectPOVDetector:
    """SubjectPOVDetectorクラスのテスト"""

    def test_no_issues(self):
        """問題のないテキスト"""
        detector = SubjectPOVDetector()
        text = "私は学校に行きました。私は本を読みました。私は帰宅しました。"
        issues = detector.check(text)
        assert len(issues) == 0

    def test_subject_changes(self):
        """主語の頻繁な変化を検出"""
        detector = SubjectPOVDetector(sensitivity='medium')
        text = "私は走った。彼は歩いた。彼女は飛んだ。猫は寝た。"
        issues = detector.check(text)

        subject_issues = [i for i in issues if i['type'] == 'subject_changes']
        assert len(subject_issues) >= 1
        assert subject_issues[0]['change_count'] >= 3

    def test_pov_inconsistency(self):
        """視点の不一致を検出"""
        detector = SubjectPOVDetector(sensitivity='medium')
        text = "私は走った。彼は歩いた。私は疲れた。彼は元気だった。"
        issues = detector.check(text)

        pov_issues = [i for i in issues if i['type'] == 'pov_inconsistency']
        assert len(pov_issues) >= 1

    def test_extract_subject_wa(self):
        """「は」による主語抽出"""
        detector = SubjectPOVDetector()
        subject = detector._extract_subject("彼は走った。")
        assert subject == "彼"

    def test_extract_subject_ga(self):
        """「が」による主語抽出"""
        detector = SubjectPOVDetector()
        subject = detector._extract_subject("猫が鳴いた。")
        assert subject == "猫"

    def test_extract_subject_mo(self):
        """「も」による主語抽出"""
        detector = SubjectPOVDetector()
        subject = detector._extract_subject("彼女も行った。")
        assert subject == "彼女"

    def test_extract_subject_none(self):
        """主語がない場合"""
        detector = SubjectPOVDetector()
        subject = detector._extract_subject("走った。")
        assert subject is None

    def test_get_pov_first_person(self):
        """一人称の検出"""
        detector = SubjectPOVDetector()
        pov = detector._get_pov("私は走った。")
        assert pov == 'first'

    def test_get_pov_second_person(self):
        """二人称の検出"""
        detector = SubjectPOVDetector()
        pov = detector._get_pov("あなたは優しい。")
        assert pov == 'second'

    def test_get_pov_third_person(self):
        """三人称の検出"""
        detector = SubjectPOVDetector()
        pov = detector._get_pov("彼は走った。")
        assert pov == 'third'

    def test_get_pov_none(self):
        """視点が判定できない場合"""
        detector = SubjectPOVDetector()
        pov = detector._get_pov("走った。")
        assert pov is None

    def test_sensitivity_high(self):
        """高感度での検出"""
        detector = SubjectPOVDetector(sensitivity='high')
        text = "私は走った。彼は歩いた。彼女は飛んだ。"
        issues = detector.check(text)

        # 高感度なので検出される
        assert len(issues) >= 1

    def test_sensitivity_low(self):
        """低感度での検出"""
        detector = SubjectPOVDetector(sensitivity='low')
        text = "私は走った。彼は歩いた。彼女は飛んだ。"
        issues = detector.check(text)

        # 低感度なので検出されにくい
        subject_issues = [i for i in issues if i['type'] == 'subject_changes']
        # 変化が少ないので検出されない可能性が高い
        assert len(subject_issues) == 0

    def test_analyze_text(self):
        """テキスト統計分析"""
        detector = SubjectPOVDetector()
        text = "私は走った。私は歩いた。彼は飛んだ。"
        stats = detector.analyze_text(text)

        assert stats['total_sentences'] == 3
        assert stats['subject_count'] == 3
        assert stats['subject_variety'] >= 2  # 「私」と「彼」

    def test_analyze_text_pov_distribution(self):
        """視点分布の分析"""
        detector = SubjectPOVDetector()
        text = "私は走った。私は歩いた。彼は飛んだ。"
        stats = detector.analyze_text(text)

        assert 'pov_distribution' in stats
        assert 'first' in stats['pov_distribution']
        assert 'third' in stats['pov_distribution']

    def test_analyze_text_dominant_pov(self):
        """主要視点の検出"""
        detector = SubjectPOVDetector()
        text = "私は走った。私は歩いた。私は飛んだ。"
        stats = detector.analyze_text(text)

        assert stats['dominant_pov'] == 'first'

    def test_format_issues_empty(self):
        """問題なしのフォーマット"""
        detector = SubjectPOVDetector()
        issues = []
        report = detector.format_issues(issues)

        assert "ブレは見つかりませんでした" in report

    def test_format_issues_with_text(self):
        """問題ありのフォーマット"""
        detector = SubjectPOVDetector(sensitivity='medium')
        text = "私は走った。彼は歩いた。彼女は飛んだ。猫は寝た。"
        issues = detector.check(text)
        report = detector.format_issues(issues, text)

        assert "件の問題が見つかりました" in report

    def test_check_and_report(self):
        """統合メソッドのテスト"""
        detector = SubjectPOVDetector()
        text = "私は走った。彼は歩いた。彼女は飛んだ。猫は寝た。"
        report = detector.check_and_report(text)

        assert isinstance(report, str)
        assert len(report) > 0

    def test_paragraph_based_detection(self):
        """段落ベースの検出"""
        detector = SubjectPOVDetector(sensitivity='medium')
        text = """
        私は走った。彼は歩いた。彼女は飛んだ。犬は寝た。

        猫は吠えた。鳥は鳴いた。魚は泳いだ。蛇は這った。
        """
        issues = detector.check(text)

        # 各段落で検出される（4文以上で主語が3回以上変化）
        assert len(issues) >= 1

    def test_short_paragraph_skip(self):
        """短い段落はスキップ"""
        detector = SubjectPOVDetector()
        text = "私は走った。彼は歩いた。"  # 2文のみ
        issues = detector.check(text)

        # 3文未満なので主語変化は検出されない
        subject_issues = [i for i in issues if i['type'] == 'subject_changes']
        assert len(subject_issues) == 0

    def test_consistent_subject(self):
        """一貫した主語"""
        detector = SubjectPOVDetector()
        text = "彼は走った。彼は歩いた。彼は飛んだ。彼は寝た。"
        issues = detector.check(text)

        # 主語が一貫しているので検出されない
        subject_issues = [i for i in issues if i['type'] == 'subject_changes']
        assert len(subject_issues) == 0

    def test_position_tracking(self):
        """位置情報の追跡"""
        detector = SubjectPOVDetector(sensitivity='medium')
        text = "私は走った。彼は歩いた。彼女は飛んだ。猫は寝た。"
        issues = detector.check(text)

        if len(issues) > 0:
            assert 'position' in issues[0]
            start, end = issues[0]['position']
            assert 0 <= start < len(text)
            assert start < end <= len(text)
