# coding: utf-8
"""
応用機能群（japhrase.applied）の統合テスト

6モジュール全てのインポート・基本動作を検証。
"""

import pytest
from japhrase.applied import (
    PreflightChecker, PreflightResult,
    EPDashboard, DashboardResult,
    HabitDriftDetector, DriftResult,
    JPENDivergenceChecker, DivergenceResult,
    CharacterStylometry, CharacterFingerprint,
    PartHealthReport, HealthGrade,
)

# 共通テストデータ
SAMPLE_EPS = {
    'EP101': 'エリスは目を覚ました。見知らぬ部屋だった。「ここはどこ？」エリスは呟いた。レティシアが入ってきた。「おはようございます、姫様」レティシアは微笑んだ。',
    'EP102': 'ソフィアの研究室。「データを確認しましょう」ソフィアはモニターを見つめた。エリスは不安そうに立っていた。「大丈夫ですか？」ソフィアが聞いた。',
    'EP103': 'レティシアは剣を構えた。近衛騎士団の訓練場だった。「もう一度」レティシアは言った。エリスはその姿を眺めていた。',
}

CHARACTERS = ['エリス', 'レティシア', 'ソフィア']


class TestPreflightChecker:
    """公開前プリフライト"""

    def test_basic_check(self):
        checker = PreflightChecker()
        result = checker.check(SAMPLE_EPS['EP101'], lang='jp')
        assert isinstance(result, PreflightResult)
        assert result.verdict in ('GO', 'WARN', 'NOGO')
        assert 0 <= result.quality_score <= 100

    def test_report_generation(self):
        checker = PreflightChecker()
        result = checker.check(SAMPLE_EPS['EP101'], lang='jp')
        report = result.report()
        assert 'プリフライト' in report

    def test_platform_check(self):
        """PF別文字数チェック"""
        checker = PreflightChecker()
        result = checker.check('短い。', lang='jp', platform='sh')
        # SH最小1000字 → NOGO
        assert result.verdict == 'NOGO'
        assert any('最小文字数' in r for r in result.nogo_reasons)

    def test_skip_detection(self):
        """SKIP検出"""
        checker = PreflightChecker()
        text = 'エリスは3.5時間走った。体温は36.5度だった。速度は10km/hだった。'
        result = checker.check(text, lang='jp')
        assert result.skip_count >= 2

    def test_en_check(self):
        """EN版チェック"""
        checker = PreflightChecker()
        result = checker.check('Eris woke up in an unfamiliar room. Where am I?', lang='en')
        assert isinstance(result, PreflightResult)


class TestEPDashboard:
    """EP間比較ダッシュボード"""

    def test_basic_analysis(self):
        dashboard = EPDashboard()
        result = dashboard.analyze(SAMPLE_EPS)
        assert isinstance(result, DashboardResult)
        assert len(result.snapshots) == 3
        assert result.total_unique_vocab > 0

    def test_adjacent_distances(self):
        dashboard = EPDashboard()
        result = dashboard.analyze(SAMPLE_EPS)
        assert len(result.adjacent_distances) == 2  # 3 EPs → 2 pairs

    def test_trends(self):
        dashboard = EPDashboard()
        result = dashboard.analyze(SAMPLE_EPS)
        assert isinstance(result.mattr_trend, float)
        assert isinstance(result.entropy_trend, float)

    def test_report(self):
        dashboard = EPDashboard()
        result = dashboard.analyze(SAMPLE_EPS)
        report = result.report()
        assert 'ダッシュボード' in report

    def test_weakest_strongest(self):
        dashboard = EPDashboard()
        result = dashboard.analyze(SAMPLE_EPS)
        assert result.weakest_ep is not None
        assert result.strongest_ep is not None


class TestHabitDriftDetector:
    """書き癖ドリフト"""

    def test_basic_detection(self):
        detector = HabitDriftDetector(min_total_freq=2)
        result = detector.analyze(SAMPLE_EPS)
        assert isinstance(result, DriftResult)
        assert isinstance(result.habits, list)

    def test_report(self):
        detector = HabitDriftDetector(min_total_freq=2)
        result = detector.analyze(SAMPLE_EPS)
        report = result.report()
        assert 'ドリフト' in report

    def test_compare_parts(self):
        detector = HabitDriftDetector(min_total_freq=2)
        report = detector.compare_parts(
            {'EP1': SAMPLE_EPS['EP101'], 'EP2': SAMPLE_EPS['EP102']},
            {'EP3': SAMPLE_EPS['EP103']},
            'PartA', 'PartB',
        )
        assert 'Part 間比較' in report


class TestJPENDivergence:
    """JP↔EN品質乖離"""

    def test_basic_pair(self):
        checker = JPENDivergenceChecker()
        result = checker.check_pair(
            SAMPLE_EPS['EP101'],
            'Eris woke up. An unfamiliar room. Where am I? Leticia entered.',
            'EP101',
        )
        assert isinstance(result, DivergenceResult)
        assert result.verdict in ('good', 'acceptable', 'degraded', 'critical')
        assert 0 <= result.translation_loss <= 1

    def test_series(self):
        checker = JPENDivergenceChecker()
        jp = {'EP101': SAMPLE_EPS['EP101'], 'EP102': SAMPLE_EPS['EP102']}
        en = {
            'EP101': 'Eris woke up. Where am I?',
            'EP102': 'Sofia lab. Let me check the data.',
        }
        series = checker.check_series(jp, en)
        assert len(series.results) == 2
        assert series.worst_ep is not None

    def test_series_report(self):
        checker = JPENDivergenceChecker()
        jp = {'EP101': SAMPLE_EPS['EP101']}
        en = {'EP101': 'Eris woke up. Unfamiliar room.'}
        series = checker.check_series(jp, en)
        report = series.report()
        assert '品質乖離' in report


class TestCharacterStylometry:
    """キャラ文体指紋"""

    def test_build_fingerprints(self):
        cs = CharacterStylometry(min_speech_chars=5)
        fps = cs.build_fingerprints(SAMPLE_EPS, CHARACTERS)
        assert isinstance(fps, dict)
        # 台詞が少ないキャラはスキップされる可能性あり
        assert len(fps) >= 1

    def test_comparison(self):
        cs = CharacterStylometry(min_speech_chars=5)
        fps = cs.build_fingerprints(SAMPLE_EPS, CHARACTERS)
        if len(fps) >= 2:
            chars = list(fps.keys())
            comp = cs.compare_characters(fps, chars[0], chars[1])
            assert 0 <= comp.separation_score <= 1

    def test_full_report(self):
        cs = CharacterStylometry(min_speech_chars=5)
        fps = cs.build_fingerprints(SAMPLE_EPS, CHARACTERS)
        report = cs.full_report(fps)
        assert '文体指紋' in report


class TestPartHealthReport:
    """Part健康診断"""

    def test_basic_diagnosis(self):
        report = PartHealthReport()
        grade = report.diagnose(SAMPLE_EPS, part_label='TestPart')
        assert isinstance(grade, HealthGrade)
        assert grade.overall_grade in ('A', 'B', 'C', 'D', 'E')
        assert 0 <= grade.overall_score <= 100

    def test_with_characters(self):
        report = PartHealthReport()
        grade = report.diagnose(
            SAMPLE_EPS,
            characters=CHARACTERS,
            part_label='TestPart',
        )
        assert len(grade.sections) == 6

    def test_report_output(self):
        report = PartHealthReport()
        grade = report.diagnose(SAMPLE_EPS, part_label='TestPart')
        text = grade.report()
        assert '健康診断' in text
        assert 'TestPart' in text

    def test_grade_mapping(self):
        """スコア→グレード変換"""
        from japhrase.applied.part_health import _score_to_grade
        assert _score_to_grade(90) == 'A'
        assert _score_to_grade(75) == 'B'
        assert _score_to_grade(60) == 'C'
        assert _score_to_grade(45) == 'D'
        assert _score_to_grade(30) == 'E'
