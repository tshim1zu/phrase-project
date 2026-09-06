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

    def test_worsening_improving_counts_are_not_biased_by_top_n(self):
        """回帰テスト: worsening_count/improving_countはtop_nによる表示件数の
        制限を受けないこと。

        以前は「悪化度順にソート→top_n件へ切り詰め→その後で
        worsening/improving を数える」という順序だったため、候補数が
        top_nを超えると改善中の癖がほぼ集計から漏れ、書き癖の健康度が
        実態より大幅に悪く見積もられていた。
        """
        from unittest.mock import MagicMock

        detector = HabitDriftDetector(min_total_freq=1, top_n=5)
        fake_score = MagicMock(pmi=0.0, t_score=1.0, log_dice=1.0)
        detector.scorer.score_phrase = lambda phrase, freq, text: fake_score
        detector.scorer.classify_collocation = lambda cs: 'habitual'

        ep_texts = {}
        for i in range(4):
            worsening_reps = (i + 1) * 3
            improving_reps = (4 - i) * 3
            ep_texts[f'EP{i+1}'] = ('WORSEN' * worsening_reps) + ('IMPROVE' * improving_reps)

        result = detector.analyze(ep_texts)

        # 表示用リストはtop_n件に絞られる
        assert len(result.habits) <= 5
        # だが集計対象(total_candidates)は全候補数であり、top_nに制限されない
        assert result.total_candidates > 5
        # 悪化度順ソートの都合で表示用リストが悪化中のものばかりになっても、
        # 全候補ベースの集計では改善中の癖もきちんと数えられている
        assert result.improving_count > 0
        assert result.worsening_count + result.improving_count <= result.total_candidates

    def test_report_improving_section_reflects_full_candidate_set(self):
        """report()の「改善中の書き癖」セクションが、表示用に切り詰められた
        habitsからの再抽出ではなく、全候補ベースのtop_improvingを使うこと。
        """
        from unittest.mock import MagicMock

        detector = HabitDriftDetector(min_total_freq=1, top_n=5)
        fake_score = MagicMock(pmi=0.0, t_score=1.0, log_dice=1.0)
        detector.scorer.score_phrase = lambda phrase, freq, text: fake_score
        detector.scorer.classify_collocation = lambda cs: 'habitual'

        ep_texts = {}
        for i in range(4):
            worsening_reps = (i + 1) * 3
            improving_reps = (4 - i) * 3
            ep_texts[f'EP{i+1}'] = ('WORSEN' * worsening_reps) + ('IMPROVE' * improving_reps)

        result = detector.analyze(ep_texts)
        assert result.top_improving, "improving habits should have been found among the full candidate set"
        assert '改善中の書き癖' in result.report()


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

    def test_extract_speech_deduplicates_repeated_name_mentions(self):
        """回帰テスト: キャラ名が短い区間内に複数回登場すると、以前は
        同じ台詞が名前の出現回数だけ重複してsegmentsに追加されていた
        （台詞そのものではなく、地の文中の名前の言及回数で水増しされる）。
        """
        cs = CharacterStylometry(context_window=200)
        text = (
            'エリスは部屋に入った。エリスは窓の外を見た。エリスは振り返った。'
            '「おはようございます、今日はいい天気ですね」'
        )
        segments = cs._extract_speech(text, 'エリス')
        assert segments == ['おはようございます、今日はいい天気ですね']

    def test_extract_speech_keeps_distinct_occurrences(self):
        """離れた位置にある別々の台詞は、それぞれ個別に検出されること
        （重複排除が過剰にマージしていないことの確認）。"""
        cs = CharacterStylometry(context_window=50)
        text = (
            'エリスは朝食をとった。「おはよう」' + ('x' * 200) +
            'エリスは夜になって戻ってきた。「ただいま」'
        )
        segments = cs._extract_speech(text, 'エリス')
        assert segments == ['おはよう', 'ただいま']

    def test_extract_narration_deduplicates_overlapping_windows(self):
        """回帰テスト: キャラ名の複数出現によりコンテキストウィンドウが
        重なると、以前は同じ地の文が出現回数だけ重複してsegmentsに
        追加されていた。"""
        cs = CharacterStylometry(context_window=200)
        text = (
            'エリスは部屋に入った。エリスは窓の外を見た。エリスは振り返った。'
            '「おはようございます」レティシアは静かに扉を閉めた。'
        )
        segments = cs._extract_narration(text, 'エリス')
        assert len(segments) == 1

    def test_extract_narration_keeps_distinct_occurrences(self):
        """離れた位置にある別々のコンテキストは、それぞれ個別に抽出されること。"""
        cs = CharacterStylometry(context_window=50)
        text = (
            'エリスは朝食をとった。「おはよう」' + ('x' * 200) +
            'エリスは夜になって戻ってきた。「ただいま」'
        )
        segments = cs._extract_narration(text, 'エリス')
        assert len(segments) == 2


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

    def test_habits_score_uses_total_candidates_not_truncated_habits_list(self):
        """回帰テスト: 書き癖負債スコアの分母には DriftResult.total_candidates を
        使うこと。以前は len(result.habits)（表示用にtop_n件へ切り詰め済み）を
        分母にしていたため、悪化度順ソートの都合でworsening_countがhabitsの
        件数を上回るケースがあり得て、比率が1を超えてスコアが不当に0になり得た。
        """
        from japhrase.applied.habit_drift import DriftResult

        report = PartHealthReport()
        # habits は表示用に2件だけに絞られているが、全候補ベースの集計では
        # worsening=3, improving=7 という健全に近い内訳だったとする
        fake_result = DriftResult(
            habits=[],  # 表示用リストは空でも(=truncationが極端でも)集計には影響しない
            total_candidates=10,
            worsening_count=3,
            improving_count=7,
            new_habits=[],
            top_worsening=[],
            top_improving=[],
            ep_labels=['EP1', 'EP2'],
        )
        report.habit_detector.analyze = lambda ep_texts: fake_result

        section = report._diagnose_habits(SAMPLE_EPS)
        # ratio = 3/10 = 0.3 -> score = 70.0 (len(habits)=0を分母にしていたら
        # ZeroDivisionErrorになるか、total=len([])=0で無条件score=100になってしまう)
        assert section.score == 70.0
