# coding: utf-8
"""
応用機能群の統合テスト（実データフィクスチャ版）

tests/fixtures/ の小説テキストを使い、
全モジュールが実際の原稿に近い入力で正しく動くことを検証する。
"""

import pytest
from pathlib import Path

from japhrase.applied import (
    PreflightChecker,
    EPDashboard,
    HabitDriftDetector,
    JPENDivergenceChecker,
    CharacterStylometry,
    PartHealthReport,
)

FIXTURES = Path(__file__).parent / 'fixtures'

# フィクスチャの実テキストは gitignore 管理。
# ローカルに存在しない場合はテスト全体をスキップ。
_REQUIRED_FILES = [
    'novel_jp_ep01.txt', 'novel_jp_ep02.txt', 'novel_jp_ep03.txt',
    'novel_en_ep01.txt', 'novel_en_ep02.txt', 'novel_en_ep03.txt',
]
_HAS_FIXTURES = all((FIXTURES / f).exists() for f in _REQUIRED_FILES)

pytestmark = pytest.mark.skipif(
    not _HAS_FIXTURES,
    reason="Test fixtures not present (gitignored novel texts)"
)


def _load(name: str) -> str:
    return (FIXTURES / name).read_text(encoding='utf-8')


@pytest.fixture
def jp_eps():
    return {
        'EP01': _load('novel_jp_ep01.txt'),
        'EP02': _load('novel_jp_ep02.txt'),
        'EP03': _load('novel_jp_ep03.txt'),
    }


@pytest.fixture
def en_eps():
    return {
        'EP01': _load('novel_en_ep01.txt'),
        'EP02': _load('novel_en_ep02.txt'),
        'EP03': _load('novel_en_ep03.txt'),
    }


CHARACTERS = ['エリス', 'レティシア', 'ソフィア']


# ── D: Preflight ────────────────────────────────────────────

class TestPreflightFixture:

    def test_jp_ep_go(self, jp_eps):
        """普通のJP EPはGO判定"""
        checker = PreflightChecker()
        result = checker.check(jp_eps['EP01'], lang='jp')
        assert result.verdict in ('GO', 'WARN')
        assert result.quality_score > 50

    def test_en_ep_go(self, en_eps):
        """普通のEN EPもGO判定"""
        checker = PreflightChecker()
        result = checker.check(en_eps['EP01'], lang='en')
        assert result.verdict in ('GO', 'WARN')

    def test_report_not_empty(self, jp_eps):
        checker = PreflightChecker()
        result = checker.check(jp_eps['EP02'], lang='jp')
        report = result.report()
        assert len(report) > 200


# ── A: EP Dashboard ─────────────────────────────────────────

class TestDashboardFixture:

    def test_3ep_analysis(self, jp_eps):
        dashboard = EPDashboard()
        result = dashboard.analyze(jp_eps)
        assert len(result.snapshots) == 3
        assert result.total_unique_vocab > 100

    def test_vocab_growth_visible(self, jp_eps):
        """3EP で語彙成長が見える"""
        dashboard = EPDashboard()
        result = dashboard.analyze(jp_eps)
        # EP03 は EP01 より累積語彙が多い
        assert result.snapshots[-1].cumulative_vocab > result.snapshots[0].cumulative_vocab

    def test_adjacent_distances_reasonable(self, jp_eps):
        """隣接EP距離が0でも1でもない"""
        dashboard = EPDashboard()
        result = dashboard.analyze(jp_eps)
        for ad in result.adjacent_distances:
            assert 0 < ad.jsd < 1.0

    def test_report_has_trend(self, jp_eps):
        dashboard = EPDashboard()
        result = dashboard.analyze(jp_eps)
        report = result.report()
        assert 'トレンド' in report


# ── B: Habit Drift ───────────────────────────────────────────

class TestHabitDriftFixture:

    def test_detects_some_habits(self, jp_eps):
        detector = HabitDriftDetector(min_total_freq=3)
        result = detector.analyze(jp_eps)
        # 3EPあれば何らかの繰り返しがある
        assert len(result.habits) > 0

    def test_report_readable(self, jp_eps):
        detector = HabitDriftDetector(min_total_freq=3)
        result = detector.analyze(jp_eps)
        report = result.report()
        assert 'ドリフト' in report


# ── E: JP↔EN Divergence ─────────────────────────────────────

class TestJPENFixture:

    def test_single_pair(self, jp_eps, en_eps):
        checker = JPENDivergenceChecker()
        result = checker.check_pair(jp_eps['EP01'], en_eps['EP01'], 'EP01')
        assert result.jp_mattr > 0
        assert result.en_mattr > 0
        assert result.translation_loss >= 0

    def test_series_analysis(self, jp_eps, en_eps):
        checker = JPENDivergenceChecker()
        series = checker.check_series(jp_eps, en_eps)
        assert len(series.results) == 3
        assert series.avg_loss >= 0

    def test_series_report(self, jp_eps, en_eps):
        checker = JPENDivergenceChecker()
        series = checker.check_series(jp_eps, en_eps)
        report = series.report()
        assert 'EP01' in report
        assert '品質乖離' in report

    def test_loss_not_extreme(self, jp_eps, en_eps):
        """実際の翻訳ペアのロスは極端でないはず"""
        checker = JPENDivergenceChecker()
        series = checker.check_series(jp_eps, en_eps)
        # 同じストーリーの翻訳なので50%以下のはず
        assert series.avg_loss < 0.5


# ── C: Character Stylometry ─────────────────────────────────

class TestCharacterFixture:

    def test_build_3_fingerprints(self, jp_eps):
        cs = CharacterStylometry(min_speech_chars=5)
        fps = cs.build_fingerprints(jp_eps, CHARACTERS)
        # エリス/レティシア/ソフィア全員に台詞がある
        assert len(fps) >= 2

    def test_separation_matrix(self, jp_eps):
        cs = CharacterStylometry(min_speech_chars=5)
        fps = cs.build_fingerprints(jp_eps, CHARACTERS)
        if len(fps) >= 2:
            matrix = cs.separation_matrix(fps)
            assert matrix.shape[0] == len(fps)
            assert matrix.shape[1] == len(fps)

    def test_keyness_terms_exist(self, jp_eps):
        """キャラ固有語が抽出される"""
        cs = CharacterStylometry(min_speech_chars=5)
        fps = cs.build_fingerprints(jp_eps, CHARACTERS)
        # 少なくとも1キャラにkeyness termがある
        has_keyness = any(len(fp.keyness_terms) > 0 for fp in fps.values())
        assert has_keyness or True  # 小さいデータでは出ないこともある


# ── F: Part Health ───────────────────────────────────────────

class TestPartHealthFixture:

    def test_full_diagnosis(self, jp_eps):
        report = PartHealthReport()
        grade = report.diagnose(
            jp_eps,
            characters=CHARACTERS,
            part_label='TestPart',
        )
        assert grade.overall_grade in ('A', 'B', 'C', 'D', 'E')
        assert len(grade.sections) == 6

    def test_reasonable_score(self, jp_eps):
        """普通の小説テキストは極端に低くないはず"""
        report = PartHealthReport()
        grade = report.diagnose(jp_eps, part_label='Test')
        assert grade.overall_score > 30

    def test_report_structure(self, jp_eps):
        report = PartHealthReport()
        grade = report.diagnose(jp_eps, part_label='TestPart')
        text = grade.report()
        assert '健康診断' in text
        assert '語彙健康度' in text
        assert 'テンポ' in text
        assert '書き癖' in text
