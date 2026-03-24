# coding: utf-8
"""
japhrase.applied — 執筆・原稿チェック・stores直結の応用機能群

統計エンジン（Phase 1-5）を実際のワークフローに接続する。
"""

from .preflight_stats import PreflightChecker, PreflightResult
from .ep_dashboard import EPDashboard, DashboardResult
from .habit_drift import HabitDriftDetector, DriftResult
from .jpen_divergence import JPENDivergenceChecker, DivergenceResult
from .character_stylometry import CharacterStylometry, CharacterFingerprint
from .part_health import PartHealthReport, HealthGrade

__all__ = [
    'PreflightChecker', 'PreflightResult',
    'EPDashboard', 'DashboardResult',
    'HabitDriftDetector', 'DriftResult',
    'JPENDivergenceChecker', 'DivergenceResult',
    'CharacterStylometry', 'CharacterFingerprint',
    'PartHealthReport', 'HealthGrade',
]
