# coding: utf-8
"""
japhrase.contamination — テキスト汚染度の多軸検知フレームワーク

テキストの物理的破損・混入・劣化を7種の検出器で多角的に評価する。
PyOD の異常検知と同じ思想: 単一スコアではなく、軸ごとのプロファイルを返す。

使い方:
    >>> from japhrase.contamination import scan, ContaminationScanner
    >>>
    >>> # 一発で全検出器を走らせる
    >>> profile = scan(text)
    >>> print(profile)                    # 多軸プロファイル
    >>> print(profile.overall)            # 総合スコア (0-100)
    >>> print(profile.is_clean())         # True/False
    >>>
    >>> # 検出器を選んで走らせる
    >>> profile = scan(text, detectors=['duplicate', 'encoding'])
    >>>
    >>> # カスタマイズ
    >>> scanner = ContaminationScanner(duplicate_threshold=0.9)
    >>> profile = scanner.scan(text)
"""

from .scanner import ContaminationScanner, scan
from .profile import ContaminationProfile, Anomaly

__all__ = [
    'ContaminationScanner',
    'scan',
    'ContaminationProfile',
    'Anomaly',
]
