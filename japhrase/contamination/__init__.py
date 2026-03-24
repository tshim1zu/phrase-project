# coding: utf-8
"""
japhrase.contamination — テキスト汚染度の多軸検知フレームワーク

■ 高水準API（これだけ知っていれば使える）

    scan(text)              → 1テキストの汚染プロファイル
    compare(text_a, text_b) → 2テキスト間の汚染比較
    batch_scan(texts)       → 複数テキストの一括スキャン
    quick_check(text)       → 汚染があるかどうか True/False

    >>> from japhrase.contamination import scan
    >>> profile = scan("テキスト")
    >>> print(profile.overall)    # 0-100
    >>> print(profile.explain())  # 何が問題で、どこで、どう直すか

■ 結果オブジェクト

    ContaminationProfile  — 8軸の汚染プロファイル
    ComparisonResult      — 2テキスト間の比較結果
    BatchResult           — 一括スキャン結果
    Anomaly               — 個別の異常

■ 低水準（上級者向け・カスタマイズ用）

    from japhrase.contamination._scanner import ContaminationScanner
    from japhrase.contamination._detectors import DETECTOR_REGISTRY
"""

# ── 高水準 API ──
from .api import scan, compare, batch_scan, quick_check
from .api import ComparisonResult, BatchResult

# ── 結果オブジェクト ──
from .profile import ContaminationProfile, AxisScore, Anomaly

# ── 後方互換: ContaminationScanner も import 可能にしておく ──
from ._scanner import ContaminationScanner

__all__ = [
    # 高水準（メインの使い方）
    'scan',
    'compare',
    'batch_scan',
    'quick_check',
    # 結果オブジェクト
    'ContaminationProfile',
    'ComparisonResult',
    'BatchResult',
    'Anomaly',
    # 低水準（上級者向け）
    'ContaminationScanner',
]
