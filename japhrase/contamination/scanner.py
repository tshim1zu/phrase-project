# coding: utf-8
"""
ContaminationScanner — 7検出器の統合実行 + アンサンブル

全検出器を走らせ、多軸プロファイルを生成する。
個別でも使えるし、アンサンブルで1次元に縮約もできる。

使い方:
    # 一発実行（全検出器、デフォルト設定）
    profile = scan(text)

    # 検出器を選択
    profile = scan(text, detectors=['duplicate', 'encoding'])

    # カスタマイズ
    scanner = ContaminationScanner(duplicate_threshold=0.85)
    profile = scanner.scan(text)

    # 1次元スコアだけ欲しい
    score = scan(text).overall  # 0-100

    # 軸ごとのスコアが欲しい
    profile = scan(text)
    print(profile.encoding.score)
    print(profile.duplicate.score)

    # アンサンブル: 複数テキストを比較
    scores = [scan(t).overall for t in texts]
"""

import logging
from typing import List, Optional, Dict, Any

from .profile import ContaminationProfile, AxisScore, Anomaly
from .detectors import (
    DETECTOR_REGISTRY,
    ALL_DETECTOR_NAMES,
    detect_encoding,
    detect_structural,
    detect_duplicate,
    detect_repetition,
    detect_distribution,
    detect_complexity,
    detect_vocabulary,
)

logger = logging.getLogger(__name__)


class ContaminationScanner:
    """
    テキスト汚染度スキャナ（7軸アンサンブル）

    Parameters:
        duplicate_threshold: 段落ほぼ一致と判定するJaccard閾値 (0-1)
        repetition_window: フレーズ反復を検出するウィンドウ幅（文字数）
        repetition_max: この回数を超えると異常反復
        distribution_jsd_threshold: 分布断絶と判定するJSD閾値
        segment_size: 分布/複雑度/語彙 検出のセグメントサイズ
        foreign_threshold: 外来語彙比率の閾値
        compression_low: 圧縮率の下限（これ以下は繰り返し）
        compression_high: 圧縮率の上限（これ以上はランダム）
    """

    def __init__(
        self,
        duplicate_threshold: float = 0.9,
        repetition_window: int = 500,
        repetition_max: int = 3,
        distribution_jsd_threshold: float = 0.35,
        segment_size: int = 300,
        foreign_threshold: float = 0.3,
        compression_low: float = 0.15,
        compression_high: float = 0.85,
    ):
        self.params = {
            'similarity_threshold': duplicate_threshold,
            'window_size': repetition_window,
            'max_repeat': repetition_max,
            'jsd_threshold': distribution_jsd_threshold,
            'segment_size': segment_size,
            'foreign_threshold': foreign_threshold,
            'compression_low': compression_low,
            'compression_high': compression_high,
        }

    def scan(
        self,
        text: str,
        detectors: Optional[List[str]] = None,
    ) -> ContaminationProfile:
        """
        テキストの汚染度を多軸評価

        Parameters:
            text: 評価対象テキスト
            detectors: 実行する検出器名のリスト (None=全部)
                選択肢: 'encoding', 'structural', 'duplicate',
                        'repetition', 'distribution', 'complexity',
                        'vocabulary'

        Returns:
            ContaminationProfile — 7軸のスコア + 全異常のリスト
        """
        if detectors is None:
            detectors = ALL_DETECTOR_NAMES

        lines = text.split('\n')

        # 各検出器を実行
        results: Dict[str, List[Anomaly]] = {}
        for name in ALL_DETECTOR_NAMES:
            if name in detectors and name in DETECTOR_REGISTRY:
                try:
                    anomalies = DETECTOR_REGISTRY[name](
                        text, lines, **self.params
                    )
                    results[name] = anomalies
                except Exception as e:
                    logger.warning(f"検出器 {name} でエラー: {e}")
                    results[name] = []
            else:
                results[name] = []

        # 各軸のスコアを計算
        def _axis(name: str, display_name: str) -> AxisScore:
            anomalies = results.get(name, [])
            score = self._compute_axis_score(anomalies)
            return AxisScore(
                name=display_name,
                score=score,
                count=len(anomalies),
                anomalies=anomalies,
            )

        return ContaminationProfile(
            encoding=_axis('encoding', 'エンコーディング'),
            structural=_axis('structural', '構造'),
            duplicate=_axis('duplicate', '重複'),
            repetition=_axis('repetition', '反復'),
            distribution=_axis('distribution', '分布断絶'),
            complexity=_axis('complexity', '複雑度異常'),
            vocabulary=_axis('vocabulary', '外来語彙'),
            text_length=len(text),
        )

    @staticmethod
    def _compute_axis_score(anomalies: List[Anomaly]) -> int:
        """
        異常リストから軸スコア (0-100) を計算

        重篤度の加重合計。飽和関数で 0-100 に収める。
        score = 100 * (1 - e^(-k * weighted_sum))
        """
        if not anomalies:
            return 0

        weighted_sum = sum(a.severity for a in anomalies)
        # k=0.1: severity合計10で約63点、30で約95点、50で約99点
        score = 100 * (1 - math.exp(-0.1 * weighted_sum))
        return min(100, int(score))


import math


# ═══════════════════════════════════════════════════════════════
# トップレベル関数（簡単に呼べるインターフェース）
# ═══════════════════════════════════════════════════════════════

_default_scanner = None


def scan(
    text: str,
    detectors: Optional[List[str]] = None,
    **kwargs,
) -> ContaminationProfile:
    """
    テキストの汚染度を評価する（トップレベル関数）

    最もシンプルな使い方:
        >>> from japhrase.contamination import scan
        >>> profile = scan("テキスト")
        >>> print(profile.overall)    # 0-100 の汚染スコア
        >>> print(profile.is_clean()) # True / False
        >>> print(profile)            # 全軸レポート

    検出器を選んで実行:
        >>> profile = scan("テキスト", detectors=['duplicate', 'encoding'])

    閾値カスタマイズ:
        >>> profile = scan("テキスト", duplicate_threshold=0.85)

    Parameters:
        text: 評価対象テキスト
        detectors: 実行する検出器名（None=全7種）
        **kwargs: ContaminationScanner のパラメータ

    Returns:
        ContaminationProfile
    """
    if kwargs:
        scanner = ContaminationScanner(**kwargs)
    else:
        global _default_scanner
        if _default_scanner is None:
            _default_scanner = ContaminationScanner()
        scanner = _default_scanner

    return scanner.scan(text, detectors=detectors)
