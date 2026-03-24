# coding: utf-8
"""
ContaminationScanner — 8検出器の統合実行 + アンサンブル（v2: 8軸版）

使い方:
    # 一発実行
    profile = scan(text)

    # テキスト間比較
    profile = scan(text, reference_text=other_text)

    # 検出器を選択
    profile = scan(text, detectors=['duplicate', 'encoding', 'consistency'])

    # 1次元に縮約
    score = scan(text).overall
"""

import math
import logging
from typing import List, Optional, Dict

from .profile import ContaminationProfile, AxisScore, Anomaly
from .detectors import DETECTOR_REGISTRY, ALL_DETECTOR_NAMES

logger = logging.getLogger(__name__)


class ContaminationScanner:
    """
    テキスト汚染度スキャナ（8軸アンサンブル）

    Parameters:
        duplicate_threshold: 段落ほぼ一致と判定するJaccard閾値
        repetition_window: フレーズ反復を検出するウィンドウ幅
        repetition_max: この回数を超えると異常反復
        distribution_jsd_threshold: 分布断絶と判定するJSD閾値
        segment_size: 分布/複雑度 検出のセグメントサイズ
        foreign_threshold: 外来語彙比率の閾値
        compression_low: 圧縮率の下限
        compression_high: 圧縮率の上限
        language_mix_threshold: 言語混在の検出閾値
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
        language_mix_threshold: float = 0.3,
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
            'mix_threshold': language_mix_threshold,
        }

    def scan(
        self,
        text: str,
        detectors: Optional[List[str]] = None,
        reference_text: Optional[str] = None,
    ) -> ContaminationProfile:
        """
        テキストの汚染度を多軸評価

        Parameters:
            text: 評価対象テキスト
            detectors: 実行する検出器名のリスト (None=全8種)
                選択肢: 'encoding', 'structural', 'duplicate',
                        'repetition', 'distribution', 'complexity',
                        'consistency', 'language'
            reference_text: 比較対象テキスト（テキスト間分析用）

        Returns:
            ContaminationProfile — 8軸のスコア + 全異常のリスト
        """
        if detectors is None:
            detectors = ALL_DETECTOR_NAMES

        lines = text.split('\n')
        params = {**self.params}
        if reference_text is not None:
            params['reference_text'] = reference_text

        # 各検出器を実行
        results: Dict[str, List[Anomaly]] = {}
        for name in ALL_DETECTOR_NAMES:
            if name in detectors and name in DETECTOR_REGISTRY:
                try:
                    results[name] = DETECTOR_REGISTRY[name](text, lines, **params)
                except Exception as e:
                    logger.warning(f"検出器 {name} でエラー: {e}")
                    results[name] = []
            else:
                results[name] = []

        def _axis(name: str, display_name: str) -> AxisScore:
            anomalies = results.get(name, [])
            return AxisScore(
                name=display_name,
                score=self._compute_axis_score(anomalies),
                count=len(anomalies),
                anomalies=anomalies,
            )

        return ContaminationProfile(
            encoding=_axis('encoding', 'エンコーディング'),
            structural=_axis('structural', '構造'),
            duplicate=_axis('duplicate', '重複'),
            repetition=_axis('repetition', '反復'),
            distribution=_axis('distribution', '分布'),
            complexity=_axis('complexity', '複雑度'),
            consistency=_axis('consistency', '一貫性'),
            language=_axis('language', '言語混在'),
            text_length=len(text),
        )

    @staticmethod
    def _compute_axis_score(anomalies: List[Anomaly]) -> int:
        """異常リスト → 軸スコア (0-100)。飽和関数で収束。"""
        if not anomalies:
            return 0
        weighted_sum = sum(a.severity for a in anomalies)
        score = 100 * (1 - math.exp(-0.1 * weighted_sum))
        return min(100, int(score))


# ═══════════════════════════════════════════════════════════════
# トップレベル関数
# ═══════════════════════════════════════════════════════════════

_default_scanner = None


def scan(
    text: str,
    detectors: Optional[List[str]] = None,
    reference_text: Optional[str] = None,
    **kwargs,
) -> ContaminationProfile:
    """
    テキストの汚染度を評価する

    基本:
        >>> profile = scan(text)
        >>> print(profile.overall)     # 0-100
        >>> print(profile.is_clean())  # True / False
        >>> print(profile.explain())   # 何が問題で、どこで、どう直すか

    テキスト間比較:
        >>> profile = scan(text_a, reference_text=text_b)

    検出器を選択:
        >>> profile = scan(text, detectors=['duplicate', 'consistency'])

    Parameters:
        text: 評価対象テキスト
        detectors: 実行する検出器名（None=全8種）
        reference_text: 比較対象テキスト（テキスト間分析用）
        **kwargs: ContaminationScanner のパラメータ
    """
    if kwargs:
        scanner = ContaminationScanner(**kwargs)
    else:
        global _default_scanner
        if _default_scanner is None:
            _default_scanner = ContaminationScanner()
        scanner = _default_scanner

    return scanner.scan(text, detectors=detectors, reference_text=reference_text)
