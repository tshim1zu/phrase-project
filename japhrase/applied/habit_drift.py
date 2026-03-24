# coding: utf-8
"""
書き癖ドリフト検出（Habit Drift Detector）

1EP単体ではなく、Part全体を通して書き癖がどう変化しているかを追跡。
「〜だった」が増え続けている、「しかし」が後半で急増した、等を検出。

使用例:
    >>> detector = HabitDriftDetector()
    >>> result = detector.analyze(
    ...     ep_texts={"EP101": text1, "EP102": text2, ...},
    ... )
    >>> print(result.report())
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023-2026"

import re
import logging
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple
from collections import Counter

import numpy as np
import pandas as pd

from ..collocation_scorer import CollocationScorer
from ..temporal_analyzer import TemporalAnalyzer
from ..distribution_comparator import DistributionComparator

logger = logging.getLogger(__name__)


@dataclass
class HabitEntry:
    """1つの書き癖エントリ"""
    phrase: str
    total_freq: int
    ep_frequencies: List[int]       # EP別頻度
    trend_slope: float              # 傾き（正=増加中）
    trend_direction: str            # 'worsening', 'improving', 'stable', 'burst'
    collocation_type: str           # 'habitual', 'fixed_expression', 'creative', 'weak'
    pmi: float
    t_score: float
    log_dice: float


@dataclass
class DriftResult:
    """ドリフト分析結果"""
    habits: List[HabitEntry]
    worsening_count: int            # 悪化中の癖の数
    improving_count: int            # 改善中の癖の数
    new_habits: List[HabitEntry]    # 後半で新たに現れた癖
    top_worsening: List[HabitEntry] # 最も悪化している上位
    ep_labels: List[str]

    def report(self) -> str:
        """レポート文字列"""
        lines = [
            "=" * 70,
            "【書き癖ドリフト分析】",
            "=" * 70,
            "",
            f"検出された癖: {len(self.habits)} 個",
            f"  悪化中: {self.worsening_count}",
            f"  改善中: {self.improving_count}",
            f"  新規出現: {len(self.new_habits)}",
            "",
        ]

        # 悪化中 TOP
        if self.top_worsening:
            lines.append("【❌ 悪化中の書き癖 TOP】")
            lines.append(
                f"  {'phrase':<12s} {'total':>5s} {'slope':>7s} "
                f"{'PMI':>6s} {'t':>6s} {'LD':>6s} {'type':<12s}"
            )
            lines.append("  " + "-" * 60)
            for h in self.top_worsening[:10]:
                lines.append(
                    f"  {h.phrase:<12s} {h.total_freq:5d} "
                    f"{h.trend_slope:+7.3f} "
                    f"{h.pmi:6.2f} {h.t_score:6.2f} {h.log_dice:6.2f} "
                    f"{h.collocation_type:<12s}"
                )
                # ミニスパークライン
                sparkline = self._sparkline(h.ep_frequencies)
                lines.append(f"  {'':12s} {sparkline}")

        # 新規出現
        if self.new_habits:
            lines.append("")
            lines.append("【🆕 後半で新たに出現した癖】")
            for h in self.new_habits[:5]:
                lines.append(
                    f"  {h.phrase:<12s} freq={h.total_freq} "
                    f"(EP: {self._sparkline(h.ep_frequencies)})"
                )

        # 改善中
        improving = [h for h in self.habits if h.trend_direction == 'improving']
        if improving:
            lines.append("")
            lines.append("【✅ 改善中の書き癖】")
            for h in improving[:5]:
                lines.append(
                    f"  {h.phrase:<12s} slope={h.trend_slope:+.3f} "
                    f"(EP: {self._sparkline(h.ep_frequencies)})"
                )

        lines.append("")
        lines.append("=" * 70)
        return "\n".join(lines)

    @staticmethod
    def _sparkline(values: List[int]) -> str:
        """簡易スパークライン"""
        if not values:
            return ""
        max_v = max(values) if max(values) > 0 else 1
        blocks = " ▁▂▃▄▅▆▇█"
        chars = []
        for v in values:
            idx = int(v / max_v * (len(blocks) - 1))
            chars.append(blocks[idx])
        return "".join(chars)


class HabitDriftDetector:
    """
    書き癖ドリフト検出器

    使用例:
        >>> detector = HabitDriftDetector()
        >>> result = detector.analyze({"EP101": t1, "EP102": t2, ...})
    """

    def __init__(
        self,
        min_total_freq: int = 5,
        max_pmi: float = 4.0,
        top_n: int = 30,
    ):
        """
        Parameters:
            min_total_freq: Part全体での最小出現回数
            max_pmi: これ以下のPMIの語を癖候補とする
            top_n: 報告する癖の最大数
        """
        self.min_total_freq = min_total_freq
        self.max_pmi = max_pmi
        self.top_n = top_n
        self.scorer = CollocationScorer()

    def analyze(
        self,
        ep_texts: Dict[str, str],
    ) -> DriftResult:
        """
        EP群全体の書き癖ドリフトを分析

        Parameters:
            ep_texts: {ラベル: テキスト}

        Returns:
            DriftResult
        """
        labels = sorted(ep_texts.keys())
        texts = [ep_texts[label] for label in labels]
        n_eps = len(texts)

        # 全テキスト結合
        full_text = '\n'.join(texts)

        # EP別の N-gram 頻度
        ep_freqs = []
        for text in texts:
            clean = re.sub(r'\s+', '', text)
            freq = Counter()
            for n in [2, 3, 4]:
                for i in range(len(clean) - n + 1):
                    gram = clean[i:i + n]
                    freq[gram] += 1
            ep_freqs.append(freq)

        # 全EP通しての頻度
        global_freq = Counter()
        for ef in ep_freqs:
            global_freq.update(ef)

        # 頻出フレーズをフィルタ
        candidates = {
            phrase: freq for phrase, freq in global_freq.items()
            if freq >= self.min_total_freq
        }

        if not candidates:
            return DriftResult(
                habits=[], worsening_count=0, improving_count=0,
                new_habits=[], top_worsening=[], ep_labels=labels,
            )

        # 各候補に対して結合度スコア + ドリフトを計算
        habits = []
        for phrase, total_freq in candidates.items():
            # EP別頻度
            per_ep = [ef.get(phrase, 0) for ef in ep_freqs]

            # 結合度スコア
            cs = self.scorer.score_phrase(phrase, total_freq, full_text)

            # PMI フィルタ（高PMI = 意図的な表現 → 癖ではない）
            if cs.pmi > self.max_pmi:
                continue

            # トレンド
            slope = self._linear_trend(per_ep)

            # 方向判定
            mean_freq = np.mean(per_ep)
            if mean_freq == 0:
                direction = 'stable'
            elif slope > 0.1 * mean_freq:
                direction = 'worsening'
            elif slope < -0.1 * mean_freq:
                direction = 'improving'
            else:
                direction = 'stable'

            # 新規癖判定（前半ゼロ、後半で出現）
            mid = n_eps // 2
            first_half = sum(per_ep[:mid])
            second_half = sum(per_ep[mid:])
            if first_half == 0 and second_half >= self.min_total_freq:
                direction = 'burst'

            # コロケーション分類
            coll_type = self.scorer.classify_collocation(cs)

            habits.append(HabitEntry(
                phrase=phrase,
                total_freq=total_freq,
                ep_frequencies=per_ep,
                trend_slope=round(slope, 4),
                trend_direction=direction,
                collocation_type=coll_type,
                pmi=round(cs.pmi, 3),
                t_score=round(cs.t_score, 3),
                log_dice=round(cs.log_dice, 3),
            ))

        # ソート（悪化度順）
        habits.sort(key=lambda h: h.trend_slope, reverse=True)
        habits = habits[:self.top_n]

        worsening = [h for h in habits if h.trend_direction == 'worsening']
        improving = [h for h in habits if h.trend_direction == 'improving']
        new_habits = [h for h in habits if h.trend_direction == 'burst']

        return DriftResult(
            habits=habits,
            worsening_count=len(worsening),
            improving_count=len(improving),
            new_habits=new_habits,
            top_worsening=worsening[:10],
            ep_labels=labels,
        )

    def compare_parts(
        self,
        part_a_texts: Dict[str, str],
        part_b_texts: Dict[str, str],
        label_a: str = "PartA",
        label_b: str = "PartB",
    ) -> str:
        """2つの Part 間で書き癖がどう変化したかを比較"""
        result_a = self.analyze(part_a_texts)
        result_b = self.analyze(part_b_texts)

        # 共通する癖フレーズ
        phrases_a = {h.phrase: h for h in result_a.habits}
        phrases_b = {h.phrase: h for h in result_b.habits}
        common = set(phrases_a.keys()) & set(phrases_b.keys())

        lines = [
            "=" * 70,
            f"【書き癖 Part 間比較】 {label_a} vs {label_b}",
            "=" * 70,
            "",
            f"  {label_a}: {len(result_a.habits)}癖 "
            f"(悪化{result_a.worsening_count}/改善{result_a.improving_count})",
            f"  {label_b}: {len(result_b.habits)}癖 "
            f"(悪化{result_b.worsening_count}/改善{result_b.improving_count})",
            f"  共通: {len(common)}癖",
            "",
        ]

        if common:
            lines.append("【共通する癖の変化】")
            lines.append(
                f"  {'phrase':<12s} "
                f"{'freq_A':>6s} {'slope_A':>8s} "
                f"{'freq_B':>6s} {'slope_B':>8s} {'変化':>6s}"
            )
            lines.append("  " + "-" * 55)
            for phrase in sorted(common):
                ha = phrases_a[phrase]
                hb = phrases_b[phrase]
                delta = hb.trend_slope - ha.trend_slope
                lines.append(
                    f"  {phrase:<12s} "
                    f"{ha.total_freq:6d} {ha.trend_slope:+8.3f} "
                    f"{hb.total_freq:6d} {hb.trend_slope:+8.3f} "
                    f"{delta:+6.3f}"
                )

        # B にだけ出現する新しい癖
        only_b = set(phrases_b.keys()) - set(phrases_a.keys())
        if only_b:
            lines.append("")
            lines.append(f"【{label_b}で新たに出現した癖】")
            for phrase in list(only_b)[:10]:
                hb = phrases_b[phrase]
                lines.append(f"  {phrase:<12s} freq={hb.total_freq}")

        lines.append("")
        lines.append("=" * 70)
        return "\n".join(lines)

    @staticmethod
    def _linear_trend(values: List[int]) -> float:
        if len(values) < 2:
            return 0.0
        x = np.arange(len(values), dtype=float)
        y = np.array(values, dtype=float)
        return float(np.polyfit(x, y, 1)[0])
