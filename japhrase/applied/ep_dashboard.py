# coding: utf-8
"""
EP間比較ダッシュボード（EP Dashboard）

Part 内の EP 群を時系列として分析し、語彙推移・テンポ変化・
伏線バースト・隣接EP距離を可視化する。

使用例:
    >>> dashboard = EPDashboard()
    >>> result = dashboard.analyze(
    ...     ep_texts={"EP101": text1, "EP102": text2, ...},
    ... )
    >>> print(result.report())
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023-2026"

import logging
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple
from collections import OrderedDict

import numpy as np
import pandas as pd

from ..temporal_analyzer import TemporalAnalyzer, TermTrend
from ..distribution_comparator import DistributionComparator
from ..complexity_metrics import ComplexityAnalyzer
from ..stylometry import StylometryAnalyzer

logger = logging.getLogger(__name__)


@dataclass
class EPSnapshot:
    """1EP のスナップショット"""
    label: str
    char_count: int
    mattr: float
    hapax_ratio: float
    perplexity: float
    compression_ratio: float
    information_rate: float
    entropy: float
    new_vocab_count: int
    cumulative_vocab: int


@dataclass
class AdjacentDistance:
    """隣接EP間の距離"""
    ep_a: str
    ep_b: str
    jsd: float
    assessment: str     # 'too_close', 'normal', 'too_far'


@dataclass
class DashboardResult:
    """ダッシュボード結果"""
    snapshots: List[EPSnapshot]
    adjacent_distances: List[AdjacentDistance]
    burst_terms: List[TermTrend]

    # トレンド（線形傾き）
    mattr_trend: float
    entropy_trend: float
    vocab_growth_trend: float
    perplexity_trend: float

    # サマリ
    vocab_saturation: float     # 語彙飽和度
    total_unique_vocab: int
    weakest_ep: Optional[str]   # 最も品質が低いEP
    strongest_ep: Optional[str] # 最も品質が高いEP

    def report(self) -> str:
        """レポート文字列"""
        lines = [
            "=" * 72,
            "【EP間比較ダッシュボード】",
            "=" * 72,
            "",
            f"EP数: {len(self.snapshots)}",
            f"累積語彙: {self.total_unique_vocab}",
            f"語彙飽和度: {self.vocab_saturation:.4f}"
            f"  ({'飽和' if self.vocab_saturation < 0.5 else '成長中'})",
            "",
            "【トレンド（傾き）】",
            f"  MATTR:      {self.mattr_trend:+.6f}"
            f"  {'↗語彙豊か化' if self.mattr_trend > 0.001 else '↘語彙枯渇' if self.mattr_trend < -0.001 else '→安定'}",
            f"  Entropy:    {self.entropy_trend:+.6f}"
            f"  {'↗複雑化' if self.entropy_trend > 0.001 else '↘単調化' if self.entropy_trend < -0.001 else '→安定'}",
            f"  Perplexity: {self.perplexity_trend:+.6f}",
            f"  新語率:     {self.vocab_growth_trend:+.4f}",
        ]

        if self.weakest_ep:
            lines.append(f"\n  最弱EP: {self.weakest_ep}")
        if self.strongest_ep:
            lines.append(f"  最強EP: {self.strongest_ep}")

        # EP一覧
        lines.append("")
        lines.append("-" * 72)
        lines.append("【EP別指標】")
        lines.append(
            f"  {'EP':<8s} {'字数':>6s} {'MATTR':>6s} {'Hapax':>6s} "
            f"{'PP':>6s} {'CR':>6s} {'IR':>6s} {'新語':>5s}"
        )
        lines.append("  " + "-" * 65)

        for s in self.snapshots:
            lines.append(
                f"  {s.label:<8s} {s.char_count:6d} "
                f"{s.mattr:6.4f} {s.hapax_ratio:6.4f} "
                f"{s.perplexity:6.1f} {s.compression_ratio:6.4f} "
                f"{s.information_rate:6.4f} {s.new_vocab_count:5d}"
            )

        # 隣接距離
        if self.adjacent_distances:
            lines.append("")
            lines.append("【隣接EP距離（JSD）】")
            for ad in self.adjacent_distances:
                icon = {'too_close': '⚠近', 'too_far': '⚠遠', 'normal': '  '}[ad.assessment]
                lines.append(
                    f"  {icon} {ad.ep_a} → {ad.ep_b}: "
                    f"JSD={ad.jsd:.6f}"
                )

        # バースト
        burst_with_intervals = [t for t in self.burst_terms if t.burst_intervals]
        if burst_with_intervals:
            lines.append("")
            lines.append("【伏線語バースト検出】")
            for trend in burst_with_intervals[:10]:
                for bi in trend.burst_intervals:
                    level_str = "!" * bi.burst_level
                    lines.append(
                        f"  {level_str:3s} [{trend.term}] "
                        f"doc{bi.doc_start}〜{bi.doc_end} "
                        f"({bi.ratio:.1f}x)"
                    )

        lines.append("")
        lines.append("=" * 72)
        return "\n".join(lines)


class EPDashboard:
    """
    EP間比較ダッシュボード

    使用例:
        >>> dashboard = EPDashboard()
        >>> texts = {"EP101": "...", "EP102": "...", ...}
        >>> result = dashboard.analyze(texts)
    """

    # 隣接EP JSD 閾値
    JSD_TOO_CLOSE = 0.02    # これ以下 = 同じことの繰り返し
    JSD_TOO_FAR = 0.50      # これ以上 = 急激な変化

    def __init__(self):
        """ダッシュボードで使用する各種分析器を初期化する。"""
        self.temporal = TemporalAnalyzer()
        self.comparator = DistributionComparator()
        self.complexity = ComplexityAnalyzer()
        self.stylometry = StylometryAnalyzer()

    def analyze(
        self,
        ep_texts: Dict[str, str],
        target_terms: Optional[List[str]] = None,
    ) -> DashboardResult:
        """
        EP群を時系列分析

        Parameters:
            ep_texts: {ラベル: テキスト} の OrderedDict or dict
            target_terms: バースト監視対象語（None で自動選択）

        Returns:
            DashboardResult
        """
        # ラベルをソート（EP番号順）
        labels = sorted(ep_texts.keys())
        texts = [ep_texts[label] for label in labels]

        # 各EPのスナップショット
        snapshots = []
        cumulative_tokens = set()

        for label, text in zip(labels, texts):
            cx = self.complexity.analyze(text)
            adv = self.stylometry.analyze_advanced_diversity(text)
            mattr_r = self.stylometry.analyze_mattr(text)

            # 新語カウント
            tokens = self._tokenize(text)
            new_vocab = set(tokens) - cumulative_tokens
            cumulative_tokens.update(tokens)

            # 文エントロピー
            series_result = self.temporal.analyze_series([text], [label])
            entropy = series_result['snapshots'][0].entropy if series_result['snapshots'] else 0.0

            snapshots.append(EPSnapshot(
                label=label,
                char_count=len(text.replace('\n', '').replace(' ', '')),
                mattr=mattr_r['mattr'],
                hapax_ratio=adv['hapax_ratio'],
                perplexity=cx['perplexity'],
                compression_ratio=cx['compression_ratio'],
                information_rate=cx['information_rate'],
                entropy=entropy,
                new_vocab_count=len(new_vocab),
                cumulative_vocab=len(cumulative_tokens),
            ))

        # トレンド計算
        mattr_values = [s.mattr for s in snapshots]
        entropy_values = [s.entropy for s in snapshots]
        pp_values = [s.perplexity for s in snapshots]
        new_vocab_values = [s.new_vocab_count for s in snapshots]

        # 隣接EP距離
        adjacent_distances = []
        for i in range(len(texts) - 1):
            freq_a = self._text_to_freq(texts[i])
            freq_b = self._text_to_freq(texts[i + 1])
            comp = self.comparator.compare(freq_a, freq_b)

            if comp.jsd < self.JSD_TOO_CLOSE:
                assessment = 'too_close'
            elif comp.jsd > self.JSD_TOO_FAR:
                assessment = 'too_far'
            else:
                assessment = 'normal'

            adjacent_distances.append(AdjacentDistance(
                ep_a=labels[i], ep_b=labels[i + 1],
                jsd=comp.jsd, assessment=assessment,
            ))

        # バースト検出
        burst_terms = self.temporal.detect_bursts(texts, target_terms)

        # 最弱/最強EP（MATTR + 情報率 の合成スコアで判定）
        ep_scores = {
            s.label: s.mattr * 0.5 + s.information_rate * 0.3 + s.hapax_ratio * 0.2
            for s in snapshots
        }
        weakest = min(ep_scores, key=ep_scores.get) if ep_scores else None
        strongest = max(ep_scores, key=ep_scores.get) if ep_scores else None

        # 語彙飽和度
        if len(new_vocab_values) >= 4:
            mid = len(new_vocab_values) // 2
            first = sum(new_vocab_values[:mid])
            second = sum(new_vocab_values[mid:])
            saturation = second / first if first > 0 else 1.0
        else:
            saturation = 1.0

        return DashboardResult(
            snapshots=snapshots,
            adjacent_distances=adjacent_distances,
            burst_terms=burst_terms,
            mattr_trend=self._linear_trend(mattr_values),
            entropy_trend=self._linear_trend(entropy_values),
            vocab_growth_trend=self._linear_trend(new_vocab_values),
            perplexity_trend=self._linear_trend(pp_values),
            vocab_saturation=saturation,
            total_unique_vocab=len(cumulative_tokens),
            weakest_ep=weakest,
            strongest_ep=strongest,
        )

    def _tokenize(self, text: str) -> List[str]:
        """空白を除いたテキストを文字2-gramへ分割する。"""
        import re
        clean = re.sub(r'\s+', '', text)
        return [clean[i:i + 2] for i in range(len(clean) - 1)]

    def _text_to_freq(self, text: str):
        """テキストの文字2-gram頻度分布を返す。"""
        from collections import Counter
        return Counter(self._tokenize(text))

    @staticmethod
    def _linear_trend(values: List[float]) -> float:
        """値列に対する最小二乗直線の傾きを返す。"""
        if len(values) < 2:
            return 0.0
        x = np.arange(len(values), dtype=float)
        y = np.array(values)
        return float(np.polyfit(x, y, 1)[0])
