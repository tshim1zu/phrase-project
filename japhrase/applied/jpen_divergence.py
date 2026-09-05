# coding: utf-8
"""
JP↔EN 品質乖離検出（JP-EN Divergence Checker）

翻訳によって語彙の豊かさ・情報密度・テンポがどう変化したかを統計的に検出。
「EN版で語彙が痩せた」「情報率が落ちた」を数値で示す。

使用例:
    >>> checker = JPENDivergenceChecker()
    >>> result = checker.check_pair(jp_text, en_text, label="EP101")
    >>> print(f"翻訳ロス率: {result.translation_loss:.1%}")
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023-2026"

import logging
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple

import numpy as np

from ..complexity_metrics import ComplexityAnalyzer
from ..stylometry import StylometryAnalyzer

logger = logging.getLogger(__name__)


@dataclass
class DivergenceResult:
    """JP↔EN 乖離結果（1EP分）"""
    label: str

    # JP 指標
    jp_mattr: float
    jp_hapax_ratio: float
    jp_perplexity: float
    jp_compression_ratio: float
    jp_information_rate: float
    jp_lexical_density: float

    # EN 指標
    en_mattr: float
    en_hapax_ratio: float
    en_perplexity: float
    en_compression_ratio: float
    en_information_rate: float
    en_lexical_density: float

    # 差分（EN - JP の正規化差分）
    delta_mattr: float
    delta_hapax: float
    delta_perplexity: float
    delta_compression: float
    delta_info_rate: float
    delta_lexical: float

    # 翻訳ロス率（0-1, 0が最良）
    translation_loss: float

    # 判定
    verdict: str                    # 'good', 'acceptable', 'degraded', 'critical'
    issues: List[str] = field(default_factory=list)


@dataclass
class SeriesDivergence:
    """EP群のJP↔EN乖離推移"""
    results: List[DivergenceResult]
    avg_loss: float
    loss_trend: float               # 正=悪化中
    worst_ep: Optional[str]
    best_ep: Optional[str]

    def report(self) -> str:
        """EP群の翻訳ロスと指標差分を整形したレポートを返す。"""
        lines = [
            "=" * 72,
            "【JP↔EN 品質乖離レポート】",
            "=" * 72,
            "",
            f"EP数: {len(self.results)}",
            f"平均翻訳ロス: {self.avg_loss:.1%}",
            f"ロストレンド: {self.loss_trend:+.6f}"
            f"  ({'↗悪化中' if self.loss_trend > 0.001 else '↘改善中' if self.loss_trend < -0.001 else '→安定'})",
        ]

        if self.worst_ep:
            lines.append(f"  最悪EP: {self.worst_ep}")
        if self.best_ep:
            lines.append(f"  最良EP: {self.best_ep}")

        # EP一覧
        lines.append("")
        lines.append("-" * 72)
        lines.append(
            f"  {'EP':<8s} {'loss':>6s} {'ΔMATTR':>7s} {'ΔHapax':>7s} "
            f"{'ΔPP':>7s} {'ΔCR':>7s} {'ΔIR':>7s} {'判定':<10s}"
        )
        lines.append("  " + "-" * 65)

        for r in self.results:
            icon = {
                'good': '✅', 'acceptable': '  ',
                'degraded': '⚠️', 'critical': '❌'
            }.get(r.verdict, '?')

            lines.append(
                f"  {r.label:<8s} {r.translation_loss:5.1%} "
                f"{r.delta_mattr:+7.4f} {r.delta_hapax:+7.4f} "
                f"{r.delta_perplexity:+7.1f} {r.delta_compression:+7.4f} "
                f"{r.delta_info_rate:+7.4f} {icon}{r.verdict:<8s}"
            )

            if r.issues:
                for issue in r.issues:
                    lines.append(f"  {'':8s}   → {issue}")

        lines.append("")
        lines.append("=" * 72)
        return "\n".join(lines)


class JPENDivergenceChecker:
    """
    JP↔EN 品質乖離チェッカー

    使用例:
        >>> checker = JPENDivergenceChecker()
        >>> result = checker.check_pair(jp_text, en_text, "EP101")
        >>> series = checker.check_series(jp_texts, en_texts)
    """

    # 乖離閾値
    THRESHOLDS = {
        'mattr_drop': -0.08,         # MATTR がこれ以上下がると劣化
        'hapax_drop': -0.10,         # Hapax がこれ以上下がると劣化
        'info_rate_drop': -0.15,     # 情報率がこれ以上下がると劣化
        'compression_rise': 0.10,    # 圧縮率がこれ以上上がると冗長化
        'critical_loss': 0.30,       # 翻訳ロス30%以上 = critical
        'degraded_loss': 0.15,       # 翻訳ロス15%以上 = degraded
    }

    def __init__(self):
        """日英テキストの比較に使う複雑度・文体分析器を初期化する。"""
        self.complexity = ComplexityAnalyzer()
        self.stylometry = StylometryAnalyzer()

    def check_pair(
        self,
        jp_text: str,
        en_text: str,
        label: str = "EP",
    ) -> DivergenceResult:
        """
        JP/EN ペアの品質乖離を測定

        Parameters:
            jp_text: 日本語テキスト
            en_text: 英語テキスト
            label: EP ラベル
        """
        # JP 分析
        jp_cx = self.complexity.analyze(jp_text)
        jp_adv = self.stylometry.analyze_advanced_diversity(jp_text)
        jp_mattr = self.stylometry.analyze_mattr(jp_text)

        # EN 分析
        en_cx = self.complexity.analyze(en_text)
        en_adv = self.stylometry.analyze_advanced_diversity(en_text)
        en_mattr = self.stylometry.analyze_mattr(en_text)

        # 差分（正規化: JP を基準にした相対変化）
        def _delta(jp_val, en_val):
            """日本語側の値を基準に英語側の相対変化を返す。"""
            if jp_val == 0:
                return 0.0
            return (en_val - jp_val) / abs(jp_val)

        d_mattr = en_mattr['mattr'] - jp_mattr['mattr']
        d_hapax = en_adv['hapax_ratio'] - jp_adv['hapax_ratio']
        d_pp = en_cx['perplexity'] - jp_cx['perplexity']
        d_cr = en_cx['compression_ratio'] - jp_cx['compression_ratio']
        d_ir = en_cx['information_rate'] - jp_cx['information_rate']
        d_ld = en_cx['lexical_density'] - jp_cx['lexical_density']

        # 翻訳ロス率: 各指標の負方向変化を集計
        losses = []
        if d_mattr < 0:
            losses.append(abs(d_mattr))
        if d_hapax < 0:
            losses.append(abs(d_hapax))
        if d_ir < 0:
            losses.append(abs(d_ir))
        if d_cr > 0.05:  # 圧縮率の上昇 = 冗長化
            losses.append(d_cr)

        translation_loss = float(np.mean(losses)) if losses else 0.0

        # 判定
        T = self.THRESHOLDS
        issues = []

        if d_mattr < T['mattr_drop']:
            issues.append(f"語彙多様性が低下 (ΔMATTR={d_mattr:+.4f})")
        if d_hapax < T['hapax_drop']:
            issues.append(f"語彙洗練度が低下 (ΔHapax={d_hapax:+.4f})")
        if d_ir < T['info_rate_drop']:
            issues.append(f"情報率が低下 (ΔIR={d_ir:+.4f})")
        if d_cr > T['compression_rise']:
            issues.append(f"冗長化 (ΔCR={d_cr:+.4f})")

        if translation_loss >= T['critical_loss']:
            verdict = 'critical'
        elif translation_loss >= T['degraded_loss']:
            verdict = 'degraded'
        elif issues:
            verdict = 'acceptable'
        else:
            verdict = 'good'

        return DivergenceResult(
            label=label,
            jp_mattr=jp_mattr['mattr'],
            jp_hapax_ratio=jp_adv['hapax_ratio'],
            jp_perplexity=jp_cx['perplexity'],
            jp_compression_ratio=jp_cx['compression_ratio'],
            jp_information_rate=jp_cx['information_rate'],
            jp_lexical_density=jp_cx['lexical_density'],
            en_mattr=en_mattr['mattr'],
            en_hapax_ratio=en_adv['hapax_ratio'],
            en_perplexity=en_cx['perplexity'],
            en_compression_ratio=en_cx['compression_ratio'],
            en_information_rate=en_cx['information_rate'],
            en_lexical_density=en_cx['lexical_density'],
            delta_mattr=round(d_mattr, 4),
            delta_hapax=round(d_hapax, 4),
            delta_perplexity=round(d_pp, 2),
            delta_compression=round(d_cr, 4),
            delta_info_rate=round(d_ir, 4),
            delta_lexical=round(d_ld, 4),
            translation_loss=round(translation_loss, 4),
            verdict=verdict,
            issues=issues,
        )

    def check_series(
        self,
        jp_texts: Dict[str, str],
        en_texts: Dict[str, str],
    ) -> SeriesDivergence:
        """
        EP群のJP↔EN乖離を系列分析

        Parameters:
            jp_texts: {ラベル: JP テキスト}
            en_texts: {ラベル: EN テキスト}（ラベルは jp_texts と一致させる）

        Returns:
            SeriesDivergence
        """
        common_labels = sorted(set(jp_texts.keys()) & set(en_texts.keys()))

        results = []
        for label in common_labels:
            r = self.check_pair(jp_texts[label], en_texts[label], label)
            results.append(r)

        if not results:
            return SeriesDivergence(
                results=[], avg_loss=0.0, loss_trend=0.0,
                worst_ep=None, best_ep=None,
            )

        losses = [r.translation_loss for r in results]
        avg_loss = float(np.mean(losses))

        # トレンド
        if len(losses) >= 2:
            x = np.arange(len(losses), dtype=float)
            loss_trend = float(np.polyfit(x, np.array(losses), 1)[0])
        else:
            loss_trend = 0.0

        worst = max(results, key=lambda r: r.translation_loss)
        best = min(results, key=lambda r: r.translation_loss)

        return SeriesDivergence(
            results=results,
            avg_loss=avg_loss,
            loss_trend=loss_trend,
            worst_ep=worst.label,
            best_ep=best.label,
        )
