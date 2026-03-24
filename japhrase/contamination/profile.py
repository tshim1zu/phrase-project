# coding: utf-8
"""
汚染プロファイル（多軸結果）
"""

from dataclasses import dataclass, field
from typing import List, Optional


@dataclass
class Anomaly:
    """検出された異常1件"""
    detector: str           # 検出器名
    severity: int           # 1-10 (1=軽微, 10=致命的)
    start: int              # テキスト内の開始位置（文字index）
    end: int                # テキスト内の終了位置
    line_no: int            # 行番号（0-based, -1=不明）
    description: str        # 人間向け説明
    snippet: str            # 該当テキストの抜粋（50字以内）


@dataclass
class AxisScore:
    """1軸のスコア"""
    name: str
    score: int              # 0-100 (0=汚染なし, 100=完全に壊れている)
    count: int              # 検出件数
    anomalies: List[Anomaly] = field(default_factory=list)

    @property
    def label(self) -> str:
        if self.score == 0:
            return 'clean'
        elif self.score < 15:
            return 'minor'
        elif self.score < 40:
            return 'moderate'
        elif self.score < 70:
            return 'severe'
        else:
            return 'critical'


@dataclass
class ContaminationProfile:
    """
    テキスト汚染の多軸プロファイル

    7軸のスコアと、検出された全異常のリストを保持。
    """
    encoding: AxisScore
    structural: AxisScore
    duplicate: AxisScore
    repetition: AxisScore
    distribution: AxisScore
    complexity: AxisScore
    vocabulary: AxisScore

    text_length: int = 0

    @property
    def axes(self) -> List[AxisScore]:
        return [
            self.encoding, self.structural, self.duplicate,
            self.repetition, self.distribution, self.complexity,
            self.vocabulary,
        ]

    @property
    def overall(self) -> int:
        """
        総合汚染スコア (0-100)

        確度の高い軸ほど重みが大きい。
        """
        weights = {
            'encoding': 0.20,       # 確定的 → 重い
            'structural': 0.15,     # 確定的
            'duplicate': 0.20,      # 確定的
            'repetition': 0.10,
            'distribution': 0.15,
            'complexity': 0.10,
            'vocabulary': 0.10,
        }
        total = sum(
            getattr(self, name).score * w
            for name, w in weights.items()
        )
        return min(100, int(total))

    @property
    def all_anomalies(self) -> List[Anomaly]:
        """全軸の異常を重篤度降順で返す"""
        all_a = []
        for axis in self.axes:
            all_a.extend(axis.anomalies)
        return sorted(all_a, key=lambda a: a.severity, reverse=True)

    @property
    def anomaly_count(self) -> int:
        return sum(ax.count for ax in self.axes)

    def is_clean(self, threshold: int = 10) -> bool:
        """汚染スコアが閾値以下なら clean"""
        return self.overall <= threshold

    def __str__(self) -> str:
        return self.report()

    def report(self) -> str:
        """人間向けレポート"""
        icon_overall = '✅' if self.is_clean() else '⚠️' if self.overall < 40 else '❌'
        lines = [
            "=" * 65,
            f"【テキスト汚染度プロファイル】 {icon_overall} 総合: {self.overall}/100",
            f"  テキスト長: {self.text_length}字 / 検出異常: {self.anomaly_count}件",
            "=" * 65,
            "",
        ]

        for axis in self.axes:
            icon = {
                'clean': '🟢', 'minor': '🟡',
                'moderate': '🟠', 'severe': '🔴', 'critical': '💀',
            }.get(axis.label, '?')
            lines.append(
                f"  {icon} {axis.name:<14s} {axis.score:3d}/100  "
                f"({axis.count}件) {axis.label}"
            )

        # 上位の異常を表示
        top_anomalies = self.all_anomalies[:15]
        if top_anomalies:
            lines.append("")
            lines.append("-" * 65)
            lines.append("【検出された異常（重篤度順）】")
            for a in top_anomalies:
                sev_bar = '█' * a.severity + '░' * (10 - a.severity)
                lines.append(
                    f"  [{a.detector:>12s}] {sev_bar} "
                    f"L{a.line_no + 1 if a.line_no >= 0 else '?':>4}"
                    f"  {a.description}"
                )
                if a.snippet:
                    lines.append(f"  {'':>14s} → {a.snippet}")

        lines.append("")
        lines.append("=" * 65)
        return "\n".join(lines)
