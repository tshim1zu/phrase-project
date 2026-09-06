# coding: utf-8
"""
Part 健康診断レポート（Part Health Report）

全統計エンジンを統合した Part 全体の健康度を A〜E 5段階で評価。
1コマンドで Part の品質を定量的に把握できる。

使用例:
    >>> report = PartHealthReport()
    >>> grade = report.diagnose(
    ...     ep_texts={"EP101": t1, "EP102": t2, ...},
    ...     characters=["エリス", "レティシア", "ソフィア"],
    ... )
    >>> print(grade.report())
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023-2026"

import logging
from dataclasses import dataclass, field
from typing import Dict, List, Optional

import numpy as np

from .ep_dashboard import EPDashboard
from .habit_drift import HabitDriftDetector
from .character_stylometry import CharacterStylometry
from .preflight_stats import PreflightChecker
from .jpen_divergence import JPENDivergenceChecker

logger = logging.getLogger(__name__)


@dataclass
class HealthSection:
    """健康診断の1セクション"""
    name: str
    score: float            # 0-100
    grade: str              # A-E
    summary: str
    details: List[str] = field(default_factory=list)


@dataclass
class HealthGrade:
    """Part 全体の健康診断結果"""
    part_label: str
    overall_score: float        # 0-100
    overall_grade: str          # A-E
    sections: List[HealthSection]
    improvement_priorities: List[str]

    def report(self) -> str:
        """各診断セクションと改善優先度を整形したレポートを返す。"""
        lines = [
            "=" * 72,
            f"【Part 健康診断レポート】 {self.part_label}",
            f"  総合: {self.overall_grade} ({self.overall_score:.1f}/100)",
            "=" * 72,
            "",
        ]

        for section in self.sections:
            icon = {'A': '🟢', 'B': '🟢', 'C': '🟡', 'D': '🟠', 'E': '🔴'}.get(section.grade, '?')
            lines.append(
                f"  {icon} {section.name:<20s} "
                f"{section.grade} ({section.score:5.1f})  {section.summary}"
            )
            for detail in section.details:
                lines.append(f"     {detail}")

        if self.improvement_priorities:
            lines.append("")
            lines.append("-" * 72)
            lines.append("【改善優先度】")
            for i, priority in enumerate(self.improvement_priorities, 1):
                lines.append(f"  {i}. {priority}")

        lines.append("")
        lines.append("=" * 72)
        return "\n".join(lines)


def _score_to_grade(score: float) -> str:
    """0〜100のスコアをA〜Eの健康度評価へ変換する。"""
    if score >= 85:
        return 'A'
    elif score >= 70:
        return 'B'
    elif score >= 55:
        return 'C'
    elif score >= 40:
        return 'D'
    else:
        return 'E'


class PartHealthReport:
    """
    Part 健康診断エンジン

    全エンジンを統合して Part の品質を定量評価。
    """

    # セクション別重み
    SECTION_WEIGHTS = {
        'vocabulary': 0.20,
        'tempo': 0.15,
        'vocab_growth': 0.15,
        'habits': 0.15,
        'character': 0.15,
        'immersion': 0.20,
    }

    def __init__(self):
        """健康診断を構成する各種分析器を初期化する。"""
        self.dashboard = EPDashboard()
        self.habit_detector = HabitDriftDetector()
        self.char_stylometry = CharacterStylometry()
        self.preflight = PreflightChecker()

    def diagnose(
        self,
        ep_texts: Dict[str, str],
        characters: Optional[List[str]] = None,
        jp_texts: Optional[Dict[str, str]] = None,
        en_texts: Optional[Dict[str, str]] = None,
        part_label: str = "Part",
    ) -> HealthGrade:
        """
        Part の健康診断を実行

        Parameters:
            ep_texts: {ラベル: テキスト} — メイン分析対象
            characters: キャラクター名リスト（None でスキップ）
            jp_texts: JP テキスト（JP/EN比較用、None でスキップ）
            en_texts: EN テキスト（JP/EN比較用、None でスキップ）
            part_label: Part のラベル

        Returns:
            HealthGrade
        """
        sections = []
        priorities = []

        # 1. 語彙健康度
        vocab_section = self._diagnose_vocabulary(ep_texts)
        sections.append(vocab_section)
        if vocab_section.score < 60:
            priorities.append(f"語彙多様性の改善（現在 {vocab_section.grade}）")

        # 2. テンポ健康度
        tempo_section = self._diagnose_tempo(ep_texts)
        sections.append(tempo_section)
        if tempo_section.score < 60:
            priorities.append(f"テンポの改善（現在 {tempo_section.grade}）")

        # 3. 語彙成長
        growth_section = self._diagnose_vocab_growth(ep_texts)
        sections.append(growth_section)
        if growth_section.score < 60:
            priorities.append(f"語彙成長の改善（現在 {growth_section.grade}）")

        # 4. 書き癖負債
        habit_section = self._diagnose_habits(ep_texts)
        sections.append(habit_section)
        if habit_section.score < 60:
            priorities.append(f"書き癖の改善（悪化中 {habit_section.details[0] if habit_section.details else ''}）")

        # 5. キャラ分離度
        if characters and len(characters) >= 2:
            char_section = self._diagnose_characters(ep_texts, characters)
            sections.append(char_section)
            if char_section.score < 60:
                priorities.append(f"キャラ分離の改善（{char_section.summary}）")
        else:
            sections.append(HealthSection(
                name='キャラ分離度', score=75.0, grade='B',
                summary='キャラ未指定（スキップ）',
            ))

        # 6. 没入度（離脱要因）
        immersion_section = self._diagnose_immersion(ep_texts)
        sections.append(immersion_section)
        if immersion_section.score < 60:
            priorities.append(f"離脱要因の除去（{immersion_section.summary}）")

        # 総合スコア
        weight_keys = ['vocabulary', 'tempo', 'vocab_growth', 'habits', 'character', 'immersion']
        total_score = sum(
            sections[i].score * self.SECTION_WEIGHTS.get(weight_keys[i], 0.15)
            for i in range(len(sections))
        )

        return HealthGrade(
            part_label=part_label,
            overall_score=round(total_score, 1),
            overall_grade=_score_to_grade(total_score),
            sections=sections,
            improvement_priorities=priorities[:5],
        )

    # ─── Diagnostic Sections ────────────────────────────────

    def _diagnose_vocabulary(self, ep_texts: Dict[str, str]) -> HealthSection:
        """語彙健康度"""
        result = self.dashboard.analyze(ep_texts)
        mattrs = [s.mattr for s in result.snapshots]
        hapaxes = [s.hapax_ratio for s in result.snapshots]

        avg_mattr = float(np.mean(mattrs)) if mattrs else 0
        avg_hapax = float(np.mean(hapaxes)) if hapaxes else 0

        # スコア: MATTR 0.7以上=100, 0.4以下=0 + Hapax 0.5以上=100, 0.1以下=0
        mattr_score = min(100, max(0, (avg_mattr - 0.4) / 0.3 * 100))
        hapax_score = min(100, max(0, (avg_hapax - 0.1) / 0.4 * 100))
        score = mattr_score * 0.6 + hapax_score * 0.4

        return HealthSection(
            name='語彙健康度',
            score=round(score, 1),
            grade=_score_to_grade(score),
            summary=f"MATTR={avg_mattr:.4f} / Hapax={avg_hapax:.4f}",
            details=[
                f"MATTR トレンド: {result.mattr_trend:+.6f}",
            ],
        )

    def _diagnose_tempo(self, ep_texts: Dict[str, str]) -> HealthSection:
        """テンポ健康度"""
        result = self.dashboard.analyze(ep_texts)
        pps = [s.perplexity for s in result.snapshots]
        crs = [s.compression_ratio for s in result.snapshots]

        avg_pp = float(np.mean(pps)) if pps else 0
        avg_cr = float(np.mean(crs)) if crs else 0

        # PP: 15-50 が理想。外れるほど減点
        if 15 <= avg_pp <= 50:
            pp_score = 100
        elif avg_pp < 15:
            pp_score = max(0, avg_pp / 15 * 100)
        else:
            pp_score = max(0, (100 - avg_pp) / 50 * 100)

        # CR: 0.4-0.7 が理想
        if 0.4 <= avg_cr <= 0.7:
            cr_score = 100
        elif avg_cr < 0.4:
            cr_score = max(0, avg_cr / 0.4 * 100)
        else:
            cr_score = max(0, (1.0 - avg_cr) / 0.3 * 100)

        score = pp_score * 0.5 + cr_score * 0.5

        return HealthSection(
            name='テンポ健康度',
            score=round(score, 1),
            grade=_score_to_grade(score),
            summary=f"PP={avg_pp:.1f} / CR={avg_cr:.4f}",
            details=[
                f"PP トレンド: {result.perplexity_trend:+.4f}",
            ],
        )

    def _diagnose_vocab_growth(self, ep_texts: Dict[str, str]) -> HealthSection:
        """語彙成長"""
        result = self.dashboard.analyze(ep_texts)
        sat = result.vocab_saturation

        # 飽和度: 0.7以上=良好, 0.3以下=枯渇
        score = min(100, max(0, (sat - 0.2) / 0.6 * 100))

        new_vocabs = [s.new_vocab_count for s in result.snapshots]
        trend = result.vocab_growth_trend

        return HealthSection(
            name='語彙成長',
            score=round(score, 1),
            grade=_score_to_grade(score),
            summary=f"飽和度={sat:.4f} / 累積={result.total_unique_vocab}",
            details=[
                f"新語トレンド: {trend:+.4f}",
                f"最小新語数: {min(new_vocabs) if new_vocabs else 0}",
            ],
        )

    def _diagnose_habits(self, ep_texts: Dict[str, str]) -> HealthSection:
        """書き癖負債"""
        result = self.habit_detector.analyze(ep_texts)

        # 悪化中の癖が少ないほど良い
        # (total_candidates は top_n による表示件数の制限を受けない全候補数。
        #  len(result.habits) は表示用に上位top_n件へ絞られているため、
        #  worsening_count と組み合わせて比率を取るとtop_n超過時に
        #  分母が実態より小さくなり不当に悪いスコアになる)
        worsening = result.worsening_count
        total = result.total_candidates

        if total == 0:
            score = 100.0
        else:
            ratio = worsening / total
            score = max(0, (1.0 - ratio) * 100)

        details = []
        if result.top_worsening:
            top3 = ', '.join(h.phrase for h in result.top_worsening[:3])
            details.append(f"悪化中: {top3}")
        if result.new_habits:
            new3 = ', '.join(h.phrase for h in result.new_habits[:3])
            details.append(f"新規癖: {new3}")

        return HealthSection(
            name='書き癖負債',
            score=round(score, 1),
            grade=_score_to_grade(score),
            summary=f"悪化{worsening}/{total}癖",
            details=details,
        )

    def _diagnose_characters(
        self, ep_texts: Dict[str, str], characters: List[str]
    ) -> HealthSection:
        """キャラ分離度"""
        fps = self.char_stylometry.build_fingerprints(ep_texts, characters)

        if len(fps) < 2:
            return HealthSection(
                name='キャラ分離度', score=75.0, grade='B',
                summary='分析可能キャラ不足',
            )

        matrix = self.char_stylometry.separation_matrix(fps)

        # 最小分離度（= 最も混ざりやすいペア）
        min_sep = float('inf')
        min_pair = ('', '')
        chars = list(matrix.columns)
        for i in range(len(chars)):
            for j in range(i + 1, len(chars)):
                val = matrix.iloc[i, j]
                if val < min_sep:
                    min_sep = val
                    min_pair = (chars[i], chars[j])

        # JSD: 0.3以上=良好, 0.05以下=混ざりすぎ
        score = min(100, max(0, (min_sep - 0.02) / 0.28 * 100))

        return HealthSection(
            name='キャラ分離度',
            score=round(score, 1),
            grade=_score_to_grade(score),
            summary=f"最小JSD={min_sep:.6f} ({min_pair[0]}↔{min_pair[1]})",
            details=[f"分析キャラ: {', '.join(fps.keys())}"],
        )

    def _diagnose_immersion(self, ep_texts: Dict[str, str]) -> HealthSection:
        """没入度（離脱要因）"""
        total_skip = 0
        total_drag = 0
        total_noise = 0

        for text in ep_texts.values():
            result = self.preflight.check(text, lang='jp')
            total_skip += result.skip_count
            total_drag += result.drag_count
            total_noise += result.noise_count

        total_issues = total_skip + total_drag + total_noise
        n_eps = len(ep_texts)

        # EP あたりの平均離脱要因数
        avg_issues = total_issues / n_eps if n_eps > 0 else 0

        # 0件=100, 5件/EP以上=0
        score = max(0, 100 - avg_issues * 20)

        return HealthSection(
            name='没入度',
            score=round(score, 1),
            grade=_score_to_grade(score),
            summary=f"SKIP={total_skip} DRAG={total_drag} NOISE={total_noise} ({avg_issues:.1f}/EP)",
            details=[],
        )
