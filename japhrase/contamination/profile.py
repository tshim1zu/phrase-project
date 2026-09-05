# coding: utf-8
"""
汚染プロファイル（多軸結果）

高水準: scan(text) → profile.overall → is_clean()
ドリルダウン: profile.explain() → 何が問題で、どこにあって、どう直すか
詳細: profile.worst_axes → profile.locate('duplicate') → anomaly.snippet
"""

from dataclasses import dataclass, field
from typing import List, Optional, Tuple


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

    @property
    def location(self) -> str:
        """人間向けの位置表記"""
        if self.line_no >= 0:
            return f"L{self.line_no + 1}"
        return f"pos {self.start}-{self.end}"

    @property
    def suggestion(self) -> str:
        """修正アクションの提案"""
        return _SUGGESTIONS.get(self.detector, 'テキストを目視確認してください')


# 検出器ごとの修正アクション辞書
_SUGGESTIONS = {
    'encoding': '文字化け箇所を正しい文字に置換してください。エンコーディング(UTF-8)を確認。',
    'structural': '括弧の対応・改行・行長・作業注釈の消し忘れを確認してください。',
    'duplicate': '重複している段落/文を削除してください（コピペミスの可能性）。',
    'repetition': '同じフレーズが短い区間内で繰り返されています。表現を変えるか、重複を削除。',
    'distribution': 'この区間の語彙が前後と大きく異なります。別テキストの混入や段落の順序間違いを確認。',
    'complexity': 'この区間の情報密度が異常です。繰り返しコピペか、ランダムデータの混入を確認。',
    'consistency': '表記・句読点を統一してください。全角/半角、漢字/ひらがな、長音の有無を確認。',
    'language': 'この区間の言語が前後と異なります。別言語テキストの混入を確認。',
}


@dataclass
class AxisScore:
    """1軸のスコア"""
    name: str
    score: int              # 0-100 (0=汚染なし, 100=完全に壊れている)
    count: int              # 検出件数
    anomalies: List[Anomaly] = field(default_factory=list)

    @property
    def label(self) -> str:
        """軸スコアに対応する汚染レベル名を返す。"""
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
    consistency: AxisScore
    language: AxisScore

    text_length: int = 0

    @property
    def axes(self) -> List[AxisScore]:
        """8つの汚染軸を定義順に返す。"""
        return [
            self.encoding, self.structural, self.duplicate,
            self.repetition, self.distribution, self.complexity,
            self.consistency, self.language,
        ]

    @property
    def overall(self) -> int:
        """
        総合汚染スコア (0-100)

        確度の高い軸ほど重みが大きい。
        """
        weights = {
            'encoding': 0.18,       # 確定的 → 重い
            'structural': 0.15,     # 確定的
            'duplicate': 0.18,      # 確定的
            'repetition': 0.08,
            'distribution': 0.12,
            'complexity': 0.08,
            'consistency': 0.12,    # 表記ゆれ・句読点
            'language': 0.09,       # 言語混在
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
        """全軸で検出された異常件数の合計を返す。"""
        return sum(ax.count for ax in self.axes)

    def is_clean(self, threshold: int = 10) -> bool:
        """汚染スコアが閾値以下なら clean"""
        return self.overall <= threshold

    # ─── ドリルダウン API ─────────────────────────────────

    @property
    def worst_axes(self) -> List['AxisScore']:
        """スコアが高い（汚染が酷い）順に軸を返す。score=0 の軸は除外。"""
        return sorted(
            [ax for ax in self.axes if ax.score > 0],
            key=lambda ax: ax.score,
            reverse=True,
        )

    @property
    def primary_issue(self) -> Optional[str]:
        """最も深刻な汚染の種類を1行で返す。汚染なしなら None。"""
        worst = self.worst_axes
        if not worst:
            return None
        ax = worst[0]
        top_anomaly = ax.anomalies[0] if ax.anomalies else None
        if top_anomaly:
            return f"{ax.name}: {top_anomaly.description} ({top_anomaly.location})"
        return f"{ax.name}: score {ax.score}/100"

    def top_issues(self, n: int = 5) -> List[Anomaly]:
        """
        最も重篤な異常を n 件返す。

        「何が問題か」を即座に把握するためのエントリポイント。
        各 Anomaly の .description, .location, .suggestion でドリルダウン。
        """
        return self.all_anomalies[:n]

    def explain(self, max_issues: int = 5) -> str:
        """
        「何が問題で、どこにあって、どう直すか」を人間が読める形で返す。

        scan(text) → profile.explain() の2ステップで完結する。
        大学1年生がこれだけ読めば次のアクションがわかる。

        Returns:
            説明文字列。汚染なしなら「問題なし」。
        """
        if self.is_clean():
            return "✅ 問題なし: テキストに汚染は検出されませんでした。"

        lines = []

        # 概要
        icon = '⚠️' if self.overall < 40 else '❌'
        lines.append(f"{icon} 汚染スコア: {self.overall}/100")
        lines.append("")

        # 軸ごとの状況（汚染ありのみ）
        for ax in self.worst_axes:
            lines.append(f"■ {ax.name} ({ax.score}/100, {ax.count}件)")

            # この軸のトップ異常を表示
            shown = 0
            for a in ax.anomalies:
                if shown >= 3:
                    remaining = len(ax.anomalies) - shown
                    if remaining > 0:
                        lines.append(f"    ...他 {remaining}件")
                    break

                lines.append(f"  {a.location}: {a.description}")
                if a.snippet:
                    lines.append(f"         → {a.snippet[:60]}")
                shown += 1

            # 修正アクション
            if ax.anomalies:
                lines.append(f"  💡 {ax.anomalies[0].suggestion}")
            lines.append("")

        return "\n".join(lines)

    def locate(self, detector_name: str) -> List[Anomaly]:
        """
        特定の検出器の異常だけを取り出す。

        >>> profile.locate('duplicate')
        [Anomaly(detector='duplicate', line_no=5, ...), ...]
        """
        return [
            a for a in self.all_anomalies
            if a.detector == detector_name
        ]

    def summary_dict(self) -> dict:
        """プログラムから扱いやすい辞書形式で返す"""
        return {
            'overall': self.overall,
            'is_clean': self.is_clean(),
            'text_length': self.text_length,
            'anomaly_count': self.anomaly_count,
            'axes': {
                ax.name: {'score': ax.score, 'count': ax.count, 'label': ax.label}
                for ax in self.axes
            },
            'primary_issue': self.primary_issue,
        }

    def __str__(self) -> str:
        """人間向け汚染レポートを文字列表現として返す。"""
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
