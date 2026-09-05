# coding: utf-8
"""
japhrase.contamination — 高水準API

大学1年生でも使える4つの関数:

    scan(text)              → 1テキストの汚染度プロファイル
    compare(text_a, text_b) → 2テキスト間の汚染比較
    batch_scan(texts)       → 複数テキストの一括スキャン
    quick_check(text)       → 汚染があるか True/False だけ返す

全て引数はテキスト文字列だけ。設定はデフォルトで動く。
"""

from typing import List, Dict, Optional, Union
from dataclasses import dataclass, field

from .profile import ContaminationProfile
from ._scanner import ContaminationScanner

# デフォルトスキャナ（初回呼び出し時に生成）
_default_scanner: Optional[ContaminationScanner] = None


def _get_scanner(**kwargs) -> ContaminationScanner:
    """デフォルトスキャナを取得。kwargs があればカスタム生成。"""
    if kwargs:
        return ContaminationScanner(**kwargs)
    global _default_scanner
    if _default_scanner is None:
        _default_scanner = ContaminationScanner()
    return _default_scanner


# ═══════════════════════════════════════════════════════════════
# 高水準 API
# ═══════════════════════════════════════════════════════════════

def scan(
    text: str,
    detectors: Optional[List[str]] = None,
    reference_text: Optional[str] = None,
    **kwargs,
) -> ContaminationProfile:
    """
    テキストの汚染度を評価する。

    これが最も基本的な関数。テキストを渡すと、8軸の汚染プロファイルが返る。

    基本:
        >>> profile = scan("ここにテキスト")
        >>> print(profile.overall)     # 0-100 の汚染スコア
        >>> print(profile.is_clean())  # True / False
        >>> print(profile.explain())   # 何が問題で、どこで、どう直すか

    テキスト間比較:
        >>> profile = scan(text_a, reference_text=text_b)

    検出器を選択:
        >>> profile = scan(text, detectors=['duplicate', 'consistency'])

    Parameters:
        text: 評価対象テキスト
        detectors: 実行する検出器名（None=全8種）
            'encoding', 'structural', 'duplicate', 'repetition',
            'distribution', 'complexity', 'consistency', 'language'
        reference_text: 比較対象テキスト（テキスト間比較用）
        **kwargs: 閾値の上書き（上級者向け）

    Returns:
        ContaminationProfile — .overall, .is_clean(), .explain() が使える
    """
    scanner = _get_scanner(**kwargs)
    return scanner.scan(text, detectors=detectors, reference_text=reference_text)


def quick_check(text: str, threshold: int = 10) -> bool:
    """
    テキストが汚染されているかどうかだけを返す。

    最もシンプルな使い方。True = 汚染あり、False = クリーン。

        >>> if quick_check(text):
        ...     print("汚染あり！")

    Parameters:
        text: 評価対象テキスト
        threshold: この汚染スコア以下なら clean（デフォルト10）

    Returns:
        True = 汚染あり / False = クリーン
    """
    return not scan(text).is_clean(threshold=threshold)


@dataclass
class ComparisonResult:
    """2テキスト間の汚染比較結果"""
    profile_a: ContaminationProfile
    profile_b: ContaminationProfile
    cross_profile: ContaminationProfile  # A を B を参照して評価した結果

    @property
    def score_a(self) -> int:
        """テキストAの総合汚染スコアを返す。"""
        return self.profile_a.overall

    @property
    def score_b(self) -> int:
        """テキストBの総合汚染スコアを返す。"""
        return self.profile_b.overall

    @property
    def cross_score(self) -> int:
        """テキスト間の汚染度（重複・分布差・一貫性不一致）"""
        return self.cross_profile.overall

    @property
    def healthier(self) -> str:
        """汚染が少ない方のラベルを返す"""
        return 'A' if self.score_a <= self.score_b else 'B'

    def report(self) -> str:
        """個別およびテキスト間の汚染度を比較レポートとして返す。"""
        lines = [
            "=" * 60,
            "【テキスト間汚染比較】",
            "=" * 60,
            "",
            f"  テキストA: {self.score_a}/100"
            f"  {'✅' if self.profile_a.is_clean() else '⚠️'}",
            f"  テキストB: {self.score_b}/100"
            f"  {'✅' if self.profile_b.is_clean() else '⚠️'}",
            f"  テキスト間: {self.cross_score}/100"
            f"  {'✅' if self.cross_profile.is_clean() else '⚠️'}",
            "",
        ]

        # テキスト間で検出された問題
        cross_issues = self.cross_profile.top_issues(5)
        if cross_issues:
            lines.append("【テキスト間の問題】")
            for a in cross_issues:
                lines.append(f"  [{a.detector}] {a.description}")
                if a.snippet:
                    lines.append(f"    → {a.snippet[:50]}")
            lines.append("")

        # 各テキスト固有の問題（worst axis）
        for label, profile in [('A', self.profile_a), ('B', self.profile_b)]:
            worst = profile.worst_axes
            if worst:
                lines.append(f"【テキスト{label}の固有問題】")
                for ax in worst[:3]:
                    lines.append(f"  {ax.name}: {ax.score}/100 ({ax.count}件)")

        lines.append("")
        lines.append("=" * 60)
        return "\n".join(lines)


def compare(
    text_a: str,
    text_b: str,
    **kwargs,
) -> ComparisonResult:
    """
    2つのテキストの汚染度を比較する。

    各テキストの個別汚染度に加えて、テキスト間の汚染
    （重複・分布の乖離・句読点スタイルの不一致）も検出する。

        >>> result = compare(text_a, text_b)
        >>> print(result.score_a, result.score_b)  # 各テキストのスコア
        >>> print(result.cross_score)              # テキスト間のスコア
        >>> print(result.report())                 # 比較レポート

    Parameters:
        text_a: テキストA
        text_b: テキストB

    Returns:
        ComparisonResult
    """
    scanner = _get_scanner(**kwargs)

    profile_a = scanner.scan(text_a)
    profile_b = scanner.scan(text_b)
    cross = scanner.scan(text_a, reference_text=text_b)

    return ComparisonResult(
        profile_a=profile_a,
        profile_b=profile_b,
        cross_profile=cross,
    )


@dataclass
class BatchResult:
    """複数テキスト一括スキャン結果"""
    profiles: Dict[str, ContaminationProfile]

    @property
    def cleanest(self) -> Optional[str]:
        """最もクリーンなテキストのキー"""
        if not self.profiles:
            return None
        return min(self.profiles, key=lambda k: self.profiles[k].overall)

    @property
    def dirtiest(self) -> Optional[str]:
        """最も汚染されたテキストのキー"""
        if not self.profiles:
            return None
        return max(self.profiles, key=lambda k: self.profiles[k].overall)

    @property
    def all_clean(self) -> bool:
        """全テキストがクリーンか"""
        return all(p.is_clean() for p in self.profiles.values())

    @property
    def contaminated_keys(self) -> List[str]:
        """汚染されているテキストのキーリスト"""
        return [k for k, p in self.profiles.items() if not p.is_clean()]

    def report(self) -> str:
        """各テキストの汚染度と問題概要を一覧レポートとして返す。"""
        lines = [
            "=" * 60,
            f"【一括汚染スキャン】 {len(self.profiles)}件",
            "=" * 60,
            "",
        ]

        for key in sorted(self.profiles.keys()):
            p = self.profiles[key]
            icon = '✅' if p.is_clean() else '⚠️' if p.overall < 40 else '❌'
            lines.append(f"  {icon} {key:<20s} {p.overall:3d}/100  ({p.anomaly_count}件)")

            # 汚染ありならトップの問題を1行
            if not p.is_clean() and p.primary_issue:
                lines.append(f"     └─ {p.primary_issue}")

        lines.append("")
        contaminated = self.contaminated_keys
        if contaminated:
            lines.append(f"  汚染あり: {len(contaminated)}/{len(self.profiles)}件")
            lines.append(f"  最も汚染: {self.dirtiest}")
        else:
            lines.append("  ✅ 全テキストクリーン")

        lines.append("")
        lines.append("=" * 60)
        return "\n".join(lines)


def batch_scan(
    texts: Dict[str, str],
    **kwargs,
) -> BatchResult:
    """
    複数のテキストを一括でスキャンする。

    話数ごとの原稿を一気にチェックしたいときに使う。

        >>> results = batch_scan({
        ...     "第1話": ep01_text,
        ...     "第2話": ep02_text,
        ...     "第3話": ep03_text,
        ... })
        >>> print(results.all_clean)          # True / False
        >>> print(results.contaminated_keys)  # 汚染ありのキーリスト
        >>> print(results.report())           # 一覧レポート

    Parameters:
        texts: {キー: テキスト} の辞書
        **kwargs: 閾値の上書き

    Returns:
        BatchResult
    """
    scanner = _get_scanner(**kwargs)
    profiles = {
        key: scanner.scan(text)
        for key, text in texts.items()
    }
    return BatchResult(profiles=profiles)
