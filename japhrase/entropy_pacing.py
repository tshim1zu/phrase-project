"""
文体リズム分析（Entropy Pacing）

分岐エントロピーを使用して、「退屈な区間」と「難解な区間」を検出。
テキストの読みやすさと流動性を数値化します。
"""

import numpy as np
import pandas as pd
from typing import List, Dict, Tuple
from dataclasses import dataclass
from collections import Counter
import logging

logger = logging.getLogger(__name__)


@dataclass
class PacingIssue:
    """リズム問題"""
    segment_start: int               # セグメント開始位置
    segment_end: int                 # セグメント終了位置
    length: int                      # セグメント長
    entropy: float                   # エントロピー値
    issue_type: str                  # 'boring' / 'chaotic'
    severity: str                    # 'critical' / 'warning'
    message: str                     # メッセージ


class EntropyPacing:
    """
    エントロピーベースの文体リズム分析

    - エントロピー低い = 「予測可能」= 「退屈」
    - エントロピー高い = 「不規則」= 「難解」
    - 適正範囲 = 読みやすくテンポが良い
    """

    # エントロピーの基準値（日本語テキスト向け）
    BORING_THRESHOLD = 3.5          # これ以下は「退屈」
    CHAOTIC_THRESHOLD = 6.5         # これ以上は「難解」
    OPTIMAL_MIN = 4.0               # 最適な最小値
    OPTIMAL_MAX = 5.5               # 最適な最大値
    MIN_SEGMENT_LENGTH = 50         # 最小セグメント長

    def __init__(self, window_size: int = 3, min_segment_length: int = 50):
        """
        Parameters:
            window_size: エントロピー計算のウィンドウサイズ
            min_segment_length: 分析対象の最小文字数
        """
        self.window_size = window_size
        self.min_segment_length = min_segment_length

    def _calculate_entropy(self, chars: List[str]) -> float:
        """
        シャノンエントロピーを計算

        Parameters:
            chars: 文字リスト

        Returns:
            エントロピー値
        """
        if not chars:
            return 0.0

        counter = Counter(chars)
        total = len(chars)
        entropy = 0.0

        for count in counter.values():
            p = count / total
            entropy -= p * np.log2(p)

        return entropy

    def calculate_entropy_profile(self, text: str) -> List[float]:
        """
        テキスト全体のエントロピープロファイルを計算

        Parameters:
            text: 入力テキスト

        Returns:
            各位置でのエントロピー値のリスト
        """
        if len(text) < self.window_size:
            return [0.0] * len(text)

        profile = []

        for i in range(len(text)):
            start = max(0, i - self.window_size)
            end = min(len(text), i + self.window_size + 1)
            window = list(text[start:end])

            entropy = self._calculate_entropy(window)
            profile.append(entropy)

        return profile

    def detect_pacing_issues(self, text: str, segment_size: int = 100) -> List[PacingIssue]:
        """
        テキストのリズム問題を検出

        Parameters:
            text: 入力テキスト
            segment_size: 分析単位（文字数）

        Returns:
            検出されたリズム問題のリスト
        """
        if len(text) < self.min_segment_length:
            return []

        # セグメント分割
        segments = []
        for i in range(0, len(text), segment_size):
            segment = text[i:i + segment_size]
            if len(segment) >= self.min_segment_length // 2:
                segments.append((i, min(i + segment_size, len(text)), segment))

        issues = []

        for seg_start, seg_end, segment in segments:
            entropy = self._calculate_entropy(list(segment))

            # リズム分類
            if entropy < self.BORING_THRESHOLD:
                issue_type = 'boring'
                severity = 'critical' if entropy < 3.0 else 'warning'
                message = (
                    f"🚨 【退屈な区間】(#{seg_start}-{seg_end})\n"
                    f"エントロピー: {entropy:.2f} (基準値: {self.BORING_THRESHOLD})\n"
                    f"→ 予測可能な文字パターンが続いており、読者が飽きやすい状態です。\n"
                    f"→ 語彙を変更したり、文構造を複雑にして変化をつけてください。"
                )

            elif entropy > self.CHAOTIC_THRESHOLD:
                issue_type = 'chaotic'
                severity = 'warning'
                message = (
                    f"⚠️ 【難解な区間】(#{seg_start}-{seg_end})\n"
                    f"エントロピー: {entropy:.2f} (基準値: {self.CHAOTIC_THRESHOLD})\n"
                    f"→ 文字パターンがランダムすぎて、読者が理解しづらい可能性があります。\n"
                    f"→ 専門用語が多い、または文構造が複雑すぎないか確認してください。"
                )

            else:
                continue  # 最適範囲内

            issues.append(
                PacingIssue(
                    segment_start=seg_start,
                    segment_end=seg_end,
                    length=seg_end - seg_start,
                    entropy=entropy,
                    issue_type=issue_type,
                    severity=severity,
                    message=message
                )
            )

        return issues

    def get_pacing_report(self, text: str) -> str:
        """
        テキストのリズム分析レポートを生成

        Parameters:
            text: 分析対象テキスト

        Returns:
            レポート文字列
        """
        issues = self.detect_pacing_issues(text)
        profile = self.calculate_entropy_profile(text)

        if not profile:
            return "テキストが短すぎて分析できません。"

        avg_entropy = np.mean(profile)
        max_entropy = np.max(profile)
        min_entropy = np.min(profile)

        report = f"""
{'='*70}
【文体リズム分析レポート】
{'='*70}

エントロピー統計:
  - 平均: {avg_entropy:.2f}
  - 最大: {max_entropy:.2f}
  - 最小: {min_entropy:.2f}
  - 最適範囲: {self.OPTIMAL_MIN:.2f} - {self.OPTIMAL_MAX:.2f}

判定:
"""

        if self.OPTIMAL_MIN <= avg_entropy <= self.OPTIMAL_MAX:
            report += f"  ✅ 全体的に良好なペースです。\n"
        elif avg_entropy < self.BORING_THRESHOLD:
            report += f"  ❌ 平均的に退屈気味です。\n"
        elif avg_entropy > self.CHAOTIC_THRESHOLD:
            report += f"  ❌ 平均的に難解です。\n"

        report += f"""
{'-'*70}
問題検出: {len(issues)}件
"""

        if issues:
            boring = [i for i in issues if i.issue_type == 'boring']
            chaotic = [i for i in issues if i.issue_type == 'chaotic']

            if boring:
                report += f"\n【退屈な区間】({len(boring)}件):\n"
                for issue in boring:
                    report += f"  {issue.message}\n\n"

            if chaotic:
                report += f"\n【難解な区間】({len(chaotic)}件):\n"
                for issue in chaotic:
                    report += f"  {issue.message}\n\n"
        else:
            report += "\n✅ リズムに問題は検出されませんでした。\n"

        report += f"""
{'-'*70}
【改善ヒント】
- 退屈な区間: 同じ文末や単語の繰り返しを避ける
- 難解な区間: 専門用語の説明を追加、または文を短くする
- 理想的なエントロピー: {self.OPTIMAL_MIN:.2f} - {self.OPTIMAL_MAX:.2f}

{'='*70}
"""
        return report
