"""
語尾単調警察（Ending Repetition Police）

文末のn文字の連続出現を監視し、退屈な語尾パターンを検出します。
「〜た。〜た。〜た。」のようなダサい文末の連続を許さない。
"""

import re
from typing import List, Dict, Tuple
from dataclasses import dataclass
import logging

logger = logging.getLogger(__name__)


@dataclass
class EndingRepetitionIssue:
    """語尾単調の問題"""
    position: int                    # 開始文番号
    count: int                       # 連続回数
    ending: str                      # 繰り返されている語尾
    sentences: List[str]             # 対象の文（最大5文）
    severity: str                    # 'critical' / 'warning' / 'info'
    message: str                     # メッセージ


class EndingRepetitionPolice:
    """
    文末の単調性を監視する「警察」

    日本語文章で最もダサい表現は「語尾の連続」。
    「〜た。〜た。〜た。」という単調さを検出して警告。
    """

    # 警告基準
    CRITICAL_THRESHOLD = 3        # 3文連続で critical
    WARNING_THRESHOLD = 2         # 2文連続で warning
    ENDING_LENGTH = 2             # 文末n文字を監視

    def __init__(self, ending_length: int = 2, critical_threshold: int = 3):
        """
        Parameters:
            ending_length: 文末何文字を監視するか（デフォルト: 「。」を含む）
            critical_threshold: critical 判定の連続数
        """
        self.ending_length = ending_length
        self.critical_threshold = critical_threshold
        self.warning_threshold = max(2, critical_threshold - 1)

    def _extract_sentences(self, text: str) -> List[str]:
        """テキストから文を抽出（句読点で分割）"""
        # 句点・疑問符を区切り文字として認識しつつ、句点自体は保持
        sentences = re.split(r'(?<=[。！？])\s*', text.strip())
        # 末尾が空の場合は削除
        sentences = [s.strip() for s in sentences if s.strip()]
        return sentences

    def _get_ending(self, sentence: str) -> str:
        """文の末尾n文字を取得"""
        return sentence[-self.ending_length:] if len(sentence) >= self.ending_length else sentence

    def detect(self, text: str) -> List[EndingRepetitionIssue]:
        """
        語尾の単調性を検出

        Parameters:
            text: 分析対象のテキスト

        Returns:
            検出された問題のリスト（位置順）
        """
        sentences = self._extract_sentences(text)
        if len(sentences) < self.warning_threshold:
            return []

        issues = []
        i = 0

        while i < len(sentences):
            ending = self._get_ending(sentences[i])

            # 同じ語尾が何文連続するかカウント
            count = 1
            while i + count < len(sentences) and self._get_ending(sentences[i + count]) == ending:
                count += 1

            # 問題判定
            if count >= self.critical_threshold:
                severity = 'critical'
                severity_label = '🚨 【致命的】'
            elif count >= self.warning_threshold:
                severity = 'warning'
                severity_label = '⚠️ 【警告】'
            else:
                i += count
                continue

            # 対象の文を列挙（最大5文）
            target_sentences = sentences[i:i + min(count, 5)]

            message = (
                f"{severity_label} 語尾「{ending}」が {count} 文連続しています。\n"
                f"文#{i+1} - #{i+count}: \n"
            )
            for idx, sent in enumerate(target_sentences, start=i+1):
                message += f"  {idx}. {sent}{ending}\n"

            issues.append(
                EndingRepetitionIssue(
                    position=i,
                    count=count,
                    ending=ending,
                    sentences=target_sentences,
                    severity=severity,
                    message=message.strip()
                )
            )

            i += count

        return issues

    def get_report(self, text: str) -> str:
        """
        語尾単調性の詳細レポート

        Parameters:
            text: 分析対象テキスト

        Returns:
            レポート文字列
        """
        issues = self.detect(text)

        if not issues:
            return "✅ 語尾の単調性は検出されませんでした。良好です！"

        # 重大度別に分類
        critical = [i for i in issues if i.severity == 'critical']
        warnings = [i for i in issues if i.severity == 'warning']

        report = f"""
{'='*70}
【語尾単調性レポート】
{'='*70}

検出数: {len(issues)}件
  - 致命的: {len(critical)}件
  - 警告: {len(warnings)}件

{'-'*70}
"""

        for issue in critical + warnings:
            report += issue.message + "\n\n"

        report += f"""
{'-'*70}
【改善提案】
- 連続している語尾を異なるものに変更してください
- 例: 「〜た。」→ 「〜だった。」「〜する。」など
- 日本語の文末は最低でも2-3パターンあると自然な流れになります

{'='*70}
"""
        return report

    def suggest_alternatives(self, ending: str) -> List[str]:
        """
        語尾の代替案を提案

        Parameters:
            ending: 現在の語尾（例: 「た。」）

        Returns:
            代替案のリスト
        """
        # 一般的な日本語の文末パターン
        alternatives = {
            'た。': ['する。', 'だった。', 'である。', 'だ。'],
            'だ。': ['である。', 'した。', 'ある。'],
            'する。': ['した。', 'される。', 'できる。'],
            'ている。': ['いた。', 'た。', 'できた。'],
            'です。': ['ました。', 'ある。', 'だ。'],
            'ました。': ['ます。', 'だ。', 'である。'],
        }

        return alternatives.get(ending, [])
