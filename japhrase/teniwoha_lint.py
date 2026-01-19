"""
てにをはLint: 助詞の連続/欠落/不自然連接を検出するモジュール

使用例:
    >>> from japhrase import TeniwohaLinter
    >>> linter = TeniwohaLinter()
    >>> issues = linter.check("彼女が私が好きだと言った。")
    >>> for issue in issues:
    ...     print(f"{issue['type']}: {issue['message']}")
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2026"

import re
import logging
from typing import List, Dict, Tuple

from .utils_robustness import TextProcessor, ConfigValidator
from .utils_advanced import MetricsCollector, StreamingAnalyzer

logger = logging.getLogger(__name__)


class TeniwohaLinter:
    """助詞の使用状況を検証し、不自然な使い方を検出するリンター"""

    # 基本的な助詞のリスト
    PARTICLES = ['が', 'を', 'に', 'へ', 'と', 'から', 'まで', 'より', 'で', 'や', 'か',
                 'は', 'も', 'こそ', 'さえ', 'でも', 'しか', 'ばかり', 'だけ', 'ほど',
                 'など', 'なり', 'やら', 'ずつ', 'のみ']

    # 格助詞（特に重複をチェックすべき）
    CASE_PARTICLES = ['が', 'を', 'に', 'へ', 'と', 'から', 'まで', 'より', 'で']

    # 係助詞
    KAKARI_PARTICLES = ['は', 'も', 'こそ', 'さえ', 'でも', 'しか']

    def __init__(self, strict_mode: bool = False):
        """
        Args:
            strict_mode: Trueの場合、より厳しいルールを適用
        """
        self.strict_mode = strict_mode

    def check(self, text: str) -> List[Dict[str, any]]:
        """
        テキスト全体をチェックし、問題のある箇所を返す

        Args:
            text: チェック対象のテキスト

        Returns:
            問題のリスト。各要素は以下の辞書:
                - type: 問題の種類
                - message: 説明メッセージ
                - position: 位置情報（開始位置、終了位置）
                - severity: 重要度（'error', 'warning', 'info'）
                - suggestion: 修正案（オプション）
        """
        issues = []

        # 文単位で分割してチェック
        sentences = self._split_sentences(text)
        position = 0

        for sentence in sentences:
            if not sentence.strip():
                position += len(sentence)
                continue

            # 各種チェックを実行
            issues.extend(self._check_particle_repetition(sentence, position))
            issues.extend(self._check_particle_sequence(sentence, position))
            issues.extend(self._check_particle_overuse(sentence, position))

            position += len(sentence)

        return sorted(issues, key=lambda x: x['position'][0])

    def _split_sentences(self, text: str) -> List[str]:
        """テキストを文単位に分割"""
        # 句点、感嘆符、疑問符で分割
        sentences = re.split(r'([。！？\n])', text)

        # 区切り文字を前の文に結合
        result = []
        for i in range(0, len(sentences) - 1, 2):
            if i + 1 < len(sentences):
                result.append(sentences[i] + sentences[i + 1])
            else:
                result.append(sentences[i])

        # 最後の要素が区切り文字でない場合
        if len(sentences) % 2 == 1:
            result.append(sentences[-1])

        return result

    def _check_particle_repetition(self, sentence: str, offset: int) -> List[Dict[str, any]]:
        """同じ助詞の連続をチェック"""
        issues = []

        # 格助詞の重複チェック（「が が」「を を」など）
        for particle in self.CASE_PARTICLES:
            # 助詞の間に数文字以内の繰り返しを検出
            pattern = f'{particle}([^{particle}]{{0,10}}?){particle}'
            matches = re.finditer(pattern, sentence)

            for match in matches:
                start = offset + match.start()
                end = offset + match.end()
                middle = match.group(1)

                # 間に他の助詞がある場合は問題なし
                if any(p in middle for p in self.PARTICLES):
                    continue

                issues.append({
                    'type': 'particle_repetition',
                    'message': f'助詞「{particle}」が近接して繰り返されています: 「{match.group(0)}」',
                    'position': (start, end),
                    'severity': 'error',
                    'particle': particle,
                    'context': match.group(0)
                })

        return issues

    def _check_particle_sequence(self, sentence: str, offset: int) -> List[Dict[str, any]]:
        """不自然な助詞の連続をチェック"""
        issues = []

        # 不自然な助詞の組み合わせパターン
        bad_sequences = [
            ('を', 'を'),  # 「をを」
            ('が', 'が'),  # 「がが」
            ('に', 'に'),  # 「ににに」
            ('は', 'は'),  # 「はは」
            ('も', 'も'),  # 「もも」
            ('で', 'で'),  # 「でで」
        ]

        for p1, p2 in bad_sequences:
            # 直接の連続
            pattern = f'{p1}{p2}'
            matches = re.finditer(pattern, sentence)

            for match in matches:
                start = offset + match.start()
                end = offset + match.end()

                issues.append({
                    'type': 'particle_sequence',
                    'message': f'助詞「{p1}」と「{p2}」が連続しています',
                    'position': (start, end),
                    'severity': 'error',
                    'context': match.group(0)
                })

        return issues

    def _check_particle_overuse(self, sentence: str, offset: int) -> List[Dict[str, any]]:
        """1文内での助詞の過剰使用をチェック"""
        issues = []

        # 文が短い場合はスキップ
        if len(sentence) < 20:
            return issues

        # 各格助詞の出現回数をカウント
        for particle in self.CASE_PARTICLES:
            count = sentence.count(particle)

            # 閾値を超えた場合
            threshold = 3 if not self.strict_mode else 2
            if count > threshold:
                # 最初の出現位置を取得
                first_pos = sentence.find(particle)
                start = offset + first_pos

                issues.append({
                    'type': 'particle_overuse',
                    'message': f'助詞「{particle}」が1文内に{count}回使用されています（推奨: {threshold}回以下）',
                    'position': (start, start + len(particle)),
                    'severity': 'warning',
                    'particle': particle,
                    'count': count,
                    'suggestion': f'文を分割するか、「{particle}」を使わない表現を検討してください'
                })

        return issues

    def format_issues(self, issues: List[Dict[str, any]], text: str = None) -> str:
        """
        問題リストを読みやすい形式にフォーマット

        Args:
            issues: check() の戻り値
            text: 元のテキスト（あればコンテキストを表示）

        Returns:
            フォーマットされた文字列
        """
        if not issues:
            return "✓ 問題は見つかりませんでした。"

        lines = [f"\n{len(issues)}件の問題が見つかりました:\n"]

        for i, issue in enumerate(issues, 1):
            severity_icon = {
                'error': '❌',
                'warning': '⚠️',
                'info': 'ℹ️'
            }.get(issue['severity'], '•')

            lines.append(f"{i}. {severity_icon} {issue['message']}")

            if text and 'position' in issue:
                start, end = issue['position']
                # 前後のコンテキストを表示
                context_start = max(0, start - 20)
                context_end = min(len(text), end + 20)
                context = text[context_start:context_end]

                # 問題箇所をハイライト
                highlight_start = start - context_start
                highlight_end = end - context_start
                highlighted = (
                    context[:highlight_start] +
                    f"[{context[highlight_start:highlight_end]}]" +
                    context[highlight_end:]
                )
                lines.append(f"   位置 {start}-{end}: ...{highlighted}...")

            if 'suggestion' in issue:
                lines.append(f"   💡 提案: {issue['suggestion']}")

            lines.append("")

        return "\n".join(lines)

    def check_and_report(self, text: str) -> str:
        """チェックを実行し、結果をフォーマットして返す便利メソッド"""
        issues = self.check(text)
        return self.format_issues(issues, text)
