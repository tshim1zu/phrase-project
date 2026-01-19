"""
場面転換の弱さ検出モジュール

時間/場所の変化記述が一定間隔以上無い場合に警告を発します。

使用例:
    >>> from japhrase import SceneTransitionDetector
    >>> detector = SceneTransitionDetector()
    >>> issues = detector.check("テキスト...")
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2026"

import re
from typing import List, Dict


class SceneTransitionDetector:
    """場面転換の不足を検出するクラス"""

    # 時間表現のパターン
    TIME_PATTERNS = [
        r'(昨日|今日|明日|先週|今週|来週|先月|今月|来月|去年|今年|来年)',
        r'(\d+年|\d+月|\d+日|\d+時|\d+分)',
        r'(朝|昼|夜|午前|午後|夕方|深夜|早朝)',
        r'(春|夏|秋|冬)',
        r'(その後|それから|次に|翌日|翌朝|数日後|数週間後|数ヶ月後|数年後)',
        r'(しばらくして|やがて|ついに|まもなく)',
    ]

    # 場所表現のパターン
    PLACE_PATTERNS = [
        r'(学校|会社|家|公園|駅|空港|病院|図書館|カフェ|レストラン|ホテル)',
        r'(教室|部屋|廊下|階段|屋上|地下|玄関|リビング|キッチン)',
        r'(東京|大阪|京都|名古屋|福岡|札幌|仙台|広島|神戸|横浜)',  # 主要都市
        r'([あこその]場所|[あこその]場)',
        r'(〜に|〜へ|〜から|〜まで)(?=向かう|行く|来る|戻る|移動)',
    ]

    def __init__(self, max_gap: int = 10):
        """
        Args:
            max_gap: 場面転換表現の最大間隔（文の数）
        """
        self.max_gap = max_gap

    def check(self, text: str) -> List[Dict[str, any]]:
        """
        テキストの場面転換の不足をチェック

        Args:
            text: チェック対象のテキスト

        Returns:
            問題のリスト
        """
        issues = []
        sentences = self._split_sentences(text)

        if len(sentences) < self.max_gap:
            return issues

        # 各文で時間・場所表現を検出
        time_indices = []
        place_indices = []

        for i, sentence in enumerate(sentences):
            if self._has_time_expression(sentence):
                time_indices.append(i)

            if self._has_place_expression(sentence):
                place_indices.append(i)

        # 時間表現の間隔をチェック
        if time_indices:
            for i in range(len(time_indices) - 1):
                gap = time_indices[i + 1] - time_indices[i]
                if gap > self.max_gap:
                    start_pos = sum(len(s) for s in sentences[:time_indices[i]])
                    end_pos = sum(len(s) for s in sentences[:time_indices[i + 1]])

                    issues.append({
                        'type': 'time_transition_gap',
                        'gap': gap,
                        'position': (start_pos, end_pos),
                        'sentence_range': (time_indices[i], time_indices[i + 1]),
                        'severity': 'warning',
                        'message': f'{gap}文にわたり時間表現がありません（文{time_indices[i]}〜{time_indices[i + 1]}）',
                        'suggestion': '時間の経過を明示する表現を追加してください'
                    })

        # 場所表現の間隔をチェック
        if place_indices:
            for i in range(len(place_indices) - 1):
                gap = place_indices[i + 1] - place_indices[i]
                if gap > self.max_gap:
                    start_pos = sum(len(s) for s in sentences[:place_indices[i]])
                    end_pos = sum(len(s) for s in sentences[:place_indices[i + 1]])

                    issues.append({
                        'type': 'place_transition_gap',
                        'gap': gap,
                        'position': (start_pos, end_pos),
                        'sentence_range': (place_indices[i], place_indices[i + 1]),
                        'severity': 'info',
                        'message': f'{gap}文にわたり場所表現がありません（文{place_indices[i]}〜{place_indices[i + 1]}）',
                        'suggestion': '場面の変化を明示する表現を追加してください'
                    })

        # 最初の時間/場所表現までの距離もチェック
        if time_indices and time_indices[0] > self.max_gap // 2:
            issues.append({
                'type': 'initial_time_missing',
                'gap': time_indices[0],
                'position': (0, sum(len(s) for s in sentences[:time_indices[0]])),
                'sentence_range': (0, time_indices[0]),
                'severity': 'info',
                'message': f'冒頭{time_indices[0]}文に時間表現がありません',
                'suggestion': '物語の時間設定を早めに提示してください'
            })

        return issues

    def _split_sentences(self, text: str) -> List[str]:
        """テキストを文単位に分割"""
        sentences = re.split(r'([。！？])', text)

        result = []
        for i in range(0, len(sentences) - 1, 2):
            if i + 1 < len(sentences):
                sentence = sentences[i] + sentences[i + 1]
                if sentence.strip():
                    result.append(sentence)

        if len(sentences) % 2 == 1 and sentences[-1].strip():
            result.append(sentences[-1])

        return result

    def _has_time_expression(self, sentence: str) -> bool:
        """文に時間表現が含まれているかチェック"""
        for pattern in self.TIME_PATTERNS:
            if re.search(pattern, sentence):
                return True
        return False

    def _has_place_expression(self, sentence: str) -> bool:
        """文に場所表現が含まれているかチェック"""
        for pattern in self.PLACE_PATTERNS:
            if re.search(pattern, sentence):
                return True
        return False

    def analyze_transitions(self, text: str) -> Dict[str, any]:
        """
        場面転換の詳細分析

        Args:
            text: 分析対象のテキスト

        Returns:
            分析結果の辞書
        """
        sentences = self._split_sentences(text)
        time_count = 0
        place_count = 0

        for sentence in sentences:
            if self._has_time_expression(sentence):
                time_count += 1
            if self._has_place_expression(sentence):
                place_count += 1

        return {
            'total_sentences': len(sentences),
            'time_expression_count': time_count,
            'place_expression_count': place_count,
            'time_frequency': time_count / len(sentences) if sentences else 0,
            'place_frequency': place_count / len(sentences) if sentences else 0,
            'avg_time_gap': len(sentences) / (time_count + 1) if time_count > 0 else len(sentences),
            'avg_place_gap': len(sentences) / (place_count + 1) if place_count > 0 else len(sentences)
        }

    def format_issues(self, issues: List[Dict[str, any]]) -> str:
        """問題リストをフォーマット"""
        if not issues:
            return "✓ 場面転換の記述は適切です。"

        lines = [f"\n{len(issues)}件の問題が見つかりました:\n"]

        for i, issue in enumerate(issues, 1):
            severity_icon = {
                'error': '❌',
                'warning': '⚠️',
                'info': 'ℹ️'
            }.get(issue['severity'], '•')

            lines.append(f"{i}. {severity_icon} {issue['message']}")

            if 'suggestion' in issue:
                lines.append(f"   💡 {issue['suggestion']}")

            lines.append("")

        return "\n".join(lines)

    def check_and_report(self, text: str) -> str:
        """チェックとレポート生成を一度に実行"""
        issues = self.check(text)
        return self.format_issues(issues)
