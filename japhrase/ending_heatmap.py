"""
冗長語尾のヒートマップモジュール

文末表現の密度を章単位で可視化し、冗長な文末表現を検出します。

使用例:
    >>> from japhrase import EndingHeatmapGenerator
    >>> generator = EndingHeatmapGenerator()
    >>> heatmap = generator.analyze("テキスト...")
    >>> generator.visualize(heatmap)
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2026"

import re
from typing import List, Dict, Tuple
from collections import defaultdict, Counter


class EndingHeatmapGenerator:
    """文末表現の密度を分析し、ヒートマップを生成するクラス"""

    # 文末表現のパターン定義（より具体的なパターンを先にチェック）
    ENDING_PATTERNS = {
        'past_progressive': [r'(.+)していた([。！？])$', r'(.+)[てで]いた([。！？])$'],
        'continuous': [r'(.+)している([。！？])$', r'(.+)[てで]いる([。！？])$'],
        'past_polite': [r'(.+)ました([。！？])$', r'(.+)でした([。！？])$'],
        'past_plain': [r'(.+)だった([。！？])$', r'(.+)[いきぎしちにびみりっ]た([。！？])$', r'(.+)んだ([。！？])$', r'(.+[^しでま])た([。！？])$'],
        'present_dearu': [r'(.+)である([。！？])$'],
        'present_desu': [r'(.+)です([。！？])$'],
        'present_masu': [r'(.+)ます([。！？])$'],
        'present_da': [r'(.+)だ([。！？])$'],
    }

    # 各パターンの説明
    PATTERN_DESCRIPTIONS = {
        'past_plain': '過去形（〜だった、〜った）',
        'past_progressive': '過去進行形（〜していた、〜ていた）',
        'past_polite': '過去丁寧形（〜ました、〜でした）',
        'present_dearu': '現在形・である調',
        'present_da': '現在形・だ調',
        'present_desu': '現在形・です調',
        'present_masu': '現在形・ます調',
        'continuous': '継続形（〜している、〜ている）',
    }

    def __init__(self, chunk_size: int = 5):
        """
        Args:
            chunk_size: ヒートマップのチャンクサイズ（文の数）
        """
        self.chunk_size = chunk_size

    def analyze(self, text: str) -> Dict[str, any]:
        """
        テキスト全体の文末表現を分析

        Args:
            text: 分析対象のテキスト

        Returns:
            分析結果の辞書
        """
        sentences = self._split_sentences(text)
        chunks = self._create_chunks(sentences)

        # 各チャンクで文末表現をカウント
        chunk_analysis = []
        for i, chunk in enumerate(chunks):
            chunk_data = {
                'chunk_id': i,
                'sentences': chunk,
                'patterns': self._analyze_chunk(chunk)
            }
            chunk_analysis.append(chunk_data)

        # 全体統計
        overall_stats = self._calculate_overall_stats(sentences)

        return {
            'chunk_analysis': chunk_analysis,
            'overall_stats': overall_stats,
            'total_sentences': len(sentences),
            'total_chunks': len(chunks)
        }

    def _split_sentences(self, text: str) -> List[str]:
        """テキストを文単位に分割"""
        sentences = re.split(r'([。！？])', text)

        result = []
        for i in range(0, len(sentences) - 1, 2):
            if i + 1 < len(sentences):
                sentence = sentences[i] + sentences[i + 1]
                if sentence.strip():
                    result.append(sentence)

        # 最後の要素が区切り文字でない場合
        if len(sentences) % 2 == 1 and sentences[-1].strip():
            result.append(sentences[-1])

        return result

    def _create_chunks(self, sentences: List[str]) -> List[List[str]]:
        """文をチャンクに分割"""
        chunks = []
        for i in range(0, len(sentences), self.chunk_size):
            chunk = sentences[i:i + self.chunk_size]
            if chunk:
                chunks.append(chunk)
        return chunks

    def _analyze_chunk(self, sentences: List[str]) -> Dict[str, int]:
        """チャンク内の文末表現をカウント"""
        pattern_counts = defaultdict(int)

        for sentence in sentences:
            # 前後の空白を削除
            sentence = sentence.strip()
            if not sentence:
                continue

            matched = False
            for pattern_name, patterns in self.ENDING_PATTERNS.items():
                if matched:
                    break
                for pattern in patterns:
                    if re.match(pattern, sentence):
                        pattern_counts[pattern_name] += 1
                        matched = True
                        break  # 内側のループを抜ける

        return dict(pattern_counts)

    def _calculate_overall_stats(self, sentences: List[str]) -> Dict[str, any]:
        """全体統計を計算"""
        pattern_counts = defaultdict(int)

        for sentence in sentences:
            # 前後の空白を削除
            sentence = sentence.strip()
            if not sentence:
                continue

            matched = False
            for pattern_name, patterns in self.ENDING_PATTERNS.items():
                if matched:
                    break
                for pattern in patterns:
                    if re.match(pattern, sentence):
                        pattern_counts[pattern_name] += 1
                        matched = True
                        break

        # 最も頻出する文末表現
        most_common = sorted(pattern_counts.items(), key=lambda x: x[1], reverse=True)

        # 多様性スコア（使用されているパターンの種類数 / 全パターン数）
        diversity_score = len(pattern_counts) / len(self.ENDING_PATTERNS) if pattern_counts else 0

        return {
            'pattern_counts': dict(pattern_counts),
            'most_common': most_common[:3],
            'diversity_score': diversity_score,
            'dominant_pattern': most_common[0] if most_common else None
        }

    def detect_issues(self, analysis: Dict[str, any], threshold: float = 0.6) -> List[Dict[str, any]]:
        """
        文末表現の問題を検出

        Args:
            analysis: analyze() の戻り値
            threshold: 同一パターンの使用率閾値（0.0-1.0）

        Returns:
            問題のリスト
        """
        issues = []

        # チャンク単位での偏りチェック
        for chunk_data in analysis['chunk_analysis']:
            patterns = chunk_data['patterns']
            total = sum(patterns.values())

            if total > 0:
                for pattern_name, count in patterns.items():
                    ratio = count / total

                    if ratio >= threshold:
                        issues.append({
                            'type': 'pattern_overuse_in_chunk',
                            'chunk_id': chunk_data['chunk_id'],
                            'pattern': pattern_name,
                            'pattern_description': self.PATTERN_DESCRIPTIONS.get(pattern_name, pattern_name),
                            'count': count,
                            'total': total,
                            'ratio': ratio,
                            'severity': 'warning' if ratio >= 0.8 else 'info',
                            'message': f'チャンク{chunk_data["chunk_id"]}で「{self.PATTERN_DESCRIPTIONS.get(pattern_name, pattern_name)}」が{ratio*100:.1f}%使用されています'
                        })

        # 全体での偏りチェック
        overall = analysis['overall_stats']
        if overall['dominant_pattern']:
            pattern_name, count = overall['dominant_pattern']
            total = analysis['total_sentences']
            ratio = count / total

            if ratio >= threshold:
                issues.append({
                    'type': 'pattern_overuse_overall',
                    'pattern': pattern_name,
                    'pattern_description': self.PATTERN_DESCRIPTIONS.get(pattern_name, pattern_name),
                    'count': count,
                    'total': total,
                    'ratio': ratio,
                    'severity': 'warning',
                    'message': f'全体で「{self.PATTERN_DESCRIPTIONS.get(pattern_name, pattern_name)}」が{ratio*100:.1f}%使用されています',
                    'suggestion': '文末表現を多様化することで、文章のリズムが向上します'
                })

        return issues

    def format_heatmap(self, analysis: Dict[str, any]) -> str:
        """
        ヒートマップをASCIIアート形式でフォーマット

        Args:
            analysis: analyze() の戻り値

        Returns:
            フォーマットされた文字列
        """
        lines = ["\n=== 文末表現ヒートマップ ===\n"]

        # 全体統計
        overall = analysis['overall_stats']
        lines.append("【全体統計】")
        lines.append(f"総文数: {analysis['total_sentences']}")
        lines.append(f"文末表現の多様性スコア: {overall['diversity_score']:.2f}")

        if overall['most_common']:
            lines.append("\n最頻出の文末表現:")
            for i, (pattern, count) in enumerate(overall['most_common'], 1):
                desc = self.PATTERN_DESCRIPTIONS.get(pattern, pattern)
                ratio = count / analysis['total_sentences'] * 100
                lines.append(f"  {i}. {desc}: {count}回 ({ratio:.1f}%)")

        # チャンク別のヒートマップ
        lines.append(f"\n【チャンク別分析】（{self.chunk_size}文ごと）\n")

        # パターン名の一覧（短縮版）
        pattern_abbrev = {
            'past_plain': '過去',
            'past_progressive': '過去進',
            'past_polite': '過去丁',
            'present_dearu': 'である',
            'present_da': 'だ',
            'present_desu': 'です',
            'present_masu': 'ます',
            'continuous': '継続',
        }

        # ヘッダー
        header = "チャンク | " + " | ".join(f"{abbrev:>6}" for abbrev in pattern_abbrev.values())
        lines.append(header)
        lines.append("-" * len(header))

        # 各チャンクのデータ
        for chunk_data in analysis['chunk_analysis']:
            chunk_id = chunk_data['chunk_id']
            patterns = chunk_data['patterns']

            row = f"   {chunk_id:2d}    |"
            for pattern_name in pattern_abbrev.keys():
                count = patterns.get(pattern_name, 0)
                # ヒートマップ的な表現（数値 + バー）
                bar = self._get_heatmap_bar(count, max_count=self.chunk_size)
                row += f" {count:2d}{bar} |"

            lines.append(row)

        return "\n".join(lines)

    def _get_heatmap_bar(self, count: int, max_count: int) -> str:
        """ヒートマップのバー表現を生成"""
        if count == 0:
            return "    "
        elif count == 1:
            return " ░  "
        elif count <= max_count * 0.4:
            return " ▒  "
        elif count < max_count * 0.9:
            return " ▓  "
        else:
            return " █  "

    def analyze_and_report(self, text: str, threshold: float = 0.6) -> str:
        """分析と レポート生成を一度に実行する便利メソッド"""
        analysis = self.analyze(text)
        issues = self.detect_issues(analysis, threshold)

        report = self.format_heatmap(analysis)

        if issues:
            report += f"\n\n=== 検出された問題 ({len(issues)}件) ===\n"
            for i, issue in enumerate(issues, 1):
                severity_icon = {
                    'error': '❌',
                    'warning': '⚠️',
                    'info': 'ℹ️'
                }.get(issue['severity'], '•')

                report += f"\n{i}. {severity_icon} {issue['message']}"
                if 'suggestion' in issue:
                    report += f"\n   💡 {issue['suggestion']}"
        else:
            report += "\n\n✓ 文末表現のバランスは良好です。"

        return report
