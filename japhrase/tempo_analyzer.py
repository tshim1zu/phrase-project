"""
文章テンポスコアモジュール

短文/長文比率と改行密度でテンポを可視化します。

使用例:
    >>> from japhrase import TempoAnalyzer
    >>> analyzer = TempoAnalyzer()
    >>> score = analyzer.analyze("テキスト...")
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2026"

import re
from typing import List, Dict


class TempoAnalyzer:
    """文章のテンポ（リズム）を分析するクラス"""

    def __init__(self, short_threshold: int = 20, long_threshold: int = 60):
        """
        Args:
            short_threshold: 短文の閾値（文字数）
            long_threshold: 長文の閾値（文字数）
        """
        self.short_threshold = short_threshold
        self.long_threshold = long_threshold

    def analyze(self, text: str) -> Dict[str, any]:
        """
        テキストのテンポを分析

        Args:
            text: 分析対象のテキスト

        Returns:
            分析結果の辞書
        """
        sentences = self._split_sentences(text)

        if not sentences:
            return self._empty_result()

        # 各文の長さを計算
        lengths = [len(s.strip()) for s in sentences]

        # 短文・中文・長文の分類
        short_count = sum(1 for l in lengths if l <= self.short_threshold)
        long_count = sum(1 for l in lengths if l >= self.long_threshold)
        medium_count = len(lengths) - short_count - long_count

        # 改行の数を数える
        newline_count = text.count('\n')
        total_chars = len(text)

        # テンポスコアの計算
        short_ratio = short_count / len(sentences)
        long_ratio = long_count / len(sentences)
        medium_ratio = medium_count / len(sentences)

        # バランススコア（0-100）: 短文と長文のバランスが良いほど高い
        balance_score = 100 - abs(short_ratio - long_ratio) * 100

        # リズムスコア（0-100）: 文の長さのばらつきが適度にあるほど高い
        if len(lengths) > 1:
            import statistics
            std_dev = statistics.stdev(lengths)
            avg_length = statistics.mean(lengths)
            rhythm_score = min(100, (std_dev / avg_length) * 100) if avg_length > 0 else 0
        else:
            rhythm_score = 0

        # 改行密度（100文字あたりの改行数）
        newline_density = (newline_count / total_chars * 100) if total_chars > 0 else 0

        # 総合テンポスコア
        tempo_score = (balance_score * 0.4 + rhythm_score * 0.4 + min(100, newline_density * 10) * 0.2)

        return {
            'total_sentences': len(sentences),
            'short_count': short_count,
            'medium_count': medium_count,
            'long_count': long_count,
            'short_ratio': short_ratio,
            'medium_ratio': medium_ratio,
            'long_ratio': long_ratio,
            'avg_length': sum(lengths) / len(lengths),
            'min_length': min(lengths),
            'max_length': max(lengths),
            'newline_count': newline_count,
            'newline_density': newline_density,
            'balance_score': balance_score,
            'rhythm_score': rhythm_score,
            'tempo_score': tempo_score,
            'assessment': self._assess_tempo(tempo_score, short_ratio, long_ratio)
        }

    def _empty_result(self) -> Dict[str, any]:
        """空のテキスト用の結果"""
        return {
            'total_sentences': 0,
            'short_count': 0,
            'medium_count': 0,
            'long_count': 0,
            'short_ratio': 0,
            'medium_ratio': 0,
            'long_ratio': 0,
            'avg_length': 0,
            'min_length': 0,
            'max_length': 0,
            'newline_count': 0,
            'newline_density': 0,
            'balance_score': 0,
            'rhythm_score': 0,
            'tempo_score': 0,
            'assessment': '分析対象なし'
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

        if len(sentences) % 2 == 1 and sentences[-1].strip():
            result.append(sentences[-1])

        return result

    def _assess_tempo(self, tempo_score: float, short_ratio: float, long_ratio: float) -> str:
        """テンポスコアから評価を生成"""
        if tempo_score >= 70:
            return '良好：文章のリズムが良く、読みやすいテンポです'
        elif tempo_score >= 50:
            return '普通：文章のテンポは標準的です'
        elif short_ratio > 0.7:
            return '要注意：短文が多すぎます。文を繋げることを検討してください'
        elif long_ratio > 0.7:
            return '要注意：長文が多すぎます。文を分割することを検討してください'
        else:
            return '要改善：文の長さにばらつきを持たせてください'

    def format_report(self, analysis: Dict[str, any]) -> str:
        """分析結果をフォーマット"""
        if analysis['total_sentences'] == 0:
            return "分析対象のテキストがありません。"

        lines = ["\n=== 文章テンポ分析 ===\n"]
        lines.append(f"総文数: {analysis['total_sentences']}")
        lines.append(f"平均文長: {analysis['avg_length']:.1f}文字")
        lines.append(f"最短文: {analysis['min_length']}文字")
        lines.append(f"最長文: {analysis['max_length']}文字")
        lines.append("")

        lines.append("【文の長さ分布】")
        lines.append(f"  短文（{self.short_threshold}文字以下）: {analysis['short_count']}文 ({analysis['short_ratio']*100:.1f}%)")
        lines.append(f"  中文: {analysis['medium_count']}文 ({analysis['medium_ratio']*100:.1f}%)")
        lines.append(f"  長文（{self.long_threshold}文字以上）: {analysis['long_count']}文 ({analysis['long_ratio']*100:.1f}%)")
        lines.append("")

        lines.append("【スコア】")
        lines.append(f"  バランススコア: {analysis['balance_score']:.1f}/100")
        lines.append(f"  リズムスコア: {analysis['rhythm_score']:.1f}/100")
        lines.append(f"  改行密度: {analysis['newline_density']:.2f} (100文字あたり)")
        lines.append(f"  総合テンポスコア: {analysis['tempo_score']:.1f}/100")
        lines.append("")

        lines.append(f"【評価】{analysis['assessment']}")

        return "\n".join(lines)

    def analyze_and_report(self, text: str) -> str:
        """分析とレポート生成を一度に実行"""
        analysis = self.analyze(text)
        return self.format_report(analysis)
