"""
比喩・専門語の過密警告モジュール

ITメタファーや専門用語の密度を章・エピソード単位で検出し、警告を発します。

使用例:
    >>> from japhrase import MetaphorDensityDetector
    >>> detector = MetaphorDensityDetector()
    >>> issues = detector.check("テキスト...")
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2026"

import re
from typing import List, Dict, Set
from collections import defaultdict


class MetaphorDensityDetector:
    """比喩・専門語の密度を検出するクラス"""

    # ITメタファーのキーワード
    IT_METAPHORS = [
        'バグ', 'デバッグ', 'ハック', 'アルゴリズム', 'プログラム', 'コード',
        'システム', 'インターフェース', 'ログイン', 'ログアウト', 'リセット',
        'バッファ', 'キャッシュ', 'メモリ', 'プロセス', 'スレッド',
        'コンパイル', '実装', '最適化', 'レスポンス', 'アクセス',
        'ダウンロード', 'アップロード', 'クリック', 'タップ', 'スワイプ',
        'ブート', 'リブート', 'シャットダウン', 'フリーズ', 'クラッシュ',
        'エラー', 'ワーニング', 'エクスポート', 'インポート',
    ]

    # 技術専門用語（カタカナ語）のパターン
    KATAKANA_PATTERN = re.compile(r'[ァ-ヴー]{4,}')

    def __init__(self, threshold: float = 0.15, chunk_size: int = 10):
        """
        Args:
            threshold: 密度の閾値（単語数に対する比率）
            chunk_size: チャンクサイズ（文の数）
        """
        self.threshold = threshold
        self.chunk_size = chunk_size

    def check(self, text: str, custom_metaphors: List[str] = None) -> List[Dict[str, any]]:
        """
        テキストの比喩・専門語の密度をチェック

        Args:
            text: チェック対象のテキスト
            custom_metaphors: カスタムメタファーのリスト

        Returns:
            問題のリスト
        """
        issues = []
        metaphors = self.IT_METAPHORS.copy()
        if custom_metaphors:
            metaphors.extend(custom_metaphors)

        # チャンク単位で分析
        sentences = self._split_sentences(text)
        chunks = self._create_chunks(sentences)
        position = 0

        for i, chunk in enumerate(chunks):
            chunk_text = ''.join(chunk)
            chunk_issues = self._check_chunk(chunk_text, metaphors, i, position)
            issues.extend(chunk_issues)
            position += len(chunk_text)

        # 全体の密度もチェック
        overall_issues = self._check_overall(text, metaphors)
        issues.extend(overall_issues)

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

    def _create_chunks(self, sentences: List[str]) -> List[List[str]]:
        """文をチャンクに分割"""
        chunks = []
        for i in range(0, len(sentences), self.chunk_size):
            chunk = sentences[i:i + self.chunk_size]
            if chunk:
                chunks.append(chunk)
        return chunks

    def _check_chunk(self, chunk_text: str, metaphors: List[str], chunk_id: int, position: int) -> List[Dict[str, any]]:
        """チャンク内の比喩・専門語密度をチェック"""
        issues = []

        # IT メタファーの出現回数
        it_count = 0
        found_metaphors = []
        for metaphor in metaphors:
            count = chunk_text.count(metaphor)
            if count > 0:
                it_count += count
                found_metaphors.append((metaphor, count))

        # カタカナ専門用語の検出
        katakana_terms = self.KATAKANA_PATTERN.findall(chunk_text)
        katakana_count = len(set(katakana_terms))  # ユニークな用語数

        # 文字数に対する密度を計算
        char_count = len(chunk_text)
        if char_count > 0:
            it_density = it_count / (char_count / 100)  # 100文字あたりの出現回数
            katakana_density = katakana_count / (char_count / 100)

            # 閾値を超えた場合
            if it_density >= self.threshold * 10:  # 調整
                issues.append({
                    'type': 'it_metaphor_overuse',
                    'chunk_id': chunk_id,
                    'position': (position, position + char_count),
                    'count': it_count,
                    'density': it_density,
                    'severity': 'warning',
                    'message': f'チャンク{chunk_id}でITメタファーが過密です（100文字あたり{it_density:.1f}回）',
                    'found_metaphors': found_metaphors[:5],  # 上位5個
                    'suggestion': 'ITメタファーの使用を減らすか、より平易な表現に置き換えてください'
                })

            if katakana_density >= self.threshold * 5:  # 調整
                issues.append({
                    'type': 'katakana_term_overuse',
                    'chunk_id': chunk_id,
                    'position': (position, position + char_count),
                    'count': katakana_count,
                    'density': katakana_density,
                    'severity': 'info',
                    'message': f'チャンク{chunk_id}でカタカナ専門用語が多用されています（{katakana_count}種類）',
                    'found_terms': list(set(katakana_terms))[:5],
                    'suggestion': '読者の理解を助けるため、専門用語の説明を追加することを検討してください'
                })

        return issues

    def _check_overall(self, text: str, metaphors: List[str]) -> List[Dict[str, any]]:
        """全体の密度をチェック"""
        issues = []

        # 全体のIT メタファー出現回数
        it_count = sum(text.count(m) for m in metaphors)
        char_count = len(text)

        if char_count > 0:
            it_density = it_count / (char_count / 100)

            if it_density >= self.threshold * 8:
                issues.append({
                    'type': 'overall_metaphor_overuse',
                    'position': (0, len(text)),
                    'count': it_count,
                    'density': it_density,
                    'severity': 'warning',
                    'message': f'全体でITメタファーが過密です（100文字あたり{it_density:.1f}回）',
                    'suggestion': 'テキスト全体でITメタファーの使用頻度を見直してください'
                })

        return issues

    def analyze_metaphors(self, text: str, custom_metaphors: List[str] = None) -> Dict[str, any]:
        """
        テキスト内の比喩・専門語を詳細に分析

        Args:
            text: 分析対象のテキスト
            custom_metaphors: カスタムメタファーのリスト

        Returns:
            分析結果の辞書
        """
        metaphors = self.IT_METAPHORS.copy()
        if custom_metaphors:
            metaphors.extend(custom_metaphors)

        # 各メタファーの出現回数
        metaphor_counts = {}
        for metaphor in metaphors:
            count = text.count(metaphor)
            if count > 0:
                metaphor_counts[metaphor] = count

        # カタカナ専門用語の抽出
        katakana_terms = self.KATAKANA_PATTERN.findall(text)
        katakana_counts = defaultdict(int)
        for term in katakana_terms:
            katakana_counts[term] += 1

        # 最頻出メタファー
        top_metaphors = sorted(metaphor_counts.items(), key=lambda x: x[1], reverse=True)[:10]
        top_katakana = sorted(katakana_counts.items(), key=lambda x: x[1], reverse=True)[:10]

        return {
            'total_metaphor_count': sum(metaphor_counts.values()),
            'unique_metaphor_count': len(metaphor_counts),
            'top_metaphors': top_metaphors,
            'total_katakana_count': len(katakana_terms),
            'unique_katakana_count': len(katakana_counts),
            'top_katakana_terms': top_katakana,
            'char_count': len(text),
            'metaphor_density': sum(metaphor_counts.values()) / (len(text) / 100) if text else 0
        }

    def format_issues(self, issues: List[Dict[str, any]]) -> str:
        """問題リストをフォーマット"""
        if not issues:
            return "✓ 比喩・専門語の密度は適切です。"

        lines = [f"\n{len(issues)}件の問題が見つかりました:\n"]

        for i, issue in enumerate(issues, 1):
            severity_icon = {
                'error': '❌',
                'warning': '⚠️',
                'info': 'ℹ️'
            }.get(issue['severity'], '•')

            lines.append(f"{i}. {severity_icon} {issue['message']}")

            if 'found_metaphors' in issue:
                lines.append(f"   頻出メタファー: {', '.join(f'{m}({c}回)' for m, c in issue['found_metaphors'])}")

            if 'found_terms' in issue:
                lines.append(f"   カタカナ用語: {', '.join(issue['found_terms'][:5])}")

            if 'suggestion' in issue:
                lines.append(f"   💡 {issue['suggestion']}")

            lines.append("")

        return "\n".join(lines)

    def check_and_report(self, text: str, custom_metaphors: List[str] = None) -> str:
        """チェックとレポート生成を一度に実行"""
        issues = self.check(text, custom_metaphors)
        return self.format_issues(issues)
