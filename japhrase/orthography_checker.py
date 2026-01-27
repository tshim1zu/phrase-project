"""
表記ゆれ検出モジュール

同一語彙で表記が割れているもの（ゆれ）を統計的に検出します。
編集距離（Levenshtein）とパターンマッチングを組み合わせた検出です。

例：
- コンピューター vs コンピュータ（長音のゆれ）
- 申し込み vs 申込み（送り仮名のゆれ）
- 出来る vs できる（漢字ひらがなのゆれ）
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023-2026"

import re
import pandas as pd
from typing import List, Dict, Tuple, Set
import logging

logger = logging.getLogger(__name__)


class OrthographyVariantDetector:
    """
    統計的アプローチによる表記ゆれ検出

    外部辞書なしに、編集距離と出現パターンから
    「同じ意味だが表記が異なる語」を検出します。

    使用例:
        >>> detector = OrthographyVariantDetector()
        >>> issues = detector.check(text)
        >>> for issue in issues:
        ...     print(issue['message'])
    """

    def __init__(self, similarity_threshold: float = 0.75):
        """
        Parameters:
            similarity_threshold (float): 類似度の閾値（0.0-1.0）
                                         高いほど「確実な重複」のみ検出
        """
        self.threshold = similarity_threshold

    def check(self, text: str) -> List[Dict]:
        """
        テキスト内の表記ゆれ候補を検出

        Parameters:
            text (str): 入力テキスト

        Returns:
            List[Dict]: 検出されたゆれの情報
                [
                    {
                        'type': 'katakana_vowel',      # ゆれの種類
                        'root': '...',
                        'variants': ['...', '...'],
                        'message': '...'
                    },
                    ...
                ]
        """
        try:
            from .extracter import PhraseExtracter

            # フレーズ抽出で頻出語をリストアップ
            extractor = PhraseExtracter(min_count=2, min_length=2, verbose=0)
            df = extractor.extract([text])

            if df.empty:
                return []

            # 列名の互換性処理
            phrase_col = 'seqchar' if 'seqchar' in df.columns else 'phrase'
            phrases = df[phrase_col].tolist()

        except ImportError:
            logger.warning("OrthographyVariantDetector: PhraseExtracterが必要です")
            return []

        variants = []

        # 1. カタカナ長音のゆれチェック
        katakana_phrases = [p for p in phrases if re.match(r'^[ァ-ヶー]+$', p)]
        variants.extend(self._check_katakana_variants(katakana_phrases))

        # 2. 編集距離による類似語チェック
        variants.extend(self._check_edit_distance_variants(phrases))

        return variants

    def _check_katakana_variants(self, phrases: List[str]) -> List[Dict]:
        """
        末尾長音の有無によるゆれを検出

        例: コンピュータ vs コンピューター
        """
        normalized = {}
        found = []

        for p in phrases:
            # 末尾のーを削除して正規化
            norm = p.rstrip('ー')
            if norm not in normalized:
                normalized[norm] = []
            normalized[norm].append(p)

        for norm, group in normalized.items():
            if len(group) > 1:
                found.append(
                    {
                        'type': 'katakana_vowel',
                        'root': norm,
                        'variants': sorted(group),
                        'message': f"カタカナ長音のゆれ: {', '.join(group)}",
                    }
                )

        return found

    def _check_edit_distance_variants(self, phrases: List[str]) -> List[Dict]:
        """
        編集距離が近く、包含関係にないものを検出

        例: 申し込み vs 申込み
        """
        from .similarity import SimilarityAnalyzer

        found = []
        seen: Set[Tuple[str, str]] = set()

        # ソートして長さが近いもの同士を比較しやすくする
        sorted_phrases = sorted(phrases, key=len)

        for i, p1 in enumerate(sorted_phrases):
            # 近傍のみ探索（計算量削減）
            for j in range(i + 1, min(i + 50, len(sorted_phrases))):
                p2 = sorted_phrases[j]

                # 完全一致や包含関係はスキップ
                if p1 == p2 or p1 in p2 or p2 in p1:
                    continue

                # 文字種チェック（構成が似ているか）
                if not self._is_similar_char_type(p1, p2):
                    continue

                # 編集距離による類似度計算
                sim = SimilarityAnalyzer().similarity_levenshtein(p1, p2)

                if sim >= self.threshold:
                    pair_key = tuple(sorted([p1, p2]))
                    if pair_key not in seen:
                        seen.add(pair_key)
                        found.append(
                            {
                                'type': 'similar_spelling',
                                'variants': [p1, p2],
                                'similarity': round(sim, 2),
                                'message': f"類似表記: {p1} / {p2} (類似度: {sim:.2f})",
                            }
                        )

        return found

    def _is_similar_char_type(self, s1: str, s2: str) -> bool:
        """
        文字種構成が似ているか簡易チェック

        漢字とカタカナなど、全く異なる文字体系の比較は避ける
        """

        def get_types(s):
            """文字列に含まれる文字種をセット返し"""
            types = set()
            if re.search(r'[ァ-ヶー]', s):
                types.add('katakana')
            if re.search(r'[一-龠]', s):
                types.add('kanji')
            if re.search(r'[ぁ-ん]', s):
                types.add('hiragana')
            if re.search(r'[a-zA-Z]', s):
                types.add('ascii')
            if re.search(r'[0-9]', s):
                types.add('digit')
            return types

        types1 = get_types(s1)
        types2 = get_types(s2)

        # 共通の文字種があるか（最低1つ共通があればOK）
        return bool(types1 & types2)

    def get_summary(self, issues: List[Dict]) -> str:
        """
        検出結果をテキスト形式でサマリー表示

        Parameters:
            issues (List[Dict]): check()の戻り値

        Returns:
            str: フォーマットされたサマリー
        """
        if not issues:
            return "表記ゆれは検出されませんでした"

        summary = []
        summary.append("=" * 60)
        summary.append("【表記ゆれ検出結果】")
        summary.append("=" * 60)
        summary.append(f"検出件数: {len(issues)}\n")

        for i, issue in enumerate(issues, 1):
            summary.append(f"{i}. {issue['message']}")
            if 'type' in issue:
                summary.append(f"   分類: {issue['type']}")
            summary.append("")

        summary.append("=" * 60)
        return "\n".join(summary)


# 便利関数
def check_orthography(text: str, threshold: float = 0.75) -> List[Dict]:
    """
    単一関数での表記ゆれ検出

    Parameters:
        text (str): 入力テキスト
        threshold (float): 類似度の閾値

    Returns:
        List[Dict]: 検出された表記ゆれ
    """
    detector = OrthographyVariantDetector(similarity_threshold=threshold)
    return detector.check(text)
