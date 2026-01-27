"""
会話・地の文バランス分析モジュール

テキストを会話文と地の文（ナレーション）に分離し、
それぞれの比率・特徴を統計的に分析します。
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023-2026"

import re
import pandas as pd
from typing import Dict, List, Optional, Tuple
import logging

logger = logging.getLogger(__name__)


class DialogueAnalyzer:
    """
    会話文と地の文の比率や特徴を分析するクラス

    日本語の一般的な括弧ペア（「」『』など）に対応しています。

    使用例:
        >>> analyzer = DialogueAnalyzer()
        >>> result = analyzer.analyze(text)
        >>> print(f"会話比率: {result['dialogue_ratio']:.2%}")
    """

    def __init__(self):
        """括弧パターンを定義"""
        # 一般的な括弧ペア（優先度順）
        self.patterns = [
            (re.compile(r'「(.*?)」', re.DOTALL), '「」'),
            (re.compile(r'『(.*?)』', re.DOTALL), '『』'),
            (re.compile(r'\"(.*?)\"', re.DOTALL), '""'),
            (re.compile(r"'(.*?)'", re.DOTALL), "''"),
        ]

    def analyze(self, text: str, extract_features: bool = False) -> Dict:
        """
        テキストを会話文と地の文に分離して分析

        Parameters:
            text (str): 入力テキスト
            extract_features (bool): Trueの場合、会話/地の文それぞれの特徴語を抽出する
                                     （PhraseExtracterが必要）

        Returns:
            Dict: 分析結果
                {
                    'total_characters': int,        # 空白・改行除外後の総文字数
                    'dialogue_ratio': float,        # 会話文の比率 (0.0-1.0)
                    'narrative_ratio': float,       # 地の文の比率 (0.0-1.0)
                    'dialogue_count': int,          # 会話文の数
                    'dialogue_avg_len': float,      # 会話文の平均長
                    'dialogue_keywords': List[str], # （extract_features=Trueの場合）
                    'narrative_keywords': List[str] # （extract_features=Trueの場合）
                }
        """
        dialogue_parts = []
        narrative_text = text

        # メインの括弧（「」）を優先処理
        pattern, bracket_type = self.patterns[0]
        matches = pattern.findall(text)

        for m in matches:
            dialogue_parts.append(m)
            # 地の文から会話を削除（正確な除去のため）
            narrative_text = narrative_text.replace(f'「{m}」', '', 1)

        # 空白・改行を除去してクリーン化
        narrative_clean = re.sub(r'\s+', '', narrative_text)
        dialogue_clean = "".join(dialogue_parts)

        # 総文字数（空白・改行除外）
        total_char = len(text.replace('\n', '').replace(' ', '').replace('　', ''))
        if total_char == 0:
            return {
                'total_characters': 0,
                'dialogue_ratio': 0.0,
                'narrative_ratio': 0.0,
                'dialogue_count': 0,
                'dialogue_avg_len': 0.0,
            }

        # 比率計算
        dialogue_ratio = len(dialogue_clean) / total_char if total_char > 0 else 0.0
        narrative_ratio = len(narrative_clean) / total_char if total_char > 0 else 0.0

        result = {
            'total_characters': total_char,
            'dialogue_ratio': dialogue_ratio,
            'narrative_ratio': narrative_ratio,
            'dialogue_count': len(dialogue_parts),
            'dialogue_avg_len': len(dialogue_clean) / len(dialogue_parts) if dialogue_parts else 0.0,
        }

        # 特徴語抽出（オプション）
        if extract_features:
            try:
                from .extracter import PhraseExtracter

                extractor = PhraseExtracter(min_count=2, verbose=0)

                # 会話文の特徴語
                if dialogue_parts:
                    df_diag = extractor.extract(dialogue_parts)
                    result['dialogue_keywords'] = (
                        df_diag.head(10)['seqchar'].tolist()
                        if not df_diag.empty and 'seqchar' in df_diag.columns
                        else []
                    )
                else:
                    result['dialogue_keywords'] = []

                # 地の文の特徴語
                if narrative_clean:
                    narrative_sentences = narrative_text.split('。')
                    narrative_sentences = [s for s in narrative_sentences if s.strip()]
                    if narrative_sentences:
                        df_narr = extractor.extract(narrative_sentences)
                        result['narrative_keywords'] = (
                            df_narr.head(10)['seqchar'].tolist()
                            if not df_narr.empty and 'seqchar' in df_narr.columns
                            else []
                        )
                    else:
                        result['narrative_keywords'] = []
                else:
                    result['narrative_keywords'] = []

            except ImportError:
                logger.warning("extract_features=Trueの場合、PhraseExtracterが必要です")

        return result

    def get_summary(self, result: Dict) -> str:
        """
        分析結果をテキスト形式でサマリー表示

        Parameters:
            result (Dict): analyze()の戻り値

        Returns:
            str: フォーマットされたサマリー
        """
        if not result:
            return "分析結果がありません"

        summary = []
        summary.append("=" * 60)
        summary.append("【会話・地の文バランス分析結果】")
        summary.append("=" * 60)
        summary.append(f"総文字数: {result.get('total_characters', 0)}")
        summary.append(f"会話文比率: {result.get('dialogue_ratio', 0.0):.1%}")
        summary.append(f"地の文比率: {result.get('narrative_ratio', 0.0):.1%}")
        summary.append(f"会話文数: {result.get('dialogue_count', 0)}")
        summary.append(f"会話文平均長: {result.get('dialogue_avg_len', 0.0):.1f}字")

        if 'dialogue_keywords' in result and result['dialogue_keywords']:
            summary.append("\n【会話文の特徴語】")
            for i, kw in enumerate(result['dialogue_keywords'][:5], 1):
                summary.append(f"  {i}. {kw}")

        if 'narrative_keywords' in result and result['narrative_keywords']:
            summary.append("\n【地の文の特徴語】")
            for i, kw in enumerate(result['narrative_keywords'][:5], 1):
                summary.append(f"  {i}. {kw}")

        summary.append("=" * 60)
        return "\n".join(summary)


# 便利関数
def analyze_dialogue(text: str, extract_features: bool = False) -> Dict:
    """
    単一関数での会話分析

    Parameters:
        text (str): 入力テキスト
        extract_features (bool): 特徴語抽出するかどうか

    Returns:
        Dict: 分析結果
    """
    analyzer = DialogueAnalyzer()
    return analyzer.analyze(text, extract_features=extract_features)
