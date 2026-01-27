"""
計量文学（Stylometry）分析モジュール

文体の定量的特徴（語彙の豊かさ、文字種の比率など）を分析します。
著者判定や文体分類に用いられる統計指標を計算します。
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023-2026"

import re
import numpy as np
from collections import Counter
from typing import Dict, List
import logging

logger = logging.getLogger(__name__)


class StylometryAnalyzer:
    """
    文体の定量的特徴を分析

    以下の指標を計算します：
    - TTR (Type-Token Ratio): 基本的な語彙多様性
    - Yule's K: 長文に強い語彙多様性指標
    - 文字種比率: 漢字/ひらがな/カタカナ/その他の構成

    使用例:
        >>> analyzer = StylometryAnalyzer()
        >>> result = analyzer.analyze_vocabulary_richness(text)
        >>> print(f"TTR: {result['ttr']:.3f}")
    """

    def __init__(self):
        """初期化"""
        pass

    def analyze_vocabulary_richness(self, text: str) -> Dict:
        """
        語彙多様性指標（TTR, Yule's K）を計算

        Parameters:
            text (str): 入力テキスト

        Returns:
            Dict: 語彙多様性の指標
                {
                    'total_tokens_est': int,    # 総トークン数（推定）
                    'unique_types_est': int,    # ユニーク語彙数（推定）
                    'ttr': float,               # Type-Token Ratio (0-1)
                    'yules_k': float,           # Yule's K指標
                    'assessment': str           # 評価テキスト
                }
        """
        try:
            from .extracter import PhraseExtracter

            # 簡易トークナイズ（PhraseExtracterのロジック利用）
            extractor = PhraseExtracter(
                min_count=1, min_length=2, verbose=0
            )
            df = extractor.extract([text])

            if df.empty:
                return {
                    'total_tokens_est': 0,
                    'unique_types_est': 0,
                    'ttr': 0.0,
                    'yules_k': 0.0,
                    'assessment': '分析対象がありません',
                }

            # 列名の互換性処理
            freq_col = 'freq' if 'freq' in df.columns else 'frequency'

            # 総トークン数（推定）
            N = int(df[freq_col].sum())
            # ユニーク語彙数
            V = len(df)

            # TTR (Type-Token Ratio)
            ttr = V / N if N > 0 else 0.0

            # Yule's K
            # K = 10^4 * (Σ(freq²) - N) / N²
            # 値が大きいほど語彙が単調（繰り返しが多い）
            sum_freq_sq = (df[freq_col] ** 2).sum()
            yules_k = 10000 * (sum_freq_sq - N) / (N ** 2) if N > 0 else 0.0

            return {
                'total_tokens_est': N,
                'unique_types_est': V,
                'ttr': round(ttr, 4),
                'yules_k': round(yules_k, 2),
                'assessment': self._assess_richness(yules_k),
            }

        except ImportError:
            logger.warning("StylometryAnalyzer: PhraseExtracterが必要です")
            return {
                'total_tokens_est': 0,
                'unique_types_est': 0,
                'ttr': 0.0,
                'yules_k': 0.0,
                'assessment': 'PhraseExtracterが未導入',
            }

    def analyze_char_type_ratio(self, text: str) -> Dict:
        """
        文字種比率（文体指紋）を計算

        Parameters:
            text (str): 入力テキスト

        Returns:
            Dict: 文字種の比率と文体診断
                {
                    'kanji_ratio': float,      # 漢字の比率
                    'hiragana_ratio': float,   # ひらがなの比率
                    'katakana_ratio': float,   # カタカナの比率
                    'ascii_ratio': float,      # ASCII（英数字など）の比率
                    'other_ratio': float,      # その他の比率
                    'style_type': str          # 文体の分類
                }
        """
        counts = Counter()
        total = 0

        for char in text:
            if char.isspace():
                continue
            total += 1
            if re.match(r'[一-龠]', char):
                counts['kanji'] += 1
            elif re.match(r'[ぁ-ん]', char):
                counts['hiragana'] += 1
            elif re.match(r'[ァ-ヶ]', char):
                counts['katakana'] += 1
            elif re.match(r'[a-zA-Z0-9]', char):
                counts['ascii'] += 1
            else:
                counts['other'] += 1

        if total == 0:
            return {
                'kanji_ratio': 0.0,
                'hiragana_ratio': 0.0,
                'katakana_ratio': 0.0,
                'ascii_ratio': 0.0,
                'other_ratio': 0.0,
                'style_type': '分析対象がありません',
            }

        kanji_ratio = counts['kanji'] / total
        hiragana_ratio = counts['hiragana'] / total
        katakana_ratio = counts['katakana'] / total
        ascii_ratio = counts['ascii'] / total
        other_ratio = counts['other'] / total

        return {
            'kanji_ratio': round(kanji_ratio, 3),
            'hiragana_ratio': round(hiragana_ratio, 3),
            'katakana_ratio': round(katakana_ratio, 3),
            'ascii_ratio': round(ascii_ratio, 3),
            'other_ratio': round(other_ratio, 3),
            'style_type': self._guess_style(kanji_ratio),
        }

    def analyze_sentence_length(self, text: str) -> Dict:
        """
        文の長さの統計情報を計算

        Parameters:
            text (str): 入力テキスト

        Returns:
            Dict: 文長の統計
                {
                    'avg_length': float,
                    'std_length': float,
                    'min_length': int,
                    'max_length': int,
                    'sentence_count': int
                }
        """
        # 句読点で分割（。！？）
        sentences = re.split(r'[。！？\n]+', text)
        sentences = [s.strip() for s in sentences if s.strip()]

        if not sentences:
            return {
                'avg_length': 0.0,
                'std_length': 0.0,
                'min_length': 0,
                'max_length': 0,
                'sentence_count': 0,
            }

        lengths = [len(s) for s in sentences]

        return {
            'avg_length': round(np.mean(lengths), 1),
            'std_length': round(np.std(lengths), 1),
            'min_length': int(np.min(lengths)),
            'max_length': int(np.max(lengths)),
            'sentence_count': len(sentences),
        }

    def _assess_richness(self, yules_k: float) -> str:
        """
        Yule's K値から語彙の豊かさを評価

        Parameters:
            yules_k (float): Yule's K値

        Returns:
            str: 評価テキスト
        """
        if yules_k > 200:
            return "語彙の繰り返しが多い（単調）"
        elif yules_k > 150:
            return "やや単調"
        elif yules_k < 50:
            return "語彙が非常に多様（豊か）"
        elif yules_k < 100:
            return "語彙が多様"
        else:
            return "標準的な語彙多様性"

    def _guess_style(self, kanji_ratio: float) -> str:
        """
        漢字比率から文体を推定

        Parameters:
            kanji_ratio (float): 漢字の比率 (0.0-1.0)

        Returns:
            str: 推定される文体
        """
        if kanji_ratio > 0.45:
            return "硬質・論文調（学術的）"
        elif kanji_ratio > 0.35:
            return "標準的（バランス型）"
        elif kanji_ratio > 0.20:
            return "軟質・平易（親しみやすい）"
        else:
            return "非常に軟質（口語的）"

    def analyze_full(self, text: str) -> Dict:
        """
        テキストの全体的な文体分析を実施

        Parameters:
            text (str): 入力テキスト

        Returns:
            Dict: 包括的な文体分析結果
        """
        return {
            'vocabulary': self.analyze_vocabulary_richness(text),
            'character_type': self.analyze_char_type_ratio(text),
            'sentence': self.analyze_sentence_length(text),
        }


# 便利関数
def analyze_stylometry(text: str) -> Dict:
    """
    単一関数での全体的な文体分析

    Parameters:
        text (str): 入力テキスト

    Returns:
        Dict: 全体的な文体分析結果
    """
    analyzer = StylometryAnalyzer()
    return analyzer.analyze_full(text)


def get_stylometry_summary(text: str) -> str:
    """
    文体分析の簡潔なサマリーを返す

    Parameters:
        text (str): 入力テキスト

    Returns:
        str: フォーマットされたサマリー
    """
    analyzer = StylometryAnalyzer()
    result = analyzer.analyze_full(text)

    summary = []
    summary.append("=" * 60)
    summary.append("【文体分析結果】")
    summary.append("=" * 60)

    # 語彙多様性
    vocab = result.get('vocabulary', {})
    summary.append("\n【語彙多様性】")
    summary.append(f"  語彙豊かさ (Yule's K): {vocab.get('yules_k', 0)}")
    summary.append(f"  評価: {vocab.get('assessment', 'N/A')}")

    # 文字種比率
    char_type = result.get('character_type', {})
    summary.append("\n【文字種比率】")
    summary.append(f"  漢字: {char_type.get('kanji_ratio', 0):.1%}")
    summary.append(f"  ひらがな: {char_type.get('hiragana_ratio', 0):.1%}")
    summary.append(f"  カタカナ: {char_type.get('katakana_ratio', 0):.1%}")
    summary.append(f"  文体: {char_type.get('style_type', 'N/A')}")

    # 文長統計
    sent = result.get('sentence', {})
    summary.append("\n【文長統計】")
    summary.append(f"  平均文長: {sent.get('avg_length', 0):.1f}字")
    summary.append(f"  文の数: {sent.get('sentence_count', 0)}")

    summary.append("\n" + "=" * 60)
    return "\n".join(summary)
