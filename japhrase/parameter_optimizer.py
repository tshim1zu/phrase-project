# coding: utf-8
"""
パラメータ自動最適化

テキストの特性に基づいて、最適な min_count、max_length などを推奨
"""

import logging
from dataclasses import dataclass
from typing import List, Dict, Optional, Tuple
import pandas as pd
from collections import Counter
import statistics

logger = logging.getLogger(__name__)


@dataclass
class TextCharacteristics:
    """テキスト特性"""
    total_texts: int                 # テキスト総数
    total_characters: int            # 総文字数
    total_words: int                 # 総単語数
    avg_text_length: float           # 平均テキスト長
    vocabulary_size: int             # 語彙数（ユニークフレーズ）
    vocabulary_diversity: float      # 語彙多様性（0-1）
    repetition_rate: float           # 重複度（0-1）
    max_phrase_length: int           # 最長フレーズ長
    min_phrase_frequency: int        # 最低出現回数
    max_phrase_frequency: int        # 最高出現回数


@dataclass
class ParameterRecommendation:
    """パラメータ推奨"""
    min_count: int
    max_length: int
    min_length: int
    target_phrase_count: int         # 期待されるフレーズ数
    reasoning: Dict[str, str]        # 推奨根拠


class ParameterOptimizer:
    """パラメータ自動最適化"""

    def __init__(self, target_phrase_count: int = 100):
        """
        Parameters:
            target_phrase_count: 目標とするフレーズ数（推奨 50-200）
        """
        self.target_phrase_count = target_phrase_count

    def analyze_text_characteristics(self, texts: List[str]) -> TextCharacteristics:
        """
        テキストの特性を分析

        Parameters:
            texts: テキストリスト

        Returns:
            TextCharacteristics オブジェクト
        """
        if not texts:
            raise ValueError("テキストリストが空です")

        total_texts = len(texts)
        total_characters = sum(len(text) for text in texts)
        total_words = sum(len(text.split()) for text in texts)

        # 各テキストの長さ
        text_lengths = [len(text) for text in texts]
        avg_text_length = statistics.mean(text_lengths)

        # 文字の出現頻度から多様性を計算
        all_chars = ''.join(texts)
        char_freq = Counter(all_chars)
        total_char_count = len(all_chars)

        # Shannon entropy で多様性を計算
        entropy = 0.0
        for count in char_freq.values():
            p = count / total_char_count
            entropy -= p * (math.log2(p) if p > 0 else 0)

        # エントロピーを 0-1 に正規化
        max_entropy = math.log2(len(char_freq)) if char_freq else 1.0
        vocabulary_diversity = entropy / max_entropy if max_entropy > 0 else 0.0

        # 重複度（同じテキストが何度も出現するか）
        unique_texts = len(set(texts))
        repetition_rate = 1.0 - (unique_texts / total_texts)

        logger.info(f"テキスト分析完了:")
        logger.info(f"  テキスト数: {total_texts}")
        logger.info(f"  平均長: {avg_text_length:.1f}")
        logger.info(f"  語彙多様性: {vocabulary_diversity:.2f}")
        logger.info(f"  重複度: {repetition_rate:.2f}")

        return TextCharacteristics(
            total_texts=total_texts,
            total_characters=total_characters,
            total_words=total_words,
            avg_text_length=avg_text_length,
            vocabulary_size=len(char_freq),
            vocabulary_diversity=vocabulary_diversity,
            repetition_rate=repetition_rate,
            max_phrase_length=max(len(text) for text in texts) if texts else 0,
            min_phrase_frequency=1,
            max_phrase_frequency=total_texts,
        )

    def analyze_phrase_characteristics(self, df_phrases: pd.DataFrame) -> Dict:
        """
        フレーズの特性を分析

        Parameters:
            df_phrases: フレーズDataFrame

        Returns:
            分析結果の辞書
        """
        phrase_col = 'seqchar' if 'seqchar' in df_phrases.columns else 'phrase'
        freq_col = 'freq' if 'freq' in df_phrases.columns else 'frequency'

        freqs = df_phrases[freq_col].tolist()
        lengths = df_phrases[phrase_col].apply(len).tolist()

        return {
            'total_phrases': len(df_phrases),
            'avg_frequency': statistics.mean(freqs),
            'median_frequency': statistics.median(freqs),
            'max_frequency': max(freqs),
            'min_frequency': min(freqs),
            'frequency_stdev': statistics.stdev(freqs) if len(freqs) > 1 else 0,
            'avg_phrase_length': statistics.mean(lengths),
            'median_phrase_length': statistics.median(lengths),
            'max_phrase_length': max(lengths),
            'min_phrase_length': min(lengths),
        }

    def recommend_parameters(
        self,
        texts: List[str],
        df_phrases: Optional[pd.DataFrame] = None
    ) -> ParameterRecommendation:
        """
        パラメータを推奨

        Parameters:
            texts: テキストリスト
            df_phrases: 現在のフレーズDataFrame（参考用）

        Returns:
            ParameterRecommendation オブジェクト
        """
        import math

        # テキスト特性を分析
        text_chars = self.analyze_text_characteristics(texts)

        # フレーズ特性を分析
        phrase_chars = None
        if df_phrases is not None and not df_phrases.empty:
            phrase_chars = self.analyze_phrase_characteristics(df_phrases)

        reasoning = {}

        # min_count を推奨
        min_count = self._recommend_min_count(
            text_chars,
            phrase_chars,
            reasoning
        )

        # max_length を推奨
        max_length = self._recommend_max_length(
            text_chars,
            phrase_chars,
            reasoning
        )

        # min_length を推奨
        min_length = self._recommend_min_length(
            text_chars,
            phrase_chars,
            reasoning
        )

        # 期待フレーズ数
        target = self.target_phrase_count

        return ParameterRecommendation(
            min_count=min_count,
            max_length=max_length,
            min_length=min_length,
            target_phrase_count=target,
            reasoning=reasoning,
        )

    def _recommend_min_count(
        self,
        text_chars: TextCharacteristics,
        phrase_chars: Optional[Dict],
        reasoning: Dict
    ) -> int:
        """min_count を推奨"""
        # テキスト量ベースの推奨
        if text_chars.total_texts < 100:
            min_count = 2
            reasoning['min_count'] = (
                f"テキスト数が {text_chars.total_texts} と少なめなため、"
                f"min_count=2 で感度を上げます"
            )
        elif text_chars.total_texts < 500:
            min_count = 3
            reasoning['min_count'] = (
                f"テキスト数 {text_chars.total_texts} から、"
                f"min_count=3 を推奨します"
            )
        elif text_chars.total_texts < 2000:
            min_count = 5
            reasoning['min_count'] = (
                f"テキスト数 {text_chars.total_texts} で中規模データ。"
                f"min_count=5 を推奨します"
            )
        else:
            min_count = max(10, text_chars.total_texts // 200)
            reasoning['min_count'] = (
                f"大規模テキスト ({text_chars.total_texts} 行) なので、"
                f"min_count={min_count} でノイズを削減します"
            )

        # 語彙多様性で調整
        if text_chars.vocabulary_diversity < 0.3:
            # 語彙が限定的 → min_count を下げる
            min_count = max(2, min_count - 1)
            reasoning['min_count'] += (
                f"。語彙多様性が低い ({text_chars.vocabulary_diversity:.2f}) ため、"
                f"min_count を {min_count} に調整"
            )
        elif text_chars.vocabulary_diversity > 0.8:
            # 語彙が豊富 → min_count を上げる
            min_count = min_count + 1
            reasoning['min_count'] += (
                f"。語彙多様性が高い ({text_chars.vocabulary_diversity:.2f}) ため、"
                f"min_count を {min_count} に調整"
            )

        return min_count

    def _recommend_max_length(
        self,
        text_chars: TextCharacteristics,
        phrase_chars: Optional[Dict],
        reasoning: Dict
    ) -> int:
        """max_length を推奨"""
        avg_length = text_chars.avg_text_length

        if avg_length < 20:
            # 短いテキスト → 短いフレーズ
            max_length = 4
            reasoning['max_length'] = (
                f"テキストが短い (平均 {avg_length:.1f} 文字) ため、"
                f"max_length=4 で短いフレーズに絞ります"
            )
        elif avg_length < 50:
            max_length = 6
            reasoning['max_length'] = (
                f"テキスト長 (平均 {avg_length:.1f} 文字) から、"
                f"max_length=6 を推奨します"
            )
        elif avg_length < 100:
            max_length = 8
            reasoning['max_length'] = (
                f"テキスト長 (平均 {avg_length:.1f} 文字) から、"
                f"max_length=8 を推奨します"
            )
        else:
            max_length = 10
            reasoning['max_length'] = (
                f"長めのテキスト (平均 {avg_length:.1f} 文字) なので、"
                f"max_length=10 で複雑なフレーズを許可"
            )

        return max_length

    def _recommend_min_length(
        self,
        text_chars: TextCharacteristics,
        phrase_chars: Optional[Dict],
        reasoning: Dict
    ) -> int:
        """min_length を推奨"""
        # 1文字フレーズはノイズが多い
        min_length = 2

        if text_chars.total_texts > 1000:
            min_length = 2
            reasoning['min_length'] = "1文字フレーズはノイズなので除外、min_length=2"
        else:
            min_length = 1
            reasoning['min_length'] = "小規模データなので min_length=1 で感度重視"

        return min_length

    def generate_report(self, recommendation: ParameterRecommendation, texts: List[str] = None) -> str:
        """パラメータ推奨レポートを生成"""
        report = "【パラメータ最適化レポート】\n\n"

        report += "【推奨パラメータ】\n"
        report += f"  min_count: {recommendation.min_count}\n"
        report += f"  max_length: {recommendation.max_length}\n"
        report += f"  min_length: {recommendation.min_length}\n"
        report += f"  期待フレーズ数: {recommendation.target_phrase_count}\n\n"

        report += "【推奨根拠】\n"
        for key, reason in recommendation.reasoning.items():
            report += f"  • {key}: {reason}\n"

        if texts:
            text_chars = self.analyze_text_characteristics(texts)
            report += f"\n【テキスト特性】\n"
            report += f"  テキスト数: {text_chars.total_texts}\n"
            report += f"  平均テキスト長: {text_chars.avg_text_length:.1f}\n"
            report += f"  語彙多様性: {text_chars.vocabulary_diversity:.2f}\n"
            report += f"  重複度: {text_chars.repetition_rate:.2f}\n"

        return report


# 追加の import
import math
