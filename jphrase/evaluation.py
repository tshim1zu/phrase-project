# coding:utf-8
"""
評価モジュール
フレーズ抽出結果の品質を評価
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023"

import pandas as pd
import numpy as np
from typing import List, Dict, Optional
from collections import Counter


class UnsupervisedEvaluator:
    """
    教師なし評価器
    ゴールドスタンダードなしで抽出結果の品質を評価

    使用例:
        >>> evaluator = UnsupervisedEvaluator()
        >>> score = evaluator.evaluate(phrases, texts)
    """

    def __init__(
        self,
        weight_diversity: float = 1.0,
        weight_coverage: float = 1.0,
        weight_balance: float = 1.0,
        weight_length: float = 0.5
    ):
        """
        Parameters:
            weight_diversity (float): 多様性の重み
            weight_coverage (float): カバー率の重み
            weight_balance (float): 頻度バランスの重み
            weight_length (float): 平均長の重み
        """
        self.weight_diversity = weight_diversity
        self.weight_coverage = weight_coverage
        self.weight_balance = weight_balance
        self.weight_length = weight_length

    def evaluate(
        self,
        phrases: List[str],
        texts: List[str],
        df: Optional[pd.DataFrame] = None
    ) -> float:
        """
        総合評価スコアを計算

        Parameters:
            phrases (List[str]): 抽出されたフレーズのリスト
            texts (List[str]): 元のテキストのリスト
            df (pd.DataFrame, optional): 抽出結果のDataFrame（頻度情報含む）

        Returns:
            float: 総合評価スコア（0〜1）
        """
        if not phrases or not texts:
            return 0.0

        scores = {
            'diversity': self.calc_diversity(phrases),
            'coverage': self.calc_coverage(phrases, texts),
            'balance': self.calc_balance(df) if df is not None else 0.5,
            'length': self.calc_length_score(phrases)
        }

        # 重み付き平均
        total_weight = (
            self.weight_diversity +
            self.weight_coverage +
            self.weight_balance +
            self.weight_length
        )

        score = (
            scores['diversity'] * self.weight_diversity +
            scores['coverage'] * self.weight_coverage +
            scores['balance'] * self.weight_balance +
            scores['length'] * self.weight_length
        ) / total_weight

        return score

    def calc_diversity(self, phrases: List[str]) -> float:
        """
        多様性スコアを計算
        異なる文字がどれだけ使われているか

        Parameters:
            phrases (List[str]): フレーズのリスト

        Returns:
            float: 多様性スコア（0〜1）
        """
        if not phrases:
            return 0.0

        # 全フレーズの文字を結合
        all_chars = ''.join(phrases)
        if not all_chars:
            return 0.0

        # ユニークな文字数 / 総文字数
        unique_chars = len(set(all_chars))
        total_chars = len(all_chars)

        # 正規化: 日本語は数千文字あるので、適度にスケーリング
        diversity = unique_chars / min(total_chars, 1000)

        return min(diversity, 1.0)

    def calc_coverage(self, phrases: List[str], texts: List[str]) -> float:
        """
        カバー率を計算
        元テキストのどれだけをフレーズでカバーできているか

        Parameters:
            phrases (List[str]): フレーズのリスト
            texts (List[str]): 元のテキストのリスト

        Returns:
            float: カバー率（0〜1）
        """
        if not phrases or not texts:
            return 0.0

        # 全テキストを結合
        full_text = ''.join(texts)
        if not full_text:
            return 0.0

        # フレーズがテキスト内に現れる回数をカウント
        covered_chars = 0
        for phrase in phrases:
            covered_chars += full_text.count(phrase) * len(phrase)

        # カバー率（重複を考慮）
        coverage = min(covered_chars / len(full_text), 1.0)

        return coverage

    def calc_balance(self, df: pd.DataFrame) -> float:
        """
        頻度分布のバランスを評価
        極端に偏っていないか（ジニ係数の逆）

        Parameters:
            df (pd.DataFrame): 抽出結果（freq列を持つ）

        Returns:
            float: バランススコア（0〜1、高いほど均等）
        """
        if df is None or 'freq' not in df.columns or len(df) == 0:
            return 0.5

        frequencies = df['freq'].values
        if len(frequencies) < 2:
            return 1.0

        # ジニ係数を計算
        gini = self._calc_gini_coefficient(frequencies)

        # ジニ係数の逆（0=完全不均等、1=完全均等）
        # 0.5程度が適度なバランス
        balance = 1.0 - gini

        return balance

    def _calc_gini_coefficient(self, values: np.ndarray) -> float:
        """
        ジニ係数を計算

        Parameters:
            values (np.ndarray): 値の配列

        Returns:
            float: ジニ係数（0〜1）
        """
        sorted_values = np.sort(values)
        n = len(values)
        index = np.arange(1, n + 1)
        return (2 * np.sum(index * sorted_values)) / (n * np.sum(sorted_values)) - (n + 1) / n

    def calc_length_score(self, phrases: List[str]) -> float:
        """
        平均文字長のスコア
        適度な長さ（4〜10文字程度）を評価

        Parameters:
            phrases (List[str]): フレーズのリスト

        Returns:
            float: 長さスコア（0〜1）
        """
        if not phrases:
            return 0.0

        avg_length = np.mean([len(p) for p in phrases])

        # 理想的な長さを6文字として、それに近いほど高スコア
        ideal_length = 6
        max_deviation = 10

        deviation = abs(avg_length - ideal_length)
        score = max(0, 1 - deviation / max_deviation)

        return score

    def get_detailed_scores(
        self,
        phrases: List[str],
        texts: List[str],
        df: Optional[pd.DataFrame] = None
    ) -> Dict[str, float]:
        """
        各指標の詳細スコアを取得

        Parameters:
            phrases (List[str]): フレーズのリスト
            texts (List[str]): 元のテキストのリスト
            df (pd.DataFrame, optional): 抽出結果のDataFrame

        Returns:
            Dict[str, float]: 各指標のスコア
        """
        return {
            'diversity': self.calc_diversity(phrases),
            'coverage': self.calc_coverage(phrases, texts),
            'balance': self.calc_balance(df) if df is not None else 0.5,
            'length': self.calc_length_score(phrases),
            'total': self.evaluate(phrases, texts, df)
        }


class SupervisedEvaluator:
    """
    教師あり評価器（将来の実装）
    ゴールドスタンダードと比較して評価

    使用例:
        >>> evaluator = SupervisedEvaluator(gold_phrases)
        >>> precision, recall, f1 = evaluator.evaluate(extracted_phrases)
    """

    def __init__(self, gold_phrases: List[str]):
        """
        Parameters:
            gold_phrases (List[str]): 正解フレーズのリスト
        """
        self.gold_phrases = set(gold_phrases)

    def evaluate(self, extracted_phrases: List[str]) -> Dict[str, float]:
        """
        精度、再現率、F1スコアを計算

        Parameters:
            extracted_phrases (List[str]): 抽出されたフレーズ

        Returns:
            Dict[str, float]: 評価指標
        """
        extracted_set = set(extracted_phrases)

        # True Positives: 正解かつ抽出された
        tp = len(self.gold_phrases & extracted_set)

        # False Positives: 抽出されたが正解でない
        fp = len(extracted_set - self.gold_phrases)

        # False Negatives: 正解だが抽出されなかった
        fn = len(self.gold_phrases - extracted_set)

        # 精度（Precision）
        precision = tp / (tp + fp) if (tp + fp) > 0 else 0.0

        # 再現率（Recall）
        recall = tp / (tp + fn) if (tp + fn) > 0 else 0.0

        # F1スコア
        f1 = 2 * (precision * recall) / (precision + recall) if (precision + recall) > 0 else 0.0

        return {
            'precision': precision,
            'recall': recall,
            'f1': f1,
            'tp': tp,
            'fp': fp,
            'fn': fn
        }

    def get_confusion_matrix(self, extracted_phrases: List[str]) -> Dict[str, List[str]]:
        """
        混同行列の詳細を取得

        Parameters:
            extracted_phrases (List[str]): 抽出されたフレーズ

        Returns:
            Dict[str, List[str]]: TP, FP, FNのリスト
        """
        extracted_set = set(extracted_phrases)

        return {
            'true_positives': list(self.gold_phrases & extracted_set),
            'false_positives': list(extracted_set - self.gold_phrases),
            'false_negatives': list(self.gold_phrases - extracted_set)
        }
