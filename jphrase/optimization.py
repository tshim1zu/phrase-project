# coding:utf-8
"""
最適化モジュール
パラメータの自動最適化
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023"

import itertools
import random
from typing import List, Dict, Any, Optional, Tuple
import pandas as pd
from .extracter import PhraseExtracter
from .evaluation import UnsupervisedEvaluator, SupervisedEvaluator


class UnsupervisedOptimizer:
    """
    教師なし最適化器
    ゴールドスタンダードなしでパラメータを最適化

    使用例:
        >>> optimizer = UnsupervisedOptimizer(param_grid={'min_count': [5, 10]})
        >>> best_params, results = optimizer.optimize(texts)
    """

    def __init__(
        self,
        param_grid: Optional[Dict[str, List[Any]]] = None,
        evaluator: Optional[UnsupervisedEvaluator] = None,
        verbose: int = 1
    ):
        """
        Parameters:
            param_grid (Dict[str, List[Any]]): 探索するパラメータの範囲
            evaluator (UnsupervisedEvaluator): 評価器
            verbose (int): 進捗表示レベル
        """
        self.param_grid = param_grid or self._get_default_param_grid()
        self.evaluator = evaluator or UnsupervisedEvaluator()
        self.verbose = verbose

    def _get_default_param_grid(self) -> Dict[str, List[Any]]:
        """デフォルトのパラメータグリッド"""
        return {
            'min_count': [3, 5, 10, 15],
            'max_length': [10, 15, 20],
            'min_length': [3, 4, 5],
            'threshold_originality': [0.3, 0.5, 0.7, 0.9]
        }

    def optimize(
        self,
        texts: List[str],
        method: str = 'grid'
    ) -> Tuple[Dict[str, Any], List[Dict]]:
        """
        パラメータ最適化を実行

        Parameters:
            texts (List[str]): テキストのリスト
            method (str): 最適化手法 ('grid' or 'random')

        Returns:
            Tuple[Dict[str, Any], List[Dict]]: (最適パラメータ, 全実験結果)
        """
        if method == 'grid':
            return self.grid_search(texts)
        elif method == 'random':
            return self.random_search(texts)
        else:
            raise ValueError(f"Unknown method: {method}")

    def grid_search(self, texts: List[str]) -> Tuple[Dict[str, Any], List[Dict]]:
        """
        グリッドサーチでパラメータ最適化

        Parameters:
            texts (List[str]): テキストのリスト

        Returns:
            Tuple[Dict[str, Any], List[Dict]]: (最適パラメータ, 全実験結果)
        """
        results = []

        # パラメータの全組み合わせを生成
        param_combinations = self._generate_param_combinations()
        total = len(param_combinations)

        if self.verbose >= 1:
            print(f"Grid Search: {total} combinations to try")

        for idx, params in enumerate(param_combinations):
            if self.verbose >= 1:
                print(f"[{idx+1}/{total}] Testing: {params}")

            try:
                # フレーズ抽出
                extractor = PhraseExtracter(verbose=0, **params)
                df = extractor.get_dfphrase(texts)

                # 評価
                if len(df) > 0:
                    phrases = df['seqchar'].tolist()
                    score = self.evaluator.evaluate(phrases, texts, df)
                    detailed_scores = self.evaluator.get_detailed_scores(phrases, texts, df)
                else:
                    score = 0.0
                    detailed_scores = {}

                result = {
                    'params': params,
                    'score': score,
                    'n_phrases': len(df),
                    'detailed_scores': detailed_scores
                }

                results.append(result)

                if self.verbose >= 1:
                    print(f"  Score: {score:.4f}, Phrases: {len(df)}")

            except Exception as e:
                if self.verbose >= 1:
                    print(f"  Error: {e}")
                continue

        # 最良結果を選択
        if not results:
            raise ValueError("No valid results found")

        best_result = max(results, key=lambda x: x['score'])

        if self.verbose >= 1:
            print(f"\nBest params: {best_result['params']}")
            print(f"Best score: {best_result['score']:.4f}")

        return best_result['params'], results

    def random_search(
        self,
        texts: List[str],
        n_iterations: int = 20
    ) -> Tuple[Dict[str, Any], List[Dict]]:
        """
        ランダムサーチでパラメータ最適化

        Parameters:
            texts (List[str]): テキストのリスト
            n_iterations (int): 試行回数

        Returns:
            Tuple[Dict[str, Any], List[Dict]]: (最適パラメータ, 全実験結果)
        """
        results = []

        if self.verbose >= 1:
            print(f"Random Search: {n_iterations} iterations")

        for i in range(n_iterations):
            # ランダムにパラメータを選択
            params = self._sample_random_params()

            if self.verbose >= 1:
                print(f"[{i+1}/{n_iterations}] Testing: {params}")

            try:
                # フレーズ抽出
                extractor = PhraseExtracter(verbose=0, **params)
                df = extractor.get_dfphrase(texts)

                # 評価
                if len(df) > 0:
                    phrases = df['seqchar'].tolist()
                    score = self.evaluator.evaluate(phrases, texts, df)
                    detailed_scores = self.evaluator.get_detailed_scores(phrases, texts, df)
                else:
                    score = 0.0
                    detailed_scores = {}

                result = {
                    'params': params,
                    'score': score,
                    'n_phrases': len(df),
                    'detailed_scores': detailed_scores
                }

                results.append(result)

                if self.verbose >= 1:
                    print(f"  Score: {score:.4f}, Phrases: {len(df)}")

            except Exception as e:
                if self.verbose >= 1:
                    print(f"  Error: {e}")
                continue

        # 最良結果を選択
        if not results:
            raise ValueError("No valid results found")

        best_result = max(results, key=lambda x: x['score'])

        if self.verbose >= 1:
            print(f"\nBest params: {best_result['params']}")
            print(f"Best score: {best_result['score']:.4f}")

        return best_result['params'], results

    def _generate_param_combinations(self) -> List[Dict[str, Any]]:
        """パラメータの全組み合わせを生成"""
        keys = list(self.param_grid.keys())
        values = list(self.param_grid.values())

        combinations = []
        for combo in itertools.product(*values):
            param_dict = dict(zip(keys, combo))
            combinations.append(param_dict)

        return combinations

    def _sample_random_params(self) -> Dict[str, Any]:
        """ランダムにパラメータを選択"""
        params = {}
        for key, values in self.param_grid.items():
            params[key] = random.choice(values)
        return params


class SupervisedOptimizer:
    """
    教師あり最適化器（将来の実装）
    ゴールドスタンダードを使ってパラメータを最適化

    使用例:
        >>> optimizer = SupervisedOptimizer(gold_phrases, param_grid)
        >>> best_params, results = optimizer.optimize(texts)
    """

    def __init__(
        self,
        gold_phrases: List[str],
        param_grid: Optional[Dict[str, List[Any]]] = None,
        metric: str = 'f1',
        verbose: int = 1
    ):
        """
        Parameters:
            gold_phrases (List[str]): 正解フレーズのリスト
            param_grid (Dict[str, List[Any]]): 探索するパラメータの範囲
            metric (str): 最適化する指標 ('precision', 'recall', 'f1')
            verbose (int): 進捗表示レベル
        """
        self.gold_phrases = gold_phrases
        self.param_grid = param_grid or self._get_default_param_grid()
        self.metric = metric
        self.verbose = verbose
        self.evaluator = SupervisedEvaluator(gold_phrases)

    def _get_default_param_grid(self) -> Dict[str, List[Any]]:
        """デフォルトのパラメータグリッド"""
        return {
            'min_count': [3, 5, 10, 15],
            'max_length': [10, 15, 20],
            'min_length': [3, 4, 5],
            'threshold_originality': [0.3, 0.5, 0.7, 0.9]
        }

    def optimize(self, texts: List[str]) -> Tuple[Dict[str, Any], List[Dict]]:
        """
        パラメータ最適化を実行

        Parameters:
            texts (List[str]): テキストのリスト

        Returns:
            Tuple[Dict[str, Any], List[Dict]]: (最適パラメータ, 全実験結果)
        """
        results = []

        # パラメータの全組み合わせを生成
        param_combinations = self._generate_param_combinations()
        total = len(param_combinations)

        if self.verbose >= 1:
            print(f"Supervised Grid Search: {total} combinations to try")

        for idx, params in enumerate(param_combinations):
            if self.verbose >= 1:
                print(f"[{idx+1}/{total}] Testing: {params}")

            try:
                # フレーズ抽出
                extractor = PhraseExtracter(verbose=0, **params)
                df = extractor.get_dfphrase(texts)

                # 評価
                if len(df) > 0:
                    phrases = df['seqchar'].tolist()
                    scores = self.evaluator.evaluate(phrases)
                else:
                    scores = {'precision': 0, 'recall': 0, 'f1': 0}

                result = {
                    'params': params,
                    'score': scores[self.metric],
                    'all_scores': scores,
                    'n_phrases': len(df)
                }

                results.append(result)

                if self.verbose >= 1:
                    print(f"  {self.metric}: {scores[self.metric]:.4f}, Phrases: {len(df)}")

            except Exception as e:
                if self.verbose >= 1:
                    print(f"  Error: {e}")
                continue

        # 最良結果を選択
        if not results:
            raise ValueError("No valid results found")

        best_result = max(results, key=lambda x: x['score'])

        if self.verbose >= 1:
            print(f"\nBest params: {best_result['params']}")
            print(f"Best {self.metric}: {best_result['score']:.4f}")

        return best_result['params'], results

    def _generate_param_combinations(self) -> List[Dict[str, Any]]:
        """パラメータの全組み合わせを生成"""
        keys = list(self.param_grid.keys())
        values = list(self.param_grid.values())

        combinations = []
        for combo in itertools.product(*values):
            param_dict = dict(zip(keys, combo))
            combinations.append(param_dict)

        return combinations
