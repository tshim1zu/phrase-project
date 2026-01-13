"""
最適化モジュール
パラメータの自動最適化
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023"

import itertools
import random
import logging
from typing import List, Dict, Any, Optional, Tuple
import pandas as pd
from .extracter import PhraseExtracter
from .evaluation import UnsupervisedEvaluator, SupervisedEvaluator

logger = logging.getLogger(__name__)


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
            logger.info(f"Grid Search: {total} combinations to try")

        for idx, params in enumerate(param_combinations):
            if self.verbose >= 1:
                logger.info(f"[{idx+1}/{total}] Testing: {params}")

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
                    logger.info(f"  Score: {score:.4f}, Phrases: {len(df)}")

            except Exception as e:
                if self.verbose >= 1:
                    logger.error(f"  Error: {e}")
                continue

        # 最良結果を選択
        if not results:
            raise ValueError("No valid results found")

        best_result = max(results, key=lambda x: x['score'])

        if self.verbose >= 1:
            logger.info(f"\nBest params: {best_result['params']}")
            logger.info(f"Best score: {best_result['score']:.4f}")

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
            logger.info(f"Random Search: {n_iterations} iterations")

        for i in range(n_iterations):
            # ランダムにパラメータを選択
            params = self._sample_random_params()

            if self.verbose >= 1:
                logger.info(f"[{i+1}/{n_iterations}] Testing: {params}")

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
                    logger.info(f"  Score: {score:.4f}, Phrases: {len(df)}")

            except Exception as e:
                if self.verbose >= 1:
                    logger.error(f"  Error: {e}")
                continue

        # 最良結果を選択
        if not results:
            raise ValueError("No valid results found")

        best_result = max(results, key=lambda x: x['score'])

        if self.verbose >= 1:
            logger.info(f"\nBest params: {best_result['params']}")
            logger.info(f"Best score: {best_result['score']:.4f}")

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
            logger.info(f"Supervised Grid Search: {total} combinations to try")

        for idx, params in enumerate(param_combinations):
            if self.verbose >= 1:
                logger.info(f"[{idx+1}/{total}] Testing: {params}")

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
                    logger.info(f"  {self.metric}: {scores[self.metric]:.4f}, Phrases: {len(df)}")

            except Exception as e:
                if self.verbose >= 1:
                    logger.error(f"  Error: {e}")
                continue

        # 最良結果を選択
        if not results:
            raise ValueError("No valid results found")

        best_result = max(results, key=lambda x: x['score'])

        if self.verbose >= 1:
            print(f"\nBest params: {best_result['params']}")
            logger.info(f"Best {self.metric}: {best_result['score']:.4f}")

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


class OptunaOptimizer:
    """
    Optunaベースのハイパーパラメータ最適化器

    ベイズ最適化（TPE）を使用して効率的にパラメータを探索します。
    開発・実験用の機能で、配布パッケージには含まれません。

    使用例:
        >>> optimizer = OptunaOptimizer(n_trials=100)
        >>> best_params, study = optimizer.optimize(texts)
        >>> print(f"Best params: {best_params}")
        >>> print(f"Best score: {study.best_value}")
    """

    def __init__(
        self,
        n_trials: int = 50,
        param_ranges: Optional[Dict[str, Tuple]] = None,
        evaluator: Optional['UnsupervisedEvaluator'] = None,
        verbose: int = 1
    ):
        """
        Parameters:
            n_trials (int): 試行回数
            param_ranges (Dict[str, Tuple]): パラメータの探索範囲
            evaluator (UnsupervisedEvaluator): 評価器
            verbose (int): 進捗表示レベル
        """
        try:
            import optuna
            self.optuna = optuna
        except ImportError:
            raise ImportError(
                "Optunaが見つかりません。開発モードでインストールしてください:\n"
                "pip install -e '.[dev]'"
            )

        self.n_trials = n_trials
        self.param_ranges = param_ranges or self._get_default_param_ranges()
        self.evaluator = evaluator or UnsupervisedEvaluator()
        self.verbose = verbose

    def _get_default_param_ranges(self) -> Dict[str, Tuple]:
        """デフォルトのパラメータ探索範囲"""
        return {
            'min_count': (2, 20),
            'max_length': (10, 30),
            'min_length': (2, 8),
            'threshold_originality': (0.1, 0.9),
        }

    def optimize(
        self,
        texts: List[str],
        study_name: Optional[str] = None,
        storage: Optional[str] = None
    ) -> Tuple[Dict[str, Any], Any]:
        """
        ハイパーパラメータ最適化を実行

        Parameters:
            texts (List[str]): テキストのリスト
            study_name (str, optional): 実験名
            storage (str, optional): データベースURL

        Returns:
            Tuple[Dict[str, Any], optuna.Study]: (最適パラメータ, Studyオブジェクト)
        """
        import os

        def objective(trial):
            """Optunaの目的関数"""
            params = {}
            for param_name, (low, high) in self.param_ranges.items():
                if param_name == 'threshold_originality':
                    params[param_name] = trial.suggest_float(param_name, low, high)
                else:
                    params[param_name] = trial.suggest_int(param_name, low, high)

            try:
                extractor = PhraseExtracter(verbose=0, **params)
                df = extractor.get_dfphrase(texts)

                if len(df) == 0:
                    return 0.0

                phrases = df['seqchar'].tolist()
                score = self.evaluator.evaluate(phrases, texts, df)
                trial.set_user_attr('n_phrases', len(df))

                return score

            except Exception as e:
                if self.verbose >= 2:
                    logger.error(f"Trial failed: {e}")
                return 0.0

        study = self.optuna.create_study(
            study_name=study_name,
            storage=storage,
            direction='maximize',
            sampler=self.optuna.samplers.TPESampler(seed=42),
            load_if_exists=True
        )

        if self.verbose >= 1:
            logger.info(f"Starting Optuna optimization: {self.n_trials} trials")

        study.optimize(
            objective,
            n_trials=self.n_trials,
            show_progress_bar=self.verbose >= 1,
            n_jobs=1
        )

        if self.verbose >= 1:
            logger.info(f"\nOptimization completed!")
            logger.info(f"Best value: {study.best_value:.4f}")
            logger.info(f"Best params: {study.best_params}")

        return study.best_params, study

    def save_results(self, study: Any, output_dir: str = 'experiments/results'):
        """最適化結果を保存"""
        import os
        import json
        from pathlib import Path

        Path(output_dir).mkdir(parents=True, exist_ok=True)

        best_params_file = os.path.join(output_dir, f'{study.study_name}_best_params.json')
        with open(best_params_file, 'w', encoding='utf-8') as f:
            json.dump({
                'best_params': study.best_params,
                'best_value': study.best_value,
                'n_trials': len(study.trials)
            }, f, indent=2, ensure_ascii=False)

        logger.info(f"Best parameters saved to: {best_params_file}")

        df_trials = study.trials_dataframe()
        trials_file = os.path.join(output_dir, f'{study.study_name}_trials.csv')
        df_trials.to_csv(trials_file, index=False, encoding='utf-8-sig')

        logger.info(f"Trial history saved to: {trials_file}")

    def visualize(self, study: Any, output_dir: str = 'experiments/results'):
        """最適化結果を可視化"""
        try:
            from optuna import visualization as vis
            import plotly.io as pio
            from pathlib import Path

            Path(output_dir).mkdir(parents=True, exist_ok=True)

            fig = vis.plot_optimization_history(study)
            pio.write_html(fig, os.path.join(output_dir, f'{study.study_name}_history.html'))

            fig = vis.plot_param_importances(study)
            pio.write_html(fig, os.path.join(output_dir, f'{study.study_name}_importances.html'))

            fig = vis.plot_parallel_coordinate(study)
            pio.write_html(fig, os.path.join(output_dir, f'{study.study_name}_parallel.html'))

            logger.info(f"Visualizations saved to: {output_dir}")

        except ImportError:
            logger.warning("可視化にはplotlyが必要です: pip install plotly")
