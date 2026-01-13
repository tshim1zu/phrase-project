# coding: utf-8
"""
統計的有意性スコアリング

カイ二乗検定、相互情報量、ジップの法則、信頼区間などの
統計指標を用いてフレーズの有意性を評価
"""

import logging
from dataclasses import dataclass
from typing import Dict, List, Tuple, Optional
from collections import Counter
import math
import numpy as np
import pandas as pd
from scipy import stats

logger = logging.getLogger(__name__)


@dataclass
class StatisticalScore:
    """統計的スコア"""
    phrase: str
    frequency: int
    total_frequency: int

    # 統計指標
    chi_square_score: float          # カイ二乗検定結果（0-1）
    mutual_information: float        # 相互情報量
    zipf_anomaly_score: float        # ジップ法則からの乖離

    # 信頼度
    confidence_lower: float          # 95%信頼区間下限
    confidence_upper: float          # 95%信頼区間上限
    p_value: float                   # p値

    # 判定
    is_significant: bool             # 統計的に有意か（p<0.05）
    significance_level: str          # 有意水準の判定
    combined_score: float            # 複合スコア（0-1）


class StatisticalScorer:
    """統計的有意性の評価"""

    def __init__(self, alpha: float = 0.05):
        """
        Parameters:
            alpha: 有意水準（デフォルト 0.05 = 95%信頼度）
        """
        self.alpha = alpha

    def score_phrase(
        self,
        phrase: str,
        phrase_freq: int,
        corpus_stats: Dict[str, int],
        context_freq: Optional[Dict] = None
    ) -> StatisticalScore:
        """
        フレーズの統計的有意性を評価

        Parameters:
            phrase: フレーズ
            phrase_freq: フレーズの出現回数
            corpus_stats: コーパス統計
                {
                    'total_texts': 1000,
                    'unique_phrases': 500,
                    'total_phrases': 10000
                }
            context_freq: コンテキスト統計（相互情報量計算用）

        Returns:
            StatisticalScore オブジェクト
        """
        total_freq = corpus_stats.get('total_phrases', 1)

        # カイ二乗検定
        chi_square_score = self._chi_square_test(
            phrase_freq,
            corpus_stats.get('total_texts', 1),
            total_freq
        )

        # 相互情報量
        mutual_info = self._mutual_information(
            phrase,
            phrase_freq,
            corpus_stats,
            context_freq
        )

        # ジップ法則からの乖離
        rank = corpus_stats.get(f'{phrase}_rank', 1)
        zipf_anomaly = self._zipf_anomaly(phrase_freq, rank, total_freq)

        # 信頼区間
        conf_lower, conf_upper = self._confidence_interval(
            phrase_freq,
            total_freq,
            self.alpha
        )

        # p値
        p_value = self._calculate_p_value(
            phrase_freq,
            corpus_stats.get('total_texts', 1),
            total_freq
        )

        # 有意性判定
        is_significant = p_value < self.alpha
        significance_level = self._get_significance_level(p_value)

        # 複合スコア
        combined = self._combined_score(
            chi_square_score,
            mutual_info,
            zipf_anomaly,
            is_significant
        )

        return StatisticalScore(
            phrase=phrase,
            frequency=phrase_freq,
            total_frequency=total_freq,
            chi_square_score=chi_square_score,
            mutual_information=mutual_info,
            zipf_anomaly_score=zipf_anomaly,
            confidence_lower=conf_lower,
            confidence_upper=conf_upper,
            p_value=p_value,
            is_significant=is_significant,
            significance_level=significance_level,
            combined_score=combined,
        )

    def score_phrases(
        self,
        df_phrases: pd.DataFrame,
        texts: Optional[List[str]] = None
    ) -> pd.DataFrame:
        """
        複数のフレーズをバッチで評価

        Parameters:
            df_phrases: フレーズDataFrame（seqchar, freq 列必須）
            texts: 元テキスト（コンテキスト分析用）

        Returns:
            統計スコア付き DataFrame
        """
        if df_phrases.empty:
            return pd.DataFrame()

        # コーパス統計を計算
        phrase_col = 'seqchar' if 'seqchar' in df_phrases.columns else 'phrase'
        freq_col = 'freq' if 'freq' in df_phrases.columns else 'frequency'

        total_freq = df_phrases[freq_col].sum()
        total_texts = len(texts) if texts else 1

        corpus_stats = {
            'total_texts': total_texts,
            'unique_phrases': len(df_phrases),
            'total_phrases': total_freq,
        }

        # ランクを付与
        df_ranked = df_phrases.copy()
        df_ranked['rank'] = df_ranked[freq_col].rank(method='min', ascending=False).astype(int)

        # コンテキスト統計を計算（テキストがあれば）
        context_freq = None
        if texts:
            context_freq = self._analyze_context_frequency(df_phrases[phrase_col].tolist(), texts)

        # 各フレーズをスコアリング
        scores = []
        for idx, row in df_ranked.iterrows():
            phrase = row[phrase_col]
            freq = row[freq_col]
            corpus_stats[f'{phrase}_rank'] = row['rank']

            score = self.score_phrase(phrase, freq, corpus_stats, context_freq)
            scores.append(score)

        # DataFrame に変換
        df_scores = pd.DataFrame([
            {
                'phrase': s.phrase,
                'frequency': s.frequency,
                'chi_square_score': s.chi_square_score,
                'mutual_information': s.mutual_information,
                'zipf_anomaly': s.zipf_anomaly_score,
                'p_value': s.p_value,
                'is_significant': s.is_significant,
                'significance_level': s.significance_level,
                'confidence_lower': s.confidence_lower,
                'confidence_upper': s.confidence_upper,
                'combined_score': s.combined_score,
            }
            for s in scores
        ])

        # 複合スコアでソート
        df_scores = df_scores.sort_values('combined_score', ascending=False)

        return df_scores

    def _chi_square_test(self, phrase_freq: int, total_texts: int, total_freq: int) -> float:
        """
        カイ二乗検定

        帰無仮説：「このフレーズは完全にランダムに分布している」
        返却値：有意性スコア（0-1）
        """
        if total_freq <= 0:
            return 0.0

        # 期待度数
        expected_freq = total_freq / total_texts

        # カイ二乗値
        if expected_freq == 0:
            return 0.0

        chi_square = ((phrase_freq - expected_freq) ** 2) / expected_freq

        # p値に変換（自由度1のカイ二乗分布）
        p_value = 1.0 - stats.chi2.cdf(chi_square, df=1)

        # スコアに変換（p値が小さいほど有意 → スコア高い）
        # p < 0.05 なら有意 → スコア 0.8-1.0
        # p < 0.01 なら高度に有意 → スコア 0.95-1.0
        score = max(0.0, 1.0 - p_value)

        return score

    def _mutual_information(
        self,
        phrase: str,
        phrase_freq: int,
        corpus_stats: Dict,
        context_freq: Optional[Dict] = None
    ) -> float:
        """
        相互情報量（Mutual Information）

        フレーズと文脈単語の独立性を測定
        MI = 0: 独立（無関係）
        MI > 0: 相関がある
        """
        if context_freq is None or phrase not in context_freq:
            return 0.5  # データなしでは中立的

        total_freq = corpus_stats.get('total_phrases', 1)
        total_texts = corpus_stats.get('total_texts', 1)

        # フレーズが出現するテキスト数
        phrase_text_count = context_freq.get(phrase, {}).get('text_count', 1)

        if phrase_text_count == 0:
            return 0.0

        # P(phrase)
        p_phrase = phrase_freq / total_freq

        # P(phrase | in_text)
        p_phrase_given_text = phrase_freq / phrase_text_count

        # MI = log(P(phrase | in_text) / P(phrase))
        if p_phrase == 0:
            return 0.0

        mi = p_phrase_given_text / p_phrase
        # ログスケールに
        mi_score = math.log(mi + 1.0)  # +1 で log(0) 回避

        # 0-1に正規化（上限を決める）
        # 最大値は約 math.log(total_texts) 程度
        max_mi = math.log(total_texts + 1.0)
        mi_score = min(1.0, mi_score / max_mi)

        return mi_score

    def _zipf_anomaly(self, freq: int, rank: int, total_freq: int) -> float:
        """
        ジップの法則からの乖離を検出

        ジップの法則：freq(rank) × rank ≈ constant
        乖離が大きい → スコア高い（異常 = 意図的な使用）
        """
        if rank <= 0 or freq <= 0:
            return 0.0

        # 理想的な定数
        # rank 1のフレーズが最頻出だと仮定して計算
        zipf_constant = freq * rank

        # 各ランクでの期待度数
        expected_freq = zipf_constant / rank

        if expected_freq == 0:
            return 0.0

        # 乖離率
        deviation_ratio = abs(freq - expected_freq) / expected_freq

        # 0-1に正規化（大きな乖離 = スコア高い）
        anomaly_score = min(1.0, deviation_ratio / 2.0)

        return anomaly_score

    def _confidence_interval(
        self,
        count: int,
        total: int,
        alpha: float = 0.05
    ) -> Tuple[float, float]:
        """
        95% 信頼区間を計算（二項分布）

        Wilson score interval を使用
        """
        if total == 0:
            return 0.0, 0.0

        p_hat = count / total

        # Z値（95%信頼度）
        z = stats.norm.ppf(1 - alpha / 2)

        denominator = 1 + z ** 2 / total

        centre_adjusted = (p_hat + z ** 2 / (2 * total)) / denominator

        adjusted_sd = math.sqrt(
            (p_hat * (1 - p_hat) + z ** 2 / (4 * total)) / total
        ) / denominator

        lower = max(0.0, centre_adjusted - z * adjusted_sd)
        upper = min(1.0, centre_adjusted + z * adjusted_sd)

        return lower, upper

    def _calculate_p_value(self, phrase_freq: int, total_texts: int, total_freq: int) -> float:
        """
        p値を計算（カイ二乗検定）
        """
        if total_freq <= 0:
            return 1.0

        expected = total_freq / total_texts

        if expected == 0:
            return 1.0

        chi_square = ((phrase_freq - expected) ** 2) / expected

        p_value = 1.0 - stats.chi2.cdf(chi_square, df=1)

        return p_value

    def _get_significance_level(self, p_value: float) -> str:
        """p値から有意水準の文字列を返す"""
        if p_value < 0.001:
            return "***"  # p < 0.001
        elif p_value < 0.01:
            return "**"   # p < 0.01
        elif p_value < 0.05:
            return "*"    # p < 0.05
        else:
            return "ns"   # not significant

    def _combined_score(
        self,
        chi_square: float,
        mutual_info: float,
        zipf_anomaly: float,
        is_significant: bool
    ) -> float:
        """複数の統計指標を統合"""
        # 重み付け
        weights = {
            'chi_square': 0.40,      # カイ二乗が最重要
            'mutual_info': 0.30,     # 文脈情報
            'zipf_anomaly': 0.20,    # 異常検出
            'significant': 0.10,     # 有意性
        }

        score = (
            chi_square * weights['chi_square'] +
            mutual_info * weights['mutual_info'] +
            zipf_anomaly * weights['zipf_anomaly'] +
            (1.0 if is_significant else 0.0) * weights['significant']
        )

        return min(1.0, score)

    def _analyze_context_frequency(self, phrases: List[str], texts: List[str]) -> Dict:
        """コンテキスト頻度を分析"""
        context_freq = {}

        for phrase in phrases:
            text_count = sum(1 for text in texts if phrase in text)
            context_freq[phrase] = {
                'text_count': text_count,
                'frequency': sum(text.count(phrase) for text in texts),
            }

        return context_freq

    def generate_report(self, df_scores: pd.DataFrame, top_k: int = 10) -> str:
        """統計分析レポートを生成"""
        report = "【統計的有意性分析レポート】\n\n"

        # 全体統計
        significant_count = df_scores['is_significant'].sum()
        total_count = len(df_scores)

        report += f"分析対象フレーズ数: {total_count}\n"
        report += f"統計的に有意なフレーズ: {significant_count} ({significant_count/total_count*100:.1f}%)\n\n"

        # 有意なフレーズ TOP 10
        df_significant = df_scores[df_scores['is_significant']].head(top_k)

        report += f"【有意なフレーズ TOP {min(top_k, len(df_significant))}】\n"
        for idx, (_, row) in enumerate(df_significant.iterrows(), 1):
            report += (
                f"{idx}. {row['phrase']} "
                f"(freq={row['frequency']}, p={row['p_value']:.4f}) "
                f"{row['significance_level']}\n"
            )

        # スコア分布
        report += f"\n【複合スコア分布】\n"
        report += f"  平均: {df_scores['combined_score'].mean():.3f}\n"
        report += f"  標準偏差: {df_scores['combined_score'].std():.3f}\n"
        report += f"  中央値: {df_scores['combined_score'].median():.3f}\n"

        return report


class PhraseTrustMetric:
    """フレーズ信頼度メトリクス"""

    def __init__(self, statistical_scorer: StatisticalScorer):
        self.scorer = statistical_scorer

    def calculate_trust_score(
        self,
        phrase: str,
        phrase_freq: int,
        corpus_stats: Dict,
        context_freq: Optional[Dict] = None
    ) -> float:
        """
        フレーズの信頼度スコア（0-1）を計算

        単なる頻度ではなく、統計的有意性を考慮した「本当に重要か」の指標
        """
        score = self.scorer.score_phrase(phrase, phrase_freq, corpus_stats, context_freq)

        # 複合スコア * 有意性ボーナス
        trust = score.combined_score

        if score.is_significant:
            # 有意なら +20%
            trust = min(1.0, trust * 1.2)

        # 信頼区間の幅が狭いほど信頼度が高い
        interval_width = score.confidence_upper - score.confidence_lower
        confidence_bonus = max(0.0, (1.0 - interval_width) * 0.1)
        trust = min(1.0, trust + confidence_bonus)

        return trust
