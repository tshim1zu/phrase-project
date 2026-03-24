# coding: utf-8
"""
コロケーション結合度スコアラー（Collocation Scorer）

PMI 一本足からの脱却。複数の結合度指標で多角的にコロケーションを評価。

指標:
- t-score: 高頻度コロケーション向き（PMIの低頻度バイアスを補正）
- z-score: 期待値からの標準偏差ベース
- Log-Dice: コーパスサイズ非依存。最も安定した指標
- MI³ (Cubic MI): PMIの低頻度バイアスを数学的に補正
- Delta-P: 方向性のある結合度（A→B と B→A の非対称性）
- Multi-metric Ranker: 全指標を統合したランキング
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023-2026"

import math
import logging
from dataclasses import dataclass
from typing import Dict, List, Optional, Tuple

import numpy as np
import pandas as pd

logger = logging.getLogger(__name__)


@dataclass
class CollocationScore:
    """コロケーション結合度スコア（1ペア分）"""
    phrase: str
    part_a: str
    part_b: str

    freq_phrase: int        # フレーズ全体の頻度
    freq_a: int             # part_a の頻度
    freq_b: int             # part_b の頻度
    corpus_size: int        # コーパス総語数

    # 結合度指標
    pmi: float              # Pointwise Mutual Information
    mi3: float              # Cubic MI (MI³)
    t_score: float          # t-score
    z_score: float          # z-score
    log_dice: float         # Log-Dice
    delta_p_ab: float       # Delta-P (A→B)
    delta_p_ba: float       # Delta-P (B→A)

    # 統合
    combined_rank_score: float  # 複合ランクスコア (0-1)


class CollocationScorer:
    """
    多指標コロケーション結合度スコアラー

    使用例:
        >>> scorer = CollocationScorer()
        >>> scores = scorer.score_phrases(df_phrases, full_text)
        >>> print(scores[['phrase', 'pmi', 't_score', 'log_dice', 'combined_rank_score']])

    各指標の特性:
        - PMI: 低頻度に偏る（レアな組合せを過大評価）
        - t-score: 高頻度に偏る（よくある組合せを重視）
        - Log-Dice: コーパスサイズに依存しない（最も安定）
        - MI³: PMI の低頻度バイアスを freq³ で補正
        - z-score: 統計的な期待値からのずれ
        - Delta-P: 方向性（「彼女の」vs「の彼女」の非対称性）
    """

    # 統合スコアの重み
    WEIGHTS = {
        'pmi': 0.10,        # 低頻度バイアスがあるので控えめ
        'mi3': 0.15,        # PMI補正版
        't_score': 0.20,    # 高頻度コロケーション検出に強い
        'z_score': 0.10,    # 補助的
        'log_dice': 0.30,   # 最も安定 → 最重要
        'delta_p': 0.15,    # 方向性情報
    }

    def __init__(self, split_ratio: float = 0.5):
        """
        Parameters:
            split_ratio: フレーズの分割位置（0.5 = 中央）
        """
        self.split_ratio = split_ratio

    # ─── Individual Metrics ─────────────────────────────────

    @staticmethod
    def calc_pmi(freq_ab: int, freq_a: int, freq_b: int, N: int) -> float:
        """
        Pointwise Mutual Information

        PMI(a,b) = log2( P(a,b) / (P(a) * P(b)) )
        """
        if freq_a == 0 or freq_b == 0 or freq_ab == 0 or N == 0:
            return 0.0
        p_ab = freq_ab / N
        p_a = freq_a / N
        p_b = freq_b / N
        return math.log2(p_ab / (p_a * p_b))

    @staticmethod
    def calc_mi3(freq_ab: int, freq_a: int, freq_b: int, N: int) -> float:
        """
        Cubic MI (MI³)

        MI³(a,b) = log2( freq_ab³ / (freq_a * freq_b) )

        PMI の低頻度バイアスを freq³ で補正。
        高頻度コロケーションも適切に評価する。
        """
        if freq_a == 0 or freq_b == 0 or freq_ab == 0:
            return 0.0
        numerator = freq_ab ** 3
        denominator = freq_a * freq_b
        if denominator == 0:
            return 0.0
        return math.log2(numerator / denominator)

    @staticmethod
    def calc_t_score(freq_ab: int, freq_a: int, freq_b: int, N: int) -> float:
        """
        t-score

        t = (O - E) / sqrt(O)
        where O = freq_ab, E = freq_a * freq_b / N

        高頻度コロケーション検出に強い。
        PMI とは逆に、頻度の高い結合を重視する。
        """
        if N == 0 or freq_ab == 0:
            return 0.0
        expected = (freq_a * freq_b) / N
        return (freq_ab - expected) / math.sqrt(freq_ab)

    @staticmethod
    def calc_z_score(freq_ab: int, freq_a: int, freq_b: int, N: int) -> float:
        """
        z-score

        z = (O - E) / sqrt(E)
        where E = freq_a * freq_b / N

        期待値からの標準偏差ベースのずれ。
        """
        if N == 0:
            return 0.0
        expected = (freq_a * freq_b) / N
        if expected == 0:
            return 0.0
        return (freq_ab - expected) / math.sqrt(expected)

    @staticmethod
    def calc_log_dice(freq_ab: int, freq_a: int, freq_b: int) -> float:
        """
        Log-Dice

        LogDice = 14 + log2( 2 * freq_ab / (freq_a + freq_b) )

        コーパスサイズ N に依存しない。
        理論最大値 = 14（完全共起）。実用範囲: 0-14。
        最も安定した結合度指標。
        """
        denom = freq_a + freq_b
        if denom == 0 or freq_ab == 0:
            return 0.0
        dice_raw = 2 * freq_ab / denom
        return 14.0 + math.log2(dice_raw)

    @staticmethod
    def calc_delta_p(freq_ab: int, freq_a: int, freq_b: int, N: int) -> Tuple[float, float]:
        """
        Delta-P (方向性結合度)

        ΔP(A→B) = P(B|A) - P(B|¬A)
        ΔP(B→A) = P(A|B) - P(A|¬B)

        非対称。A が B を予測する力と、B が A を予測する力は異なる。
        例: 「強い + 風」→ ΔP(強い→風) ≠ ΔP(風→強い)

        Returns:
            (delta_p_ab, delta_p_ba)
        """
        if freq_a == 0 or freq_b == 0 or N == 0:
            return 0.0, 0.0

        # P(B|A) = freq_ab / freq_a
        p_b_given_a = freq_ab / freq_a

        # P(B|¬A) = (freq_b - freq_ab) / (N - freq_a)
        denom_not_a = N - freq_a
        if denom_not_a <= 0:
            p_b_given_not_a = 0.0
        else:
            p_b_given_not_a = max(0, freq_b - freq_ab) / denom_not_a

        delta_p_ab = p_b_given_a - p_b_given_not_a

        # P(A|B) = freq_ab / freq_b
        p_a_given_b = freq_ab / freq_b

        # P(A|¬B) = (freq_a - freq_ab) / (N - freq_b)
        denom_not_b = N - freq_b
        if denom_not_b <= 0:
            p_a_given_not_b = 0.0
        else:
            p_a_given_not_b = max(0, freq_a - freq_ab) / denom_not_b

        delta_p_ba = p_a_given_b - p_a_given_not_b

        return delta_p_ab, delta_p_ba

    # ─── High-Level API ─────────────────────────────────────

    def score_phrase(
        self,
        phrase: str,
        freq_phrase: int,
        full_text: str,
        corpus_size: Optional[int] = None,
    ) -> CollocationScore:
        """
        1フレーズの全結合度指標を計算

        Parameters:
            phrase: フレーズ
            freq_phrase: フレーズの出現頻度
            full_text: コーパス全文
            corpus_size: コーパスサイズ（None なら len(full_text)）
        """
        N = corpus_size if corpus_size else len(full_text)

        # フレーズを分割
        mid = max(1, int(len(phrase) * self.split_ratio))
        part_a = phrase[:mid]
        part_b = phrase[mid:]

        freq_a = full_text.count(part_a)
        freq_b = full_text.count(part_b)

        pmi = self.calc_pmi(freq_phrase, freq_a, freq_b, N)
        mi3 = self.calc_mi3(freq_phrase, freq_a, freq_b, N)
        t_score = self.calc_t_score(freq_phrase, freq_a, freq_b, N)
        z_score = self.calc_z_score(freq_phrase, freq_a, freq_b, N)
        log_dice = self.calc_log_dice(freq_phrase, freq_a, freq_b)
        delta_p_ab, delta_p_ba = self.calc_delta_p(freq_phrase, freq_a, freq_b, N)

        return CollocationScore(
            phrase=phrase,
            part_a=part_a,
            part_b=part_b,
            freq_phrase=freq_phrase,
            freq_a=freq_a,
            freq_b=freq_b,
            corpus_size=N,
            pmi=pmi,
            mi3=mi3,
            t_score=t_score,
            z_score=z_score,
            log_dice=log_dice,
            delta_p_ab=delta_p_ab,
            delta_p_ba=delta_p_ba,
            combined_rank_score=0.0,  # 後で計算
        )

    def score_phrases(
        self,
        df_phrases: pd.DataFrame,
        full_text: str,
        phrase_col: str = 'phrase',
        freq_col: str = 'freq',
    ) -> pd.DataFrame:
        """
        複数フレーズをバッチスコアリング

        Parameters:
            df_phrases: フレーズ DataFrame
            full_text: コーパス全文
            phrase_col: フレーズ列名
            freq_col: 頻度列名

        Returns:
            全指標付き DataFrame
        """
        # 列名の自動検出
        if phrase_col not in df_phrases.columns:
            phrase_col = 'seqchar' if 'seqchar' in df_phrases.columns else df_phrases.columns[0]
        if freq_col not in df_phrases.columns:
            for candidate in ['frequency', 'count', 'freq']:
                if candidate in df_phrases.columns:
                    freq_col = candidate
                    break

        N = len(full_text)
        scores = []

        for _, row in df_phrases.iterrows():
            phrase = row[phrase_col]
            freq = int(row[freq_col])
            cs = self.score_phrase(phrase, freq, full_text, N)
            scores.append(cs)

        if not scores:
            return pd.DataFrame()

        # DataFrame 化
        df = pd.DataFrame([
            {
                'phrase': s.phrase,
                'part_a': s.part_a,
                'part_b': s.part_b,
                'freq': s.freq_phrase,
                'freq_a': s.freq_a,
                'freq_b': s.freq_b,
                'pmi': s.pmi,
                'mi3': s.mi3,
                't_score': s.t_score,
                'z_score': s.z_score,
                'log_dice': s.log_dice,
                'delta_p_ab': s.delta_p_ab,
                'delta_p_ba': s.delta_p_ba,
            }
            for s in scores
        ])

        # 統合ランクスコア計算
        df['combined_rank_score'] = self._compute_combined_rank_score(df)

        # ソート
        df = df.sort_values('combined_rank_score', ascending=False).reset_index(drop=True)
        return df

    def _compute_combined_rank_score(self, df: pd.DataFrame) -> pd.Series:
        """
        複数指標のランクを統合したスコア (0-1)

        各指標をパーセンタイルランクに変換し、重み付き平均を取る。
        """
        if df.empty:
            return pd.Series(dtype=float)

        n = len(df)
        if n == 1:
            return pd.Series([1.0], index=df.index)

        # 各指標のパーセンタイルランク (0-1)
        rank_pmi = df['pmi'].rank(pct=True)
        rank_mi3 = df['mi3'].rank(pct=True)
        rank_t = df['t_score'].rank(pct=True)
        rank_z = df['z_score'].rank(pct=True)
        rank_ld = df['log_dice'].rank(pct=True)
        # Delta-P: 最大方向を使用
        rank_dp = df[['delta_p_ab', 'delta_p_ba']].max(axis=1).rank(pct=True)

        combined = (
            self.WEIGHTS['pmi'] * rank_pmi +
            self.WEIGHTS['mi3'] * rank_mi3 +
            self.WEIGHTS['t_score'] * rank_t +
            self.WEIGHTS['z_score'] * rank_z +
            self.WEIGHTS['log_dice'] * rank_ld +
            self.WEIGHTS['delta_p'] * rank_dp
        )

        return combined

    def classify_collocation(self, score: CollocationScore) -> str:
        """
        コロケーションの性質を分類

        Returns:
            'fixed_expression': 固定表現（高PMI + 高t-score）
            'habitual': 書き癖（高t-score + 低PMI）
            'creative': 創造的用法（高PMI + 低t-score）
            'weak': 弱い結合
        """
        high_pmi = score.pmi > 3.0
        high_t = score.t_score > 2.0
        high_ld = score.log_dice > 7.0

        if high_pmi and high_t:
            return 'fixed_expression'
        elif high_t and not high_pmi:
            return 'habitual'
        elif high_pmi and not high_t:
            return 'creative'
        elif high_ld:
            return 'stable'
        else:
            return 'weak'

    def generate_report(
        self,
        df_scores: pd.DataFrame,
        top_k: int = 15,
    ) -> str:
        """結合度分析レポート"""
        if df_scores.empty:
            return "データがありません"

        lines = [
            "=" * 75,
            "【コロケーション結合度分析レポート】",
            "=" * 75,
            "",
            f"分析対象: {len(df_scores)}フレーズ",
            "",
            "【指標間相関】",
        ]

        # 指標間相関の簡易表示
        metrics = ['pmi', 'mi3', 't_score', 'z_score', 'log_dice']
        corr = df_scores[metrics].corr()
        lines.append(f"  PMI vs t-score:  {corr.loc['pmi', 't_score']:+.3f}")
        lines.append(f"  PMI vs Log-Dice: {corr.loc['pmi', 'log_dice']:+.3f}")
        lines.append(f"  t-score vs Log-Dice: {corr.loc['t_score', 'log_dice']:+.3f}")
        lines.append(f"  MI³ vs Log-Dice: {corr.loc['mi3', 'log_dice']:+.3f}")
        lines.append("")

        # 統合ランク TOP
        lines.append(f"【統合ランク TOP {min(top_k, len(df_scores))}】")
        lines.append(f"  {'#':>3s} {'phrase':<12s} {'PMI':>6s} {'MI³':>7s} {'t':>6s} {'z':>6s} {'LD':>6s} {'ΔP→':>5s} {'score':>5s}")
        lines.append("  " + "-" * 70)

        for i, (_, row) in enumerate(df_scores.head(top_k).iterrows(), 1):
            lines.append(
                f"  {i:3d} {row['phrase']:<12s} "
                f"{row['pmi']:6.2f} {row['mi3']:7.2f} "
                f"{row['t_score']:6.2f} {row['z_score']:6.2f} "
                f"{row['log_dice']:6.2f} {row['delta_p_ab']:5.3f} "
                f"{row['combined_rank_score']:5.3f}"
            )

        # PMI vs t-score の乖離（書き癖 vs 創造的用法）
        lines.append("")
        lines.append("【PMI-高/t-score-低 = 創造的用法】")
        creative = df_scores[(df_scores['pmi'] > df_scores['pmi'].quantile(0.75)) &
                             (df_scores['t_score'] < df_scores['t_score'].quantile(0.25))]
        for _, row in creative.head(5).iterrows():
            lines.append(f"  {row['phrase']:<12s} PMI={row['pmi']:.2f} t={row['t_score']:.2f}")

        lines.append("")
        lines.append("【t-score-高/PMI-低 = 書き癖候補】")
        habitual = df_scores[(df_scores['t_score'] > df_scores['t_score'].quantile(0.75)) &
                             (df_scores['pmi'] < df_scores['pmi'].quantile(0.25))]
        for _, row in habitual.head(5).iterrows():
            lines.append(f"  {row['phrase']:<12s} PMI={row['pmi']:.2f} t={row['t_score']:.2f}")

        lines.append("")
        lines.append("=" * 75)
        return "\n".join(lines)
