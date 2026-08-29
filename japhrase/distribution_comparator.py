# coding: utf-8
"""
分布比較エンジン（Distribution Comparator）

テキスト間・Part間・キャラ間の語彙分布の「違い」を精密測定する。
χ² 一本足からの脱却。

指標:
- Log-Likelihood Ratio (G²): スパースに強いキーネス分析
- Jensen-Shannon Divergence: 対称的な分布距離
- Effect Size (Log Ratio): 有意性ではなく「どれだけ違うか」
- Dice Coefficient: コーパスサイズ非依存の結合度
- Keyness Profiler: G² + Effect Size の統合キーワード抽出
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023-2026"

import math
import logging
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple
from collections import Counter

import numpy as np
import pandas as pd
from scipy import stats as sp_stats

logger = logging.getLogger(__name__)


@dataclass
class KeynessResult:
    """キーネス分析結果（1語分）"""
    term: str
    freq_target: int
    freq_reference: int
    total_target: int
    total_reference: int

    # 統計指標
    log_likelihood: float       # G² 値
    p_value: float              # G² の p 値
    log_ratio: float            # Effect Size (log2)
    dice_coefficient: float     # Dice 係数
    jsd_contribution: float     # JSD への寄与度

    # 判定
    significance_level: str     # ***, **, *, ns
    direction: str              # 'overuse' / 'underuse'
    keyness_score: float        # 複合キーネススコア (0-1)


@dataclass
class DistributionComparison:
    """2テキスト間の分布比較結果"""
    jsd: float                              # Jensen-Shannon Divergence
    jsd_sqrt: float                         # √JSD = Jensen-Shannon Distance
    total_target: int
    total_reference: int
    unique_target: int
    unique_reference: int
    shared_terms: int
    keyness_results: List[KeynessResult] = field(default_factory=list)


class DistributionComparator:
    """
    テキスト間の語彙分布比較エンジン

    使用例:
        >>> comp = DistributionComparator()
        >>> freq_a = Counter(["彼女", "彼女", "騎士", "剣", "剣", "剣"])
        >>> freq_b = Counter(["研究", "研究", "実験", "実験", "実験", "データ"])
        >>> result = comp.compare(freq_a, freq_b)
        >>> print(f"JSD: {result.jsd:.4f}")
    """

    def __init__(self, alpha: float = 0.05, min_freq: int = 2):
        """
        Parameters:
            alpha: 有意水準
            min_freq: キーネス分析の最小頻度（target + reference の合計）
        """
        self.alpha = alpha
        self.min_freq = min_freq

    # ─── Core Metrics ───────────────────────────────────────

    @staticmethod
    def log_likelihood_ratio(
        o11: int, o12: int, o21: int, o22: int
    ) -> float:
        """
        Log-Likelihood Ratio (G²)

        2×2 分割表:
            |  target  | reference |
        term|   o11    |   o12     |
        rest|   o21    |   o22     |

        G² = 2 * Σ O * ln(O/E)
        χ² よりスパースデータに強い。キーネス分析の標準指標。
        """
        table = np.array([[o11, o12], [o21, o22]], dtype=np.float64)
        # ゼロ対策
        table = np.maximum(table, 1e-10)
        total = table.sum()
        row_sums = table.sum(axis=1, keepdims=True)
        col_sums = table.sum(axis=0, keepdims=True)
        expected = row_sums * col_sums / total
        expected = np.maximum(expected, 1e-10)

        g2 = 2.0 * np.sum(table * np.log(table / expected))
        return float(g2)

    @staticmethod
    def log_ratio(freq_target: int, freq_reference: int,
                  total_target: int, total_reference: int) -> float:
        """
        Effect Size: Log Ratio (log2)

        log2(normalized_freq_target / normalized_freq_reference)
        正: target で過剰使用 / 負: target で過少使用
        """
        # ゼロ補正（Laplace smoothing）
        norm_t = (freq_target + 0.5) / (total_target + 1)
        norm_r = (freq_reference + 0.5) / (total_reference + 1)
        return math.log2(norm_t / norm_r)

    @staticmethod
    def dice_coefficient(freq_target: int, freq_reference: int,
                         total_target: int, total_reference: int) -> float:
        """
        Dice Coefficient

        2 * min(norm_t, norm_r) / (norm_t + norm_r)
        コーパスサイズに依存しない重複度。0-1。
        """
        norm_t = freq_target / max(total_target, 1)
        norm_r = freq_reference / max(total_reference, 1)
        denom = norm_t + norm_r
        if denom == 0:
            return 0.0
        return 2.0 * min(norm_t, norm_r) / denom

    @staticmethod
    def jensen_shannon_divergence(
        p: np.ndarray, q: np.ndarray
    ) -> float:
        """
        Jensen-Shannon Divergence

        JSD(P||Q) = 0.5 * KL(P||M) + 0.5 * KL(Q||M)
        where M = 0.5 * (P + Q)

        対称的・有界（0-1 when using log2）。分布間距離の標準。
        """
        # ゼロ補正
        p = np.asarray(p, dtype=np.float64)
        q = np.asarray(q, dtype=np.float64)

        # 正規化
        p_sum = p.sum()
        q_sum = q.sum()
        if p_sum == 0 or q_sum == 0:
            return 1.0  # 完全に異なる

        p = p / p_sum
        q = q / q_sum

        m = 0.5 * (p + q)

        # KL divergence（ゼロ回避）
        def _kl(a: np.ndarray, b: np.ndarray) -> float:
            """確率が正の要素についてKLダイバージェンスを計算する。"""
            mask = a > 0
            return float(np.sum(a[mask] * np.log2(a[mask] / b[mask])))

        jsd = 0.5 * _kl(p, m) + 0.5 * _kl(q, m)
        return max(0.0, min(1.0, jsd))

    # ─── High-Level API ─────────────────────────────────────

    def compare(
        self,
        freq_target: Counter,
        freq_reference: Counter,
        label_target: str = "target",
        label_reference: str = "reference",
    ) -> DistributionComparison:
        """
        2つの頻度分布を包括的に比較

        Parameters:
            freq_target: ターゲットテキストの語彙頻度
            freq_reference: リファレンステキストの語彙頻度
            label_target: ターゲットのラベル
            label_reference: リファレンスのラベル

        Returns:
            DistributionComparison
        """
        total_t = sum(freq_target.values())
        total_r = sum(freq_reference.values())

        # 全語彙の和集合
        all_terms = set(freq_target.keys()) | set(freq_reference.keys())
        shared = set(freq_target.keys()) & set(freq_reference.keys())

        # JSD 計算用ベクトル
        terms_list = sorted(all_terms)
        p_vec = np.array([freq_target.get(t, 0) for t in terms_list], dtype=np.float64)
        q_vec = np.array([freq_reference.get(t, 0) for t in terms_list], dtype=np.float64)

        jsd = self.jensen_shannon_divergence(p_vec, q_vec)

        # JSD 寄与度（各語の寄与を計算）
        p_norm = p_vec / max(p_vec.sum(), 1)
        q_norm = q_vec / max(q_vec.sum(), 1)
        m_norm = 0.5 * (p_norm + q_norm)

        jsd_contributions = {}
        for i, term in enumerate(terms_list):
            contrib = 0.0
            if p_norm[i] > 0 and m_norm[i] > 0:
                contrib += 0.5 * p_norm[i] * math.log2(p_norm[i] / m_norm[i])
            if q_norm[i] > 0 and m_norm[i] > 0:
                contrib += 0.5 * q_norm[i] * math.log2(q_norm[i] / m_norm[i])
            jsd_contributions[term] = max(0.0, contrib)

        # キーネス分析（全語に対して）
        keyness_results = []
        for term in terms_list:
            ft = freq_target.get(term, 0)
            fr = freq_reference.get(term, 0)

            # 最小頻度フィルタ
            if ft + fr < self.min_freq:
                continue

            # 2×2 分割表
            o11 = ft
            o12 = fr
            o21 = total_t - ft
            o22 = total_r - fr

            g2 = self.log_likelihood_ratio(o11, o12, o21, o22)
            lr = self.log_ratio(ft, fr, total_t, total_r)
            dice = self.dice_coefficient(ft, fr, total_t, total_r)

            # p 値（G² は自由度1のχ²分布に近似）
            p_value = 1.0 - sp_stats.chi2.cdf(abs(g2), df=1)

            # 有意水準判定
            if p_value < 0.001:
                sig = "***"
            elif p_value < 0.01:
                sig = "**"
            elif p_value < self.alpha:
                sig = "*"
            else:
                sig = "ns"

            direction = "overuse" if lr > 0 else "underuse"

            # 複合キーネススコア
            # G² の正規化 + |log_ratio| + JSD寄与
            g2_norm = min(1.0, g2 / 100.0)  # G²=100 で飽和
            lr_norm = min(1.0, abs(lr) / 5.0)  # |LR|=5 で飽和
            jsd_c = jsd_contributions.get(term, 0.0)
            jsd_c_norm = min(1.0, jsd_c / 0.01)  # 0.01 で飽和

            keyness_score = (
                0.45 * g2_norm +
                0.35 * lr_norm +
                0.20 * jsd_c_norm
            )

            keyness_results.append(KeynessResult(
                term=term,
                freq_target=ft,
                freq_reference=fr,
                total_target=total_t,
                total_reference=total_r,
                log_likelihood=g2,
                p_value=p_value,
                log_ratio=lr,
                dice_coefficient=dice,
                jsd_contribution=jsd_c,
                significance_level=sig,
                direction=direction,
                keyness_score=keyness_score,
            ))

        # キーネススコア降順
        keyness_results.sort(key=lambda r: r.keyness_score, reverse=True)

        return DistributionComparison(
            jsd=jsd,
            jsd_sqrt=math.sqrt(jsd),
            total_target=total_t,
            total_reference=total_r,
            unique_target=len(freq_target),
            unique_reference=len(freq_reference),
            shared_terms=len(shared),
            keyness_results=keyness_results,
        )

    def compare_to_dataframe(
        self,
        freq_target: Counter,
        freq_reference: Counter,
        top_k: Optional[int] = None,
    ) -> pd.DataFrame:
        """比較結果を DataFrame で返す"""
        result = self.compare(freq_target, freq_reference)
        rows = []
        for kr in result.keyness_results:
            rows.append({
                'term': kr.term,
                'freq_target': kr.freq_target,
                'freq_reference': kr.freq_reference,
                'log_likelihood': kr.log_likelihood,
                'p_value': kr.p_value,
                'log_ratio': kr.log_ratio,
                'dice_coefficient': kr.dice_coefficient,
                'jsd_contribution': kr.jsd_contribution,
                'significance': kr.significance_level,
                'direction': kr.direction,
                'keyness_score': kr.keyness_score,
            })

        df = pd.DataFrame(rows)
        if top_k and not df.empty:
            df = df.head(top_k)
        return df

    def generate_report(
        self,
        freq_target: Counter,
        freq_reference: Counter,
        label_target: str = "Target",
        label_reference: str = "Reference",
        top_k: int = 20,
    ) -> str:
        """比較レポート生成"""
        result = self.compare(freq_target, freq_reference, label_target, label_reference)

        lines = [
            "=" * 70,
            f"【分布比較レポート】 {label_target} vs {label_reference}",
            "=" * 70,
            "",
            f"Jensen-Shannon Divergence: {result.jsd:.6f}",
            f"Jensen-Shannon Distance:   {result.jsd_sqrt:.6f}",
            f"  ({label_target}: {result.total_target}語 / {label_reference}: {result.total_reference}語)",
            f"  ユニーク語彙: {result.unique_target} / {result.unique_reference}",
            f"  共有語彙: {result.shared_terms}",
            "",
        ]

        # JSD の解釈
        if result.jsd < 0.05:
            lines.append("  → 分布は非常に類似しています")
        elif result.jsd < 0.15:
            lines.append("  → 分布にやや差があります")
        elif result.jsd < 0.30:
            lines.append("  → 分布に明確な差があります")
        else:
            lines.append("  → 分布は大きく異なります")

        lines.append("")
        lines.append("-" * 70)

        # Overuse TOP
        overuse = [kr for kr in result.keyness_results if kr.direction == "overuse"]
        underuse = [kr for kr in result.keyness_results if kr.direction == "underuse"]

        lines.append(f"【{label_target} で過剰使用 TOP {min(top_k, len(overuse))}】")
        for i, kr in enumerate(overuse[:top_k], 1):
            lines.append(
                f"  {i:2d}. {kr.term:<12s} "
                f"G²={kr.log_likelihood:7.2f} {kr.significance_level:>3s}  "
                f"LR={kr.log_ratio:+.2f}  "
                f"({kr.freq_target}/{kr.freq_reference})"
            )

        lines.append("")
        lines.append(f"【{label_target} で過少使用 TOP {min(top_k, len(underuse))}】")
        for i, kr in enumerate(underuse[:top_k], 1):
            lines.append(
                f"  {i:2d}. {kr.term:<12s} "
                f"G²={kr.log_likelihood:7.2f} {kr.significance_level:>3s}  "
                f"LR={kr.log_ratio:+.2f}  "
                f"({kr.freq_target}/{kr.freq_reference})"
            )

        lines.append("")
        lines.append("=" * 70)
        return "\n".join(lines)

    # ─── Utilities ──────────────────────────────────────────

    @staticmethod
    def text_to_freq(text: str, min_length: int = 2, max_length: int = 10) -> Counter:
        """
        テキストから N-gram 頻度カウンターを生成（簡易版）

        PhraseExtracter と併用する場合は不要。
        単独で使いたいときの便利メソッド。
        """
        import re
        freq = Counter()
        # 空白・改行で分割しない文字 N-gram
        clean = re.sub(r'\s+', '', text)
        for n in range(min_length, max_length + 1):
            for i in range(len(clean) - n + 1):
                gram = clean[i:i + n]
                freq[gram] += 1
        return freq

    @staticmethod
    def dataframe_to_freq(
        df: pd.DataFrame,
        phrase_col: str = 'phrase',
        freq_col: str = 'freq',
    ) -> Counter:
        """DataFrame を Counter に変換"""
        # 列名の自動検出
        if phrase_col not in df.columns:
            phrase_col = 'seqchar' if 'seqchar' in df.columns else df.columns[0]
        if freq_col not in df.columns:
            freq_col = 'frequency' if 'frequency' in df.columns else 'count'

        return Counter(dict(zip(df[phrase_col], df[freq_col])))
