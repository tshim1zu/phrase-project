# coding: utf-8
"""
時系列・進行分析エンジン（Temporal Analyzer）

EP間・Part間・チャプター間の「推移」を追跡。
伏線語のバースト検出、語彙成長の変動、エントロピー推移を可視化。

指標:
- Burstiness: 語の突発的使用区間を検出（Kleinberg 簡易版）
- Term Frequency Trend: 特定語の出現頻度トレンド（増加/減少/バースト）
- MATTR 推移: EP 進行に伴う語彙多様性の変動
- Entropy Rate 推移: 文単位エントロピーの推移
- Vocabulary Accumulation: 新規語彙数の推移
- Document Comparison Matrix: 全指標のドキュメント間マトリクス
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023-2026"

import math
import re
import logging
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple, Sequence
from collections import Counter, OrderedDict

import numpy as np
import pandas as pd

logger = logging.getLogger(__name__)


@dataclass
class BurstInterval:
    """バースト区間"""
    term: str
    doc_start: int          # バースト開始ドキュメントインデックス
    doc_end: int            # バースト終了ドキュメントインデックス
    burst_level: int        # バーストレベル（1=軽微, 2=中, 3=強烈）
    avg_freq_in_burst: float
    avg_freq_outside: float
    ratio: float            # burst内 / burst外 の頻度比


@dataclass
class TermTrend:
    """語のトレンド情報"""
    term: str
    frequencies: List[int]
    trend_slope: float          # 線形トレンドの傾き
    trend_direction: str        # 'increasing', 'decreasing', 'stable', 'burst'
    burst_intervals: List[BurstInterval]
    coefficient_of_variation: float  # 変動係数


@dataclass
class TemporalSnapshot:
    """1ドキュメントの時系列スナップショット"""
    doc_index: int
    label: str
    total_tokens: int
    unique_types: int
    ttr: float
    mattr: float
    entropy: float
    new_vocab_count: int    # このドキュメントで初出の語彙数
    cumulative_vocab: int   # 累積語彙数


class TemporalAnalyzer:
    """
    時系列テキスト分析エンジン

    使用例:
        >>> analyzer = TemporalAnalyzer()
        >>> docs = [ep101_text, ep102_text, ep103_text, ...]
        >>> labels = ["EP101", "EP102", "EP103", ...]
        >>> result = analyzer.analyze_series(docs, labels)
        >>> print(result['vocabulary_trend'])
    """

    def __init__(
        self,
        ngram_size: int = 2,
        mattr_window: int = 50,
        burst_threshold: float = 2.0,
    ):
        """
        Parameters:
            ngram_size: トークン化のN-gramサイズ
            mattr_window: MATTR のウィンドウサイズ
            burst_threshold: バースト検出の閾値（平均の何倍でバースト判定）
        """
        self.ngram_size = ngram_size
        self.mattr_window = mattr_window
        self.burst_threshold = burst_threshold

    # ─── Burstiness Detection ───────────────────────────────

    def detect_bursts(
        self,
        documents: List[str],
        target_terms: Optional[List[str]] = None,
        top_k: int = 20,
    ) -> List[TermTrend]:
        """
        語のバースト（突発的使用）を検出

        Kleinberg 簡易版: 移動平均と比較して急増した区間を検出。

        Parameters:
            documents: ドキュメントのリスト（時系列順）
            target_terms: 監視対象語（None なら自動で頻出語を選択）
            top_k: 自動選択時の上位語数

        Returns:
            TermTrend のリスト（バースト強度順）
        """
        n_docs = len(documents)
        if n_docs < 3:
            return []

        # 各ドキュメントの語彙頻度
        doc_freqs = []
        global_freq = Counter()
        for doc in documents:
            tokens = self._tokenize(doc)
            freq = Counter(tokens)
            doc_freqs.append(freq)
            global_freq.update(freq)

        # 対象語の決定
        if target_terms is None:
            # 全ドキュメントでの頻出語（ただし全ドキュメントに均等に出るものは除外）
            candidates = []
            for term, total_freq in global_freq.most_common(top_k * 3):
                # 出現ドキュメント数
                doc_count = sum(1 for df in doc_freqs if term in df)
                # 全ドキュメントに出る語は面白くない
                if doc_count < n_docs * 0.9 and total_freq >= 3:
                    candidates.append(term)
                if len(candidates) >= top_k:
                    break
            target_terms = candidates

        # 各語のトレンドとバースト検出
        trends = []
        for term in target_terms:
            freqs = [df.get(term, 0) for df in doc_freqs]
            trend = self._analyze_term_trend(term, freqs)
            trends.append(trend)

        # バースト強度順にソート
        trends.sort(
            key=lambda t: (len(t.burst_intervals), t.coefficient_of_variation),
            reverse=True,
        )

        return trends

    def _analyze_term_trend(self, term: str, freqs: List[int]) -> TermTrend:
        """1語のトレンドとバーストを分析"""
        n = len(freqs)
        arr = np.array(freqs, dtype=float)

        # 線形トレンド
        if n >= 2:
            x = np.arange(n, dtype=float)
            coeffs = np.polyfit(x, arr, 1)
            slope = float(coeffs[0])
        else:
            slope = 0.0

        # 変動係数
        mean_val = arr.mean()
        std_val = arr.std()
        cv = float(std_val / mean_val) if mean_val > 0 else 0.0

        # バースト区間の検出
        # 移動平均（窓サイズ = max(3, n//4)）との比較
        burst_intervals = []
        if n >= 3 and mean_val > 0:
            window = max(3, n // 4)
            # パディング付き移動平均
            padded = np.pad(arr, (window // 2, window // 2), mode='edge')
            moving_avg = np.convolve(padded, np.ones(window) / window, mode='valid')[:n]

            # バースト: 移動平均の burst_threshold 倍を超える区間
            in_burst = False
            burst_start = 0

            for i in range(n):
                threshold = max(moving_avg[i] * self.burst_threshold, mean_val * 1.5)
                if arr[i] > threshold and not in_burst:
                    in_burst = True
                    burst_start = i
                elif (arr[i] <= threshold or i == n - 1) and in_burst:
                    burst_end = i if arr[i] <= threshold else i + 1

                    # バースト区間の統計
                    burst_freqs = arr[burst_start:burst_end]
                    outside_mask = np.ones(n, dtype=bool)
                    outside_mask[burst_start:burst_end] = False
                    outside_freqs = arr[outside_mask]

                    avg_in = float(burst_freqs.mean())
                    avg_out = float(outside_freqs.mean()) if len(outside_freqs) > 0 else 0.0
                    ratio = avg_in / avg_out if avg_out > 0 else float('inf')

                    # バーストレベル
                    if ratio > 5.0:
                        level = 3
                    elif ratio > 3.0:
                        level = 2
                    else:
                        level = 1

                    burst_intervals.append(BurstInterval(
                        term=term,
                        doc_start=burst_start,
                        doc_end=burst_end,
                        burst_level=level,
                        avg_freq_in_burst=round(avg_in, 2),
                        avg_freq_outside=round(avg_out, 2),
                        ratio=round(ratio, 2),
                    ))
                    in_burst = False

        # トレンド方向
        if burst_intervals:
            direction = 'burst'
        elif slope > 0.1 * mean_val and mean_val > 0:
            direction = 'increasing'
        elif slope < -0.1 * mean_val and mean_val > 0:
            direction = 'decreasing'
        else:
            direction = 'stable'

        return TermTrend(
            term=term,
            frequencies=freqs,
            trend_slope=round(slope, 4),
            trend_direction=direction,
            burst_intervals=burst_intervals,
            coefficient_of_variation=round(cv, 4),
        )

    # ─── Series Analysis ────────────────────────────────────

    def analyze_series(
        self,
        documents: List[str],
        labels: Optional[List[str]] = None,
    ) -> Dict:
        """
        ドキュメント系列の時系列分析

        Parameters:
            documents: ドキュメントのリスト（時系列順）
            labels: 各ドキュメントのラベル

        Returns:
            Dict: 時系列分析結果
        """
        n = len(documents)
        if labels is None:
            labels = [f"doc_{i}" for i in range(n)]

        snapshots = []
        cumulative_vocab = set()

        for i, (doc, label) in enumerate(zip(documents, labels)):
            tokens = self._tokenize(doc)
            token_set = set(tokens)

            # 新規語彙
            new_vocab = token_set - cumulative_vocab
            cumulative_vocab.update(token_set)

            # TTR
            total = len(tokens)
            unique = len(token_set)
            ttr = unique / total if total > 0 else 0.0

            # MATTR
            mattr = self._calc_mattr(tokens)

            # 文単位エントロピー
            entropy = self._calc_sentence_entropy(doc)

            snapshots.append(TemporalSnapshot(
                doc_index=i,
                label=label,
                total_tokens=total,
                unique_types=unique,
                ttr=round(ttr, 4),
                mattr=round(mattr, 4),
                entropy=round(entropy, 4),
                new_vocab_count=len(new_vocab),
                cumulative_vocab=len(cumulative_vocab),
            ))

        # トレンド計算
        ttr_values = [s.ttr for s in snapshots]
        mattr_values = [s.mattr for s in snapshots]
        entropy_values = [s.entropy for s in snapshots]
        new_vocab_values = [s.new_vocab_count for s in snapshots]

        return {
            'snapshots': snapshots,
            'ttr_trend': self._linear_trend(ttr_values),
            'mattr_trend': self._linear_trend(mattr_values),
            'entropy_trend': self._linear_trend(entropy_values),
            'vocab_growth_trend': self._linear_trend(new_vocab_values),
            'total_unique_vocab': len(cumulative_vocab),
            'vocab_saturation': self._calc_saturation(new_vocab_values),
        }

    def series_to_dataframe(
        self,
        documents: List[str],
        labels: Optional[List[str]] = None,
    ) -> pd.DataFrame:
        """時系列分析結果を DataFrame で返す"""
        result = self.analyze_series(documents, labels)
        rows = []
        for s in result['snapshots']:
            rows.append({
                'doc_index': s.doc_index,
                'label': s.label,
                'total_tokens': s.total_tokens,
                'unique_types': s.unique_types,
                'ttr': s.ttr,
                'mattr': s.mattr,
                'entropy': s.entropy,
                'new_vocab': s.new_vocab_count,
                'cumulative_vocab': s.cumulative_vocab,
            })
        return pd.DataFrame(rows)

    # ─── Comparison Matrix ──────────────────────────────────

    def comparison_matrix(
        self,
        documents: List[str],
        labels: Optional[List[str]] = None,
    ) -> pd.DataFrame:
        """
        全ドキュメントペアの JSD マトリクス

        Returns:
            DataFrame: n×n の JSD 距離行列
        """
        from .distribution_comparator import DistributionComparator

        n = len(documents)
        if labels is None:
            labels = [f"doc_{i}" for i in range(n)]

        comparator = DistributionComparator()

        # 各ドキュメントの頻度分布
        freqs = []
        for doc in documents:
            tokens = self._tokenize(doc)
            freqs.append(Counter(tokens))

        # 距離行列
        matrix = np.zeros((n, n))
        for i in range(n):
            for j in range(i + 1, n):
                result = comparator.compare(freqs[i], freqs[j])
                matrix[i, j] = result.jsd
                matrix[j, i] = result.jsd

        return pd.DataFrame(matrix, index=labels, columns=labels)

    # ─── Report ─────────────────────────────────────────────

    def generate_report(
        self,
        documents: List[str],
        labels: Optional[List[str]] = None,
        target_terms: Optional[List[str]] = None,
    ) -> str:
        """時系列分析レポート生成"""
        result = self.analyze_series(documents, labels)
        bursts = self.detect_bursts(documents, target_terms)

        lines = [
            "=" * 70,
            "【時系列テキスト分析レポート】",
            "=" * 70,
            "",
            f"ドキュメント数: {len(documents)}",
            f"累積語彙数: {result['total_unique_vocab']}",
            f"語彙飽和度: {result['vocab_saturation']:.4f}",
            "",
        ]

        # トレンド
        lines.append("【トレンド（線形傾き）】")
        lines.append(f"  TTR:      {result['ttr_trend']:+.6f}")
        lines.append(f"  MATTR:    {result['mattr_trend']:+.6f}")
        lines.append(f"  Entropy:  {result['entropy_trend']:+.6f}")
        lines.append(f"  新語数:   {result['vocab_growth_trend']:+.4f}")
        lines.append("")

        # スナップショット一覧
        lines.append("-" * 70)
        lines.append("【ドキュメント別指標】")
        lines.append(f"  {'#':>3s} {'label':<10s} {'tokens':>7s} {'types':>6s} "
                      f"{'TTR':>6s} {'MATTR':>6s} {'H':>6s} {'new':>5s} {'cum':>6s}")
        lines.append("  " + "-" * 65)

        for s in result['snapshots']:
            lines.append(
                f"  {s.doc_index:3d} {s.label:<10s} "
                f"{s.total_tokens:7d} {s.unique_types:6d} "
                f"{s.ttr:6.4f} {s.mattr:6.4f} {s.entropy:6.4f} "
                f"{s.new_vocab_count:5d} {s.cumulative_vocab:6d}"
            )

        # バースト検出
        if bursts:
            lines.append("")
            lines.append("-" * 70)
            lines.append("【バースト検出】")

            burst_terms = [t for t in bursts if t.burst_intervals]
            if burst_terms:
                for trend in burst_terms[:10]:
                    for bi in trend.burst_intervals:
                        labels_list = labels or [f"doc_{i}" for i in range(len(documents))]
                        start_label = labels_list[bi.doc_start] if bi.doc_start < len(labels_list) else str(bi.doc_start)
                        end_label = labels_list[min(bi.doc_end - 1, len(labels_list) - 1)] if labels_list else str(bi.doc_end)

                        level_str = "!" * bi.burst_level
                        lines.append(
                            f"  {level_str:3s} [{trend.term}] "
                            f"{start_label}〜{end_label} "
                            f"(内:{bi.avg_freq_in_burst:.1f} / 外:{bi.avg_freq_outside:.1f} "
                            f"= {bi.ratio:.1f}x)"
                        )
            else:
                lines.append("  バーストは検出されませんでした")

        lines.append("")
        lines.append("=" * 70)
        return "\n".join(lines)

    # ─── Private Helpers ────────────────────────────────────

    def _tokenize(self, text: str) -> List[str]:
        """簡易トークナイザ（N-gram）"""
        clean = re.sub(r'\s+', '', text)
        n = self.ngram_size
        return [clean[i:i + n] for i in range(len(clean) - n + 1)]

    def _calc_mattr(self, tokens: List[str]) -> float:
        """MATTR 計算"""
        n = len(tokens)
        w = self.mattr_window

        if n < w:
            return len(set(tokens)) / n if n > 0 else 0.0

        ttrs = []
        for i in range(n - w + 1):
            window = tokens[i:i + w]
            ttrs.append(len(set(window)) / w)

        return float(np.mean(ttrs))

    def _calc_sentence_entropy(self, text: str) -> float:
        """文単位のシャノンエントロピー"""
        sentences = re.split(r'[。！？\n]+', text)
        sentences = [s.strip() for s in sentences if len(s.strip()) >= 2]

        if not sentences:
            return 0.0

        # 各文の長さの分布からエントロピーを計算
        lengths = [len(s) for s in sentences]
        total = sum(lengths)

        if total == 0:
            return 0.0

        entropy = 0.0
        for length in lengths:
            p = length / total
            if p > 0:
                entropy -= p * math.log2(p)

        return entropy

    @staticmethod
    def _linear_trend(values: List[float]) -> float:
        """線形トレンドの傾き"""
        if len(values) < 2:
            return 0.0
        x = np.arange(len(values), dtype=float)
        y = np.array(values)
        coeffs = np.polyfit(x, y, 1)
        return float(coeffs[0])

    @staticmethod
    def _calc_saturation(new_vocab_counts: List[int]) -> float:
        """
        語彙飽和度

        後半の新語出現率 / 前半の新語出現率
        1に近い = まだ飽和していない / 0に近い = 飽和
        """
        n = len(new_vocab_counts)
        if n < 4:
            return 1.0

        mid = n // 2
        first_half = sum(new_vocab_counts[:mid])
        second_half = sum(new_vocab_counts[mid:])

        if first_half == 0:
            return 1.0

        return second_half / first_half
