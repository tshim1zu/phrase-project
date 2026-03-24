# coding: utf-8
"""
テキスト複雑度・情報密度メトリクス（Complexity Metrics）

エントロピーの上位互換。テキストの「予測困難度」「情報密度」「語彙密度」を
複数の手法で精密測定する。

指標:
- N-gram Perplexity: 言語モデル的な予測困難度
- Compression Ratio: zlib/lzma 圧縮率 = 情報の冗長度
- Lexical Density: 内容語 / 全語 の比率
- Information Rate: 文あたりの新情報量
- Segment Complexity Profile: テキスト全体の複雑度推移
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023-2026"

import math
import zlib
import re
import logging
from dataclasses import dataclass
from typing import Dict, List, Optional, Tuple
from collections import Counter

import numpy as np

logger = logging.getLogger(__name__)


@dataclass
class ComplexityProfile:
    """セグメント単位の複雑度プロファイル"""
    segment_index: int
    start_pos: int
    end_pos: int
    perplexity: float
    compression_ratio: float
    lexical_density: float
    information_rate: float
    combined_complexity: float


class ComplexityAnalyzer:
    """
    テキスト複雑度分析エンジン

    使用例:
        >>> analyzer = ComplexityAnalyzer()
        >>> result = analyzer.analyze(text)
        >>> print(f"Perplexity: {result['perplexity']:.2f}")
        >>> print(f"Compression: {result['compression_ratio']:.4f}")
    """

    # 日本語の機能語パターン（助詞・助動詞・接続詞など）
    FUNCTION_PATTERNS = re.compile(
        r'[はがのにをでとへもやかなどえばけどなどしかさえだけまでより'
        r'からについてとしてにおいてによってに対してに関してながらつつ'
        r'たりだりけれどもそしてしかしだがまたさらになおかつあるいは'
        r'すなわちつまりしたがってそれでそこでだからゆえにもしもしくは'
        r'ますですたいれるられるせるさせるないぬんだろうでしょう'
        r'べきはずようらしいそうみたいだったであるでございます'
        r'こそすらのみ]'
    )

    def __init__(self, ngram_order: int = 3, segment_size: int = 200):
        """
        Parameters:
            ngram_order: N-gram の次数（Perplexity 計算用）
            segment_size: セグメントサイズ（文字数）
        """
        self.ngram_order = ngram_order
        self.segment_size = segment_size

    # ─── Core Metrics ───────────────────────────────────────

    def calc_ngram_perplexity(self, text: str, order: Optional[int] = None) -> float:
        """
        N-gram Perplexity

        テキスト内の N-gram 出現確率から計算するセルフパープレキシティ。
        外部言語モデル不要。テキスト自体の「予測困難度」を測定。

        PP = 2^(-1/N * Σ log2 P(w_i | w_{i-n+1}...w_{i-1}))

        高い = 予測困難 = 多様/難解
        低い = 予測容易 = 単調/退屈

        Parameters:
            text: 入力テキスト
            order: N-gram の次数（None なら self.ngram_order）
        """
        n = order or self.ngram_order
        chars = list(re.sub(r'\s+', '', text))

        if len(chars) < n + 1:
            return 0.0

        # N-gram カウント
        ngram_counts = Counter()
        context_counts = Counter()

        for i in range(len(chars) - n + 1):
            ngram = tuple(chars[i:i + n])
            context = tuple(chars[i:i + n - 1])
            ngram_counts[ngram] += 1
            context_counts[context] += 1

        # 対数確率の合計
        log_prob_sum = 0.0
        count = 0

        for ngram, freq in ngram_counts.items():
            context = ngram[:-1]
            ctx_freq = context_counts[context]
            if ctx_freq > 0:
                prob = freq / ctx_freq
                log_prob_sum += freq * math.log2(prob)
                count += freq

        if count == 0:
            return 0.0

        # Perplexity = 2^(-avg_log_prob)
        avg_log_prob = log_prob_sum / count
        perplexity = 2.0 ** (-avg_log_prob)

        return perplexity

    def calc_compression_ratio(self, text: str) -> float:
        """
        Compression Ratio (圧縮率)

        compressed_size / original_size

        低い = 冗長（同じパターンの繰り返しが多い）
        高い = 情報密度が高い（パターンが少ない）

        zlib (deflate) を使用。高速かつ安定。
        """
        if not text:
            return 0.0

        encoded = text.encode('utf-8')
        original_size = len(encoded)

        if original_size == 0:
            return 0.0

        compressed = zlib.compress(encoded, level=9)
        compressed_size = len(compressed)

        return compressed_size / original_size

    def calc_lexical_density(self, text: str) -> float:
        """
        Lexical Density (語彙密度)

        content_chars / total_chars

        機能語（助詞・助動詞・接続詞）を除いた「内容語」の比率。
        高い = 情報が詰まっている / 低い = 機能語が多い（文法的に冗長）
        """
        clean = re.sub(r'\s+', '', text)
        if not clean:
            return 0.0

        total = len(clean)
        function_chars = len(self.FUNCTION_PATTERNS.findall(clean))

        content_chars = total - function_chars
        return max(0.0, content_chars / total)

    def calc_information_rate(self, text: str) -> float:
        """
        Information Rate (文あたりの新情報量)

        各文で初めて登場する 2-gram の割合の平均。
        高い = 各文が新情報を提供 / 低い = 繰り返しが多い
        """
        sentences = re.split(r'[。！？\n]+', text)
        sentences = [s.strip() for s in sentences if len(s.strip()) >= 4]

        if not sentences:
            return 0.0

        seen_bigrams = set()
        info_rates = []

        for sent in sentences:
            clean = re.sub(r'\s+', '', sent)
            bigrams = [clean[i:i + 2] for i in range(len(clean) - 1)]

            if not bigrams:
                continue

            new_count = sum(1 for bg in bigrams if bg not in seen_bigrams)
            rate = new_count / len(bigrams)
            info_rates.append(rate)

            seen_bigrams.update(bigrams)

        return float(np.mean(info_rates)) if info_rates else 0.0

    # ─── Segment Profile ────────────────────────────────────

    def calc_complexity_profile(self, text: str) -> List[ComplexityProfile]:
        """
        テキスト全体の複雑度推移をセグメント単位で計算

        Parameters:
            text: 入力テキスト

        Returns:
            各セグメントの ComplexityProfile リスト
        """
        clean = re.sub(r'\n{3,}', '\n\n', text)
        segments = []

        for i in range(0, len(clean), self.segment_size):
            segment = clean[i:i + self.segment_size]
            if len(segment) < 20:  # 最小セグメント長
                continue
            segments.append((i, min(i + self.segment_size, len(clean)), segment))

        profiles = []
        for idx, (start, end, segment) in enumerate(segments):
            pp = self.calc_ngram_perplexity(segment)
            cr = self.calc_compression_ratio(segment)
            ld = self.calc_lexical_density(segment)
            ir = self.calc_information_rate(segment)

            # 複合複雑度: 各指標を正規化して重み付き平均
            # Perplexity: 高い = 複雑 (正規化: /100, 上限1.0)
            # Compression: 高い = 複雑 (そのまま 0-1)
            # Lexical Density: 高い = 密 (そのまま 0-1)
            # Information Rate: 高い = 新情報多い (そのまま 0-1)
            pp_norm = min(1.0, pp / 100.0) if pp > 0 else 0.0
            combined = (
                0.30 * pp_norm +
                0.25 * cr +
                0.25 * ld +
                0.20 * ir
            )

            profiles.append(ComplexityProfile(
                segment_index=idx,
                start_pos=start,
                end_pos=end,
                perplexity=round(pp, 2),
                compression_ratio=round(cr, 4),
                lexical_density=round(ld, 4),
                information_rate=round(ir, 4),
                combined_complexity=round(combined, 4),
            ))

        return profiles

    # ─── High-Level API ─────────────────────────────────────

    def analyze(self, text: str) -> Dict:
        """
        テキストの複雑度を包括的に分析

        Returns:
            Dict: 全複雑度指標
        """
        pp = self.calc_ngram_perplexity(text)
        cr = self.calc_compression_ratio(text)
        ld = self.calc_lexical_density(text)
        ir = self.calc_information_rate(text)
        profile = self.calc_complexity_profile(text)

        # プロファイルの統計
        if profile:
            complexities = [p.combined_complexity for p in profile]
            complexity_std = float(np.std(complexities))
            complexity_trend = self._calc_trend(complexities)
        else:
            complexity_std = 0.0
            complexity_trend = 0.0

        return {
            'perplexity': round(pp, 2),
            'compression_ratio': round(cr, 4),
            'lexical_density': round(ld, 4),
            'information_rate': round(ir, 4),
            'complexity_std': round(complexity_std, 4),
            'complexity_trend': round(complexity_trend, 4),
            'num_segments': len(profile),
            'profile': profile,
            'assessment': self._assess(pp, cr, ld, ir),
        }

    def generate_report(self, text: str) -> str:
        """複雑度分析レポート"""
        result = self.analyze(text)

        lines = [
            "=" * 65,
            "【テキスト複雑度分析レポート】",
            "=" * 65,
            "",
            f"N-gram Perplexity:  {result['perplexity']:8.2f}  (高い=予測困難/多様)",
            f"Compression Ratio:  {result['compression_ratio']:8.4f}  (高い=情報密度高)",
            f"Lexical Density:    {result['lexical_density']:8.4f}  (高い=内容語が多い)",
            f"Information Rate:   {result['information_rate']:8.4f}  (高い=新情報が多い)",
            "",
            f"複雑度の変動(σ):   {result['complexity_std']:8.4f}",
            f"複雑度トレンド:     {result['complexity_trend']:+8.4f}  (正=後半が複雑化)",
            "",
            f"評価: {result['assessment']}",
            "",
        ]

        # プロファイル
        profile = result.get('profile', [])
        if profile:
            lines.append("-" * 65)
            lines.append("【セグメント別複雑度推移】")
            lines.append(f"  {'#':>3s} {'位置':>10s} {'PP':>7s} {'CR':>6s} {'LD':>6s} {'IR':>6s} {'合計':>6s}")
            lines.append("  " + "-" * 55)

            for p in profile:
                lines.append(
                    f"  {p.segment_index:3d} "
                    f"{p.start_pos:5d}-{p.end_pos:5d} "
                    f"{p.perplexity:7.2f} "
                    f"{p.compression_ratio:6.4f} "
                    f"{p.lexical_density:6.4f} "
                    f"{p.information_rate:6.4f} "
                    f"{p.combined_complexity:6.4f}"
                )

        lines.append("")
        lines.append("=" * 65)
        return "\n".join(lines)

    # ─── Private Helpers ────────────────────────────────────

    @staticmethod
    def _calc_trend(values: List[float]) -> float:
        """線形トレンド（正=増加傾向、負=減少傾向）"""
        if len(values) < 2:
            return 0.0
        x = np.arange(len(values), dtype=float)
        y = np.array(values)
        # 最小二乗
        coeffs = np.polyfit(x, y, 1)
        return float(coeffs[0])

    @staticmethod
    def _assess(pp: float, cr: float, ld: float, ir: float) -> str:
        """複雑度の総合評価"""
        signals = []

        if pp > 50:
            signals.append("予測困難度が高い（多様な表現）")
        elif pp < 15:
            signals.append("予測可能性が高い（パターン的）")

        if cr > 0.6:
            signals.append("情報密度が高い")
        elif cr < 0.3:
            signals.append("冗長性が高い（繰り返しが多い）")

        if ld > 0.7:
            signals.append("内容語が密集")
        elif ld < 0.4:
            signals.append("機能語が多い（文法的に冗長）")

        if ir > 0.6:
            signals.append("各文が新情報を提供")
        elif ir < 0.2:
            signals.append("情報の繰り返しが多い")

        if not signals:
            return "標準的な複雑度"
        return " / ".join(signals)
