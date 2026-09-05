"""
計量文学（Stylometry）分析モジュール

文体の定量的特徴（語彙の豊かさ、文字種の比率など）を分析します。
著者判定や文体分類に用いられる統計指標を計算します。

Phase 2 強化 (2026-03):
- Hapax Legomena Ratio / Dis Legomena Ratio
- Brunet's W / Honoré's R / Sichel's S / Simpson's D
- MATTR (Moving Average Type-Token Ratio)
- Vocabulary Growth Curve (Heaps' Law fitting)
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023-2026"

import re
import math
import numpy as np
from collections import Counter
from typing import Dict, List, Optional, Tuple
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

    # ─── Phase 2: 語彙多様性の深化 ─────────────────────────

    def analyze_advanced_diversity(self, text: str) -> Dict:
        """
        高度な語彙多様性指標群を計算

        TTR / Yule's K だけでは長文で崩壊する。
        以下の指標は長文耐性がある:

        - Hapax Legomena Ratio: 1回語の割合（語彙の洗練度）
        - Dis Legomena Ratio: 2回語の割合
        - Brunet's W: 対数ベース。長文に強い語彙多様性
        - Honoré's R: Hapax と総語数の統合指標
        - Sichel's S: Dis Legomena / Unique Types
        - Simpson's D: 「同じ語に2回当たる確率」の逆数

        Parameters:
            text: 入力テキスト

        Returns:
            Dict: 高度な語彙多様性指標
        """
        freq_dist = self._get_frequency_distribution(text)
        if not freq_dist:
            return self._empty_advanced_diversity()

        N = sum(freq_dist.values())       # 総トークン数
        V = len(freq_dist)                # ユニーク語彙数
        V1 = sum(1 for f in freq_dist.values() if f == 1)  # hapax legomena
        V2 = sum(1 for f in freq_dist.values() if f == 2)  # dis legomena

        # Hapax Legomena Ratio
        hapax_ratio = V1 / V if V > 0 else 0.0

        # Dis Legomena Ratio (Sichel's S)
        sichels_s = V2 / V if V > 0 else 0.0

        # Brunet's W: W = N^(V^-0.172)
        # 小さいほど語彙が豊か
        brunets_w = N ** (V ** -0.172) if V > 0 and N > 0 else 0.0

        # Honoré's R: R = 100 * log(N) / (1 - V1/V)
        # 大きいほど語彙が豊か
        if V > 0 and V1 < V:
            honores_r = 100.0 * math.log(N) / (1.0 - V1 / V)
        else:
            honores_r = 0.0

        # Simpson's Diversity Index: D = 1 - Σ(n_i * (n_i - 1)) / (N * (N - 1))
        # 1 に近いほど多様
        if N > 1:
            sum_ni_ni_1 = sum(f * (f - 1) for f in freq_dist.values())
            simpsons_d = 1.0 - sum_ni_ni_1 / (N * (N - 1))
        else:
            simpsons_d = 0.0

        return {
            'total_tokens': N,
            'unique_types': V,
            'hapax_legomena': V1,
            'dis_legomena': V2,
            'hapax_ratio': round(hapax_ratio, 4),
            'sichels_s': round(sichels_s, 4),
            'brunets_w': round(brunets_w, 4),
            'honores_r': round(honores_r, 2),
            'simpsons_d': round(simpsons_d, 6),
            'assessment': self._assess_advanced_diversity(
                hapax_ratio, brunets_w, simpsons_d
            ),
        }

    def analyze_mattr(self, text: str, window_size: int = 100) -> Dict:
        """
        MATTR (Moving Average Type-Token Ratio)

        固定ウィンドウをスライドさせて TTR を計算し、平均を取る。
        テキスト長に依存しない安定した語彙多様性指標。

        Parameters:
            text: 入力テキスト
            window_size: ウィンドウサイズ（トークン数）

        Returns:
            Dict: MATTR 結果と推移データ
        """
        tokens = self._tokenize_simple(text)
        n = len(tokens)

        if n < window_size:
            # ウィンドウより短い場合は通常の TTR
            unique = len(set(tokens))
            ttr = unique / n if n > 0 else 0.0
            return {
                'mattr': round(ttr, 4),
                'window_size': window_size,
                'num_windows': 1 if n > 0 else 0,
                'ttr_profile': [ttr] if n > 0 else [],
                'ttr_std': 0.0,
                'ttr_min': ttr,
                'ttr_max': ttr,
            }

        # スライディングウィンドウ
        ttr_values = []
        for i in range(n - window_size + 1):
            window = tokens[i:i + window_size]
            ttr = len(set(window)) / window_size
            ttr_values.append(ttr)

        mattr = float(np.mean(ttr_values))

        return {
            'mattr': round(mattr, 4),
            'window_size': window_size,
            'num_windows': len(ttr_values),
            'ttr_profile': [round(v, 4) for v in ttr_values],
            'ttr_std': round(float(np.std(ttr_values)), 4),
            'ttr_min': round(float(np.min(ttr_values)), 4),
            'ttr_max': round(float(np.max(ttr_values)), 4),
        }

    def analyze_vocabulary_growth(
        self, text: str, num_samples: int = 50
    ) -> Dict:
        """
        語彙成長曲線 (Vocabulary Growth Curve) と Heaps' Law フィッティング

        Heaps' Law: V(n) = K * n^β
        - K: 定数（高いほど初期語彙が豊富）
        - β: 成長率（1に近いほど新語が出続ける。0.4-0.6が典型）

        Parameters:
            text: 入力テキスト
            num_samples: サンプリングポイント数

        Returns:
            Dict: 成長曲線データとフィッティングパラメータ
        """
        tokens = self._tokenize_simple(text)
        n = len(tokens)

        if n < 10:
            return {
                'heaps_K': 0.0,
                'heaps_beta': 0.0,
                'growth_curve': [],
                'assessment': '分析対象が少なすぎます',
            }

        # サンプリングポイントでの語彙数を計測
        sample_points = np.linspace(10, n, min(num_samples, n // 2), dtype=int)
        sample_points = sorted(set(sample_points))

        growth = []
        seen = set()
        token_idx = 0

        for target_n in sample_points:
            while token_idx < target_n and token_idx < n:
                seen.add(tokens[token_idx])
                token_idx += 1
            growth.append({'n': int(target_n), 'V': len(seen)})

        # Heaps' Law フィッティング: log(V) = log(K) + β * log(n)
        if len(growth) >= 3:
            log_n = np.array([math.log(g['n']) for g in growth])
            log_v = np.array([math.log(g['V']) for g in growth if g['V'] > 0])

            if len(log_n) == len(log_v) and len(log_v) >= 2:
                # 最小二乗法
                coeffs = np.polyfit(log_n[:len(log_v)], log_v, 1)
                beta = float(coeffs[0])
                K = float(math.exp(coeffs[1]))
            else:
                beta = 0.0
                K = 0.0
        else:
            beta = 0.0
            K = 0.0

        return {
            'heaps_K': round(K, 4),
            'heaps_beta': round(beta, 4),
            'growth_curve': growth,
            'total_tokens': n,
            'final_vocabulary': len(seen),
            'assessment': self._assess_heaps(beta),
        }

    # ─── Private Helpers ────────────────────────────────────

    def _get_frequency_distribution(self, text: str) -> Counter:
        """テキストから簡易的な語彙頻度分布を取得"""
        tokens = self._tokenize_simple(text)
        return Counter(tokens)

    def _tokenize_simple(self, text: str) -> List[str]:
        """
        簡易トークナイザ（2-4文字 N-gram ベース）

        形態素解析不要。文字 N-gram で語彙多様性を近似。
        """
        clean = re.sub(r'\s+', '', text)
        if len(clean) < 2:
            return list(clean)

        tokens = []
        # 2文字と3文字のN-gramを語彙単位として使用
        for n in [2, 3]:
            for i in range(len(clean) - n + 1):
                gram = clean[i:i + n]
                tokens.append(gram)
        return tokens

    @staticmethod
    def _empty_advanced_diversity() -> Dict:
        """分析対象がない場合の高度語彙多様性指標を返す。"""
        return {
            'total_tokens': 0, 'unique_types': 0,
            'hapax_legomena': 0, 'dis_legomena': 0,
            'hapax_ratio': 0.0, 'sichels_s': 0.0,
            'brunets_w': 0.0, 'honores_r': 0.0,
            'simpsons_d': 0.0, 'assessment': '分析対象がありません',
        }

    @staticmethod
    def _assess_advanced_diversity(
        hapax_ratio: float, brunets_w: float, simpsons_d: float
    ) -> str:
        """高度な多様性指標から総合評価"""
        scores = 0
        if hapax_ratio > 0.5:
            scores += 2  # 半数以上が1回語 = 非常に豊か
        elif hapax_ratio > 0.3:
            scores += 1

        if simpsons_d > 0.98:
            scores += 2
        elif simpsons_d > 0.95:
            scores += 1

        if scores >= 3:
            return "語彙が非常に豊か（高度に多様）"
        elif scores >= 2:
            return "語彙が豊か"
        elif scores >= 1:
            return "標準的な語彙多様性"
        else:
            return "語彙がやや単調"

    @staticmethod
    def _assess_heaps(beta: float) -> str:
        """Heaps' β から語彙成長を評価"""
        if beta <= 0:
            return "フィッティング不可"
        elif beta > 0.7:
            return "新語が非常に多い（語彙が拡散的）"
        elif beta > 0.5:
            return "新語の出現が活発"
        elif beta > 0.3:
            return "標準的な語彙成長"
        else:
            return "語彙が収束している（繰り返しが多い）"

    # ─── analyze_full 拡張 ──────────────────────────────────

    def analyze_full(self, text: str) -> Dict:
        """
        テキストの全体的な文体分析を実施（強化版）

        Parameters:
            text (str): 入力テキスト

        Returns:
            Dict: 包括的な文体分析結果
        """
        return {
            'vocabulary': self.analyze_vocabulary_richness(text),
            'advanced_diversity': self.analyze_advanced_diversity(text),
            'mattr': self.analyze_mattr(text),
            'vocabulary_growth': self.analyze_vocabulary_growth(text),
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
    文体分析の簡潔なサマリーを返す（強化版）

    Parameters:
        text (str): 入力テキスト

    Returns:
        str: フォーマットされたサマリー
    """
    analyzer = StylometryAnalyzer()
    result = analyzer.analyze_full(text)

    summary = []
    summary.append("=" * 60)
    summary.append("【文体分析結果（強化版）】")
    summary.append("=" * 60)

    # 基本語彙多様性
    vocab = result.get('vocabulary', {})
    summary.append("\n【基本語彙多様性】")
    summary.append(f"  TTR: {vocab.get('ttr', 0):.4f}")
    summary.append(f"  Yule's K: {vocab.get('yules_k', 0)}")
    summary.append(f"  評価: {vocab.get('assessment', 'N/A')}")

    # 高度な語彙多様性
    adv = result.get('advanced_diversity', {})
    summary.append("\n【高度な語彙多様性】")
    summary.append(f"  Hapax Legomena: {adv.get('hapax_legomena', 0)} ({adv.get('hapax_ratio', 0):.1%})")
    summary.append(f"  Sichel's S:    {adv.get('sichels_s', 0):.4f}")
    summary.append(f"  Brunet's W:    {adv.get('brunets_w', 0):.4f}")
    summary.append(f"  Honoré's R:    {adv.get('honores_r', 0):.2f}")
    summary.append(f"  Simpson's D:   {adv.get('simpsons_d', 0):.6f}")
    summary.append(f"  評価: {adv.get('assessment', 'N/A')}")

    # MATTR
    mattr = result.get('mattr', {})
    summary.append("\n【MATTR (Moving Average TTR)】")
    summary.append(f"  MATTR: {mattr.get('mattr', 0):.4f}")
    summary.append(f"  TTR変動(σ): {mattr.get('ttr_std', 0):.4f}")
    summary.append(f"  TTR範囲: {mattr.get('ttr_min', 0):.4f} - {mattr.get('ttr_max', 0):.4f}")

    # 語彙成長
    growth = result.get('vocabulary_growth', {})
    summary.append("\n【語彙成長曲線 (Heaps' Law)】")
    summary.append(f"  K (初期語彙量): {growth.get('heaps_K', 0):.4f}")
    summary.append(f"  β (成長率):     {growth.get('heaps_beta', 0):.4f}")
    summary.append(f"  評価: {growth.get('assessment', 'N/A')}")

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
    summary.append(f"  標準偏差: {sent.get('std_length', 0):.1f}字")
    summary.append(f"  文の数: {sent.get('sentence_count', 0)}")

    summary.append("\n" + "=" * 60)
    return "\n".join(summary)
