"""
テキストセグメンテーション モジュール

分岐エントロピー (Branching Entropy) を活用した教師なしテキスト分割機能。

このモジュールは、統計的な観点から「意味の切れ目」を自動検出し、
テキストを複数のセグメント（チャンク）に分割します。

使用例:
    >>> from japhrase import TextSegmenter
    >>> segmenter = TextSegmenter()
    >>> segments = segmenter.split_by_threshold(text, threshold=0.5)
    >>> chunks = segmenter.split_top_n(text, n=3)

応用例:
    - 句読点なしテキストの「。」挿入
    - RAG（検索拡張生成）用チャンク分割
    - 読みやすさ向上のための改行挿入
    - 音声認識テキストの段落復元
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023-2026"

import numpy as np
from collections import Counter
from typing import List, Dict, Tuple, Optional
import logging

logger = logging.getLogger(__name__)


class TextSegmenter:
    """
    分岐エントロピーに基づくテキストセグメンテーション
    
    テキストを統計的な切れ目で複数のセグメントに分割します。
    
    Attributes:
        window_size (int): エントロピー計算時の左側コンテキスト長
        max_entropy (float): 正規化用の最大エントロピー値
    """
    
    def __init__(self, window_size: int = 4):
        """
        Parameters:
            window_size (int): エントロピー計算時の左側コンテキスト長（デフォルト: 4）
                             この文字数を「直前フレーズ」として見なします
        """
        self.window_size = window_size
        self.max_entropy = np.log(256)  # 最大256個の異なる文字
        
    def calculate_right_entropy_profile(self, text: str) -> Tuple[np.ndarray, List[str]]:
        """
        テキスト全体の「右側分岐エントロピー」プロファイルを計算（最適化版）

        各位置で、直前のフレーズ（window_size分）の直後に来る文字の
        分岐エントロピーを計算します。

        最適化: 辞書ベースのアルゴリズムで計算量を O(n²) → O(n×w) に削減
        - STEP 1: コンテキスト辞書を1回のスキャンで構築 O(n)
        - STEP 2: 各位置でエントロピーを辞書参照で計算 O(n)
        - (w = window_size は通常4なので実質 O(n))

        Parameters:
            text (str): 入力テキスト

        Returns:
            (entropy_profile, contexts):
                - entropy_profile: 各位置のエントロピー値 (numpy.ndarray)
                - contexts: 各位置での直前フレーズ (List[str])
        """
        if not text or len(text) < 2:
            return np.array([]), []

        # STEP 1: コンテキスト辞書を構築 O(n×w)
        # context_map[context] = [next_char1, next_char2, ...]
        context_map = {}

        for i in range(len(text)):
            # 複数のコンテキスト長を考慮（1～window_size文字）
            for context_len in range(1, min(self.window_size + 1, i + 1)):
                context = text[i - context_len:i]
                if i < len(text):
                    next_char = text[i]
                    if context not in context_map:
                        context_map[context] = []
                    context_map[context].append(next_char)

        # STEP 2: エントロピープロファイルを計算 O(n)
        entropy_profile = []
        contexts = []

        for i in range(len(text)):
            # 直前のフレーズを抽出
            start_idx = max(0, i - self.window_size)
            context = text[start_idx:i]
            contexts.append(context)

            # コンテキストに対応する後続文字を辞書から取得 O(1)
            right_chars = context_map.get(context, [])

            # エントロピーを計算
            entropy = self._calculate_entropy(right_chars)
            entropy_profile.append(entropy)

        return np.array(entropy_profile), contexts
    
    @staticmethod
    def _calculate_entropy(characters: List[str]) -> float:
        """
        与えられた文字リストの分岐エントロピーを計算
        
        Parameters:
            characters (List[str]): 文字のリスト
            
        Returns:
            float: 分岐エントロピー値（0.0 - log(256)）
        """
        if not characters:
            return 0.0
        
        counter = Counter(characters)
        total = len(characters)
        entropy = 0.0
        
        for count in counter.values():
            p = count / total
            if p > 0:
                entropy -= p * np.log(p)
        
        return entropy
    
    def find_peaks(self, entropy_profile: np.ndarray, 
                   prominence: float = 0.1) -> List[int]:
        """
        エントロピープロファイルから局所最大値（ピーク）を検出
        
        Parameters:
            entropy_profile (np.ndarray): エントロピープロファイル
            prominence (float): ピーク検出の閾値（0.0-1.0）
                               高いほど顕著なピークのみを検出
        
        Returns:
            List[int]: ピークのインデックスリスト（テキスト位置）
        """
        if len(entropy_profile) < 3:
            return []
        
        peaks = []
        
        for i in range(1, len(entropy_profile) - 1):
            # 両隣より高いかチェック
            if (entropy_profile[i] > entropy_profile[i-1] and 
                entropy_profile[i] > entropy_profile[i+1]):
                # 閾値チェック
                neighborhood_mean = (entropy_profile[i-1] + entropy_profile[i+1]) / 2
                if entropy_profile[i] - neighborhood_mean >= prominence:
                    peaks.append(i)
        
        return peaks
    
    def split_by_threshold(self, text: str, threshold: float = 0.5,
                          min_chunk_length: int = 5) -> List[str]:
        """
        エントロピー閾値に基づいてテキストを分割
        
        エントロピーが閾値を超えた位置すべてで切ります。
        
        Parameters:
            text (str): 入力テキスト
            threshold (float): エントロピー閾値（0.0-1.0）
                              正規化済みの値。高いほど高いエントロピーの場所で切る
            min_chunk_length (int): 最小チャンク長（この以下は切らない）
        
        Returns:
            List[str]: セグメント化されたテキストのリスト
        """
        if not text:
            return []
        
        entropy_profile, _ = self.calculate_right_entropy_profile(text)
        
        if len(entropy_profile) == 0:
            return [text]
        
        # 正規化
        normalized_entropy = entropy_profile / self.max_entropy
        
        # 閾値を超える位置を検出
        cut_points = []
        last_cut = 0
        
        for i, score in enumerate(normalized_entropy):
            if score >= threshold and (i - last_cut) >= min_chunk_length:
                cut_points.append(i)
                last_cut = i
        
        # テキストを分割
        if not cut_points:
            return [text]
        
        segments = []
        prev = 0
        for cut in cut_points:
            segments.append(text[prev:cut])
            prev = cut
        segments.append(text[prev:])
        
        # 空の段を除外
        return [s for s in segments if s.strip()]
    
    def split_top_n(self, text: str, n: int = 3,
                   min_chunk_length: int = 10) -> List[str]:
        """
        エントロピーが高い上位 n 個の位置でテキストを分割
        
        「指定した数に分割したい」という用途に向く分割方式。
        
        Parameters:
            text (str): 入力テキスト
            n (int): 分割数（この数だけ切り目を作る）
            min_chunk_length (int): 最小チャンク長
                                   隣同士の切り目がこれより近いと無視
        
        Returns:
            List[str]: n+1 個のセグメント（n 個の切り目で n+1 個に分割）
        """
        if not text or n <= 0:
            return [text]
        
        entropy_profile, _ = self.calculate_right_entropy_profile(text)
        
        if len(entropy_profile) == 0:
            return [text]
        
        # ピークを検出
        peaks = self.find_peaks(entropy_profile, prominence=0.05)
        
        if not peaks:
            # ピークがない場合は上位 n 個をエントロピー値で選ぶ
            sorted_indices = np.argsort(entropy_profile)[::-1][:n]
            peaks = sorted(sorted_indices.tolist())
        
        # 制約付きで上位 n 個を選定
        cut_points = []
        last_cut = 0
        
        for peak_idx in peaks:
            if len(cut_points) >= n:
                break
            if peak_idx - last_cut >= min_chunk_length:
                cut_points.append(peak_idx)
                last_cut = peak_idx
        
        if not cut_points:
            return [text]
        
        # テキストを分割
        segments = []
        prev = 0
        for cut in sorted(cut_points):
            segments.append(text[prev:cut])
            prev = cut
        segments.append(text[prev:])
        
        return [s for s in segments if s.strip()]
    
    def smart_split(self, text: str, target_chunks: Optional[int] = None,
                   threshold: Optional[float] = None,
                   min_chunk_length: int = 10) -> List[str]:
        """
        スマート分割：目的に応じた最適な分割方式を自動選択
        
        Parameters:
            text (str): 入力テキスト
            target_chunks (int, optional): 目標チャンク数。指定時は top_n 方式
            threshold (float, optional): エントロピー閾値。指定時は threshold 方式
            min_chunk_length (int): 最小チャンク長
        
        Returns:
            List[str]: セグメント化されたテキスト
        """
        if target_chunks is not None:
            return self.split_top_n(text, n=target_chunks-1, 
                                   min_chunk_length=min_chunk_length)
        elif threshold is not None:
            return self.split_by_threshold(text, threshold=threshold,
                                          min_chunk_length=min_chunk_length)
        else:
            # デフォルト：適応的な閾値選択
            entropy_profile, _ = self.calculate_right_entropy_profile(text)
            if len(entropy_profile) == 0:
                return [text]
            normalized = entropy_profile / self.max_entropy
            adaptive_threshold = np.mean(normalized) + np.std(normalized)
            adaptive_threshold = np.clip(adaptive_threshold, 0.1, 0.9)
            return self.split_by_threshold(text, threshold=adaptive_threshold,
                                          min_chunk_length=min_chunk_length)
    
    def add_punctuation(self, text: str, marker: str = "。") -> str:
        """
        テキストに句読点がない場合、統計的に正しい位置に句読点を挿入
        
        句読点復元用途。
        
        Parameters:
            text (str): 句読点なしのテキスト
            marker (str): 挿入するマーカー（デフォルト: "。"）
        
        Returns:
            str: 句読点が挿入されたテキスト
        """
        segments = self.split_by_threshold(text, threshold=0.6, min_chunk_length=3)
        return marker.join(segments)
    
    def analyze_entropy_profile(self, text: str) -> Dict:
        """
        エントロピープロファイルの統計情報を返す
        
        デバッグ・分析用。
        
        Parameters:
            text (str): 入力テキスト
        
        Returns:
            Dict: 統計情報
                {
                    'profile': numpy.ndarray,
                    'mean': float,
                    'std': float,
                    'min': float,
                    'max': float,
                    'peaks': List[int],
                }
        """
        entropy_profile, _ = self.calculate_right_entropy_profile(text)
        
        if len(entropy_profile) == 0:
            return {
                'profile': np.array([]),
                'mean': 0.0,
                'std': 0.0,
                'min': 0.0,
                'max': 0.0,
                'peaks': [],
            }
        
        normalized = entropy_profile / self.max_entropy
        peaks = self.find_peaks(entropy_profile, prominence=0.05)
        
        return {
            'profile': normalized,
            'mean': float(np.mean(normalized)),
            'std': float(np.std(normalized)),
            'min': float(np.min(normalized)),
            'max': float(np.max(normalized)),
            'peaks': peaks,
        }


# 便利関数
def segment_text(text: str, method: str = 'adaptive', **kwargs) -> List[str]:
    """
    単一関数でのテキストセグメンテーション
    
    Parameters:
        text (str): 入力テキスト
        method (str): 分割方式 ('adaptive', 'threshold', 'top_n')
        **kwargs: 各方式のパラメータ
    
    Returns:
        List[str]: セグメント化されたテキスト
    """
    segmenter = TextSegmenter()
    
    if method == 'threshold':
        threshold = kwargs.get('threshold', 0.5)
        return segmenter.split_by_threshold(text, threshold=threshold)
    elif method == 'top_n':
        n = kwargs.get('n', 3)
        return segmenter.split_top_n(text, n=n)
    else:  # adaptive
        return segmenter.smart_split(text)
