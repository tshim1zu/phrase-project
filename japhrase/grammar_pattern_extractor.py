"""
文法パターンベースのフレーズ抽出モジュール

正規表現による文法パターン検出とPMI分析を統合し、
「〜だった」「〜していた」などの文法的フレーズを抽出します。
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023"

import re
import numpy as np
import pandas as pd
from collections import Counter, defaultdict
from typing import Dict, List, Optional, Tuple, Union
import logging

logger = logging.getLogger(__name__)


# デフォルトの文法パターン定義
DEFAULT_GRAMMAR_PATTERNS = {
    # 過去形パターン
    'past_datta': r'(\S{1,15}だった)',
    'past_ta': r'(\S{1,15}[^い]た)',

    # 進行形パターン
    'progressive_teita': r'(\S{1,15}ていた)',
    'progressive_teiru': r'(\S{1,15}ている)',

    # 完了形パターン
    'completed_ta': r'(\S{1,15}してしまった)',
    'completed_chatta': r'(\S{1,15}ちゃった)',

    # 存在・状態パターン
    'existence_dearu': r'(\S{1,15}である)',
    'existence_desu': r'(\S{1,15}です)',

    # 意志・推量パターン
    'will_darou': r'(\S{1,15}だろう)',
    'will_deshou': r'(\S{1,15}でしょう)',

    # 否定パターン
    'negative_nai': r'(\S{1,15}ない)',
    'negative_nakatta': r'(\S{1,15}なかった)',

    # 可能・受身パターン
    'passive_reru': r'(\S{1,15}[らりれろ]れる)',
    'potential_eru': r'(\S{1,15}[えける]る)',

    # 使役パターン
    'causative_seru': r'(\S{1,15}[させせ]る)',
}


class GrammarPatternExtractor:
    """
    文法パターンベースのフレーズ抽出クラス

    正規表現で文法パターンを検出し、PMI（自己相互情報量）や頻度分析を
    組み合わせて、テキスト中の重要な文法的フレーズを抽出します。

    特徴:
        - 正規表現による柔軟なパターンマッチング
        - PMI分析による意味的結合度の評価
        - 低PMI/高PMI/頻度の複合評価
        - カスタマイズ可能なパターン定義

    使用例:
        >>> extractor = GrammarPatternExtractor()
        >>> df = extractor.extract("text.txt")
        >>> print(df[df['pattern'] == 'past_datta'])
    """

    def __init__(
        self,
        patterns: Optional[Dict[str, str]] = None,
        use_pmi: bool = True,
        use_frequency: bool = True,
        min_count: int = 3,
        window_size: int = 2,
        verbose: bool = True,
    ):
        """
        Parameters:
            patterns (dict): 文法パターンの辞書 {パターン名: 正規表現}
            use_pmi (bool): PMI（自己相互情報量）を計算するか
            use_frequency (bool): 頻度情報を含めるか
            min_count (int): 最小出現回数
            window_size (int): PMI計算時のウィンドウサイズ
            verbose (bool): 進捗を表示するか
        """
        self.patterns = patterns if patterns is not None else DEFAULT_GRAMMAR_PATTERNS
        self.use_pmi = use_pmi
        self.use_frequency = use_frequency
        self.min_count = min_count
        self.window_size = window_size
        self.verbose = verbose

        # 正規表現をコンパイル
        self.compiled_patterns = {
            name: re.compile(pattern)
            for name, pattern in self.patterns.items()
        }

    def extract(
        self,
        text: Union[str, List[str]],
        pattern_names: Optional[List[str]] = None,
    ) -> pd.DataFrame:
        """
        文法パターンに基づいてフレーズを抽出

        Parameters:
            text (str or list): 入力テキストまたは文のリスト
            pattern_names (list): 使用するパターン名のリスト（Noneなら全パターン）

        Returns:
            DataFrame: 抽出されたフレーズと統計情報
                - phrase: 抽出されたフレーズ
                - pattern: パターン名
                - count: 出現回数
                - pmi: PMI値（use_pmi=Trueの場合）
                - score: 総合スコア
        """
        # テキストの前処理
        if isinstance(text, str):
            # 改行で分割
            sentences = text.split('\n')
        else:
            sentences = text

        full_text = '\n'.join(sentences)

        # パターン名のフィルタリング
        if pattern_names is not None:
            patterns_to_use = {
                name: self.compiled_patterns[name]
                for name in pattern_names
                if name in self.compiled_patterns
            }
        else:
            patterns_to_use = self.compiled_patterns

        # フレーズ抽出
        phrase_matches = []
        for pattern_name, pattern in patterns_to_use.items():
            matches = pattern.findall(full_text)
            for match in matches:
                phrase_matches.append({
                    'phrase': match,
                    'pattern': pattern_name,
                })

        if not phrase_matches:
            if self.verbose:
                logger.warning("パターンにマッチするフレーズが見つかりませんでした")
            return pd.DataFrame(columns=['phrase', 'pattern', 'count', 'pmi', 'score'])

        # DataFrameに変換
        df = pd.DataFrame(phrase_matches)

        # 頻度カウント
        phrase_counts = Counter(df['phrase'])
        df['count'] = df['phrase'].map(phrase_counts)

        # 最小出現回数でフィルタリング
        df = df[df['count'] >= self.min_count].copy()

        if df.empty:
            if self.verbose:
                logger.warning(f"min_count={self.min_count}以上のフレーズが見つかりませんでした")
            return df

        # PMI計算
        if self.use_pmi:
            df['pmi'] = df['phrase'].apply(
                lambda p: self._calculate_pmi(p, full_text)
            )
        else:
            df['pmi'] = 0.0

        # スコア計算（頻度とPMIの重み付き平均）
        df['score'] = self._calculate_score(df)

        # 重複削除してソート
        df = df.drop_duplicates(subset=['phrase', 'pattern'])
        df = df.sort_values('score', ascending=False).reset_index(drop=True)

        return df

    def extract_low_pmi(
        self,
        text: Union[str, List[str]],
        max_pmi: float = 2.0,
        pattern_names: Optional[List[str]] = None,
    ) -> pd.DataFrame:
        """
        低PMIフレーズを抽出

        文法的には固定されているが、意味的結合が弱いフレーズを検出します。
        例: 「〜である」「〜だった」など

        Parameters:
            text (str or list): 入力テキスト
            max_pmi (float): PMIの最大値
            pattern_names (list): 使用するパターン名

        Returns:
            DataFrame: 低PMIフレーズのリスト
        """
        df = self.extract(text, pattern_names=pattern_names)

        if df.empty or not self.use_pmi:
            return df

        return df[df['pmi'] <= max_pmi].copy()

    def extract_high_pmi(
        self,
        text: Union[str, List[str]],
        min_pmi: float = 5.0,
        pattern_names: Optional[List[str]] = None,
    ) -> pd.DataFrame:
        """
        高PMIフレーズを抽出

        意味的に強く結合している文法パターンを検出します。
        例: 「走っていた」「考えていた」など

        Parameters:
            text (str or list): 入力テキスト
            min_pmi (float): PMIの最小値
            pattern_names (list): 使用するパターン名

        Returns:
            DataFrame: 高PMIフレーズのリスト
        """
        df = self.extract(text, pattern_names=pattern_names)

        if df.empty or not self.use_pmi:
            return df

        return df[df['pmi'] >= min_pmi].copy()

    def rank_by_pmi(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        PMIベースでランキング

        Parameters:
            df (DataFrame): extract()の結果

        Returns:
            DataFrame: PMI順にソートされたDataFrame
        """
        if df.empty or 'pmi' not in df.columns:
            return df

        return df.sort_values('pmi', ascending=False).reset_index(drop=True)

    def rank_by_frequency(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        頻度ベースでランキング

        Parameters:
            df (DataFrame): extract()の結果

        Returns:
            DataFrame: 頻度順にソートされたDataFrame
        """
        if df.empty or 'count' not in df.columns:
            return df

        return df.sort_values('count', ascending=False).reset_index(drop=True)

    def group_by_pattern(self, df: pd.DataFrame) -> Dict[str, pd.DataFrame]:
        """
        パターン別にグループ化

        Parameters:
            df (DataFrame): extract()の結果

        Returns:
            dict: {パターン名: DataFrame} の辞書
        """
        if df.empty:
            return {}

        return {
            pattern: group.reset_index(drop=True)
            for pattern, group in df.groupby('pattern')
        }

    def _calculate_pmi(self, phrase: str, full_text: str) -> float:
        """
        PMI（自己相互情報量）を計算

        PMI(x,y) = log2(P(x,y) / (P(x) * P(y)))

        ここでは、フレーズを前半と後半に分割してPMIを計算します。
        """
        if len(phrase) < 2:
            return 0.0

        # フレーズを分割（中間点で分ける）
        mid = len(phrase) // 2
        part1 = phrase[:mid]
        part2 = phrase[mid:]

        # 全体のトークン数
        total_chars = len(full_text)

        if total_chars == 0:
            return 0.0

        # 各部分の出現回数
        count_phrase = full_text.count(phrase)
        count_part1 = full_text.count(part1)
        count_part2 = full_text.count(part2)

        # 確率計算
        p_phrase = count_phrase / total_chars
        p_part1 = count_part1 / total_chars
        p_part2 = count_part2 / total_chars

        # PMI計算
        if p_part1 > 0 and p_part2 > 0 and p_phrase > 0:
            pmi = np.log2(p_phrase / (p_part1 * p_part2))
            return max(0.0, pmi)  # 負のPMIは0にする

        return 0.0

    def _calculate_score(self, df: pd.DataFrame) -> pd.Series:
        """
        総合スコアを計算（頻度とPMIの組み合わせ）
        """
        # 正規化された頻度
        if 'count' in df.columns and df['count'].max() > 0:
            norm_freq = df['count'] / df['count'].max()
        else:
            norm_freq = 0.0

        # 正規化されたPMI
        if self.use_pmi and 'pmi' in df.columns and df['pmi'].max() > 0:
            norm_pmi = df['pmi'] / df['pmi'].max()
        else:
            norm_pmi = 0.0

        # 重み付き平均
        freq_weight = 0.6 if self.use_frequency else 0.0
        pmi_weight = 0.4 if self.use_pmi else 0.0

        total_weight = freq_weight + pmi_weight
        if total_weight == 0:
            return pd.Series([1.0] * len(df), index=df.index)

        score = (norm_freq * freq_weight + norm_pmi * pmi_weight) / total_weight
        return score

    @classmethod
    def from_file(
        cls,
        filepath: str,
        encoding: str = 'utf-8',
        **kwargs
    ) -> pd.DataFrame:
        """
        ファイルから文法パターンを抽出

        Parameters:
            filepath (str): ファイルパス
            encoding (str): エンコーディング
            **kwargs: GrammarPatternExtractor.__init__()のパラメータ

        Returns:
            DataFrame: 抽出結果
        """
        # エンコーディング自動検出
        try:
            with open(filepath, 'r', encoding=encoding) as f:
                text = f.read()
        except UnicodeDecodeError:
            # Shift-JISで再試行
            with open(filepath, 'r', encoding='shift-jis') as f:
                text = f.read()

        extractor = cls(**kwargs)
        return extractor.extract(text)

    def export_csv(self, df: pd.DataFrame, output_path: str):
        """
        結果をCSVにエクスポート

        Parameters:
            df (DataFrame): 抽出結果
            output_path (str): 出力ファイルパス
        """
        df.to_csv(output_path, index=False, encoding='utf-8-sig')
        if self.verbose:
            logger.info(f"結果を保存しました: {output_path}")

    def get_pattern_summary(self, df: pd.DataFrame) -> pd.DataFrame:
        """
        パターン別の統計サマリーを取得

        Parameters:
            df (DataFrame): extract()の結果

        Returns:
            DataFrame: パターン別の統計
                - pattern: パターン名
                - unique_phrases: ユニークなフレーズ数
                - total_count: 総出現回数
                - avg_pmi: 平均PMI
                - max_pmi: 最大PMI
        """
        if df.empty:
            return pd.DataFrame()

        summary = df.groupby('pattern').agg({
            'phrase': 'nunique',
            'count': 'sum',
            'pmi': ['mean', 'max'] if 'pmi' in df.columns else 'count',
        }).reset_index()

        # 列名を整理
        summary.columns = ['pattern', 'unique_phrases', 'total_count', 'avg_pmi', 'max_pmi']
        summary = summary.sort_values('total_count', ascending=False)

        return summary
