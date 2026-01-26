"""
書き癖・手癖検出モジュール

純粋に統計ベース（高頻度×低PMI）で、
著者の書き癖や手癖になっているフレーズを自動検出します。
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023"

import re
import numpy as np
import pandas as pd
from typing import Union, List, Optional
import logging

logger = logging.getLogger(__name__)


class WritingHabitDetector:
    """
    書き癖・手癖検出クラス

    高頻度 × 低PMI のフレーズを統計的に検出し、
    著者の無意識的な書き癖を発見します。

    原理:
        - 高頻度: よく使われている
        - 低PMI: 意味的には重要でない（固定表現や癖）
        → この組み合わせが「書き癖」「手癖」

    例:
        - 文末表現: 「〜だった」「〜である」
        - 接続詞: 「そして」「しかし」
        - 副詞: 「本当に」「とても」
        - 指示語: 「それは」「これは」

    使用例:
        >>> detector = WritingHabitDetector()
        >>> df = detector.detect("your_text.txt")
        >>> print(df.head(10))  # 上位10個の書き癖を表示
    """

    def __init__(
        self,
        min_count: int = 10,
        max_pmi: float = 3.0,
        min_length: int = 2,
        max_length: int = 15,
        top_n: Optional[int] = None,
        verbose: bool = True,
    ):
        """
        Parameters:
            min_count (int): 最小出現回数（これ以上の頻度があるフレーズのみ）
            max_pmi (float): 最大PMI値（これ以下のPMIのフレーズのみ）
            min_length (int): 最小フレーズ長
            max_length (int): 最大フレーズ長
            top_n (int): 上位N個のみ返す（Noneなら全て）
            verbose (bool): 進捗表示
        """
        self.min_count = min_count
        self.max_pmi = max_pmi
        self.min_length = min_length
        self.max_length = max_length
        self.top_n = top_n
        self.verbose = verbose

    def _extract_simple_phrases(self, text: str) -> pd.DataFrame:
        """
        簡易フレーズ抽出（N-gramベース）

        Parameters:
            text (str): 入力テキスト

        Returns:
            DataFrame: 抽出されたフレーズ
        """
        from collections import Counter

        # N-gram抽出（2〜max_lengthまで）
        phrases = []
        for n in range(self.min_length, self.max_length + 1):
            for i in range(len(text) - n + 1):
                phrase = text[i:i+n]
                # 空白や改行を含むものは除外
                if not re.search(r'[\s\n\r\t]', phrase):
                    phrases.append(phrase)

        # 頻度カウント
        phrase_counts = Counter(phrases)

        # DataFrameに変換
        df = pd.DataFrame([
            {'phrase': phrase, 'count': count}
            for phrase, count in phrase_counts.items()
            if count >= self.min_count
        ])

        if df.empty:
            return df

        # PMI計算
        df['pmi'] = df['phrase'].apply(
            lambda p: self._calculate_simple_pmi(p, text)
        )

        return df

    def detect(
        self,
        text: Union[str, List[str]],
        use_simple_extraction: bool = True,
    ) -> pd.DataFrame:
        """
        書き癖を検出

        Parameters:
            text (str or list): 入力テキストまたは文のリスト
            use_simple_extraction (bool): 簡易抽出を使用するか（デフォルトTrue）

        Returns:
            DataFrame: 検出された書き癖
                - phrase: フレーズ
                - count: 出現回数
                - pmi: PMI値
                - habit_score: 書き癖スコア（高いほど強い癖）
                - frequency_rank: 頻度ランク（1が最頻出）
                - pmi_rank: PMIランク（1が最低PMI）
        """
        if isinstance(text, str):
            full_text = text
        else:
            full_text = '\n'.join(text)

        # フレーズ抽出
        if use_simple_extraction:
            # 簡易抽出（N-gramベース）
            df = self._extract_simple_phrases(full_text)
            if 'seqchar' not in df.columns and 'phrase' in df.columns:
                # 列名を統一
                df = df.rename(columns={'count': 'freq'})
        else:
            # PhraseExtracterを使った高度な抽出
            from .extracter import PhraseExtracter

            positive_patterns = {
                "Kana": re.compile('[ァ-ヶー]{2,}'),
                "HAN": re.compile('[一-龠]{2,}'),
                "alpha": re.compile("[a-zA-Z]{2,}"),
                "Gana": re.compile('[ぁ-ゖ]{2,}'),
                "Mixed": re.compile('.{2,}'),
            }

            negative_patterns = {
                'x_start': re.compile("^[ンッー、んっ～。](.*)+"),
                'x_Yen': re.compile(r"^\d+(千|万|億|兆)?円$"),
                'x_Phone': re.compile(r'((\d{2,4}|\(\d{2,4}\))(\s|-)(\d{3,4})(\s|-)(\d{4}))'),
                'x_mail': re.compile(r'[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+'),
                'x_url': re.compile(r'https?://[\w/:%#\$&\?\(\)~\.=\+\-]+'),
                'x_Num': re.compile(r"^[\d,，]+$"),
            }

            extractor = PhraseExtracter(
                min_count=self.min_count,
                min_length=self.min_length,
                max_length=self.max_length,
                use_pmi=True,
                verbose=0,
                positive=positive_patterns,
                negative=negative_patterns,
            )

            sentences = full_text.split('\n')
            df = extractor.get_dfphrase(sentences)
            if 'seqchar' in df.columns:
                df = df.rename(columns={'seqchar': 'phrase', 'freq': 'freq'})

        if df.empty:
            if self.verbose:
                logger.warning("フレーズが抽出されませんでした")
            return pd.DataFrame(columns=['phrase', 'count', 'pmi', 'habit_score'])

        # 列名を統一
        if 'seqchar' in df.columns:
            df = df.rename(columns={'seqchar': 'phrase'})
        if 'freq' in df.columns:
            df = df.rename(columns={'freq': 'count'})

        # PMI列の確認
        if 'pmi' not in df.columns:
            df['pmi'] = df['phrase'].apply(
                lambda p: self._calculate_simple_pmi(p, full_text)
            )

        # 高頻度×低PMIのフィルタリング
        df_habits = df[df['pmi'] <= self.max_pmi].copy()

        if df_habits.empty:
            if self.verbose:
                logger.warning(f"PMI<={self.max_pmi}のフレーズが見つかりませんでした")
            return pd.DataFrame(columns=['phrase', 'count', 'pmi', 'habit_score'])

        # 書き癖スコアの計算
        df_habits['habit_score'] = self._calculate_habit_score(df_habits)

        # ランク付け
        df_habits['frequency_rank'] = df_habits['count'].rank(ascending=False, method='min').astype(int)
        df_habits['pmi_rank'] = df_habits['pmi'].rank(ascending=True, method='min').astype(int)

        # 列の順序を整理
        result_df = df_habits[['phrase', 'count', 'pmi', 'habit_score', 'frequency_rank', 'pmi_rank']]

        # スコアでソート
        result_df = result_df.sort_values('habit_score', ascending=False).reset_index(drop=True)

        # 上位N個のみ
        if self.top_n is not None:
            result_df = result_df.head(self.top_n)

        if self.verbose:
            logger.info(f"{len(result_df)}個の書き癖を検出しました")

        return result_df

    def detect_by_category(
        self,
        text: Union[str, List[str]],
    ) -> dict:
        """
        カテゴリ別に書き癖を検出

        Returns:
            dict: カテゴリ別のDataFrame
                - 'high_frequency': 超高頻度（count > 平均の2倍）
                - 'very_low_pmi': 超低PMI（PMI < 1.0）
                - 'balanced': バランス型（頻度とPMIのバランスが良い）
        """
        df = self.detect(text)

        if df.empty:
            return {
                'high_frequency': pd.DataFrame(),
                'very_low_pmi': pd.DataFrame(),
                'balanced': pd.DataFrame(),
            }

        avg_count = df['count'].mean()

        return {
            'high_frequency': df[df['count'] > avg_count * 2].copy(),
            'very_low_pmi': df[df['pmi'] < 1.0].copy(),
            'balanced': df[
                (df['count'] <= avg_count * 2) &
                (df['pmi'] >= 1.0)
            ].copy(),
        }

    def get_summary(self, df: pd.DataFrame) -> dict:
        """
        検出結果のサマリー統計

        Parameters:
            df (DataFrame): detect()の結果

        Returns:
            dict: サマリー統計
        """
        if df.empty:
            return {}

        return {
            'total_habits': len(df),
            'avg_frequency': df['count'].mean(),
            'max_frequency': df['count'].max(),
            'avg_pmi': df['pmi'].mean(),
            'min_pmi': df['pmi'].min(),
            'top_habit': df.iloc[0]['phrase'] if len(df) > 0 else None,
            'top_habit_score': df.iloc[0]['habit_score'] if len(df) > 0 else None,
        }

    def compare_texts(
        self,
        text1: Union[str, List[str]],
        text2: Union[str, List[str]],
        text1_name: str = "Text1",
        text2_name: str = "Text2",
    ) -> pd.DataFrame:
        """
        2つのテキストの書き癖を比較

        Parameters:
            text1 (str or list): テキスト1
            text2 (str or list): テキスト2
            text1_name (str): テキスト1の名前
            text2_name (str): テキスト2の名前

        Returns:
            DataFrame: 比較結果
                - phrase: フレーズ
                - count_text1: テキスト1での出現回数
                - count_text2: テキスト2での出現回数
                - diff: 差分（text1 - text2）
                - pmi_text1: テキスト1のPMI
                - pmi_text2: テキスト2のPMI
        """
        df1 = self.detect(text1)
        df2 = self.detect(text2)

        # マージ
        merged = pd.merge(
            df1[['phrase', 'count', 'pmi']],
            df2[['phrase', 'count', 'pmi']],
            on='phrase',
            how='outer',
            suffixes=(f'_{text1_name}', f'_{text2_name}')
        ).fillna(0)

        # 差分計算
        merged['diff'] = merged[f'count_{text1_name}'] - merged[f'count_{text2_name}']

        # ソート（差分の絶対値が大きい順）
        merged = merged.reindex(
            merged['diff'].abs().sort_values(ascending=False).index
        ).reset_index(drop=True)

        return merged

    def _calculate_habit_score(self, df: pd.DataFrame) -> pd.Series:
        """
        書き癖スコアを計算

        スコア = 正規化された頻度 × (1 / 正規化されたPMI)

        高頻度 & 低PMI → 高スコア
        """
        # 列名を確認
        count_col = 'count' if 'count' in df.columns else 'freq'

        # 正規化（0-1の範囲に）
        max_count = df[count_col].max()
        max_pmi = df['pmi'].max()

        if max_count == 0 or max_pmi == 0:
            return pd.Series([0.0] * len(df), index=df.index)

        norm_freq = df[count_col] / max_count

        # PMIは逆数を使う（低いほど高スコア）
        # 0除算を避けるため、最小値を0.1にする
        norm_pmi_inv = 1.0 / (df['pmi'] + 0.1)
        norm_pmi_inv = norm_pmi_inv / norm_pmi_inv.max()

        # 重み付き平均（頻度70%、PMI逆数30%）
        score = 0.7 * norm_freq + 0.3 * norm_pmi_inv

        return score

    def _calculate_simple_pmi(self, phrase: str, full_text: str) -> float:
        """
        簡易PMI計算（フォールバック用）
        """
        if len(phrase) < 2:
            return 0.0

        mid = len(phrase) // 2
        part1 = phrase[:mid]
        part2 = phrase[mid:]

        total_chars = len(full_text)
        if total_chars == 0:
            return 0.0

        count_phrase = full_text.count(phrase)
        count_part1 = full_text.count(part1)
        count_part2 = full_text.count(part2)

        p_phrase = count_phrase / total_chars
        p_part1 = count_part1 / total_chars
        p_part2 = count_part2 / total_chars

        if p_part1 > 0 and p_part2 > 0 and p_phrase > 0:
            pmi = np.log2(p_phrase / (p_part1 * p_part2))
            return max(0.0, pmi)

        return 0.0

    @classmethod
    def from_file(
        cls,
        filepath: str,
        encoding: str = 'utf-8',
        **kwargs
    ) -> pd.DataFrame:
        """
        ファイルから書き癖を検出

        Parameters:
            filepath (str): ファイルパス
            encoding (str): エンコーディング
            **kwargs: WritingHabitDetector.__init__()のパラメータ

        Returns:
            DataFrame: 検出結果
        """
        # エンコーディング自動検出
        try:
            with open(filepath, 'r', encoding=encoding) as f:
                text = f.read()
        except UnicodeDecodeError:
            # Shift-JISで再試行
            with open(filepath, 'r', encoding='shift-jis') as f:
                text = f.read()

        detector = cls(**kwargs)
        return detector.detect(text)

    def export_csv(self, df: pd.DataFrame, output_path: str):
        """
        結果をCSVにエクスポート

        Parameters:
            df (DataFrame): 検出結果
            output_path (str): 出力ファイルパス
        """
        df.to_csv(output_path, index=False, encoding='utf-8-sig')
        if self.verbose:
            logger.info(f"結果を保存しました: {output_path}")

    def visualize_top_habits(self, df: pd.DataFrame, top_n: int = 10):
        """
        上位の書き癖を可視化（テキストベース）

        Parameters:
            df (DataFrame): detect()の結果
            top_n (int): 表示する数
        """
        if df.empty:
            print("書き癖が見つかりませんでした")
            return

        print("=" * 60)
        print(f"上位{top_n}個の書き癖")
        print("=" * 60)

        for i, row in df.head(top_n).iterrows():
            print(f"\n{i+1}. 【{row['phrase']}】")
            print(f"   出現回数: {row['count']:>3}回")
            print(f"   PMI:     {row['pmi']:>6.2f}")
            print(f"   癖スコア: {row['habit_score']:>6.2f}")
            print(f"   ランク:   頻度#{row['frequency_rank']} / PMI#{row['pmi_rank']}")

        print("\n" + "=" * 60)
