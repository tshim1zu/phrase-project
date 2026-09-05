"""
テキスト間の類似度分析モジュール

複数ファイル/テキスト間の類似度を計算し、コピペ検出や重複分析を行います。
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023"

import numpy as np
import pandas as pd
from typing import List, Dict, Tuple, Optional, Union
import logging
from pathlib import Path

logger = logging.getLogger(__name__)


class SimilarityAnalyzer:
    """
    複数テキスト間の類似度を分析するクラス

    複数の類似度計算手法を提供：
    - 'levenshtein': レーベンシュタイン距離（正確だが遅い）
    - 'jaccard': N-gram Jaccard係数（高速）
    - 'cosine': TF-IDFコサイン類似度（長文向け）
    - 'auto': 自動選択（デフォルト、実用性重視）

    使用例:
        >>> analyzer = SimilarityAnalyzer(method='auto')
        >>> matrix = analyzer.compare_files(['doc1.txt', 'doc2.txt', 'doc3.txt'])
        >>> pairs = analyzer.find_similar_pairs(matrix, threshold=0.7)
    """

    def __init__(self, method: str = 'auto', ngram_size: int = 3):
        """
        Parameters:
            method (str): 類似度計算手法
                - 'auto': 自動選択（デフォルト）
                - 'levenshtein': レーベンシュタイン距離
                - 'jaccard': N-gram Jaccard係数
                - 'cosine': TF-IDFコサイン類似度
            ngram_size (int): N-gramのサイズ（jaccard使用時）
        """
        self.method = method
        self.ngram_size = ngram_size
        self._validate_method()

    def _validate_method(self):
        """メソッドの妥当性を検証"""
        valid_methods = ['auto', 'levenshtein', 'jaccard', 'cosine']
        if self.method not in valid_methods:
            raise ValueError(
                f"method は {valid_methods} のいずれかである必要があります。"
                f"指定された値: {self.method}"
            )

    def _select_method(self, text_length: int) -> str:
        """
        テキスト長に応じて最適な手法を自動選択

        Parameters:
            text_length (int): テキストの平均文字数

        Returns:
            str: 選択された手法名
        """
        if self.method != 'auto':
            return self.method

        # 実用性重視の自動選択
        if text_length < 500:
            # 短文: レーベンシュタイン距離（正確）
            return 'levenshtein'
        elif text_length < 5000:
            # 中文: Jaccard係数（速度と精度のバランス）
            return 'jaccard'
        else:
            # 長文: コサイン類似度（高速）
            return 'cosine'

    def similarity_levenshtein(self, text1: str, text2: str) -> float:
        """
        レーベンシュタイン距離ベースの類似度

        Parameters:
            text1 (str): テキスト1
            text2 (str): テキスト2

        Returns:
            float: 類似度（0.0-1.0）
        """
        try:
            import Levenshtein
        except ImportError:
            distance = self._levenshtein_pure_python(text1, text2)
        else:
            distance = Levenshtein.distance(text1, text2)

        max_len = max(len(text1), len(text2))
        if max_len == 0:
            return 1.0

        similarity = 1.0 - (distance / max_len)
        return similarity

    def _levenshtein_pure_python(self, s1: str, s2: str) -> int:
        """
        純粋Pythonでのレーベンシュタイン距離計算（フォールバック用）

        Parameters:
            s1 (str): 文字列1
            s2 (str): 文字列2

        Returns:
            int: レーベンシュタイン距離
        """
        if len(s1) < len(s2):
            return self._levenshtein_pure_python(s2, s1)

        if len(s2) == 0:
            return len(s1)

        previous_row = range(len(s2) + 1)
        for i, c1 in enumerate(s1):
            current_row = [i + 1]
            for j, c2 in enumerate(s2):
                insertions = previous_row[j + 1] + 1
                deletions = current_row[j] + 1
                substitutions = previous_row[j] + (c1 != c2)
                current_row.append(min(insertions, deletions, substitutions))
            previous_row = current_row

        return previous_row[-1]

    def similarity_jaccard(self, text1: str, text2: str) -> float:
        """
        N-gram Jaccard係数ベースの類似度

        Parameters:
            text1 (str): テキスト1
            text2 (str): テキスト2

        Returns:
            float: 類似度（0.0-1.0）
        """
        ngrams1 = self._get_ngrams(text1, self.ngram_size)
        ngrams2 = self._get_ngrams(text2, self.ngram_size)

        if not ngrams1 and not ngrams2:
            return 1.0
        if not ngrams1 or not ngrams2:
            return 0.0

        intersection = len(ngrams1 & ngrams2)
        union = len(ngrams1 | ngrams2)

        return intersection / union if union > 0 else 0.0

    def _get_ngrams(self, text: str, n: int) -> set:
        """
        テキストからN-gramを抽出

        Parameters:
            text (str): テキスト
            n (int): N-gramのサイズ

        Returns:
            set: N-gramの集合
        """
        if len(text) < n:
            return {text} if text else set()

        return {text[i:i+n] for i in range(len(text) - n + 1)}

    def similarity_cosine(self, text1: str, text2: str) -> float:
        """
        TF-IDFコサイン類似度

        Parameters:
            text1 (str): テキスト1
            text2 (str): テキスト2

        Returns:
            float: 類似度（0.0-1.0）
        """
        try:
            from sklearn.feature_extraction.text import TfidfVectorizer
            from sklearn.metrics.pairwise import cosine_similarity

            vectorizer = TfidfVectorizer(analyzer='char', ngram_range=(2, 3))
            vectors = vectorizer.fit_transform([text1, text2])
            similarity = cosine_similarity(vectors[0:1], vectors[1:2])[0][0]

            return float(similarity)

        except ImportError:
            logger.warning(
                "scikit-learn が見つかりません。jaccard法にフォールバックします。"
                "TF-IDF使用には 'pip install scikit-learn' が必要です。"
            )
            return self.similarity_jaccard(text1, text2)

    def calculate_similarity(self, text1: str, text2: str, method: Optional[str] = None) -> float:
        """
        2つのテキスト間の類似度を計算

        Parameters:
            text1 (str): テキスト1
            text2 (str): テキスト2
            method (str, optional): 使用する手法（Noneの場合はインスタンス設定を使用）

        Returns:
            float: 類似度（0.0-1.0）
        """
        if method is None:
            avg_length = (len(text1) + len(text2)) / 2
            method = self._select_method(int(avg_length))

        if method == 'levenshtein':
            return self.similarity_levenshtein(text1, text2)
        elif method == 'jaccard':
            return self.similarity_jaccard(text1, text2)
        elif method == 'cosine':
            return self.similarity_cosine(text1, text2)
        else:
            raise ValueError(f"不明な手法: {method}")

    def compare_texts(
        self,
        texts: List[str],
        labels: Optional[List[str]] = None
    ) -> pd.DataFrame:
        """
        複数テキスト間の類似度行列を計算

        Parameters:
            texts (List[str]): テキストのリスト
            labels (List[str], optional): 各テキストのラベル

        Returns:
            pd.DataFrame: 類似度行列
        """
        n = len(texts)

        if labels is None:
            labels = [f"text_{i+1}" for i in range(n)]

        if len(labels) != n:
            raise ValueError(
                f"labels の数({len(labels)})が texts の数({n})と一致しません"
            )

        # 平均テキスト長を計算して手法を決定
        avg_length = sum(len(t) for t in texts) / n
        selected_method = self._select_method(int(avg_length))

        logger.info(f"類似度計算手法: {selected_method} (平均文字数: {int(avg_length)})")

        # 類似度行列を計算
        matrix = np.zeros((n, n))

        for i in range(n):
            for j in range(i, n):
                if i == j:
                    similarity = 1.0
                else:
                    similarity = self.calculate_similarity(
                        texts[i], texts[j], method=selected_method
                    )

                matrix[i, j] = similarity
                matrix[j, i] = similarity

        # DataFrameに変換
        df_matrix = pd.DataFrame(matrix, index=labels, columns=labels)

        return df_matrix

    def compare_files(
        self,
        filepaths: List[str],
        encoding: str = 'auto'
    ) -> pd.DataFrame:
        """
        複数ファイル間の類似度行列を計算

        Parameters:
            filepaths (List[str]): ファイルパスのリスト
            encoding (str): 文字エンコーディング

        Returns:
            pd.DataFrame: 類似度行列
        """
        from .utils import read_file

        texts = []
        labels = []

        for filepath in filepaths:
            text_lines = read_file(filepath, encoding=encoding)
            text = '\n'.join(text_lines)
            texts.append(text)

            # ファイル名をラベルとして使用
            label = Path(filepath).name
            labels.append(label)

        return self.compare_texts(texts, labels)

    def find_similar_pairs(
        self,
        similarity_matrix: pd.DataFrame,
        threshold: float = 0.7,
        top_n: Optional[int] = None
    ) -> List[Dict[str, Union[str, float]]]:
        """
        類似度の高いペアを抽出

        Parameters:
            similarity_matrix (pd.DataFrame): 類似度行列
            threshold (float): 類似度の閾値（0.0-1.0）
            top_n (int, optional): 上位N件のみ返す

        Returns:
            List[Dict]: 類似ペアのリスト
                [{'item1': 'text_1', 'item2': 'text_2', 'similarity': 0.85}, ...]
        """
        pairs = []
        labels = similarity_matrix.index.tolist()
        n = len(labels)

        for i in range(n):
            for j in range(i + 1, n):
                similarity = similarity_matrix.iloc[i, j]

                if similarity >= threshold:
                    pairs.append({
                        'item1': labels[i],
                        'item2': labels[j],
                        'similarity': float(similarity)
                    })

        # 類似度の降順でソート
        pairs.sort(key=lambda x: x['similarity'], reverse=True)

        if top_n is not None:
            pairs = pairs[:top_n]

        return pairs

    def export_matrix(
        self,
        similarity_matrix: pd.DataFrame,
        filepath: str,
        format: str = 'csv'
    ):
        """
        類似度行列をファイルに出力

        Parameters:
            similarity_matrix (pd.DataFrame): 類似度行列
            filepath (str): 出力先ファイルパス
            format (str): 出力形式（'csv', 'excel', 'json'）
        """
        if format == 'csv':
            similarity_matrix.to_csv(filepath, encoding='utf-8-sig')
        elif format == 'excel':
            similarity_matrix.to_excel(filepath, engine='openpyxl')
        elif format == 'json':
            similarity_matrix.to_json(filepath, orient='index', indent=2, force_ascii=False)
        else:
            raise ValueError(f"未対応の形式: {format}")

        logger.info(f"類似度行列を保存しました: {filepath}")

    def export_heatmap(
        self,
        similarity_matrix: pd.DataFrame,
        filepath: str,
        figsize: Tuple[int, int] = (10, 8),
        cmap: str = 'RdYlGn'
    ):
        """
        類似度行列をヒートマップとして出力

        Parameters:
            similarity_matrix (pd.DataFrame): 類似度行列
            filepath (str): 出力先ファイルパス
            figsize (Tuple[int, int]): 図のサイズ
            cmap (str): カラーマップ
        """
        try:
            import matplotlib.pyplot as plt
            import seaborn as sns

            plt.figure(figsize=figsize)
            sns.heatmap(
                similarity_matrix,
                annot=True,
                fmt='.2f',
                cmap=cmap,
                vmin=0,
                vmax=1,
                square=True,
                cbar_kws={'label': '類似度'}
            )
            plt.title('テキスト類似度行列')
            plt.tight_layout()
            plt.savefig(filepath, dpi=300, bbox_inches='tight')
            plt.close()

            logger.info(f"ヒートマップを保存しました: {filepath}")

        except ImportError as e:
            logger.error(
                f"ヒートマップ出力にはmatplotlibとseabornが必要です: {e}\n"
                "インストール: pip install matplotlib seaborn"
            )
            raise
