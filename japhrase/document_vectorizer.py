"""
ドキュメント ベクトル化 モジュール

NMF（Non-negative Matrix Factorization）を使用して複数のドキュメントを
ベクトル化し、差分を分析・可視化します。

特徴:
- PMIフィルタリングで意味的差だけでなく文体の手癖の差も検出
- 複数の可視化方法（ヒートマップ、バーチャート、レーダープロット）
- Python APIおよびCLIインターフェース
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023"

import numpy as np
import pandas as pd
from pathlib import Path
from typing import List, Dict, Tuple, Optional, Union
from collections import Counter
import logging
import pickle
import json

from .vectorization_utils import (
    build_document_term_matrix,
    fit_nmf_model,
    normalize_matrix_by_row,
    get_top_terms_for_topic,
    calculate_distance_matrix
)
from .utils import read_file, detect_encoding

logger = logging.getLogger(__name__)


class VectorizationResult:
    """
    NMFベクトル化結果を保持するコンテナクラス

    Attributes:
        document_topic_matrix (np.ndarray): W行列（n_docs × n_topics）
        topic_term_matrix (np.ndarray): H行列（n_topics × n_features）
        labels (List[str]): ドキュメントラベル
        feature_names (List[str]): 特徴/ターム名
        metadata (Dict): メタデータ（feature_mode, PMI情報など）
        vectorizer: フィッティングされたベクトライザー
        nmf_model: フィッティングされたNMFモデル
    """

    def __init__(
        self,
        document_topic_matrix: np.ndarray,
        topic_term_matrix: np.ndarray,
        labels: List[str],
        feature_names: List[str],
        metadata: Dict,
        vectorizer: any,
        nmf_model: any
    ):
        """
        VectorizationResultを初期化

        Parameters:
            document_topic_matrix (np.ndarray): W行列（n_docs × n_topics）
            topic_term_matrix (np.ndarray): H行列（n_topics × n_features）
            labels (List[str]): ドキュメントラベル
            feature_names (List[str]): 特徴/ターム名
            metadata (Dict): メタデータ
            vectorizer: フィッティングされたベクトライザー
            nmf_model: フィッティングされたNMFモデル
        """
        self.document_topic_matrix = document_topic_matrix
        self.topic_term_matrix = topic_term_matrix
        self.labels = labels
        self.feature_names = feature_names
        self.metadata = metadata
        self.vectorizer = vectorizer
        self.nmf_model = nmf_model

        # 正規化されたドキュメント-トピック行列
        self._normalized_doc_topic = normalize_matrix_by_row(document_topic_matrix)

    def get_document_vector(self, doc_index: int) -> np.ndarray:
        """
        ドキュメントのベクトル を取得

        Parameters:
            doc_index (int): ドキュメントのインデックス

        Returns:
            np.ndarray: ドキュメント-トピック ベクトル（正規化済み）
        """
        if doc_index < 0 or doc_index >= len(self.labels):
            raise IndexError(f"Document index {doc_index} out of range [0, {len(self.labels)})")
        return self._normalized_doc_topic[doc_index]

    def get_topic_terms(self, topic_index: int, n_terms: int = 10) -> List[Tuple[str, float]]:
        """
        トピックの上位ターム を取得

        Parameters:
            topic_index (int): トピックのインデックス
            n_terms (int): 返すターム数

        Returns:
            List[Tuple[str, float]]: (ターム、スコア) のタプルのリスト
        """
        if topic_index < 0 or topic_index >= self.topic_term_matrix.shape[0]:
            raise IndexError(f"Topic index {topic_index} out of range")

        topic_vector = self.topic_term_matrix[topic_index]
        return get_top_terms_for_topic(topic_vector, self.feature_names, n_terms)

    def to_dataframe(self, matrix_type: str = 'document_topic') -> pd.DataFrame:
        """
        行列をDataFrameに変換

        Parameters:
            matrix_type (str): 変換する行列タイプ
                - 'document_topic': ドキュメント-トピック行列（W）
                - 'topic_term': トピック-ターム行列（H）
                - 'normalized': 正規化されたドキュメント-トピック行列

        Returns:
            pd.DataFrame: 行列をDataFrameとして返した結果
        """
        if matrix_type == 'document_topic':
            return pd.DataFrame(
                self.document_topic_matrix,
                index=self.labels,
                columns=[f'topic_{i}' for i in range(self.document_topic_matrix.shape[1])]
            )
        elif matrix_type == 'normalized':
            return pd.DataFrame(
                self._normalized_doc_topic,
                index=self.labels,
                columns=[f'topic_{i}' for i in range(self.document_topic_matrix.shape[1])]
            )
        elif matrix_type == 'topic_term':
            return pd.DataFrame(
                self.topic_term_matrix,
                index=[f'topic_{i}' for i in range(self.topic_term_matrix.shape[0])],
                columns=self.feature_names
            )
        else:
            raise ValueError(f"Unknown matrix type: {matrix_type}")

    def save(self, filepath: Union[str, Path]):
        """
        結果をファイルに保存

        Parameters:
            filepath (Union[str, Path]): 保存先ファイルパス（.pkl拡張子）
        """
        filepath = Path(filepath)
        with open(filepath, 'wb') as f:
            pickle.dump(self, f)
        logger.info(f"VectorizationResult saved to {filepath}")

    @classmethod
    def load(cls, filepath: Union[str, Path]) -> 'VectorizationResult':
        """
        ファイルから結果を読み込む

        Parameters:
            filepath (Union[str, Path]): 読み込むファイルパス

        Returns:
            VectorizationResult: 読み込まれた結果
        """
        filepath = Path(filepath)
        with open(filepath, 'rb') as f:
            result = pickle.load(f)
        logger.info(f"VectorizationResult loaded from {filepath}")
        return result


class DocumentVectorizer:
    """
    複数のドキュメントをNMFでベクトル化し、差分を分析・可視化するクラス

    特徴:
    - PMIフィルタリングで意味的な差だけでなく文体の手癖の差も検出可能
    - 複数の特徴抽出モード（tfidf, low_pmi, high_pmi, hybrid）
    - 複数の可視化方法（ヒートマップ、バーチャート、レーダープロット）

    使用例:
        >>> vectorizer = DocumentVectorizer(n_topics=10, feature_mode='tfidf')
        >>> result = vectorizer.from_files(['doc1.txt', 'doc2.txt', 'doc3.txt'])
        >>> diffs = vectorizer.calculate_differences(result, 0, [1, 2])
        >>> vectorizer.export_difference_bars(diffs, 'differences.png')
    """

    def __init__(
        self,
        n_topics: int = 10,
        feature_mode: str = 'tfidf',
        max_features: int = 1000,
        ngram_range: Tuple[int, int] = (2, 3),
        min_df: int = 1,
        max_df: float = 0.95,
        pmi_threshold: float = 3.0,
        min_count: int = 6,
        nmf_init: str = 'nndsvd',
        nmf_max_iter: int = 1000,
        random_state: int = 42,
        verbose: int = 1
    ):
        """
        DocumentVectorizerを初期化

        Parameters:
            n_topics (int): NMFのトピック数
            feature_mode (str): 特徴抽出モード
                - 'tfidf': 通常のTF-IDF（意味的差を検出）
                - 'low_pmi': PMIが低い語句のみ（手癖・文体差を検出）
                - 'high_pmi': PMIが高い語句のみ（意味のある専門用語の差）
                - 'hybrid': 両方を組み合わせた特徴空間
            max_features (int): 最大特徴数
            ngram_range (Tuple[int, int]): 文字N-gramの範囲（日本語向け）
            min_df (int): ドキュメント最小出現数
            max_df (float): ドキュメント最大出現率
            pmi_threshold (float): PMIフィルタリングの閾値
            min_count (int): 語句の最小出現回数（PMI計算に必要）
            nmf_init (str): NMF初期化方法
            nmf_max_iter (int): NMF最大反復回数
            random_state (int): ランダムシード
            verbose (int): ログレベル
        """
        self.n_topics = n_topics
        self.feature_mode = feature_mode
        self.max_features = max_features
        self.ngram_range = ngram_range
        self.min_df = min_df
        self.max_df = max_df
        self.pmi_threshold = pmi_threshold
        self.min_count = min_count
        self.nmf_init = nmf_init
        self.nmf_max_iter = nmf_max_iter
        self.random_state = random_state
        self.verbose = verbose

        logger.info(
            f"DocumentVectorizer initialized: "
            f"topics={n_topics}, mode={feature_mode}, pmi_threshold={pmi_threshold}"
        )

    def fit_transform(
        self,
        texts: List[str],
        labels: Optional[List[str]] = None
    ) -> VectorizationResult:
        """
        テキストをフィッティング・変換してVectorizationResultを返す

        Parameters:
            texts (List[str]): テキストのリスト
            labels (Optional[List[str]]): ドキュメントラベル

        Returns:
            VectorizationResult: ベクトル化結果
        """
        if len(texts) == 0:
            raise ValueError("texts は空でないリストである必要があります")

        if labels is None:
            labels = [f"doc_{i+1}" for i in range(len(texts))]
        elif len(labels) != len(texts):
            raise ValueError(f"labels の数({len(labels)})が texts の数({len(texts)})と一致しません")

        if self.verbose:
            logger.info(f"Fitting {len(texts)} documents with {self.n_topics} topics")

        # ステップ1: ドキュメント-ターム行列を構築
        matrix, vectorizer, feature_names, metadata = build_document_term_matrix(
            texts,
            feature_mode=self.feature_mode,
            pmi_threshold=self.pmi_threshold,
            min_count=self.min_count,
            max_features=self.max_features,
            ngram_range=self.ngram_range,
            min_df=self.min_df,
            max_df=self.max_df,
            analyzer='char',
            verbose=self.verbose
        )

        # ステップ2: NMFモデルをフィッティング
        nmf_model, W, H = fit_nmf_model(
            matrix,
            n_topics=self.n_topics,
            nmf_init=self.nmf_init,
            nmf_max_iter=self.nmf_max_iter,
            random_state=self.random_state,
            verbose=self.verbose
        )

        # ステップ3: VectorizationResultを構築
        result = VectorizationResult(
            document_topic_matrix=W,
            topic_term_matrix=H,
            labels=labels,
            feature_names=feature_names,
            metadata=metadata,
            vectorizer=vectorizer,
            nmf_model=nmf_model
        )

        if self.verbose:
            logger.info("fit_transform complete")

        return result

    def from_files(
        self,
        filepaths: List[str],
        encoding: str = 'auto',
    ) -> VectorizationResult:
        """
        ファイルからドキュメントを読み込んでベクトル化

        このインスタンスのコンストラクタ引数（n_topics, feature_mode,
        pmi_threshold, min_count 等）がそのまま使われる。

        Parameters:
            filepaths (List[str]): ファイルパスのリスト
            encoding (str): ファイルエンコーディング（'auto'で自動検出）

        Returns:
            VectorizationResult: ベクトル化結果
        """
        texts = []
        for filepath in filepaths:
            text_lines = read_file(filepath, encoding=encoding)
            texts.append('\n'.join(text_lines))

        labels = self._make_unique_labels(filepaths)

        return self.fit_transform(texts, labels)

    def from_texts(
        self,
        texts: List[str],
        labels: Optional[List[str]] = None,
    ) -> VectorizationResult:
        """
        テキストのリストからベクトル化

        このインスタンスのコンストラクタ引数（n_topics, feature_mode,
        pmi_threshold, min_count 等）がそのまま使われる。

        Parameters:
            texts (List[str]): テキストのリスト
            labels (Optional[List[str]]): ドキュメントラベル

        Returns:
            VectorizationResult: ベクトル化結果
        """
        return self.fit_transform(texts, labels)

    @staticmethod
    def _make_unique_labels(filepaths: List[str]) -> List[str]:
        """ファイルパスから一意なラベルを作る

        通常はファイル名（basename）を使うが、同名ファイルが複数の
        ディレクトリから渡された場合は与えられたパスそのものを使い、
        それでも衝突する場合は連番を付けて区別する。
        """
        basenames = [Path(fp).name for fp in filepaths]
        basename_counts = Counter(basenames)

        labels = []
        seen_counts: Dict[str, int] = {}
        for filepath, basename in zip(filepaths, basenames):
            label = basename if basename_counts[basename] == 1 else str(Path(filepath))
            seen_counts[label] = seen_counts.get(label, 0) + 1
            if seen_counts[label] > 1:
                label = f"{label} ({seen_counts[label]})"
            labels.append(label)
        return labels

    def calculate_differences(
        self,
        result: VectorizationResult,
        reference_doc_index: int,
        comparison_doc_indices: List[int]
    ) -> pd.DataFrame:
        """
        参照ドキュメントと比較ドキュメント間のトピック差を計算

        Parameters:
            result (VectorizationResult): ベクトル化結果
            reference_doc_index (int): 参照ドキュメントのインデックス
            comparison_doc_indices (List[int]): 比較ドキュメントのインデックスリスト

        Returns:
            pd.DataFrame: 差分行列
                - 行: トピック
                - 列: 比較ドキュメント
                - 値: 参照ドキュメントからの差分（正=より顕著, 負=より低い）
        """
        if reference_doc_index < 0 or reference_doc_index >= len(result.labels):
            raise IndexError(f"Reference index {reference_doc_index} out of range")

        ref_vector = result.get_document_vector(reference_doc_index)
        n_topics = result.document_topic_matrix.shape[1]

        # 差分行列を構築
        differences = {}
        for comp_idx in comparison_doc_indices:
            if comp_idx < 0 or comp_idx >= len(result.labels):
                raise IndexError(f"Comparison index {comp_idx} out of range")

            comp_vector = result.get_document_vector(comp_idx)
            diff = comp_vector - ref_vector
            differences[result.labels[comp_idx]] = diff

        df_diffs = pd.DataFrame(
            differences,
            index=[f'topic_{i}' for i in range(n_topics)]
        )

        return df_diffs

    def calculate_pairwise_distances(
        self,
        result: VectorizationResult,
        metric: str = 'cosine'
    ) -> pd.DataFrame:
        """
        ドキュメント間のペアワイズ距離を計算

        Parameters:
            result (VectorizationResult): ベクトル化結果
            metric (str): 距離計算方法（'cosine', 'euclidean', 'manhattan'）

        Returns:
            pd.DataFrame: 距離行列
        """
        distances = calculate_distance_matrix(
            result._normalized_doc_topic,
            metric=metric
        )

        df_distances = pd.DataFrame(
            distances,
            index=result.labels,
            columns=result.labels
        )

        return df_distances

    def export_heatmap(
        self,
        result: VectorizationResult,
        filepath: Union[str, Path],
        figsize: Tuple[int, int] = (12, 8),
        cmap: str = 'YlOrRd'
    ):
        """
        ドキュメント-トピック行列をヒートマップとして出力

        Parameters:
            result (VectorizationResult): ベクトル化結果
            filepath (Union[str, Path]): 出力先ファイルパス
            figsize (Tuple[int, int]): 図のサイズ
            cmap (str): カラーマップ
        """
        try:
            import matplotlib.pyplot as plt
            import seaborn as sns

            df = result.to_dataframe(matrix_type='normalized')

            plt.figure(figsize=figsize)
            sns.heatmap(
                df,
                annot=True,
                fmt='.2f',
                cmap=cmap,
                cbar_kws={'label': 'トピック寄与度'},
                linewidths=0.5
            )
            plt.title(f'ドキュメント-トピック行列 (mode={self.feature_mode})')
            plt.xlabel('トピック')
            plt.ylabel('ドキュメント')
            plt.tight_layout()
            plt.savefig(filepath, dpi=300, bbox_inches='tight')
            plt.close()

            logger.info(f"Heatmap saved to {filepath}")

        except ImportError as e:
            logger.error(f"Matplotlib/seaborn required: {e}")
            raise

    def export_difference_heatmap(
        self,
        differences: pd.DataFrame,
        filepath: Union[str, Path],
        figsize: Tuple[int, int] = (12, 8),
        cmap: str = 'RdBu_r'
    ):
        """
        差分ベクトルをヒートマップとして出力（発散カラーマップ）

        Parameters:
            differences (pd.DataFrame): 差分行列
            filepath (Union[str, Path]): 出力先ファイルパス
            figsize (Tuple[int, int]): 図のサイズ
            cmap (str): 発散カラーマップ
        """
        try:
            import matplotlib.pyplot as plt
            import seaborn as sns

            # 差分の絶対値の最大値で対称的にスケーリング
            vmax = abs(differences.values).max()
            vmin = -vmax

            plt.figure(figsize=figsize)
            sns.heatmap(
                differences,
                annot=True,
                fmt='.2f',
                cmap=cmap,
                center=0,
                vmin=vmin,
                vmax=vmax,
                cbar_kws={'label': 'トピック差分'},
                linewidths=0.5
            )
            plt.title('ドキュメント間のトピック差分（赤=上側, 青=参照側）')
            plt.xlabel('比較ドキュメント')
            plt.ylabel('トピック')
            plt.tight_layout()
            plt.savefig(filepath, dpi=300, bbox_inches='tight')
            plt.close()

            logger.info(f"Difference heatmap saved to {filepath}")

        except ImportError as e:
            logger.error(f"Matplotlib/seaborn required: {e}")
            raise

    def export_difference_bars(
        self,
        differences: pd.DataFrame,
        filepath: Union[str, Path],
        top_n_topics: Optional[int] = None,
        figsize: Optional[Tuple[int, int]] = None
    ):
        """
        差分をグループ化バーチャートとして出力

        Parameters:
            differences (pd.DataFrame): 差分行列
            filepath (Union[str, Path]): 出力先ファイルパス
            top_n_topics (Optional[int]): 最も差が大きい上位Nトピックのみ表示
            figsize (Optional[Tuple[int, int]]): 図のサイズ（自動計算）
        """
        try:
            import matplotlib.pyplot as plt

            df = differences.copy()

            # 上位トピックを選択
            if top_n_topics is not None:
                # 全体の絶対差分の最大値を計算
                max_diff = df.abs().max(axis=1)
                top_topics = max_diff.nlargest(top_n_topics).index
                df = df.loc[top_topics]

            # 図のサイズを計算
            if figsize is None:
                n_topics = len(df)
                figsize = (max(12, len(df.columns) * 2), max(6, n_topics * 0.5))

            fig, ax = plt.subplots(figsize=figsize)

            # グループバーを作成
            df.plot(kind='barh', ax=ax, width=0.8)

            ax.set_xlabel('トピック差分')
            ax.set_ylabel('トピック')
            ax.set_title('ドキュメント間のトピック差分')
            ax.axvline(x=0, color='black', linestyle='-', linewidth=0.8)
            ax.legend(title='ドキュメント', bbox_to_anchor=(1.05, 1), loc='upper left')
            plt.tight_layout()
            plt.savefig(filepath, dpi=300, bbox_inches='tight')
            plt.close()

            logger.info(f"Difference bar chart saved to {filepath}")

        except ImportError as e:
            logger.error(f"Matplotlib required: {e}")
            raise

    def export_radar_plot(
        self,
        result: VectorizationResult,
        doc_indices: List[int],
        filepath: Union[str, Path],
        figsize: Tuple[int, int] = (10, 10)
    ):
        """
        複数ドキュメントをレーダープロットで比較

        Parameters:
            result (VectorizationResult): ベクトル化結果
            doc_indices (List[int]): ドキュメントのインデックスリスト（2～5個推奨）
            filepath (Union[str, Path]): 出力先ファイルパス
            figsize (Tuple[int, int]): 図のサイズ
        """
        try:
            import matplotlib.pyplot as plt
            from math import pi

            if len(doc_indices) < 2 or len(doc_indices) > 5:
                logger.warning(f"Radar plot works best with 2-5 documents, got {len(doc_indices)}")

            # トピック数を取得
            n_topics = result.document_topic_matrix.shape[1]
            angles = [2 * pi * i / n_topics for i in range(n_topics)]
            angles += angles[:1]  # 円を閉じる

            fig, ax = plt.subplots(figsize=figsize, subplot_kw=dict(projection='polar'))

            colors = plt.cm.Set1(np.linspace(0, 1, len(doc_indices)))

            for color_idx, doc_idx in enumerate(doc_indices):
                vector = result.get_document_vector(doc_idx)
                values = vector.tolist()
                values += values[:1]  # 円を閉じる

                ax.plot(angles, values, 'o-', linewidth=2, label=result.labels[doc_idx], color=colors[color_idx])
                ax.fill(angles, values, alpha=0.25, color=colors[color_idx])

            ax.set_xticks(angles[:-1])
            ax.set_xticklabels([f'T{i}' for i in range(n_topics)])
            ax.set_ylim(0, 1)
            ax.set_title(f'ドキュメント比較（{len(doc_indices)}件）', pad=20)
            ax.legend(loc='upper right', bbox_to_anchor=(1.3, 1.1))
            ax.grid(True)

            plt.tight_layout()
            plt.savefig(filepath, dpi=300, bbox_inches='tight')
            plt.close()

            logger.info(f"Radar plot saved to {filepath}")

        except ImportError as e:
            logger.error(f"Matplotlib required: {e}")
            raise

    def export_matrix(
        self,
        result: VectorizationResult,
        filepath: Union[str, Path],
        format: str = 'csv',
        matrix_type: str = 'document_topic'
    ):
        """
        行列をファイルに出力

        Parameters:
            result (VectorizationResult): ベクトル化結果
            filepath (Union[str, Path]): 出力先ファイルパス
            format (str): 出力形式（'csv', 'excel', 'json'）
            matrix_type (str): 行列タイプ（'document_topic', 'normalized', 'topic_term'）
        """
        df = result.to_dataframe(matrix_type=matrix_type)

        if format == 'csv':
            df.to_csv(filepath, encoding='utf-8-sig')
        elif format == 'excel':
            df.to_excel(filepath, engine='openpyxl')
        elif format == 'json':
            df.to_json(filepath, orient='index', indent=2, force_ascii=False)
        else:
            raise ValueError(f"Unknown format: {format}")

        logger.info(f"Matrix exported to {filepath} (format={format}, type={matrix_type})")

    def format_differences_as_text(
        self,
        differences: pd.DataFrame,
        top_n_topics: Optional[int] = None
    ) -> str:
        """
        差分データをテキスト形式でフォーマット

        Parameters:
            differences (pd.DataFrame): 差分行列
            top_n_topics (Optional[int]): 表示する上位トピック数

        Returns:
            str: フォーマットされたテキスト
        """
        lines = []
        lines.append("=" * 80)
        lines.append("📊 ドキュメント間のトピック差分分析")
        lines.append("=" * 80)

        # 全体統計
        lines.append("\n【全体統計】")
        for col in differences.columns:
            abs_diff = differences[col].abs()
            max_diff = abs_diff.max()
            mean_diff = abs_diff.mean()
            max_topic = abs_diff.idxmax()
            lines.append(f"  {col}:")
            lines.append(f"    - 最大差分: {max_diff:.3f} ({max_topic})")
            lines.append(f"    - 平均差分: {mean_diff:.3f}")

        # 差分の可視化（上位トピック）
        df = differences.copy()
        if top_n_topics is not None:
            max_diff = df.abs().max(axis=1)
            top_topics = max_diff.nlargest(top_n_topics).index
            df = df.loc[top_topics]

        lines.append("\n【トピック別差分】")
        lines.append(f"{'トピック':<15} {' | '.join([f'{col:>15}' for col in df.columns])}")
        lines.append("-" * (15 + 3 + 17 * len(df.columns)))

        for idx, row in df.iterrows():
            topic_name = str(idx).replace('topic_', 'T')
            row_str = f"{topic_name:<15}"
            for val in row:
                # 差分を視覚的に表示（正は+, 負は-）
                if val > 0.01:
                    bar = "▲ " + f"{val:>6.3f}"
                elif val < -0.01:
                    bar = "▼ " + f"{val:>6.3f}"
                else:
                    bar = "  " + f"{val:>6.3f}"
                row_str += f" | {bar:>15}"
            lines.append(row_str)

        # 凡例
        lines.append("\n【凡例】")
        lines.append("  ▲: 比較ドキュメントの方が高いトピック寄与度")
        lines.append("  ▼: 参照ドキュメントの方が高いトピック寄与度")
        lines.append("  正の値: 比較ドキュメントが参照ドキュメントより大きい差分")
        lines.append("  負の値: 参照ドキュメントが比較ドキュメントより大きい差分")

        return "\n".join(lines)

    def format_document_profiles_as_text(
        self,
        result: VectorizationResult,
        doc_indices: Optional[List[int]] = None
    ) -> str:
        """
        ドキュメント-トピック行列をテキスト形式で表示

        Parameters:
            result (VectorizationResult): ベクトル化結果
            doc_indices (Optional[List[int]]): 表示するドキュメントのインデックス

        Returns:
            str: フォーマットされたテキスト
        """
        lines = []
        lines.append("=" * 80)
        lines.append("📋 ドキュメント-トピック プロファイル")
        lines.append("=" * 80)

        if doc_indices is None:
            doc_indices = list(range(len(result.labels)))

        for doc_idx in doc_indices:
            label = result.labels[doc_idx]
            vector = result.get_document_vector(doc_idx)

            lines.append(f"\n【{label}】")

            # トピック寄与度を降順で表示
            topic_contributions = [(i, v) for i, v in enumerate(vector)]
            topic_contributions.sort(key=lambda x: x[1], reverse=True)

            # ヒストグラム表示
            max_val = max(vector)
            for topic_idx, value in topic_contributions:
                bar_length = int(value * 40)
                bar = "█" * bar_length + "░" * (40 - bar_length)
                pct = value * 100
                lines.append(f"  Topic{topic_idx:2d}: {bar} {pct:5.1f}%")

            # 統計情報
            lines.append(f"\n  統計情報:")
            lines.append(f"    - 最大寄与度: {max(vector):.3f} (Topic{np.argmax(vector)})")
            lines.append(f"    - 平均寄与度: {np.mean(vector):.3f}")
            lines.append(f"    - 標準偏差:   {np.std(vector):.3f}")

            # 上位トピック
            top_indices = np.argsort(vector)[::-1][:3]
            lines.append(f"\n  上位3トピック:")
            for rank, tidx in enumerate(top_indices, 1):
                top_terms = result.get_topic_terms(tidx, n_terms=5)
                terms_str = " > ".join([term for term, _ in top_terms])
                lines.append(f"    {rank}. Topic{tidx}: {terms_str}")

        return "\n".join(lines)

    def format_topic_summary_as_text(
        self,
        result: VectorizationResult,
        n_terms: int = 10
    ) -> str:
        """
        各トピックの上位タームをテキスト形式で表示

        Parameters:
            result (VectorizationResult): ベクトル化結果
            n_terms (int): 各トピックの表示ターム数

        Returns:
            str: フォーマットされたテキスト
        """
        lines = []
        lines.append("=" * 80)
        lines.append("🔍 トピック詳細分析")
        lines.append("=" * 80)

        lines.append(f"\nモード: {result.metadata.get('feature_mode', 'unknown')}")
        if 'vocabulary_size' in result.metadata:
            lines.append(f"語彙数: {result.metadata.get('vocabulary_size', 'N/A')}")

        for topic_idx in range(result.topic_term_matrix.shape[0]):
            top_terms = result.get_topic_terms(topic_idx, n_terms=n_terms)

            lines.append(f"\n【Topic {topic_idx}】")
            lines.append("-" * 40)

            max_score = top_terms[0][1] if top_terms else 1.0
            for rank, (term, score) in enumerate(top_terms, 1):
                bar_length = int((score / max_score) * 30)
                bar = "█" * bar_length
                lines.append(f"  {rank:2d}. {term:15s} {bar} {score:.3f}")

        return "\n".join(lines)

    def export_top_terms(
        self,
        result: VectorizationResult,
        filepath: Union[str, Path],
        n_terms: int = 15,
        format: str = 'json'
    ):
        """
        各トピックの上位タームを出力

        Parameters:
            result (VectorizationResult): ベクトル化結果
            filepath (Union[str, Path]): 出力先ファイルパス
            n_terms (int): 各トピックの上位ターム数
            format (str): 出力形式（'json', 'csv'）
        """
        topics_data = {}

        for topic_idx in range(result.topic_term_matrix.shape[0]):
            top_terms = result.get_topic_terms(topic_idx, n_terms=n_terms)
            topics_data[f'topic_{topic_idx}'] = [
                {'term': term, 'score': float(score)}
                for term, score in top_terms
            ]

        if format == 'json':
            with open(filepath, 'w', encoding='utf-8') as f:
                json.dump(topics_data, f, ensure_ascii=False, indent=2)
        elif format == 'csv':
            # CSV形式で出力（トピック × ターム）
            rows = []
            for topic_name, terms_list in topics_data.items():
                for rank, item in enumerate(terms_list, 1):
                    rows.append({
                        'topic': topic_name,
                        'rank': rank,
                        'term': item['term'],
                        'score': item['score']
                    })
            df = pd.DataFrame(rows)
            df.to_csv(filepath, index=False, encoding='utf-8-sig')
        else:
            raise ValueError(f"Unknown format: {format}")

        logger.info(f"Top terms exported to {filepath} (format={format})")
