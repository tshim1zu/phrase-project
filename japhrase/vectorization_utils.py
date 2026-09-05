"""
ドキュメント ベクトル化 ユーティリティ

NMF（Non-negative Matrix Factorization）ベースのドキュメント ベクトル化と
PMIフィルタリングに関するヘルパー関数を提供します。
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023"

import numpy as np
import pandas as pd
from typing import List, Tuple, Optional, Any
import logging

from .constants import DEFAULT_REMOVES
try:
    from sklearn.feature_extraction.text import TfidfVectorizer, CountVectorizer
    from sklearn.decomposition import NMF
    _HAS_SKLEARN = True
except ImportError:
    TfidfVectorizer = None
    CountVectorizer = None
    NMF = None
    _HAS_SKLEARN = False

logger = logging.getLogger(__name__)


class _CharRemover:
    """指定文字を除去する、pickle可能な前処理コールable

    PhraseExtracter.make_ngrampieces()はN-gram生成前にtok.clean()で
    DEFAULT_REMOVES相当の文字（句読点等）を除去してから文字を連結する。
    high_pmiモードのvocabularyはこの「除去済みテキスト」から作られるため、
    _PhraseAnalyzerが元の生テキストをそのまま検索すると、除去文字を挟んで
    生成されたフレーズ（例:"猫.犬"->"猫犬"）が見つからない。
    このクラスで同じ除去処理を検索前に再現する。
    """

    def __init__(self, chars: str):
        self.chars = chars

    def __call__(self, text: str) -> str:
        for ch in self.chars:
            text = text.replace(ch, "")
        return text


class HybridVectorizer:
    """tfidf空間とlow_pmi語彙空間を連結する合成vectorizer（hybridモード用）

    DocumentVectorizerのVectorizationResultは「フィッティング済みvectorizer」を
    保持する設計だが、hybridの特徴空間はtfidf_vectorizerとpmi_vectorizerの
    2つを連結したものなので、どちらか一方だけを保持すると
    transform(new_texts)がhybrid空間の一部しか再現できず、
    そのままnmf_model.transform()に渡すと列数が合わずに失敗する。
    このクラスは両方を保持し、fit_transform/transform/get_feature_names_out
    を連結済みの1つのvectorizerであるかのように振る舞わせる。
    """

    def __init__(self, tfidf_vectorizer, pmi_vectorizer=None):
        self.tfidf_vectorizer = tfidf_vectorizer
        self.pmi_vectorizer = pmi_vectorizer

    def _combine(self, tfidf_matrix, pmi_matrix):
        if pmi_matrix is None:
            return tfidf_matrix.tocsr()
        from scipy.sparse import hstack
        return hstack([tfidf_matrix, pmi_matrix]).tocsr()

    def fit_transform(self, texts):
        tfidf_matrix = self.tfidf_vectorizer.fit_transform(texts)
        pmi_matrix = self.pmi_vectorizer.fit_transform(texts) if self.pmi_vectorizer is not None else None
        return self._combine(tfidf_matrix, pmi_matrix)

    def transform(self, texts):
        tfidf_matrix = self.tfidf_vectorizer.transform(texts)
        pmi_matrix = self.pmi_vectorizer.transform(texts) if self.pmi_vectorizer is not None else None
        return self._combine(tfidf_matrix, pmi_matrix)

    def get_feature_names_out(self):
        names = [f"tfidf:{n}" for n in self.tfidf_vectorizer.get_feature_names_out()]
        if self.pmi_vectorizer is not None:
            names += [f"low_pmi:{n}" for n in self.pmi_vectorizer.get_feature_names_out()]
        return np.array(names)


class _PhraseAnalyzer:
    """固定vocabulary中の各フレーズが文書に出現する回数をトークンとして返すanalyzer

    TfidfVectorizerの既定analyzer（analyzer='char' + ngram_range）は固定長の
    文字N-gramしか生成できない。一方、PMIフィルタ由来のvocabularyは2〜15文字の
    可変長フレーズであり、既定analyzerが生成するN-gramとほぼ一致しないため、
    vocabulary側の特徴列がゼロのまま埋まってしまう。
    このanalyzerはvocabularyのフレーズをそのまま文字列として数えるため、
    vocabulary制限モード（low_pmi/high_pmi/hybrid）で正しく機能する。

    クロージャではなくクラスにしているのは、VectorizationResult.save()が
    fitted vectorizerをpickle保存するため（ローカル関数はpickle不可）。

    preprocess: vocabulary生成時に適用された前処理があれば、検索前に
    同じ前処理をdocに適用する（例: high_pmiモードの文字除去）。
    """

    def __init__(self, vocabulary: List[str], preprocess: Optional[_CharRemover] = None):
        self.vocabulary = vocabulary
        self.preprocess = preprocess

    def __call__(self, doc: str) -> List[str]:
        if self.preprocess is not None:
            doc = self.preprocess(doc)
        tokens = []
        for phrase in self.vocabulary:
            count = self._count_overlapping(doc, phrase)
            if count:
                tokens.extend([phrase] * count)
        return tokens

    @staticmethod
    def _count_overlapping(doc: str, phrase: str) -> int:
        """重複を許して出現回数を数える（str.countは重複を数えない）

        PhraseExtracter.make_ngrampieces()は1文字ずつ位置をずらしてN-gramを
        生成するため、"aaaa"中の"aa"は3回（位置0,1,2）とカウントされる。
        vocabularyの頻度定義とここでのカウントを一致させるため、
        str.count()（"aaaa"で2回）ではなく重複を許す数え方を使う。
        """
        if not phrase:
            return 0
        count = 0
        start = 0
        while True:
            pos = doc.find(phrase, start)
            if pos == -1:
                break
            count += 1
            start = pos + 1
        return count


def extract_pmi_filtered_phrases(
    texts: List[str],
    mode: str = 'low_pmi',
    pmi_threshold: float = 3.0,
    min_count: int = 6,
    min_length: int = 2,
    max_length: int = 15
) -> Optional[List[str]]:
    """
    PMIスコアでフィルタリングされたフレーズを抽出

    Parameters:
        texts (List[str]): テキストのリスト
        mode (str): フィルタリングモード
            - 'low_pmi': 低PMIフレーズのみ（習慣的表現）
            - 'high_pmi': 高PMIフレーズのみ（意味的表現）
            - 'all': フィルタリングなし
        pmi_threshold (float): PMI閾値
        min_count (int): 最小出現回数
        min_length (int): 最小フレーズ長
        max_length (int): 最大フレーズ長

    Returns:
        List[str]: フィルタリングされたフレーズリスト、またはNone（allモードの場合）
    """
    if mode == 'all':
        return None

    # テキストを結合
    combined_text = '\n'.join(texts)

    if mode == 'low_pmi':
        # WritingHabitDetectorを使用して低PMIフレーズを抽出
        from .writing_habit_detector import WritingHabitDetector

        detector = WritingHabitDetector(
            min_count=min_count,
            max_pmi=pmi_threshold,
            min_length=min_length,
            max_length=max_length,
            verbose=False
        )

        df = detector.detect(combined_text)
        if df.empty:
            logger.warning(f"Low PMI mode: フレーズが抽出されませんでした")
            return None

        if 'phrase' in df.columns:
            return df['phrase'].tolist()
        else:
            return None

    elif mode == 'high_pmi':
        # PhraseExtracterを使用して高PMIフレーズを抽出
        from .extracter import PhraseExtracter

        extractor = PhraseExtracter(
            min_count=min_count,
            min_length=min_length,
            max_length=max_length,
            use_pmi=True,
            verbose=0
        )

        sentences = combined_text.split('\n')
        df = extractor.get_dfphrase(sentences)

        if df.empty:
            logger.warning(f"High PMI mode: フレーズが抽出されませんでした")
            return None

        # PMIが高いフレーズのみ
        if 'pmi' in df.columns:
            df_filtered = df[df['pmi'] >= pmi_threshold]
            if df_filtered.empty:
                logger.warning(f"High PMI mode: PMI >= {pmi_threshold} のフレーズがありません")
                return None
            seqchar_col = 'seqchar' if 'seqchar' in df_filtered.columns else 'phrase'
            return df_filtered[seqchar_col].tolist()
        else:
            # PMIがない場合は全て返す
            seqchar_col = 'seqchar' if 'seqchar' in df.columns else 'phrase'
            return df[seqchar_col].tolist()

    else:
        raise ValueError(f"Unknown PMI mode: {mode}")


def build_document_term_matrix(
    texts: List[str],
    feature_mode: str = 'tfidf',
    pmi_threshold: float = 3.0,
    min_count: int = 6,
    max_features: int = 1000,
    ngram_range: Tuple[int, int] = (2, 3),
    min_df: int = 1,
    max_df: float = 0.95,
    analyzer: str = 'char',
    verbose: int = 1
) -> Tuple[np.ndarray, Any, List[str], dict]:
    """
    PMIベースのフィルタリングを適用したドキュメント-ターム行列を構築

    Pipeline:
    1. PMIフィルタリングで語彙を制限（必要な場合）
    2. TF-IDFまたはカウント行列を構築
    3. 行列と特徴名を返す

    Parameters:
        texts (List[str]): テキストのリスト
        feature_mode (str): 特徴抽出モード
            - 'tfidf': 通常のTF-IDF（意味的差）
            - 'low_pmi': 低PMIフレーズのみ（手癖・文体差）
            - 'high_pmi': 高PMIフレーズのみ（意味的表現差）
            - 'hybrid': TF-IDFと低PMIを組み合わせ
        pmi_threshold (float): PMI閾値
        min_count (int): 最小出現回数
        max_features (int): 最大特徴数
        ngram_range (Tuple[int, int]): N-gramの範囲
        min_df (int): ドキュメント最小出現数
        max_df (float): ドキュメント最大出現率
        analyzer (str): ベクトル化のアナライザー（char, word）
        verbose (int): ログレベル

    Returns:
        Tuple: (行列, ベクトライザー, 特徴名, メタデータ)
    """
    metadata = {
        'feature_mode': feature_mode,
        'pmi_threshold': pmi_threshold,
        'min_count': min_count,
    }

    if feature_mode in ('low_pmi', 'high_pmi'):
        # ステップ1: PMIフィルタリングで語彙を制限
        if verbose:
            logger.info(f"PMI filtering mode: {feature_mode}")

        vocabulary = extract_pmi_filtered_phrases(
            texts,
            mode=feature_mode,
            pmi_threshold=pmi_threshold,
            min_count=min_count
        )

        if vocabulary is None or len(vocabulary) == 0:
            raise ValueError(
                f"No phrases found with {feature_mode} filtering. "
                f"Try adjusting pmi_threshold or min_count."
            )

        if verbose:
            logger.info(f"Extracted {len(vocabulary)} phrases for {feature_mode} mode")

        metadata['vocabulary_size'] = len(vocabulary)

        # high_pmiのvocabularyはPhraseExtracterがDEFAULT_REMOVES相当の文字を
        # 除去した上で作られる（例:"猫.犬"->"猫犬"）ため、検索前に同じ除去を
        # 再現する必要がある。low_pmi（WritingHabitDetector）は除去しないため不要。
        preprocess = _CharRemover(DEFAULT_REMOVES) if feature_mode == 'high_pmi' else None

        # ステップ2-3: vocabularyのフレーズをそのまま数えるanalyzerでベクトル化
        # （既定のchar N-gram analyzerでは可変長フレーズがほぼ一致しない = 常にゼロ）
        vectorizer = TfidfVectorizer(
            analyzer=_PhraseAnalyzer(vocabulary, preprocess=preprocess),
            vocabulary=vocabulary,
            min_df=min_df,
            max_df=max_df
        )
        if verbose:
            logger.info("Using TfidfVectorizer with phrase-counting analyzer (vocabulary restriction)")

        matrix_sparse = vectorizer.fit_transform(texts)
        feature_names = vectorizer.get_feature_names_out().tolist()

    elif feature_mode == 'hybrid':
        # ハイブリッドモード：通常TF-IDF空間 + low_pmi語彙空間を連結する
        tfidf_vectorizer = TfidfVectorizer(
            analyzer=analyzer,
            ngram_range=ngram_range,
            max_features=max_features,
            min_df=min_df,
            max_df=max_df
        )
        tfidf_matrix = tfidf_vectorizer.fit_transform(texts)

        pmi_vocabulary = extract_pmi_filtered_phrases(
            texts,
            mode='low_pmi',
            pmi_threshold=pmi_threshold,
            min_count=min_count
        )

        if pmi_vocabulary:
            pmi_vectorizer = TfidfVectorizer(
                analyzer=_PhraseAnalyzer(pmi_vocabulary),
                vocabulary=pmi_vocabulary,
                min_df=min_df,
                max_df=max_df
            )
            pmi_matrix = pmi_vectorizer.fit_transform(texts)

            vectorizer = HybridVectorizer(tfidf_vectorizer, pmi_vectorizer)
            matrix_sparse = vectorizer._combine(tfidf_matrix, pmi_matrix)
            feature_names = vectorizer.get_feature_names_out().tolist()
            metadata['vocabulary_size'] = len(pmi_vocabulary)

            if verbose:
                logger.info(
                    f"Hybrid mode: {tfidf_matrix.shape[1]} tfidf features + "
                    f"{pmi_matrix.shape[1]} low_pmi features"
                )
        else:
            # low_pmiフレーズが見つからない場合は通常のtfidf空間のみにフォールバック
            vectorizer = HybridVectorizer(tfidf_vectorizer, pmi_vectorizer=None)
            matrix_sparse = vectorizer._combine(tfidf_matrix, None)
            feature_names = vectorizer.get_feature_names_out().tolist()
            metadata['vocabulary_size'] = 'tfidf_only'

            if verbose:
                logger.info("Hybrid mode: no low_pmi phrases found, falling back to tfidf only")

    else:
        # 通常のtfidfモード
        vectorizer = TfidfVectorizer(
            analyzer=analyzer,
            ngram_range=ngram_range,
            max_features=max_features,
            min_df=min_df,
            max_df=max_df
        )
        if verbose:
            logger.info(f"Using TfidfVectorizer with analyzer={analyzer}, ngram_range={ngram_range}")

        matrix_sparse = vectorizer.fit_transform(texts)
        feature_names = vectorizer.get_feature_names_out().tolist()

    matrix = matrix_sparse.toarray()

    if verbose:
        logger.info(f"Document-term matrix shape: {matrix.shape}")
        logger.info(f"Number of features: {len(feature_names)}")
        sparsity = (matrix == 0).sum() / (matrix.shape[0] * matrix.shape[1])
        logger.info(f"Sparsity: {sparsity:.1%}")

    return matrix, vectorizer, feature_names, metadata


def fit_nmf_model(
    matrix: np.ndarray,
    n_topics: int = 10,
    nmf_init: str = 'nndsvd',
    nmf_max_iter: int = 1000,
    random_state: int = 42,
    verbose: int = 1
) -> Tuple[NMF, np.ndarray, np.ndarray]:
    """
    NMFモデルを行列に適用

    Parameters:
        matrix (np.ndarray): ドキュメント-ターム行列
        n_topics (int): トピック数
        nmf_init (str): NMF初期化方法
        nmf_max_iter (int): 最大反復回数
        random_state (int): ランダムシード
        verbose (int): ログレベル

    Returns:
        Tuple: (NMFモデル, W行列, H行列)
    """
    if verbose:
        logger.info(f"Fitting NMF model with {n_topics} topics")

    # nndsvd初期化は制約がある: n_components <= min(n_samples, n_features)
    # これを満たさない場合はランダム初期化にフォールバック
    n_samples, n_features = matrix.shape
    actual_init = nmf_init
    if nmf_init == 'nndsvd' and n_topics > min(n_samples, n_features):
        if verbose:
            logger.warning(
                f"nndsvd requires n_topics ({n_topics}) <= min(samples={n_samples}, "
                f"features={n_features}). Using 'random' initialization instead."
            )
        actual_init = 'random'

    nmf = NMF(
        n_components=n_topics,
        init=actual_init,
        max_iter=nmf_max_iter,
        random_state=random_state
    )

    W = nmf.fit_transform(matrix)  # Document-topic行列
    H = nmf.components_  # Topic-term行列

    if verbose:
        logger.info(f"NMF fitting complete")
        logger.info(f"W (Document-topic) shape: {W.shape}")
        logger.info(f"H (Topic-term) shape: {H.shape}")

    return nmf, W, H


def normalize_matrix_by_row(matrix: np.ndarray) -> np.ndarray:
    """
    行ごとに行列を正規化（各行の合計が1になるように）

    Parameters:
        matrix (np.ndarray): 入力行列

    Returns:
        np.ndarray: 正規化された行列
    """
    row_sums = matrix.sum(axis=1, keepdims=True)
    # ゼロ除算を避ける
    row_sums[row_sums == 0] = 1
    return matrix / row_sums


def get_top_terms_for_topic(
    topic_vector: np.ndarray,
    feature_names: List[str],
    n_terms: int = 15
) -> List[Tuple[str, float]]:
    """
    トピック分布から上位ターム を取得

    Parameters:
        topic_vector (np.ndarray): トピック-ターム ベクトル
        feature_names (List[str]): ターム名のリスト
        n_terms (int): 返す上位ターム数

    Returns:
        List[Tuple[str, float]]: (ターム、スコア) のタプルのリスト
    """
    # スコアが高い順にソート
    top_indices = np.argsort(topic_vector)[::-1][:n_terms]
    return [
        (feature_names[idx], float(topic_vector[idx]))
        for idx in top_indices
    ]


def calculate_distance_matrix(
    matrix: np.ndarray,
    metric: str = 'cosine'
) -> np.ndarray:
    """
    ドキュメント間の距離行列を計算

    Parameters:
        matrix (np.ndarray): ドキュメント-トピック行列
        metric (str): 距離計算方法（'cosine', 'euclidean', 'manhattan'）

    Returns:
        np.ndarray: 距離行列
    """
    from sklearn.metrics.pairwise import cosine_distances, euclidean_distances

    if metric == 'cosine':
        return cosine_distances(matrix)
    elif metric == 'euclidean':
        return euclidean_distances(matrix)
    elif metric == 'manhattan':
        from scipy.spatial.distance import pdist, squareform
        distances = pdist(matrix, metric='cityblock')
        return squareform(distances)
    else:
        raise ValueError(f"Unknown metric: {metric}")
