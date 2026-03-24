"""
日本語フレーズ抽出モジュール
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023"

import numpy as np
import pandas as pd
from collections import Counter
import re
import logging
from typing import List, Dict, Any, Optional, Union
try:
    from tqdm import tqdm
except ImportError:
    def tqdm(iterable, **kwargs):
        return iterable

from .constants import DEFAULT_REMOVES, DEFAULT_UNNECESSARY
from .patterns import get_positive_patterns, get_negative_patterns

logger = logging.getLogger(__name__)

# Optional dependency for optimized Levenshtein distance
try:
    import Levenshtein
    HAS_LEVENSHTEIN = True
except ImportError:
    HAS_LEVENSHTEIN = False

# Lazy import to avoid circular dependency
_similarity_analyzer = None

def _get_similarity_analyzer():
    """Get or create SimilarityAnalyzer instance (lazy initialization)"""
    global _similarity_analyzer
    if _similarity_analyzer is None:
        from .similarity import SimilarityAnalyzer
        _similarity_analyzer = SimilarityAnalyzer(method='levenshtein')
    return _similarity_analyzer


# Evidence-based presets from Optuna optimization experiments
PRESETS = {
    'sns': {
        'min_count': 6,
        'max_length': 9,
        'min_length': 5,
        'threshold_originality': 0.52,
        'description': 'SNS/Twitter向け最適化パラメータ（短文、頻出フレーズ）',
    },
    'news': {
        'min_count': 5,
        'max_length': 10,
        'min_length': 3,
        'threshold_originality': 0.64,
        'description': 'ニュース/記事向け最適化パラメータ（やや長文、専門用語）',
    },
    'novel': {
        'min_count': 4,
        'max_length': 16,
        'min_length': 3,
        'threshold_originality': 0.6,
        'description': '小説向けパラメータ（繰り返し表現、情緒的フレーズ）',
    },
    'report': {
        'min_count': 10,
        'max_length': 24,
        'min_length': 4,
        'threshold_originality': 0.78,
        'description': 'レポート/論文採点向けパラメータ（定型表現、学術用語）',
    },
    'default': {
        'min_count': 6,
        'max_length': 16,
        'min_length': 4,
        'threshold_originality': 0.5,
        'description': 'デフォルト設定（汎用的なバランス型）',
    },
}


class PhraseExtracter:
    """
    日本語テキストから頻出フレーズを抽出するクラス

    N-gramベースの頻度分析により、テキスト中で頻繁に出現するフレーズを検出します。
    SNSトレンド分析、ニュース話題抽出、頻出キーワード発見などに適しています。

    注意: このツールは「頻出フレーズ検出」であり、厳密な「新語判定」ではありません。

    使用例:
        >>> extractor = PhraseExtracter(min_count=6, max_length=16)
        >>> df_result = extractor.get_dfphrase(sentences)
    """

    # 列名
    clm_seqchar = "seqchar"
    clm_sc = "sc_index"
    clm_freq = "freq"
    clm_length = "length"
    clm_originality = "originality"
    clm_knowns = "knowns"
    clm_periodic = "periodic"

    def __init__(
        self,
        min_count=6,
        max_length=16,
        min_length=4,
        weight_freq=1.0,
        weight_len=1.0,
        removes=DEFAULT_REMOVES,
        unnecessary=DEFAULT_UNNECESSARY,
        threshold_originality=0.5,
        size_sentence=5000,
        knowns=None,
        selection=1,
        verbose=1,
        positive=None,
        negative=None,
        use_pmi=False,
        use_branching_entropy=False,
        pmi_weight=1.0,
        entropy_weight=1.0,
    ):
        """
        Parameters:
            min_count (int): フレーズ出現回数の最小閾値
            max_length (int): フレーズの最大文字数
            min_length (int): フレーズの最小文字数
            weight_freq (float): 頻度への重み
            weight_len (float): 長さへの重み
            removes (str): 走査前に除去する文字
            unnecessary (list): 走査後に除去する文字列
            threshold_originality (float): 類似フレーズの除去閾値
            size_sentence (int): 一度にスキャンする配列のサイズ
            knowns (list): 既知語のリスト
            selection (int): セレクション機能の有無（0:無効, 1:有効）
            verbose (int): 進捗表示レベル
            positive (dict): ポジティブフィルター（Noneの場合はデフォルト使用）
            negative (dict): ネガティブフィルター（Noneの場合はデフォルト使用）
            use_pmi (bool): PMI（自己相互情報量）を使用するかどうか
            use_branching_entropy (bool): 分岐エントロピーを使用するかどうか
            pmi_weight (float): PMIの重み係数
            entropy_weight (float): エントロピーの重み係数
        """
        self.min_count = min_count
        self.weight_freq = weight_freq
        self.weight_len = weight_len
        self.max_length = max_length + 1  # 指定された数よりも１つ多く数えて処理
        self.min_length = min_length
        self.removes = removes
        self.unnecessary = unnecessary
        self.knowns = knowns if knowns is not None else []
        self.size_sentence = size_sentence
        self.threshold_originality = threshold_originality
        self.selection = selection
        self.verbose = verbose
        self.positive_filter = positive if positive is not None else get_positive_patterns()
        self.negative_filter = negative if negative is not None else get_negative_patterns()
        self.use_pmi = use_pmi
        self.use_branching_entropy = use_branching_entropy
        self.pmi_weight = pmi_weight
        self.entropy_weight = entropy_weight

    def make_ngrampieces(self, sentences: List[str]) -> List[str]:
        """文章リストからN-gramフレーズを生成"""
        max_length = self.max_length
        if max_length == -1:
            max_length = len(sentences) // 2
        min_length = self.min_length

        phrases = []
        for a_sentence in sentences:
            for x in self.removes:
                a_sentence = a_sentence.replace(x, "")

            for char_length in range(1, max_length + 1):
                for i, c in enumerate(a_sentence):
                    if i + char_length - 1 < len(a_sentence):
                        phr = "".join(a_sentence[i:i+char_length])
                        if len(phr) >= min_length:
                            phrases.append(phr)
        return phrases

    def count_characters(self, phrases: List[str]) -> pd.DataFrame:
        """フレーズの出現回数をカウント"""
        cnt_ = Counter(phrases)
        seqchars, lengths, freqs = [], [], []
        for k, v in cnt_.most_common():
            if v > self.min_count:
                seq_char = k
                seqchars.append(seq_char)
                lengths.append(len(seq_char))
                freqs.append(float(v))

        df_ret = pd.DataFrame({
            self.clm_seqchar: seqchars,
            self.clm_length: lengths,
            self.clm_freq: freqs,
        })
        return df_ret

    def count_knowns(self, sentences: List[str]) -> pd.DataFrame:
        """既知語を必ずカウント"""
        def count_all(sent, target):
            def find_all(a_str, sub):
                start = 0
                while True:
                    start = a_str.find(sub, start)
                    if start == -1:
                        return
                    yield start
                    start += len(sub)
            return len(list(find_all(sent, target)))

        dict_n = {}
        for k in self.knowns:
            dict_n[k] = 0
            for s in sentences:
                dict_n[k] += count_all(s, k)

        df = pd.DataFrame({
            self.clm_seqchar: dict_n.keys(),
            self.clm_length: [len(k) for k in dict_n.keys()],
            self.clm_freq: dict_n.values()
        })
        return df

    def calculate_pmi(self, phrases_or_freq, all_text_or_counter=None, total_chars=None) -> Dict[str, float]:
        """
        PMI (Pointwise Mutual Information) を計算（最適化版 + 後方互換性）

        PMI(phrase) = log(P(phrase) / product(P(char_i)))

        高いPMI = 文字の結合度が強い (例: "機械学習")
        低いPMI = 単なる組み合わせ (例: "ていう")

        最適化: 事前計算済みの頻度データを使用し、str.count()呼び出しを排除
        - 計算量: O(n+m) （n=フレーズ数, m=テキスト長）
        - 旧版: O(n×m) （フレーズごとに全テキスト走査）

        後方互換性: 以下の2つの呼び出し形式をサポート
        1. 最適化版: calculate_pmi(phrase_freq: Dict, char_counter: Counter, total_chars: int)
        2. 旧版（互換性）: calculate_pmi(phrases: List[str], all_text: str)

        Parameters:
            phrases_or_freq: フレーズリスト（旧版）またはフレーズ→頻度の辞書（新版）
            all_text_or_counter: テキスト文字列（旧版）またはCounter（新版）
            total_chars: テキスト総文字数（新版のみ）

        Returns:
            Dict[phrase: pmi_score]
        """
        # 引数形式の判定と変換
        if isinstance(phrases_or_freq, dict) and isinstance(all_text_or_counter, Counter):
            # 新版: 最適化版（phrase_freq, char_counter, total_chars）
            phrase_freq = phrases_or_freq
            char_counter = all_text_or_counter
        elif isinstance(phrases_or_freq, (list, tuple)) and isinstance(all_text_or_counter, str):
            # 旧版互換性: リスト形式（phrases, all_text）
            # テキストから頻度情報を構築
            phrases = phrases_or_freq
            all_text = all_text_or_counter
            if not phrases or not all_text:
                return {}

            char_counter = Counter(all_text)
            total_chars = len(all_text)

            # フレーズ の出現回数を計算（完全一致のみ）
            phrase_freq = {}
            for phrase in phrases:
                phrase_freq[phrase] = all_text.count(phrase)
        else:
            return {}

        if not phrase_freq or total_chars == 0:
            return {}

        pmi_scores = {}

        for phrase, phrase_count in phrase_freq.items():
            if phrase_count == 0:
                pmi_scores[phrase] = 0.0
                continue

            # フレーズの確率
            p_phrase = phrase_count / (total_chars - len(phrase) + 1)

            # 各文字の確率の積を計算
            p_chars_product = 1.0
            for char in phrase:
                p_char = char_counter.get(char, 1) / total_chars
                p_chars_product *= p_char

            # PMI = log(P(phrase) / product(P(char)))
            if p_chars_product > 0:
                pmi = np.log(p_phrase / p_chars_product) if p_phrase > 0 else 0.0
                pmi_scores[phrase] = float(np.clip(pmi, -10, 10))  # 数値安定性のためクリップ
            else:
                pmi_scores[phrase] = 0.0

        return pmi_scores

    def calculate_branching_entropy(self, sentences: List[str], phrases: List[str]) -> Dict[str, tuple]:
        """
        分岐エントロピー (Branching Entropy) を計算

        BE = -sum(p(x) * log(p(x)))

        高いBE = 多様な文字が続く = 単語境界の可能性大
        低いBE = 特定文字しか来ない = 単語の途中

        Parameters:
            sentences: センテンスのリスト
            phrases: フレーズのリスト

        Returns:
            Dict[phrase: (left_entropy, right_entropy, boundary_score)]
        """
        if not phrases or not sentences:
            return {}

        # テキストを結合
        all_text = "".join(sentences)

        entropy_scores = {}

        for phrase in phrases:
            if len(phrase) == 0:
                entropy_scores[phrase] = (0.0, 0.0, 0.0)
                continue

            # フレーズの前後の文字を収集
            left_chars = []
            right_chars = []

            for i, char in enumerate(all_text):
                if all_text[i:i+len(phrase)] == phrase:
                    # 左側の文字
                    if i > 0:
                        left_chars.append(all_text[i - 1])
                    # 右側の文字
                    if i + len(phrase) < len(all_text):
                        right_chars.append(all_text[i + len(phrase)])

            # 左側のエントロピーを計算
            left_entropy = 0.0
            if left_chars:
                left_counter = Counter(left_chars)
                left_total = len(left_chars)
                for count in left_counter.values():
                    p = count / left_total
                    if p > 0:
                        left_entropy -= p * np.log(p)

            # 右側のエントロピーを計算
            right_entropy = 0.0
            if right_chars:
                right_counter = Counter(right_chars)
                right_total = len(right_chars)
                for count in right_counter.values():
                    p = count / right_total
                    if p > 0:
                        right_entropy -= p * np.log(p)

            # 境界スコア = min(left_entropy, right_entropy)
            # エントロピーが高いほど、異なる文字が続く = 単語境界
            # スコアを0-1の範囲に正規化（最大エントロピーは log(26) ≈ 3.26）
            max_entropy = np.log(256)  # 最大256個の異なる文字
            boundary_score = (min(left_entropy, right_entropy) if (left_entropy > 0 or right_entropy > 0) else 0.0) / max_entropy
            boundary_score = np.clip(boundary_score, 0.0, 1.0)

            entropy_scores[phrase] = (
                float(left_entropy),
                float(right_entropy),
                float(boundary_score)
            )

        return entropy_scores

    def hold_higherrank(self, df: pd.DataFrame) -> pd.DataFrame:
        """情報量でソートして包含関係にある下位フレーズを除外"""
        # 基本スコアを計算
        base_score = self.weight_freq * np.log(1 + df[self.clm_freq].astype(float)) \
              + self.weight_len * np.log(df[self.clm_length].astype(float))

        # PMIを適用
        if self.use_pmi and "pmi" in df.columns:
            base_score = base_score * (1 + self.pmi_weight * df["pmi"])

        # 分岐エントロピーを適用
        if self.use_branching_entropy and "boundary_score" in df.columns:
            base_score = base_score * (1 + self.entropy_weight * df["boundary_score"])

        df[self.clm_sc] = base_score

        df[self.clm_knowns] = df[self.clm_seqchar].astype(str).apply(lambda x: x in self.knowns)
        df = df.sort_values(by=[self.clm_knowns, self.clm_sc], ascending=False).reset_index()

        # ソート上位との重複のフラグを立て除外
        dups = []
        for i, row in df.iterrows():
            flags = [(row[self.clm_seqchar] in higher_phrase)
                     for higher_phrase in df.loc[:i-1, self.clm_seqchar].values]
            dups.append(any(flags))
        if len(df):
            df = df.loc[~np.array(dups)]
        return df

    def exclude_unnecessary(self, df):
        """不要文字列を含むシーケンスを除外"""
        mask_unnec = np.array([False] * len(df))
        for unnec in self.unnecessary:
            mask_unnec = mask_unnec | df[self.clm_seqchar].str.contains(unnec)
        df = df[~mask_unnec]
        return df

    def doubt_periodic_letter(self, str_scan, len_period=2):
        """周期的な語（繰り返しパターン）を検出"""
        if len(str_scan) <= 2:
            return 0
        if len(str_scan) == 3:
            if (str_scan[0] == str_scan[1]) & (str_scan[0] == str_scan[2]):
                return True
            else:
                return False

        doubt = str_scan[0:len_period]
        ret = True
        step = len(doubt)
        for i in range(0, len(str_scan), step):
            if i + 1 == len(str_scan):
                break
            ret = ret & (doubt in str_scan[i: i+step])
        return ret

    def select_patterns(self, sr, dict_patterns):
        """複数パターンとの一致を判定"""
        df_ret = pd.DataFrame()
        for key_ptn in dict_patterns:
            sr_ret = self.select_pattern(sr, dict_patterns[key_ptn], key_ptn)
            df_ret = pd.concat([df_ret, sr_ret], axis=1)
        return df_ret

    def select_pattern(self, sr, pattern, colname):
        """正規表現パターンへの完全一致判定"""
        def equal_search(s):
            res = re.search(pattern, s)
            if res:
                st, ed = res.span()[0], res.span()[1]
                return bool(s == s[st: ed])
            return False
        ret = sr.astype(str).apply(equal_search)
        ret.name = colname
        return ret

    def contains_patterns(self, sr, dict_patterns):
        """複数パターンの包含判定"""
        df_ret = pd.DataFrame()
        for pname in dict_patterns:
            sr_ret = self.contains_pattern(sr, dict_patterns[pname], pname)
            df_ret = pd.concat([df_ret, sr_ret], axis=1)
        return df_ret

    def contains_pattern(self, sr, pattern, colname="contains"):
        """正規表現パターンに一致するものを含む場合にTrue"""
        def equal_search(s):
            res = re.search(pattern, s)
            if res:
                st, ed = res.span()[0], res.span()[1]
                return s[st: ed]
            return None
        ret = sr.astype(str).apply(equal_search)
        ret.name = colname
        return ret

    def select_phrase(self, df: pd.DataFrame) -> pd.DataFrame:
        """ポジティブ・ネガティブフィルターを適用してフレーズを選定"""
        df = df.reset_index(drop=True)
        sr = df[self.clm_seqchar]

        # 正規表現との全一致（ポジティブフィルター）
        df_match = self.select_patterns(sr, self.positive_filter)
        df = pd.concat([df, df_match], axis=1)

        clm_ptn = "match_ptn"
        f_posi = pd.Series(np.array([False] * len(sr)), name="Select")
        for c in df_match.columns:
            f_match = df.loc[:, c] == True
            df.loc[f_match, clm_ptn] = c
            f_posi = f_posi | f_match

        # 正規表現との全一致（ネガティブフィルター）
        df_nega = self.select_patterns(sr, self.negative_filter)
        df = pd.concat([df, df_nega], axis=1)

        clm_nptn = "negative_ptn"
        f_nega = pd.Series(np.array([False] * len(sr)), name="Remove")
        for c in df_nega.columns:
            f_match = df.loc[:, c]
            f_nega = f_nega | f_match
            df.loc[f_match, clm_nptn] = c

        # 長さフィルター
        f_len = df[self.clm_length] < self.max_length

        # 周期性フィルター
        f_periodic = df[self.clm_periodic] = df[self.clm_seqchar].map(self.doubt_periodic_letter)

        return df.loc[f_posi & f_len & ~f_nega & ~f_periodic, :]

    def find_uniques(self, sentences):
        """センテンスからユニークなフレーズを抽出"""
        many_ngrams = self.make_ngrampieces(sentences)
        df_count = self.count_characters(many_ngrams)
        df_knowns = self.count_knowns(sentences)
        df_concat = pd.concat([df_knowns, df_count])

        if not len(df_concat):
            return df_concat

        # PMI計算を追加（最適化版）
        if self.use_pmi:
            all_text = "".join(sentences)
            # 頻度データを事前準備（str.count()の呼び出しを排除）
            phrase_freq = dict(zip(df_concat[self.clm_seqchar], df_concat[self.clm_freq]))
            char_counter = Counter(all_text)
            total_chars = len(all_text)
            # 最適化されたメソッドを呼び出し
            pmi_scores = self.calculate_pmi(phrase_freq, char_counter, total_chars)
            df_concat["pmi"] = df_concat[self.clm_seqchar].map(pmi_scores).fillna(0.0)

        # 分岐エントロピー計算を追加
        if self.use_branching_entropy:
            entropy_scores = self.calculate_branching_entropy(sentences, df_concat[self.clm_seqchar].tolist())
            df_concat[["left_entropy", "right_entropy", "boundary_score"]] = pd.DataFrame(
                [entropy_scores.get(phrase, (0.0, 0.0, 0.0)) for phrase in df_concat[self.clm_seqchar]],
                index=df_concat.index
            )

        df_sorted = self.hold_higherrank(df_concat)
        df_sorted = self.exclude_unnecessary(df_sorted)
        df_sorted.drop(columns="index", inplace=True)

        if self.selection > 0:
            return self.select_phrase(df_sorted)

        return df_sorted

    def gen_sentences(self, sent_array):
        """テキストを小分けに処理"""
        sentences = []
        for multiple_sentence in sent_array:
            # センテンスの区切り文字を統一
            for delim in ["\r", "。", "．"]:
                multiple_sentence = multiple_sentence.replace(delim, "\n")

            for a_sentence in multiple_sentence.split("\n"):
                if len(a_sentence):
                    sentences.append(a_sentence)
                if len(sentences) >= self.size_sentence:
                    yield np.array(sentences)
                    sentences = []
        yield sentences

    def remove_similar(self, df_tmp: pd.DataFrame) -> pd.DataFrame:
        """類似度を計算して独自性のあるフレーズのみを残す"""
        def get_originality(i):
            phrase = df_tmp.loc[i, self.clm_seqchar]
            max_similarity = 0.0
            for j in range(i):
                phrase_above = df_tmp.loc[j, self.clm_seqchar]
                sim = self.similarity(phrase, phrase_above)
                max_similarity = max(sim, max_similarity)
            return 1 - max_similarity

        df_tmp[self.clm_originality] = df_tmp.index.map(get_originality)
        mask_similar = df_tmp[self.clm_originality] > self.threshold_originality
        return df_tmp[mask_similar]

    def similarity(self, seq_x: str, seq_y: str) -> float:
        """
        レーベンシュタイン距離から類似性を計算

        Note: This method delegates to SimilarityAnalyzer for optimized computation
        """
        analyzer = _get_similarity_analyzer()
        return analyzer.similarity_levenshtein(seq_x, seq_y)

    def levenshtein(self, seq_x: str, seq_y: str) -> float:
        """
        レーベンシュタイン距離を計算

        HAS_LEVENSHTEIN フラグに基づいて最適化バージョンを使用
        """
        if HAS_LEVENSHTEIN:
            # python-Levenshteinライブラリを使用（高速）
            return float(Levenshtein.distance(seq_x, seq_y))
        else:
            # フォールバック：純Python実装
            analyzer = _get_similarity_analyzer()
            # 類似度から距離を計算
            similarity = analyzer.similarity_levenshtein(seq_x, seq_y)
            seq_length = (len(seq_x) + len(seq_y)) / 2
            return (1 - similarity) * seq_length

    def get_dfphrase(self, sentences: List[str]) -> pd.DataFrame:
        """Extract phrases from texts and return a DataFrame."""
        if sentences is None or len(sentences) == 0:
            raise ValueError("Input text is empty.")

        sentences = np.array(sentences).reshape(-1,)

        if self.verbose >= 1:
            logger.info(f"テキスト処理: {len(sentences)}行を読み込みました")

        # N-gramベースのフレーズ抽出
        df_tmp = pd.DataFrame()
        for sentences_batch in self.gen_sentences(sentences):
            if len(sentences_batch) > 0:
                df_unique = self.find_uniques(sentences_batch)
                df_tmp = pd.concat([df_tmp, df_unique], ignore_index=True)

        if len(df_tmp) == 0:
            if self.verbose >= 1:
                logger.warning(
                    f"フレーズが見つかりませんでした。min_count={self.min_count}を下げてください"
                )
            return pd.DataFrame()

        # 類似フレーズの除去
        if self.threshold_originality > 0:
            df_tmp = self.remove_similar(df_tmp)

        if self.verbose >= 1:
            logger.info(f"抽出完了: {len(df_tmp)}個のフレーズを検出しました")

        return df_tmp

    def count_phrases(self, sentences: List[str]) -> Counter:
        """Count phrases from sentences without applying min_count."""
        tag_counter = Counter()

        for text in tqdm(sentences, desc="Extracting tags", unit="text", leave=False):
            text_str = str(text).strip()
            if not text_str:
                continue

            for part in text_str.split("BREAK"):
                for tag in part.split(","):
                    tag = tag.strip()
                    if not tag or len(tag) < self.min_length:
                        continue

                    while tag and tag[0] in "[({":
                        close_idx = tag.find("]}")
                        if close_idx >= 0:
                            tag = tag[close_idx + 1:]
                        else:
                            tag = tag[1:]

                    if ":" in tag:
                        tag = tag.split(":")[0]

                    tag = tag.strip()
                    if tag and len(tag) >= self.min_length:
                        tag_counter[tag] += 1

        return tag_counter

    def df_from_counts(self, tag_counter: Dict[str, int]) -> pd.DataFrame:
        """Build a phrase DataFrame from counts, applying min_count."""
        filtered_tags = {
            tag: count
            for tag, count in tag_counter.items()
            if count >= self.min_count
        }

        if not filtered_tags:
            return pd.DataFrame()

        df = pd.DataFrame(
            [
                {
                    "phrase": tag,
                    "freq": int(count),
                    "score": int(count),
                }
                for tag, count in sorted(filtered_tags.items(), key=lambda x: x[1], reverse=True)
            ]
        )
        df["seqchar"] = df["phrase"]
        df["length"] = df["phrase"].astype(str).map(len)
        return df


    @classmethod
    def infer_preset_name(cls, texts: List[str]) -> str:
        """Infer a preset name from simple text statistics."""
        cleaned = [str(t).strip() for t in texts if str(t).strip()]
        if not cleaned:
            return "default"

        lengths = [len(t) for t in cleaned]
        avg_len = sum(lengths) / len(lengths)
        short_rate = sum(l <= 30 for l in lengths) / len(lengths)
        long_rate = sum(l >= 120 for l in lengths) / len(lengths)

        total_chars = sum(lengths)
        if total_chars == 0:
            return "default"

        digits = sum(ch.isdigit() for t in cleaned for ch in t)
        ascii_letters = sum(ch.isascii() and ch.isalpha() for t in cleaned for ch in t)
        symbols = sum(ch in "#@/" for t in cleaned for ch in t)

        digit_ratio = digits / total_chars
        ascii_ratio = ascii_letters / total_chars
        symbol_ratio = symbols / total_chars

        if avg_len <= 35 or short_rate >= 0.55 or symbol_ratio >= 0.02:
            return "sns"
        if avg_len >= 140 or long_rate >= 0.30:
            return "novel"
        if digit_ratio >= 0.08 or ascii_ratio >= 0.25:
            return "report"
        if avg_len >= 60:
            return "news"
        return "default"


    @classmethod
    def preset(cls, preset_name: str, **kwargs):
        """
        プリセットパラメータでPhraseExtracterを初期化

        Optunaによる最適化実験で得られたエビデンスベースのパラメータを使用します。

        Parameters:
            preset_name (str): プリセット名 ('sns', 'news', 'default')
            **kwargs: プリセットを上書きする追加パラメータ

        Returns:
            PhraseExtracter: プリセットで初期化されたインスタンス

        使用例:
            >>> # SNS向けプリセットを使用
            >>> extractor = PhraseExtracter.preset('sns')
            >>> df = extractor.extract("tweets.txt")

            >>> # ニュース向けプリセットを使用
            >>> extractor = PhraseExtracter.preset('news')
            >>>
            >>> # プリセットを一部上書き
            >>> extractor = PhraseExtracter.preset('sns', min_count=10)

        利用可能なプリセット:
            - 'sns': SNS/Twitter向け（短文、頻出フレーズ）
            - 'news': ニュース/記事向け（やや長文、専門用語）
            - 'default': デフォルト設定（汎用的なバランス型）
        """
        if preset_name not in PRESETS:
            available = ', '.join(PRESETS.keys())
            raise ValueError(
                f"Unknown preset: '{preset_name}'. "
                f"Available presets: {available}"
            )

        preset_params = PRESETS[preset_name].copy()
        preset_params.pop('description', None)  # description は除外
        preset_params.update(kwargs)  # ユーザー指定で上書き

        logger.info(f"Using preset: '{preset_name}' - {PRESETS[preset_name]['description']}")

        return cls(**preset_params)

    @classmethod
    def list_presets(cls) -> None:
        """
        利用可能なプリセット一覧を表示

        使用例:
            >>> PhraseExtracter.list_presets()
        """
        print("利用可能なプリセット:")
        print("=" * 70)
        for name, config in PRESETS.items():
            print(f"\n[{name}]")
            print(f"  {config['description']}")
            print(f"  パラメータ:")
            for key, value in config.items():
                if key != 'description':
                    print(f"    {key}: {value}")
        print("\n" + "=" * 70)
        print("\n使用方法:")
        print("  extractor = PhraseExtracter.preset('sns')")
        print("  df = extractor.extract('input.txt')")

    @classmethod
    def demo(cls, **kwargs) -> pd.DataFrame:
        """
        デモ用サンプルデータでフレーズ抽出を試す

        Parameters:
            **kwargs: PhraseExtracterのコンストラクタ引数

        Returns:
            pandas.DataFrame: 抽出されたフレーズ

        使用例:
            >>> df = PhraseExtracter.demo()
            >>> print(df)
            >>> # カスタムパラメータで試す
            >>> df = PhraseExtracter.demo(min_count=3, max_length=20)
        """
        sample_texts = [
            "フォローありがとうございます。よろしくお願いします。",
            "フォローしてください。お願いします。",
            "プレゼントキャンペーン開催中です。応募してください。",
            "プレゼントキャンペーンに応募しました。",
            "よろしくお願いします。フォローありがとうございます。",
            "キャンペーン開催中です。ぜひ応募してください。",
            "応募してください。プレゼントがもらえます。",
            "ありがとうございます。よろしくお願いします。",
            "開催中です。プレゼントキャンペーンです。",
            "フォローお願いします。よろしくお願いします。",
        ]

        logger.info("デモモードで実行中...")
        logger.info(f"サンプルテキスト: {len(sample_texts)}件")

        # デモ用にデフォルト設定を調整（ユーザー指定があればそちらを優先）
        demo_defaults = {
            'min_count': 2,  # サンプルデータが少ないので低めに設定
            'min_length': 3,
            'verbose': 0,
        }
        demo_defaults.update(kwargs)

        extractor = cls(**demo_defaults)
        return extractor.get_dfphrase(sample_texts)

    @classmethod
    def from_file(cls, filepath: str, column: str = None, encoding: str = 'auto', **kwargs) -> pd.DataFrame:
        """
        ファイルから直接フレーズを抽出

        Parameters:
            filepath (str): 入力ファイルパス (.txt, .csv, .tsv)
            column (str): CSV/TSVの場合の列名
            encoding (str): 文字エンコーディング ('auto'で自動検出、デフォルト: 'auto')
            **kwargs: PhraseExtracterのコンストラクタ引数

        Returns:
            pandas.DataFrame: 抽出されたフレーズ

        使用例:
            >>> df = PhraseExtracter.from_file("input.txt")
            >>> df = PhraseExtracter.from_file("data.csv", column="text", min_count=10)
            >>> # エンコーディングを明示指定
            >>> df = PhraseExtracter.from_file("shift_jis.txt", encoding="shift_jis")
        """
        from .utils import read_file

        sentences = read_file(filepath, column, encoding)
        extractor = cls(**kwargs)
        return extractor.get_dfphrase(sentences)

    @classmethod
    def from_files(cls, filepaths: List[str], column: str = None, encoding: str = 'auto', **kwargs) -> pd.DataFrame:
        """
        複数のファイルから直接フレーズを抽出

        Parameters:
            filepaths (list): 入力ファイルパスのリスト
            column (str): CSV/TSVの場合の列名
            encoding (str): 文字エンコーディング ('auto'で自動検出、デフォルト: 'auto')
            **kwargs: PhraseExtracterのコンストラクタ引数

        Returns:
            pandas.DataFrame: 抽出されたフレーズ

        使用例:
            >>> df = PhraseExtracter.from_files(["file1.txt", "file2.txt"])
        """
        from .utils import read_files

        sentences = read_files(filepaths, column, encoding)
        extractor = cls(**kwargs)
        return extractor.get_dfphrase(sentences)

    @classmethod
    def from_formatted_file(
        cls,
        filepath: str,
        format: str = 'auto',
        encoding: str = 'auto',
        **kwargs
    ) -> tuple:
        """
        複数フォーマット（JSON、CSV等）に対応したファイル読み込み

        Parameters:
            filepath (str): ファイルパス
            format (str): ファイル形式 ('auto', 'json', 'jsonl', 'csv', 'tsv', 'text')
            encoding (str): 文字エンコーディング
            **kwargs: フォーマッター固有のオプション

        Returns:
            tuple: (フレーズDataFrame, メタデータ辞書)

        使用例:
            >>> df, metadata = PhraseExtracter.from_formatted_file("data.json")
            >>> df, metadata = PhraseExtracter.from_formatted_file("data.csv", column="text_column")
        """
        from .formatters import UniversalFormatter
        import inspect

        # PhraseExtracterの初期化引数を取得
        valid_params = inspect.signature(cls.__init__).parameters.keys()

        # kwargsを分離：フォーマッター用とPhraseExtracter用
        formatter_kwargs = {}
        extractor_kwargs = {}

        for key, value in kwargs.items():
            if key in valid_params:
                extractor_kwargs[key] = value
            else:
                formatter_kwargs[key] = value

        # UniversalFormatterを使用してテキスト + メタデータを抽出
        texts, metadata = UniversalFormatter.extract(
            filepath,
            format_hint=format,
            encoding=encoding,
            **formatter_kwargs
        )

        # フレーズを抽出
        if texts:
            extractor = cls(**extractor_kwargs)
            df = extractor.get_dfphrase(texts)
        else:
            # テキストがない場合は空のDataFrameを返す
            df = pd.DataFrame(columns=['phrase', 'freq', 'score'])

        return df, metadata

    @classmethod
    def from_formatted_files(
        cls,
        filepaths: List[str],
        format: str = 'auto',
        encoding: str = 'auto',
        **kwargs
    ) -> tuple:
        """
        複数の異なるフォーマットのファイルから統一的にフレーズを抽出

        Parameters:
            filepaths (list): ファイルパスのリスト
            format (str): ファイル形式 ('auto'で各ファイルを自動判定)
            encoding (str): 文字エンコーディング
            **kwargs: フォーマッター固有のオプション

        Returns:
            tuple: (統合フレーズDataFrame, ファイル別メタデータ辞書)

        使用例:
            >>> df, metadatas = PhraseExtracter.from_formatted_files(
            ...     ["data1.json", "data2.csv", "data3.txt"],
            ...     format='auto'
            ... )
        """
        from .formatters import UniversalFormatter
        import inspect

        # PhraseExtracterの初期化引数を取得
        valid_params = inspect.signature(cls.__init__).parameters.keys()

        # kwargsを分離：フォーマッター用とPhraseExtracter用
        formatter_kwargs = {}
        extractor_kwargs = {}

        for key, value in kwargs.items():
            if key in valid_params:
                extractor_kwargs[key] = value
            else:
                formatter_kwargs[key] = value

        all_texts = []
        all_metadata = {}

        for filepath in tqdm(filepaths, desc="Reading formatted files", unit="file"):
            texts, metadata = UniversalFormatter.extract(
                filepath,
                format_hint=format,
                encoding=encoding,
                **formatter_kwargs
            )
            all_texts.extend(texts)
            all_metadata[filepath] = metadata

        # 全テキストからフレーズを抽出
        if all_texts:
            extractor = cls(**extractor_kwargs)
            df = extractor.get_dfphrase(all_texts)
        else:
            df = pd.DataFrame(columns=['phrase', 'freq', 'score'])

        return df, all_metadata

    def extract_with_metadata(
        self,
        input_data: Union[str, List[str]],
        column: str = None,
        encoding: str = 'auto',
        **kwargs
    ) -> tuple:
        """
        フレーズ抽出 + 入力ファイルのメタデータを返す

        Parameters:
            input_data (str or List[str]): ファイルパスまたはテキストリスト
            column (str): CSV/TSVの列名
            encoding (str): 文字エンコーディング
            **kwargs: フォーマッター固有のオプション

        Returns:
            tuple: (フレーズDataFrame, メタデータ辞書)

        使用例:
            >>> df, metadata = extractor.extract_with_metadata("data.json")
            >>> print(metadata)  # {'format': 'json', 'encoding': 'utf-8', ...}
        """
        from pathlib import Path
        from .formatters import UniversalFormatter

        metadata = {}

        # ファイルパスの場合
        if isinstance(input_data, str) and Path(input_data).exists():
            texts, metadata = UniversalFormatter.extract(
                input_data,
                format_hint=None,
                encoding=encoding,
                **kwargs
            )
        else:
            # テキストリストの場合
            texts = input_data if isinstance(input_data, list) else [input_data]
            metadata = {
                'format': 'list',
                'encoding': 'unicode',
                'num_items': len(texts)
            }

        # フレーズを抽出
        df = self.get_dfphrase(texts)

        return df, metadata

    def extract(self, input_data: Union[str, List[str]], column: str = None, encoding: str = 'auto') -> pd.DataFrame:
        """
        ファイルパスまたは文字列リストからフレーズを抽出

        Parameters:
            input_data (str or List[str]): 入力ファイルパスまたはテキストのリスト
            column (str): CSV/TSVの場合の列名（input_dataがファイルパスの場合のみ有効）
            encoding (str): 文字エンコーディング ('auto'で自動検出、デフォルト: 'auto')

        Returns:
            pandas.DataFrame: 抽出されたフレーズ

        使用例:
            >>> extractor = PhraseExtracter(min_count=10)
            >>> # ファイルから抽出
            >>> df = extractor.extract("input.txt")
            >>> # 文字列リストから直接抽出
            >>> texts = ["テキスト1", "テキスト2", "テキスト3"]
            >>> df = extractor.extract(texts)
        """
        from .utils import read_file
        from pathlib import Path

        # 入力がファイルパスか文字列リストかを判定
        if isinstance(input_data, str):
            # ファイルパスとして扱う
            sentences = read_file(input_data, column, encoding)
        elif isinstance(input_data, (list, tuple, pd.Series)):
            # 文字列リストとして扱う
            sentences = input_data
        else:
            raise TypeError(
                f"input_dataは文字列（ファイルパス）またはリスト/タプルである必要があります。"
                f"実際の型: {type(input_data)}"
            )

        return self.get_dfphrase(sentences)

    def export_csv(self, df, filepath: str, encoding: str = 'utf-8-sig'):
        """
        結果をCSVファイルに出力

        Parameters:
            df (pandas.DataFrame): 出力するDataFrame
            filepath (str): 出力先ファイルパス
            encoding (str): 文字エンコーディング（デフォルトはBOM付きUTF-8）

        使用例:
            >>> extractor = PhraseExtracter()
            >>> df = extractor.extract("input.txt")
            >>> extractor.export_csv(df, "output.csv")
        """
        from .utils import export_to_csv, ensure_directory

        ensure_directory(filepath)
        export_to_csv(df, filepath, encoding)

    def export_json(self, df, filepath: str, encoding: str = 'utf-8'):
        """
        結果をJSONファイルに出力

        Parameters:
            df (pandas.DataFrame): 出力するDataFrame
            filepath (str): 出力先ファイルパス
            encoding (str): 文字エンコーディング

        使用例:
            >>> extractor = PhraseExtracter()
            >>> df = extractor.extract("input.txt")
            >>> extractor.export_json(df, "output.json")
        """
        from .utils import export_to_json, ensure_directory

        ensure_directory(filepath)
        export_to_json(df, filepath, encoding)

    def export_excel(self, df, filepath: str):
        """
        結果をExcelファイルに出力

        Parameters:
            df (pandas.DataFrame): 出力するDataFrame
            filepath (str): 出力先ファイルパス

        使用例:
            >>> extractor = PhraseExtracter()
            >>> df = extractor.extract("input.txt")
            >>> extractor.export_excel(df, "output.xlsx")
        """
        from .utils import export_to_excel, ensure_directory

        ensure_directory(filepath)
        export_to_excel(df, filepath)

    # ==================== テストメソッド ====================

    def test_random(
        self,
        num_sent=50,
        wnum_in_asent=12,
        words=None
    ):
        """
        ランダムなテストセンテンスでフレーズ抽出をテスト
        """
        if words is None:
            words = [
                "こんにちは", "はじめまして",
                "ランダム", "センテンス", "を", "大量に", "生成", "させて", "検知", "できる", "か",
                "どうか", "実験的に", "確かめ", "て", "みよう", "と", "思います",
                "コーディング", "の", "最中", "マグカップ", "から", "飲み物", "が", "こぼれて",
                "しまい", "ました", "明日", "再度", "やり直し", "ます"
            ]

        def gen_sentence(word, n_word_in_sent, pi):
            sentence = "".join(np.random.choice(word, n_word_in_sent, p=pi/sum(pi)))
            return sentence

        def gen_sentences(num_sentence, word, n_word_in_sent, pi):
            sentences = []
            for i in range(num_sentence):
                sentence = gen_sentence(word, n_word_in_sent, pi)
                sentences.append(sentence)
            return sentences

        pi = np.ones(len(words))
        sentences = gen_sentences(num_sent, words, wnum_in_asent, pi)

        logger.debug(sentences)

        df = self.get_dfphrase(sentences)
        logger.info(f"Test extraction completed:\n{df.head().to_string()}")
        return df


# 後方互換性のためのエイリアス
extracter = PhraseExtracter


if __name__ == "__main__":
    jpex = PhraseExtracter()
    jpex.test_random()
