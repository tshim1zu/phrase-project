"""
日本語フレーズ抽出モジュール
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023"

import numpy as np
import pandas as pd
from collections import Counter
from IPython.display import display
import re
import logging
from typing import List, Dict, Any, Optional, Union

from .constants import DEFAULT_REMOVES, DEFAULT_UNNECESSARY
from .patterns import get_positive_patterns, get_negative_patterns

logger = logging.getLogger(__name__)


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

    def hold_higherrank(self, df: pd.DataFrame) -> pd.DataFrame:
        """情報量でソートして包含関係にある下位フレーズを除外"""
        df[self.clm_sc] = self.weight_freq * np.log(1 + df[self.clm_freq].astype(float)) \
              + self.weight_len * np.log(df[self.clm_length].astype(float))

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
        """レーベンシュタイン距離から類似性を計算"""
        d = self.levenshtein(seq_x, seq_y)
        seq_length = (len(seq_x) + len(seq_y)) / 2
        d_ratio = d / seq_length
        return 1 - d_ratio

    def levenshtein(self, seq_x: str, seq_y: str) -> float:
        """レーベンシュタイン距離を計算"""
        size_x = len(seq_x) + 1
        size_y = len(seq_y) + 1
        matrix = np.zeros((size_x, size_y))
        for x in range(size_x):
            matrix[x, 0] = x
        for y in range(size_y):
            matrix[0, y] = y

        for x in range(1, size_x):
            for y in range(1, size_y):
                if seq_x[x-1] == seq_y[y-1]:
                    matrix[x, y] = min(matrix[x-1, y] + 1, matrix[x-1, y-1], matrix[x, y-1] + 1)
                else:
                    matrix[x, y] = min(matrix[x-1, y] + 1, matrix[x-1, y-1] + 1, matrix[x, y-1] + 1)
        return (matrix[size_x - 1, size_y - 1])

    def get_dfphrase(self, sentences: List[str]) -> pd.DataFrame:
        """
        メイン処理：センテンスからフレーズを抽出

        Parameters:
            sentences: 文章のリストまたはpandas.Series

        Returns:
            pandas.DataFrame: 抽出されたフレーズのデータフレーム

        Raises:
            ValueError: 入力が空、またはすべての文が短すぎる場合
        """
        # 入力バリデーション
        if sentences is None or len(sentences) == 0:
            raise ValueError(
                "入力テキストが空です。少なくとも1つの文章を指定してください。\n"
                "使用例: extractor.get_dfphrase(['サンプルテキスト1', 'サンプルテキスト2'])"
            )

        sentences = np.array(sentences).reshape(-1,)

        # テキストの長さチェック
        total_chars = sum(len(str(s)) for s in sentences)
        if total_chars < self.min_length:
            raise ValueError(
                f"入力テキストが短すぎます（合計{total_chars}文字）。\n"
                f"最小フレーズ長が{self.min_length}文字に設定されているため、\n"
                f"より長いテキストを入力するか、min_lengthを小さくしてください。"
            )

        def dict_agg(df_concat):
            """groupbyでdfを集計するときに文字列も統一的に扱う"""
            return {c: ("first" if (d == object) else
                        ("sum" if c == self.clm_freq else "mean"))
                    for c, d in zip(df_concat.columns, df_concat.dtypes)
                    if (d != bool) | (c == self.clm_knowns)
                    }

        df_concat = pd.DataFrame()
        batch_count = 0
        total_sentences = len(sentences)

        if self.verbose >= 1:
            logger.info(f"処理開始: {total_sentences}件の文章を分析します")

        for partial_sentences in self.gen_sentences(sentences):
            batch_count += 1
            df_tmp = self.find_uniques(partial_sentences)
            df_concat = pd.concat([df_concat, df_tmp])

            if len(df_concat) > 0 and (self.verbose >= 1):
                logger.info(f"バッチ{batch_count}処理完了 (ユニークフレーズ: {len(df_concat)}件)")
                df_toshow = df_concat\
                    .groupby(self.clm_seqchar, as_index=False).agg(dict_agg(df_concat))\
                    .sort_values(by=[self.clm_knowns, self.clm_sc], ascending=False)
                display(df_toshow.iloc[:5, :5])

        if not len(df_concat):
            if self.verbose >= 1:
                logger.warning(
                    f"フレーズが見つかりませんでした。\n"
                    f"  現在の設定: min_count={self.min_count}, min_length={self.min_length}\n"
                    f"  対処法:\n"
                    f"    - min_count を小さくする（現在: {self.min_count} → 推奨: 3-5）\n"
                    f"    - min_length を小さくする（現在: {self.min_length} → 推奨: 2-3）\n"
                    f"    - より多くのテキストを入力する"
                )
            return df_concat
        else:
            if self.verbose >= 1:
                logger.info("走査終了 -> 並び変え -> 類似削除 ")

            df_uniques_all = df_concat.groupby(self.clm_seqchar, as_index=False).agg(dict_agg(df_concat))
            df_phrase = self.hold_higherrank(df_uniques_all)
            df_phrase = df_phrase.drop(columns="index").reset_index(drop=True)
            df_phrase = self.remove_similar(df_phrase)

            if self.selection > 0:
                df_phrase = self.select_phrase(df_phrase)

            if self.verbose >= 1:
                logger.info(f"抽出完了: {len(df_phrase)}個のフレーズを検出しました")
                if len(df_phrase) > 0:
                    top_phrase = df_phrase.iloc[0][self.clm_seqchar]
                    logger.info(f"最頻出フレーズ: 「{top_phrase}」")

            return df_phrase

    # ==================== ユーティリティメソッド ====================

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
        display(df)
        return


# 後方互換性のためのエイリアス
extracter = PhraseExtracter


if __name__ == "__main__":
    jpex = PhraseExtracter()
    jpex.test_random()
