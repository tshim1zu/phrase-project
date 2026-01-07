"""
統計的要約モジュール

PhraseExtracterの抽出能力（PMI/エントロピー）を活用し、
「重要文の抜粋」と「文の圧縮」を行う統計的サマライザー。
生成AIを使わないため、ハルシネーション（嘘）がなく、極めて高速に動作する。
"""

import re
from typing import List, Dict, Tuple, Optional
import logging
import numpy as np

from .extracter import PhraseExtracter
from .constants import DEFAULT_UNNECESSARY

logger = logging.getLogger(__name__)


class Summarizer:
    """
    統計的アプローチによる文書要約クラス
    """

    def __init__(
        self,
        extractor: Optional[PhraseExtracter] = None,
        min_sentence_length: int = 10,
        compression_threshold: float = 0.0,
        unnecessary_words: List[str] = None
    ):
        """
        Args:
            extractor: PhraseExtracterインスタンス（Noneならデフォルト設定で生成）
            min_sentence_length: 要約対象とする文の最小文字数
            compression_threshold: 文圧縮時に削除するフレーズのスコア閾値（0.0で無効化）
            unnecessary_words: 削除対象の不要語リスト
        """
        if extractor is None:
            # 要約向けにPMIとエントロピーを有効化した設定
            self.extractor = PhraseExtracter(
                min_count=2,
                min_length=2,
                use_pmi=True,
                use_branching_entropy=True
            )
        else:
            self.extractor = extractor

        self.min_sentence_length = min_sentence_length
        self.compression_threshold = compression_threshold
        self.unnecessary_words = unnecessary_words or DEFAULT_UNNECESSARY

    def split_sentences(self, text: str) -> List[str]:
        """テキストを文単位に分割する"""
        # 句点、改行などで分割
        text = text.replace('\r', '')
        sentences = re.split(r'([。\n\.]\s?)', text)
        
        # 分割された区切り文字を前の文に結合して復元
        merged = []
        current = ""
        for s in sentences:
            if re.match(r'[。\n\.]', s):
                current += s
                if len(current.strip()) >= self.min_sentence_length:
                    merged.append(current.strip())
                current = ""
            else:
                current += s
        if current.strip():
            merged.append(current.strip())
            
        return merged

    def _calculate_sentence_importance(
        self, 
        sentences: List[str], 
        phrase_scores: Dict[str, float]
    ) -> List[Tuple[str, float]]:
        """
        各文の重要度スコアを計算する
        スコア = 文に含まれる重要フレーズのスコア合計 / 文の長さの対数（長文有利の補正抑制）
        """
        ranked_sentences = []
        
        for sent in sentences:
            score = 0.0
            hit_phrases = []
            
            # 文中の重要フレーズを探して加点
            for phrase, p_score in phrase_scores.items():
                if phrase in sent:
                    score += p_score
                    hit_phrases.append(phrase)
            
            # 正規化: 長すぎる文が有利になりすぎないように
            if len(sent) > 0:
                normalized_score = score / np.log(len(sent) + 1)
            else:
                normalized_score = 0
                
            ranked_sentences.append((sent, normalized_score))
            
        # スコア順にソート
        return sorted(ranked_sentences, key=lambda x: x[1], reverse=True)

    def _compress_sentence(self, sentence: str, phrase_scores: Dict[str, float]) -> str:
        """
        文圧縮: 重要度が低い部分や不要語を削ぎ落とす
        """
        # 簡易的な実装: 文をフレーズ分解し、スコアが低いものや不要語を除外して再結合
        # ※本来は形態素解析や係り受け解析があるとより自然だが、
        # ここではエンジンの検出した「強いフレーズ」を残す方針で行く
        
        # まず、エンジンが認識している重要フレーズを保護する
        kept_parts = []
        
        # 文をスキャンして重要フレーズを特定（貪欲法）
        current_pos = 0
        while current_pos < len(sentence):
            best_match = None
            best_len = 0
            
            # 現在位置から始まる最長の高スコアフレーズを探す
            for phrase, score in phrase_scores.items():
                if sentence.startswith(phrase, current_pos):
                    if len(phrase) > best_len:
                        best_match = (phrase, score)
                        best_len = len(phrase)
            
            if best_match:
                phrase, score = best_match
                # スコアが閾値以上、かつ不要語でなければ採用
                if score >= self.compression_threshold and phrase not in self.unnecessary_words:
                    kept_parts.append(phrase)
                current_pos += len(phrase)
            else:
                # 重要フレーズでない文字は、圧縮モードならスキップ（あるいは残す）
                # ここでは「文脈をつなぐ助詞などは残す」ために1文字進める
                # ※厳密な圧縮には形態素解析が必要だが、ここでは簡易的に全残し
                # （本格的な圧縮は `compression_threshold` を高く設定して行う）
                kept_parts.append(sentence[current_pos])
                current_pos += 1
                
        return "".join(kept_parts)

    def summarize(
        self, 
        text: str, 
        ratio: float = 0.3, 
        top_n: Optional[int] = None,
        compress: bool = False
    ) -> str:
        """
        要約実行のメインメソッド

        Args:
            text: 入力テキスト
            ratio: 要約率（0.0 ~ 1.0）。top_nが指定されていない場合に使用。
            top_n: 抽出する文の数。指定された場合、ratioより優先される。
            compress: Trueの場合、抽出後の文に対して圧縮処理を行う。

        Returns:
            str: 要約されたテキスト
        """
        # 1. 文分割
        sentences = self.split_sentences(text)
        if not sentences:
            return ""

        # 2. フレーズ抽出とスコアリング（全文コンテキストでPMI計算）
        # 文のリストを渡して解析させる
        df = self.extractor.extract(sentences)
        
        # フレーズごとのスコア辞書を作成
        if df.empty:
            logger.warning("No phrases extracted. Returning original text.")
            return text
            
        phrase_scores = dict(zip(df['seqchar'], df['sc_index']))

        # 3. 文の重要度評価
        ranked_sentences_with_score = self._calculate_sentence_importance(sentences, phrase_scores)
        
        # 4. 上位文の選定
        num_sentences = len(sentences)
        if top_n is None:
            top_n = max(1, int(num_sentences * ratio))
        
        # スコア上位の文を取得（元の出現順序情報は失われている）
        top_sentences_with_score = ranked_sentences_with_score[:top_n]
        
        # 5. 文の順序を復元（元のテキストの流れに沿わせる）
        # 元のsentencesリスト内でのインデックスを探してソート
        indices = []
        for sent, _ in top_sentences_with_score:
            try:
                idx = sentences.index(sent)
                indices.append(idx)
            except ValueError:
                continue
        
        indices.sort()
        final_sentences = [sentences[i] for i in indices]

        # 6. 文圧縮（オプション）
        if compress:
            final_sentences = [
                self._compress_sentence(s, phrase_scores) 
                for s in final_sentences
            ]

        return "\n".join(final_sentences)
