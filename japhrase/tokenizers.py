"""
トークナイザ — 言語別のN-gram生成戦略

設計思想:
  extracter.py の make_ngrampieces() を言語非依存にするための Strategy。
  日本語（デフォルト）は文字レベル、英語は単語レベルでN-gramを生成する。
  extracter.py には一切の言語分岐を書かない。

  保守ルール:
  - 新しい言語を追加するときは、このファイルに Tokenizer クラスを1つ追加するだけ。
  - extracter.py / constants.py への変更は不要。
  - 各 Tokenizer は tokenize() / join() / clean() の3メソッドを持つ。
"""

import re
from typing import List


class JapaneseTokenizer:
    """日本語: 文字レベルN-gram（従来の japhrase の挙動そのまま）"""

    def tokenize(self, text: str) -> List[str]:
        """テキストを文字のリストに分解"""
        return list(text)

    def join(self, tokens: List[str]) -> str:
        """トークン列をフレーズ文字列に結合"""
        return "".join(tokens)

    def clean(self, text: str, removes: str = "") -> str:
        """前処理: removes に含まれる文字を除去"""
        for ch in removes:
            text = text.replace(ch, "")
        return text

    def unit_name(self) -> str:
        return "文字"


class EnglishTokenizer:
    """英語: 単語レベルN-gram"""

    # 最低限のストップワード（フレーズ抽出のノイズになる最頻語のみ）
    STOP_WORDS = frozenset({
        "the", "a", "an", "is", "are", "was", "were", "be", "been", "being",
        "have", "has", "had", "do", "does", "did", "will", "would", "shall",
        "should", "may", "might", "must", "can", "could",
        "i", "you", "he", "she", "it", "we", "they",
        "me", "him", "her", "us", "them",
        "my", "your", "his", "its", "our", "their",
        "this", "that", "these", "those",
        "in", "on", "at", "to", "for", "of", "with", "by", "from",
        "and", "or", "but", "not", "no", "if", "so", "as",
    })

    def __init__(self, remove_stopwords: bool = False):
        self.remove_stopwords = remove_stopwords

    def tokenize(self, text: str) -> List[str]:
        """テキストを単語のリストに分解（小文字化 + 句読点除去）"""
        text = text.lower()
        # 句読点を空白に置換してから split
        text = re.sub(r'[^\w\s\'-]', ' ', text)
        tokens = text.split()
        if self.remove_stopwords:
            tokens = [t for t in tokens if t not in self.STOP_WORDS]
        return tokens

    def join(self, tokens: List[str]) -> str:
        """トークン列をフレーズ文字列に結合（空白区切り）"""
        return " ".join(tokens)

    def clean(self, text: str, removes: str = "") -> str:
        """前処理: 英語では removes は使わない（tokenize 時に処理する）"""
        return text

    def unit_name(self) -> str:
        return "word"


def get_tokenizer(lang: str = "ja", **kwargs):
    """言語コードからトークナイザを返す。

    Args:
        lang: 'ja' (日本語, デフォルト) or 'en' (英語)
        **kwargs: EnglishTokenizer に渡すオプション (remove_stopwords 等)

    Returns:
        JapaneseTokenizer or EnglishTokenizer

    Raises:
        ValueError: 未知の言語コード
    """
    lang = lang.lower().strip()
    if lang in ("ja", "jp", "japanese"):
        return JapaneseTokenizer()
    elif lang in ("en", "eng", "english"):
        return EnglishTokenizer(**kwargs)
    else:
        raise ValueError(
            f"未対応の言語: '{lang}'。'ja' または 'en' を指定してください。"
        )
