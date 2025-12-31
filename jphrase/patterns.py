"""
正規表現パターン定義モジュール
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023"

import re
from .constants import FIRST_KANJI


def get_positive_patterns():
    """
    ポジティブフィルター（抽出したいパターン）を返す

    Returns:
        dict: パターン名をキーとする正規表現の辞書
    """
    return {
        "Kana": re.compile('[ァ-ヶー]{2,}'),  # カタカナ
        "Hana": re.compile('[ｦ-ﾟ]{2,}'),  # 半角カタカナ
        "HAN": re.compile(f'[{FIRST_KANJI}]{{2,}}'),  # 漢字二文字以上
        "ZA": re.compile("[Ａ-Ｚ]{2,}"),  # 全角英文字
        "alpha": re.compile("[a-zA-Z]{2,}"),  # アルファベット
        "ALPHA": re.compile("[A-Z]{2,}"),  # 大文字アルファベット
        "Kana_HAN": re.compile(f"^[ァ-ヶー]+[{FIRST_KANJI}]+"),
        "HAN_Kana": re.compile(f"^[{FIRST_KANJI}]+[ァ-ヶー]+"),
        "HAN_GaKa": re.compile(f"^[{FIRST_KANJI}]+[ぁ-ゖ]+[ァ-ヶー]+"),
        "ZaGaKa": re.compile(f"^[Ａ-Ｚ]+[ぁ-ゖ]+[{FIRST_KANJI}]+"),
        "Kana_Gana": re.compile("^[ァ-ヶー]{2,}[ぁ-ゖ]{2,}"),
    }


def get_negative_patterns():
    """
    ネガティブフィルター（除外したいパターン）を返す

    Returns:
        dict: パターン名をキーとする正規表現の辞書
    """
    return {
        "x_Gana": re.compile("[ぁ-ゖ]+"),  # ひらがなのみ
        "x_smalla": re.compile("[a-z* _.]+"),  # ほぼノイズ
        "x_start": re.compile("^[ンッー、んっ～。](.*)+"),
        "x_Yen": re.compile(r"^\d+(千|万|億|兆)?円$"),
        "x_Dollar": re.compile(r"^\d+(千|万|億|兆)?ドル$"),
        "x_Euro": re.compile(r"^\d+(千|万|億|兆)?ユーロ$"),
        "x_Phone": re.compile(r'((\d{2,4}|\(\d{2,4}\))(\s|-)(\d{3,4})(\s|-)(\d{4}))'),
        "x_Tel": re.compile(r'[(]?\d{2,4}[-)]?\d{2,4}-\d{3,4}'),
        "x_mobile": re.compile("0[789]0-[0-9]{4}-[0-9]{4}$"),
        "x_mail": re.compile(r'[a-zA-Z0-9_.+-]+@[a-zA-Z0-9-]+\.[a-zA-Z0-9-.]+'),
        "x_url": re.compile(r'https?://[\w/:%#\$&\?\(\)~\.=\+\-]+'),
        "x_Num": re.compile(r"[\d,，]+"),  # 全半角数字
        "x_ymd": re.compile(r'\d{4}[-年/]\d{1,2}[-月/]\d{1,2}日?'),
        "x_Num_ymd": re.compile(r"[\d]{1,4}(日|月|年)"),  # 全角数字の日付
    }


# モジュールレベルで利用可能にする（後方互換性のため）
dict_match = get_positive_patterns()
dict_negative = get_negative_patterns()
