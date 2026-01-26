"""
WritingHabitDetector のデバッグ
"""

import re
import pandas as pd
from japhrase import PhraseExtracter


def main():
    text = """
    それは良い。それは悪い。それは普通。それは面白い。
    それは素晴らしい。それは微妙。それは最高。それは最悪。
    それは楽しい。それは退屈。それは興味深い。それは重要。
    """ * 20

    sentences = text.split('\n')
    print("=" * 60)
    print("WritingHabitDetector デバッグ")
    print("=" * 60)
    print(f"\n文数: {len(sentences)}")
    print(f"文字数: {len(text)}")

    # 書き癖検出用の緩いフィルター設定
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

    print("\n[PhraseExtracter で抽出]")
    extractor = PhraseExtracter(
        min_count=10,
        min_length=2,
        max_length=15,
        use_pmi=True,
        verbose=1,
        positive=positive_patterns,
        negative=negative_patterns,
    )
    df = extractor.get_dfphrase(sentences)

    print(f"\n抽出されたフレーズ数: {len(df)}")
    if not df.empty:
        print("\n全抽出結果:")
        pd.set_option('display.max_rows', None)
        print(df[['seqchar', 'freq', 'pmi']].to_string(index=False))

        print("\n[PMI統計]")
        if 'pmi' in df.columns:
            print(f"  最小PMI: {df['pmi'].min():.2f}")
            print(f"  最大PMI: {df['pmi'].max():.2f}")
            print(f"  平均PMI: {df['pmi'].mean():.2f}")
            print(f"  PMI <= 3.0の数: {len(df[df['pmi'] <= 3.0])}")
            print(f"  PMI <= 5.0の数: {len(df[df['pmi'] <= 5.0])}")
    else:
        print("フレーズが抽出されませんでした")


if __name__ == '__main__':
    main()
