"""
PhraseExtracter のデバッグ
"""

from japhrase import PhraseExtracter


def main():
    text = """
    それは良い。それは悪い。それは普通。それは面白い。
    それは素晴らしい。それは微妙。それは最高。それは最悪。
    それは楽しい。それは退屈。それは興味深い。それは重要。
    """ * 20

    print("=" * 60)
    print("PhraseExtracter デバッグ")
    print("=" * 60)

    sentences = text.split('\n')
    print(f"\n文数: {len(sentences)}")
    print(f"文字数: {len(text)}")

    print("\n[設定1] min_count=10, min_length=2, max_length=15")
    extractor1 = PhraseExtracter(
        min_count=10,
        min_length=2,
        max_length=15,
        use_pmi=True,
        verbose=1
    )
    df1 = extractor1.get_dfphrase(sentences)
    print(f"抽出数: {len(df1)}")
    if not df1.empty:
        print(df1.head(10))

    print("\n[設定2] min_count=5, min_length=2, max_length=10")
    extractor2 = PhraseExtracter(
        min_count=5,
        min_length=2,
        max_length=10,
        use_pmi=True,
        verbose=1
    )
    df2 = extractor2.get_dfphrase(sentences)
    print(f"抽出数: {len(df2)}")
    if not df2.empty:
        print(df2.head(10))

    print("\n[設定3] min_count=3, min_length=2, max_length=8, threshold_originality=0.3")
    extractor3 = PhraseExtracter(
        min_count=3,
        min_length=2,
        max_length=8,
        threshold_originality=0.3,
        use_pmi=True,
        verbose=1
    )
    df3 = extractor3.get_dfphrase(sentences)
    print(f"抽出数: {len(df3)}")
    if not df3.empty:
        print(df3.head(20))


if __name__ == '__main__':
    main()
