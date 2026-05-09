"""
WritingHabitDetector デモ

純粋に統計ベース（高頻度×低PMI）で書き癖を検出する実用例
"""

from japhrase.writing_habit_detector import WritingHabitDetector


def demo_basic_detection():
    """基本的な書き癖検出"""
    print("=" * 60)
    print("1. 基本的な書き癖検出")
    print("=" * 60)

    # 「それは」を多用するテキスト
    text = """
    それは良い。それは悪い。それは普通。それは面白い。
    それは素晴らしい。それは微妙。それは最高。それは最悪。
    それは楽しい。それは退屈。それは興味深い。それは重要。
    """ * 15

    detector = WritingHabitDetector(min_count=10, max_pmi=5.0)
    df = detector.detect(text)

    print("\n検出された書き癖:")
    detector.visualize_top_habits(df, top_n=5)


def demo_novel_analysis():
    """小説風テキストの書き癖分析"""
    print("\n" + "=" * 60)
    print("2. 小説風テキストの書き癖分析")
    print("=" * 60)

    novel_text = """
    それは良い。それは悪い。それは普通。
    彼はそう思った。彼はそう考えた。彼はそう感じた。
    彼女はそう思った。彼女はそう考えた。
    そして彼は歩いた。そして彼女は走った。そして終わった。
    しかし結果は違った。しかし答えは同じだった。しかし続いた。
    本当に良かった。本当に悪かった。本当に面白かった。
    """ * 20

    detector = WritingHabitDetector(min_count=10, max_pmi=5.0, top_n=10)
    df = detector.detect(novel_text)

    print("\n小説の書き癖:")
    print(df.to_string(index=False))


def demo_category_detection():
    """カテゴリ別検出"""
    print("\n" + "=" * 60)
    print("3. カテゴリ別の書き癖検出")
    print("=" * 60)

    text = """
    それは良い。それは悪い。それは普通。それは面白い。
    これは素晴らしい。これは微妙。これは最高。
    あれは良かった。あれは悪かった。
    そして進む。そして止まる。そして考える。
    しかし違う。しかし同じ。しかし難しい。
    本当に良い。本当に悪い。本当に普通。
    とても面白い。とても退屈。とても楽しい。
    """ * 20

    detector = WritingHabitDetector(min_count=15, max_pmi=3.0)
    categories = detector.detect_by_category(text)

    print("\n【超高頻度の書き癖】")
    if not categories['high_frequency'].empty:
        print(categories['high_frequency'][['phrase', 'count', 'pmi']].to_string(index=False))
    else:
        print("該当なし")

    print("\n【超低PMIの書き癖】")
    if not categories['very_low_pmi'].empty:
        print(categories['very_low_pmi'][['phrase', 'count', 'pmi']].to_string(index=False))
    else:
        print("該当なし")

    print("\n【バランス型の書き癖】")
    if not categories['balanced'].empty:
        print(categories['balanced'][['phrase', 'count', 'pmi']].to_string(index=False))
    else:
        print("該当なし")


def demo_author_comparison():
    """著者比較（書き癖の違い）"""
    print("\n" + "=" * 60)
    print("4. 著者間の書き癖比較")
    print("=" * 60)

    # 著者A: 「それは」「である」を多用
    author_a_text = """
    それは良い作品である。それは素晴らしい内容である。
    それは面白い話である。それは興味深い展開である。
    """ * 20

    # 著者B: 「〜だった」「〜ていた」を多用
    author_b_text = """
    彼は走っていた。彼女も走っていた。
    天気が良かった。昨日も良かった。
    空が青かった。海も青かった。
    """ * 20

    detector = WritingHabitDetector(min_count=10, max_pmi=3.0)
    comparison = detector.compare_texts(
        author_a_text,
        author_b_text,
        "著者A",
        "著者B"
    )

    print("\n著者間の書き癖の違い:")
    print(comparison.head(10).to_string(index=False))


def demo_summary_stats():
    """サマリー統計"""
    print("\n" + "=" * 60)
    print("5. サマリー統計")
    print("=" * 60)

    text = """
    それは良い。それは悪い。それは普通。
    これは面白い。これは素晴らしい。
    あれは微妙。あれは最高。
    そして進む。そして止まる。
    本当に良い。本当に悪い。
    """ * 30

    detector = WritingHabitDetector(min_count=20, max_pmi=3.0)
    df = detector.detect(text)
    summary = detector.get_summary(df)

    print("\n統計サマリー:")
    for key, value in summary.items():
        if isinstance(value, float):
            print(f"  {key:20s}: {value:.2f}")
        else:
            print(f"  {key:20s}: {value}")


def demo_real_world_scenario():
    """実用シナリオ: ブログ記事の書き癖チェック"""
    print("\n" + "=" * 60)
    print("6. 実用例: ブログ記事の書き癖チェック")
    print("=" * 60)

    blog_text = """
    今日は良い天気だった。本当に良い天気だった。
    それは素晴らしい体験である。それは忘れられない体験である。
    しかし問題もある。しかし課題もある。
    そして考える必要がある。そして行動する必要がある。
    これは重要なポイントである。これは大切なポイントである。
    とても面白い内容だった。とても興味深い内容だった。
    本当に良かった。本当に楽しかった。
    """ * 15

    detector = WritingHabitDetector(min_count=10, max_pmi=3.0, top_n=5)
    df = detector.detect(blog_text)

    print("\nブログ記事の書き癖（改善推奨）:")
    detector.visualize_top_habits(df)

    print("\n[改善アドバイス]")
    if not df.empty:
        top_habit = df.iloc[0]
        print(f"  最も強い書き癖: 「{top_habit['phrase']}」")
        print(f"  -> この表現を別の言い回しに変えると、文章のバリエーションが増えます")


def demo_parameter_tuning():
    """パラメータチューニングの例"""
    print("\n" + "=" * 60)
    print("7. パラメータチューニング")
    print("=" * 60)

    text = """
    それは良い。それは悪い。それは普通。
    これは面白い。これは素晴らしい。
    """ * 30

    print("\n【緩い設定】min_count=5, max_pmi=5.0")
    detector_loose = WritingHabitDetector(min_count=5, max_pmi=5.0)
    df_loose = detector_loose.detect(text)
    print(f"検出数: {len(df_loose)}個")

    print("\n【通常設定】min_count=10, max_pmi=3.0")
    detector_normal = WritingHabitDetector(min_count=10, max_pmi=3.0)
    df_normal = detector_normal.detect(text)
    print(f"検出数: {len(df_normal)}個")

    print("\n【厳しい設定】min_count=20, max_pmi=2.0")
    detector_strict = WritingHabitDetector(min_count=20, max_pmi=2.0)
    df_strict = detector_strict.detect(text)
    print(f"検出数: {len(df_strict)}個")


def demo_export_csv():
    """CSV出力の例"""
    print("\n" + "=" * 60)
    print("8. CSV出力")
    print("=" * 60)

    text = """
    それは良い。それは悪い。それは普通。
    これは面白い。これは素晴らしい。
    """ * 30

    detector = WritingHabitDetector(min_count=10, max_pmi=3.0)
    df = detector.detect(text)

    output_path = "writing_habits.csv"
    detector.export_csv(df, output_path)
    print(f"\n結果を {output_path} に保存しました")


if __name__ == '__main__':
    # 全デモを実行
    demo_basic_detection()
    demo_novel_analysis()
    demo_category_detection()
    demo_author_comparison()
    demo_summary_stats()
    demo_real_world_scenario()
    demo_parameter_tuning()

    # CSV出力はコメントアウト（実際のファイル作成を避けるため）
    # demo_export_csv()

    print("\n" + "=" * 60)
    print("デモ終了")
    print("=" * 60)

    print("\n[使い方のヒント]")
    print("  1. min_count を調整して、検出する頻度の閾値を変更")
    print("  2. max_pmi を調整して、PMIの上限を変更")
    print("  3. top_n で上位N個のみ取得")
    print("  4. カテゴリ別検出で詳細な分析")
    print("  5. 著者比較で書き癖の違いを可視化")
