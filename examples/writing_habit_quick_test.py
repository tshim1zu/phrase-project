"""
WritingHabitDetector クイックテスト
"""

from japhrase.writing_habit_detector import WritingHabitDetector


def main():
    print("=" * 60)
    print("WritingHabitDetector クイックテスト")
    print("=" * 60)

    # 高頻度×低PMIのテキスト
    text = """
    それは良い。それは悪い。それは普通。それは面白い。
    それは素晴らしい。それは微妙。それは最高。それは最悪。
    それは楽しい。それは退屈。それは興味深い。それは重要。
    これは良い。これは悪い。これは普通。これは面白い。
    あれは良い。あれは悪い。あれは普通。
    """ * 20

    print("\n[テキスト統計]")
    print(f"  行数: {len(text.split())}")
    print(f"  文字数: {len(text)}")

    print("\n[検出中...]")
    detector = WritingHabitDetector(
        min_count=10,
        max_pmi=5.0,
        top_n=10,
        verbose=True
    )
    df = detector.detect(text)

    print("\n[検出結果]")
    if not df.empty:
        print(df.to_string(index=False))

        print("\n[サマリー統計]")
        summary = detector.get_summary(df)
        for key, value in summary.items():
            if isinstance(value, float):
                print(f"  {key:20s}: {value:.2f}")
            else:
                print(f"  {key:20s}: {value}")

        print("\n[上位5個の書き癖]")
        detector.visualize_top_habits(df, top_n=5)
    else:
        print("書き癖が検出されませんでした")

    print("\n" + "=" * 60)
    print("テスト完了")
    print("=" * 60)


if __name__ == '__main__':
    main()
