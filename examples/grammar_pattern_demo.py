"""
GrammarPatternExtractor デモ

文法パターン（「〜だった」「〜していた」など）の抽出と
PMI分析を組み合わせた実用例を示します。
"""

from japhrase.grammar_pattern_extractor import GrammarPatternExtractor, DEFAULT_GRAMMAR_PATTERNS


def demo_basic_extraction():
    """基本的な抽出のデモ"""
    print("=" * 60)
    print("1. 基本的な文法パターン抽出")
    print("=" * 60)

    text = """
    彼は公園を走っていた。毎朝走っていた。
    天気が良かった。昨日も良かった。一昨日も良かった。
    それは面白いである。本当に面白いである。
    彼女は笑っていた。ずっと笑っていた。
    """

    extractor = GrammarPatternExtractor(min_count=2)
    df = extractor.extract(text)

    print("\n抽出結果:")
    print(df.to_string(index=False))


def demo_pattern_filtering():
    """特定パターンのみ抽出するデモ"""
    print("\n" + "=" * 60)
    print("2. 特定パターンのみ抽出（過去形のみ）")
    print("=" * 60)

    text = """
    彼は走っていた。彼女も走っていた。
    犬が吠えていた。猫も吠えていた。
    雨が降っていた。雪も降っていた。
    """ * 3

    extractor = GrammarPatternExtractor(min_count=3)

    # 「〜ていた」パターンのみ抽出
    df = extractor.extract(text, pattern_names=['progressive_teita'])

    print("\n「〜ていた」パターンの抽出結果:")
    print(df.to_string(index=False))


def demo_low_pmi_phrases():
    """低PMIフレーズ（文法的に固定だが意味的結合が弱い）のデモ"""
    print("\n" + "=" * 60)
    print("3. 低PMIフレーズの抽出")
    print("=" * 60)

    text = """
    それは良いである。これは悪いである。
    あれは正しいである。どれも間違いである。
    彼は学生である。彼女も学生である。
    """ * 3

    extractor = GrammarPatternExtractor(min_count=2)

    # 低PMIフレーズ（意味的結合が弱い）
    df_low = extractor.extract_low_pmi(text, max_pmi=2.0)

    print("\n低PMIフレーズ（文法的には固定、意味的には弱い）:")
    if not df_low.empty:
        print(df_low[['phrase', 'count', 'pmi', 'pattern']].to_string(index=False))
    else:
        print("該当なし")


def demo_high_pmi_phrases():
    """高PMIフレーズ（意味的に強く結合）のデモ"""
    print("\n" + "=" * 60)
    print("4. 高PMIフレーズの抽出")
    print("=" * 60)

    text = """
    速く走っていた。速く走っていた。速く走っていた。
    ゆっくり歩いていた。ゆっくり歩いていた。ゆっくり歩いていた。
    静かに考えていた。静かに考えていた。静かに考えていた。
    """ * 2

    extractor = GrammarPatternExtractor(min_count=3)

    # 高PMIフレーズ（意味的に強く結合）
    df_high = extractor.extract_high_pmi(text, min_pmi=1.0)

    print("\n高PMIフレーズ（意味的に強く結合）:")
    if not df_high.empty:
        print(df_high[['phrase', 'count', 'pmi', 'pattern']].to_string(index=False))
    else:
        print("該当なし")


def demo_pattern_summary():
    """パターン別サマリーのデモ"""
    print("\n" + "=" * 60)
    print("5. パターン別サマリー")
    print("=" * 60)

    text = """
    走っていた。歩いていた。考えていた。
    良かった。悪かった。楽しかった。
    面白いである。素晴らしいである。
    走っている。歩いている。
    """ * 5

    extractor = GrammarPatternExtractor(min_count=3)
    df = extractor.extract(text)
    summary = extractor.get_pattern_summary(df)

    print("\nパターン別統計:")
    print(summary.to_string(index=False))


def demo_ranking():
    """ランキングのデモ"""
    print("\n" + "=" * 60)
    print("6. PMI/頻度ランキング")
    print("=" * 60)

    text = """
    速く走っていた。速く走っていた。速く走っていた。
    走っていた。走っていた。走っていた。走っていた。走っていた。
    ゆっくり歩いていた。ゆっくり歩いていた。
    """ * 2

    extractor = GrammarPatternExtractor(min_count=3)
    df = extractor.extract(text)

    print("\nPMIランキング（意味的結合度順）:")
    df_pmi = extractor.rank_by_pmi(df)
    print(df_pmi[['phrase', 'pmi', 'count']].head(5).to_string(index=False))

    print("\n頻度ランキング（出現回数順）:")
    df_freq = extractor.rank_by_frequency(df)
    print(df_freq[['phrase', 'count', 'pmi']].head(5).to_string(index=False))


def demo_custom_patterns():
    """カスタムパターンのデモ"""
    print("\n" + "=" * 60)
    print("7. カスタム文法パターンの定義")
    print("=" * 60)

    # カスタムパターンを定義
    custom_patterns = {
        'desiderative': r'(\S{1,15}たい)',  # 「〜したい」
        'negative_desire': r'(\S{1,15}たくない)',  # 「〜したくない」
        'hearsay': r'(\S{1,15}そうだ)',  # 「〜そうだ」（伝聞）
    }

    text = """
    旅行に行きたい。海外に行きたい。
    働きたくない。起きたくない。
    雨が降りそうだ。明日も降りそうだ。
    """ * 3

    extractor = GrammarPatternExtractor(
        patterns=custom_patterns,
        min_count=2
    )
    df = extractor.extract(text)

    print("\nカスタムパターンの抽出結果:")
    print(df[['phrase', 'pattern', 'count', 'pmi']].to_string(index=False))


def demo_group_by_pattern():
    """パターン別グループ化のデモ"""
    print("\n" + "=" * 60)
    print("8. パターン別グループ化")
    print("=" * 60)

    text = """
    走っていた。歩いていた。
    良かった。悪かった。
    面白いである。素晴らしいである。
    """ * 5

    extractor = GrammarPatternExtractor(min_count=3)
    df = extractor.extract(text)
    grouped = extractor.group_by_pattern(df)

    for pattern_name, group_df in grouped.items():
        print(f"\n【{pattern_name}】")
        print(group_df[['phrase', 'count', 'pmi']].to_string(index=False))


def demo_real_world_novel():
    """実用例: 小説テキストの分析"""
    print("\n" + "=" * 60)
    print("9. 実用例: 小説風テキストの分析")
    print("=" * 60)

    novel_text = """
    彼は静かに考えていた。何を考えていたのかは誰も知らなかった。
    外では雨が降っていた。激しく降っていた。
    彼女は部屋で本を読んでいた。ずっと読んでいた。
    時計の音が聞こえていた。カチカチと聞こえていた。
    窓の外を見ていた。ぼんやりと見ていた。
    """ * 3

    extractor = GrammarPatternExtractor(min_count=3)
    df = extractor.extract(novel_text, pattern_names=['progressive_teita'])

    print("\n小説から抽出された「〜ていた」パターン:")
    print(df[['phrase', 'count', 'pmi', 'score']].to_string(index=False))

    print("\nPMIランキング（意味的に結びつきが強い表現）:")
    df_pmi = extractor.rank_by_pmi(df)
    print(df_pmi[['phrase', 'pmi']].head(3).to_string(index=False))


def demo_available_patterns():
    """利用可能なデフォルトパターンの一覧"""
    print("\n" + "=" * 60)
    print("10. 利用可能なデフォルト文法パターン")
    print("=" * 60)

    print("\nパターン名 → 正規表現:")
    for name, pattern in DEFAULT_GRAMMAR_PATTERNS.items():
        print(f"  {name:20s} → {pattern}")


if __name__ == '__main__':
    # 全デモを実行
    demo_basic_extraction()
    demo_pattern_filtering()
    demo_low_pmi_phrases()
    demo_high_pmi_phrases()
    demo_pattern_summary()
    demo_ranking()
    demo_custom_patterns()
    demo_group_by_pattern()
    demo_real_world_novel()
    demo_available_patterns()

    print("\n" + "=" * 60)
    print("デモ終了")
    print("=" * 60)
