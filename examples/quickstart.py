#!/usr/bin/env python3
# coding: utf-8
"""
japhrase クイックスタート

pip install japhrase した後、このスクリプトを動かせば全機能の動作を確認できる。
コア依存（numpy, pandas, scipy）だけで動く。optional は不要。

    python examples/quickstart.py
"""


def main():
    print("=" * 65)
    print("japhrase クイックスタート")
    print("=" * 65)

    # ─── 1. フレーズ抽出 ───────────────────────────────
    print("\n■ 1. フレーズ抽出（コア機能）")
    print("-" * 50)

    from japhrase import PhraseExtracter

    sentences = [
        'ChatGPTの登場により生成AIが注目を集めている。',
        '生成AIはテキストだけでなく画像生成にも使われる。',
        '大規模言語モデルは生成AIの中核技術である。',
        '生成AIの倫理的課題について議論が活発化している。',
        '企業における生成AIの導入事例が増加している。',
        '生成AIを活用した業務効率化が進んでいる。',
        '生成AIの精度向上には大量のデータが必要である。',
        '生成AIと著作権の問題は未解決のままである。',
        'プロンプトエンジニアリングは生成AI活用の鍵となる。',
        'オープンソースの大規模言語モデルが相次いで公開されている。',
    ] * 5

    ext = PhraseExtracter(min_count=2, max_length=10, min_length=2, verbose=0)
    df = ext.extract(sentences)
    print(f"  入力: {len(sentences)}文")
    print(f"  抽出: {len(df)}フレーズ")
    if not df.empty:
        for _, row in df.head(5).iterrows():
            print(f"    {row['seqchar']:<16s} freq={int(row['freq'])}")

    # ─── 2. テキスト類似度 ─────────────────────────────
    print("\n■ 2. テキスト類似度")
    print("-" * 50)

    from japhrase import StylometryAnalyzer

    text_a = '朝靄の中を歩いていると、足元で小さな花が揺れた。名前は知らない。淡い紫色の花弁が、露を含んで光っている。'
    text_b = '男は歩いた。男は止まった。男はまた歩いた。男は見た。男は聞いた。男は歩いた。男は止まった。'

    stylo = StylometryAnalyzer()
    adv_a = stylo.analyze_advanced_diversity(text_a)
    adv_b = stylo.analyze_advanced_diversity(text_b)
    print(f"  テキストA: Hapax比={adv_a['hapax_ratio']:.3f}  Simpson={adv_a['simpsons_d']:.4f}  → {adv_a['assessment']}")
    print(f"  テキストB: Hapax比={adv_b['hapax_ratio']:.3f}  Simpson={adv_b['simpsons_d']:.4f}  → {adv_b['assessment']}")

    # ─── 3. 分布比較 ──────────────────────────────────
    print("\n■ 3. 分布比較（2テキスト間の語彙の違い）")
    print("-" * 50)

    from japhrase import DistributionComparator
    from collections import Counter

    comp = DistributionComparator()
    freq_a = Counter({'騎士': 10, '剣': 8, '王城': 5, '訓練': 6})
    freq_b = Counter({'研究': 12, '実験': 9, 'データ': 7, '端末': 5})
    result = comp.compare(freq_a, freq_b)
    print(f"  JS距離: {result.jsd:.4f}  (0=同一, 1=完全に別)")
    print(f"  共有語彙: {result.shared_terms}  キーネス結果: {len(result.keyness_results)}件")

    # ─── 4. コロケーション分析 ─────────────────────────
    print("\n■ 4. コロケーション分析（語の結びつきの強さ）")
    print("-" * 50)

    from japhrase import CollocationScorer

    scorer = CollocationScorer()
    cs = scorer.score_phrase('生成AI', 45, '生成AIが注目されている。' * 50)
    print(f"  「生成AI」:")
    print(f"    PMI={cs.pmi:.2f}  t-score={cs.t_score:.2f}  Log-Dice={cs.log_dice:.2f}")

    # ─── 5. テキスト複雑度 ─────────────────────────────
    print("\n■ 5. テキスト複雑度")
    print("-" * 50)

    from japhrase import ComplexityAnalyzer

    cx = ComplexityAnalyzer()
    result = cx.analyze(text_a)
    print(f"  パープレキシティ: {result['perplexity']:.2f}")
    print(f"  圧縮率: {result['compression_ratio']:.4f}")
    print(f"  語彙密度: {result['lexical_density']:.4f}")
    print(f"  情報率: {result['information_rate']:.4f}")

    # ─── 6. 汚染検出 ──────────────────────────────────
    print("\n■ 6. 汚染検出（8軸）")
    print("-" * 50)

    from japhrase.contamination import scan, quick_check

    clean = '正常なテキスト。問題のない文章。きれいな日本語。'
    dirty = '正常â€™文字化け。\n「閉じない台詞\n出来ることをできる限りやる。'

    print(f"  クリーンなテキスト: quick_check → {quick_check(clean)}")
    print(f"  汚染テキスト:       quick_check → {quick_check(dirty)}")

    profile = scan(dirty)
    print(f"  汚染スコア: {profile.overall}/100")
    for ax in profile.worst_axes[:3]:
        print(f"    {ax.name}: {ax.score}/100 ({ax.count}件)")

    # ─── 7. 公開前プリフライト ─────────────────────────
    print("\n■ 7. 公開前プリフライト")
    print("-" * 50)

    from japhrase.applied import PreflightChecker

    checker = PreflightChecker()
    result = checker.check(text_a * 10, lang='jp')
    print(f"  判定: {result.verdict}  スコア: {result.quality_score:.0f}/100")

    # ─── 完了 ─────────────────────────────────────────
    print("\n" + "=" * 65)
    print("全機能の動作確認完了。")
    print("各機能の詳細は help() で確認できます:")
    print("  help(PhraseExtracter)")
    print("  help(scan)")
    print("  help(StylometryAnalyzer)")
    print("=" * 65)


if __name__ == '__main__':
    main()
