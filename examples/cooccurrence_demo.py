"""
共起語分析デモ

CooccurrenceAnalyzerの使用例。
特定の単語の周辺に出現する特徴語を統計的に抽出する。
"""

from japhrase.cooccurrence import CooccurrenceAnalyzer
from japhrase.extracter import PhraseExtracter

# 分析対象のテキスト
text = """
人工知能（AI）の技術革新が急速に進んでいる。特にディープラーニングの登場以降、画像認識や自然言語処理の精度は飛躍的に向上した。
AIは医療診断にも応用されている。医療現場ではAIが医師の診断を支援する役割を果たしている。医療画像解析ではAIの精度が飛躍的に向上している。
医療業界はAI導入により大きく変わろうとしている。医療データの活用もAIの発展と共に進んでいる。
しかし、AIの進化には課題も多い。計算コストの増大や、ブラックボックス問題などが指摘されている。
AIの透明性向上のための研究も進められている。AIの説明可能性は重要な課題である。
一方、AIの活用範囲は拡大している。医療診断、自動運転、製造業の最適化など、様々な分野でのAI応用が期待されている。
自動運転技術ではAIが中核をなしている。自動運転の実現にはAIの安全性向上が不可欠である。
AIセキュリティも重要なテーマだ。AIシステムへの攻撃防止が急務となっている。
昨今の状況を鑑みますと、我々はAIと共存する道を模索する必要があると言えるのではないでしょうか。
そのためには、AIの透明性向上やセキュリティの強化が急務となっている。
AIと人間の協働体制を整備する必要がある。
課題として指摘されているのは、AIの倫理的な問題である。多くの課題がAIの発展に伴って顕在化している。
課題の解決には技術的な工夫とともに、社会的な合意形成が必要である。
"""

def demo_character_profiling():
    """キャラクター分析の例（テキストが多くないので文字列は簡略版）"""
    print("=" * 70)
    print("【デモ1】キーワード「AI」の周辺分析（特徴語抽出）")
    print("=" * 70)
    
    analyzer = CooccurrenceAnalyzer(
        window_size=40,      # 前後40文字（狭めに）
        min_cooccurrence=1   # 1回以上の共起で考慮
    )
    # PhraseExtracterを明示的に設定して、パラメータを細かく制御
    from japhrase import PhraseExtracter
    extractor = PhraseExtracter(min_count=1, min_length=2, use_pmi=True)
    analyzer = CooccurrenceAnalyzer(
        extractor=extractor,
        window_size=40
    )
    
    df = analyzer.analyze(text, "AI", top_n=15)
    
    if not df.empty:
        print("\n【特徴語ランキング（Lift値ベース）】")
        print("Lift値が高いほど、その言葉は「AI」の周辺に特異的に出現しています。")
        print()
        print(df.to_string(index=False))
        print("\n【解釈】")
        print("- freq: 周辺での出現頻度")
        print("- lift: 全体での出現率の何倍の頻度で出現するか（1.0より大きい＝特異的）")
        print("- score: Lift値と頻度を考慮した総合スコア")
    else:
        print("分析結果なし")


def demo_product_review_analysis():
    """製品評価テキストの分析例"""
    print("\n" + "=" * 70)
    print("【デモ2】広いコンテキスト（前後2文分）での分析")
    print("=" * 70)
    
    # より広いウィンドウで「関連トピック」をキャッチ
    from japhrase import PhraseExtracter
    extractor = PhraseExtracter(min_count=1, min_length=2, use_pmi=True)
    analyzer = CooccurrenceAnalyzer(
        extractor=extractor,
        window_size=100      # 前後100文字（広め）
    )
    
    df = analyzer.analyze(text, "医療", top_n=10)
    
    if not df.empty:
        print("\n【「医療」の関連語分析】")
        print("広いウィンドウで、医療関連の話題全体を把握できます。")
        print()
        print(df.to_string(index=False))
    else:
        print("「医療」の出現が少ないため、分析対象外です。")


def demo_narrow_context():
    """狭いコンテキストでの修飾語抽出"""
    print("\n" + "=" * 70)
    print("【デモ3】狭いコンテキスト（修飾語）での分析")
    print("=" * 70)
    
    from japhrase import PhraseExtracter
    extractor = PhraseExtracter(min_count=1, min_length=2, use_pmi=True)
    analyzer = CooccurrenceAnalyzer(
        extractor=extractor,
        window_size=15       # 前後15文字（狭い）
    )
    
    df = analyzer.analyze(text, "課題", top_n=10)
    
    if not df.empty:
        print("\n【「課題」を修飾する言葉】")
        print("狭いウィンドウで直接的な修飾語や説明語をキャッチします。")
        print()
        print(df.to_string(index=False))
    else:
        print("「課題」の周辺フレーズが見つかりません。")


if __name__ == "__main__":
    demo_character_profiling()
    demo_product_review_analysis()
    demo_narrow_context()
