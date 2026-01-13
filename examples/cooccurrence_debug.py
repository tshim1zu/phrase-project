"""
共起分析の検証スクリプト

CooccurrenceAnalyzerの内部動作を確認し、
分析方法が統計的に正当かどうかを検証する。
"""

from japhrase.cooccurrence import CooccurrenceAnalyzer
from japhrase.extracter import PhraseExtracter

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

def debug_context_extraction():
    """ステップ1: コンテキスト抽出の検証"""
    print("=" * 70)
    print("【検証1】「課題」の周辺コンテキスト抽出")
    print("=" * 70)
    
    analyzer = CooccurrenceAnalyzer(window_size=30)
    contexts = analyzer.extract_context(text, "課題")
    
    print(f"\n出現回数: {len(contexts)}")
    print("\n【抽出されたコンテキスト一覧】")
    for i, ctx in enumerate(contexts, 1):
        print(f"\n{i}. {repr(ctx)}")
        print(f"   長さ: {len(ctx)}文字")


def debug_phrase_extraction():
    """ステップ2: コンテキスト内のフレーズ抽出検証"""
    print("\n" + "=" * 70)
    print("【検証2】全体コーパスでのフレーズ抽出")
    print("=" * 70)
    
    extractor = PhraseExtracter(min_count=1, min_length=2, use_pmi=True)
    df_global = extractor.extract([text])
    
    print(f"\n抽出されたフレーズ数: {len(df_global)}")
    print("\n【全体コーパスの上位フレーズ（freq順）】")
    print(df_global[['seqchar', 'freq', 'sc_index']].head(15).to_string(index=False))
    
    print("\n\n【「課題」関連フレーズの検索】")
    df_task = df_global[df_global['seqchar'].str.contains('課題', na=False)]
    if not df_task.empty:
        print(df_task[['seqchar', 'freq', 'sc_index']].to_string(index=False))
    else:
        print("「課題」を含むフレーズなし")


def debug_local_phrase_extraction():
    """ステップ3: 「課題」周辺でのフレーズ抽出検証"""
    print("\n" + "=" * 70)
    print("【検証3】「課題」周辺コンテキストでのフレーズ抽出")
    print("=" * 70)
    
    analyzer = CooccurrenceAnalyzer(window_size=30)
    contexts = analyzer.extract_context(text, "課題")
    
    print(f"コンテキスト数: {len(contexts)}")
    print(f"\nコンテキストの結合長: {sum(len(c) for c in contexts)}文字")
    
    extractor = PhraseExtracter(min_count=1, min_length=2, use_pmi=True)
    df_local = extractor.extract(contexts)
    
    if df_local.empty:
        print("\n⚠️ フレーズが抽出されませんでした")
        print("\n原因の可能性:")
        print("1. コンテキストが短すぎるか少なすぎる")
        print("2. min_countまたはmin_lengthの設定が厳しすぎる")
    else:
        print(f"\n抽出されたフレーズ数: {len(df_local)}")
        print("\n【周辺コンテキストの上位フレーズ】")
        print(df_local[['seqchar', 'freq', 'sc_index']].head(10).to_string(index=False))


def debug_lift_calculation():
    """ステップ4: Lift値の計算検証"""
    print("\n" + "=" * 70)
    print("【検証4】Lift値の計算プロセス")
    print("=" * 70)
    
    analyzer = CooccurrenceAnalyzer(window_size=40)
    
    # 全体フレーズの確率
    extractor = PhraseExtracter(min_count=1, min_length=2, use_pmi=True)
    df_global = extractor.extract([text])
    
    total_freq_global = df_global['freq'].sum()
    
    # 周辺フレーズの確率
    contexts = analyzer.extract_context(text, "課題")
    df_local = extractor.extract(contexts)
    
    if df_local.empty:
        print("周辺フレーズが抽出されないため計算できません")
        return
    
    total_freq_local = df_local['freq'].sum()
    
    print(f"\n全体コーパス統計:")
    print(f"  総フレーズ数: {len(df_global)}")
    print(f"  総出現度数: {total_freq_global}")
    
    print(f"\n周辺コンテキスト統計:")
    print(f"  総フレーズ数: {len(df_local)}")
    print(f"  総出現度数: {total_freq_local}")
    
    # いくつかのフレーズで具体的に計算
    print(f"\n【具体例：Lift値の計算】")
    for _, row in df_local.head(5).iterrows():
        phrase = row['seqchar']
        freq_local = row['freq']
        
        prob_local = freq_local / total_freq_local
        
        freq_global = df_global[df_global['seqchar'] == phrase]['freq'].values
        if len(freq_global) > 0:
            prob_global = freq_global[0] / total_freq_global
            lift = prob_local / prob_global
            print(f"\n'{phrase}':")
            print(f"  周辺での確率: {prob_local:.6f}")
            print(f"  全体での確率: {prob_global:.6f}")
            print(f"  Lift値: {lift:.2f}倍")
        else:
            print(f"\n'{phrase}': 全体にはなし（周辺だけ出現）→ Lift = 無限大")


def recommend_parameters():
    """パラメータ推奨"""
    print("\n" + "=" * 70)
    print("【推奨】パラメータの最適化")
    print("=" * 70)
    
    print("""
分析が「正当」かどうかは、以下の点で判定します：

✅ 統計的に正当な条件:
  1. コンテキストサンプル数 ≥ 3
     → 少なくとも3回の共起を確認してLift値を計算
  
  2. 周辺フレーズ出現数 ≥ min_cooccurrence
     → 稀な出現（偶然の一致）をノイズとして除外
  
  3. Lift > 1.0
     → その言葉が「偶然より有意に」多く出現している証拠
  
  4. スコア = Lift値 × log(頻度)
     → 稀な語の過大評価を対数で補正

⚠️ 問題がある場合の対処:
  - フレーズが見つからない
    → window_sizeを大きくする（20 → 50 → 100）
    → min_countを下げる（2 → 1）
    → min_lengthを下げる（3 → 2）
    
  - Lift値が異常に高い（100万倍など）
    → 実は「稀な語」であり、サンプルが少ない
    → min_cooccurrenceを上げて、より確実な共起を見る
""")


if __name__ == "__main__":
    debug_context_extraction()
    debug_phrase_extraction()
    debug_local_phrase_extraction()
    debug_lift_calculation()
    recommend_parameters()
