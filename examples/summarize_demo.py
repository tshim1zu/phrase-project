"""
統計的要約エンジンの使用例

PhraseExtracterを活用した要約の実演
"""

from japhrase.summarizer import Summarizer
from japhrase.extracter import PhraseExtracter

text = """
人工知能（AI）の技術革新が急速に進んでいる。特にディープラーニングの登場以降、画像認識や自然言語処理の精度は飛躍的に向上した。
しかし、AIの進化には課題も多い。計算コストの増大や、ブラックボックス問題などが指摘されている。
一方、AIの活用範囲は拡大している。医療診断、自動運転、製造業の最適化など、様々な分野での応用が期待されている。
昨今の状況を鑑みますと、我々はAIと共存する道を模索する必要があると言えるのではないでしょうか。
そのためには、AIの透明性向上やセキュリティの強化が急務となっている。
"""

def demo_basic_summarization():
    """基本的な要約のデモ"""
    print("=" * 60)
    print("【デモ1】基本的な要約（30%圧縮）")
    print("=" * 60)
    
    # 1. 強力なエンジンを用意（PMI重視）
    extractor = PhraseExtracter(
        min_count=1,       # 要約なので1回出現でも重要語なら拾いたい
        min_length=2,      # 短いフレーズも対象にする
        use_pmi=True,      # 重要度判定のキモ
        pmi_weight=2.0     # PMIを重視
    )

    # 2. サマライザーを起動
    summarizer = Summarizer(extractor=extractor)

    # 3. 要約実行（30%に圧縮）
    summary = summarizer.summarize(text, ratio=0.3)
    print("\n【元のテキスト】")
    print(text)
    print("\n【要約結果】")
    print(summary)


def demo_compressed_summarization():
    """文圧縮を含む要約のデモ"""
    print("\n" + "=" * 60)
    print("【デモ2】文圧縮モード有効")
    print("=" * 60)
    
    extractor = PhraseExtracter(
        min_count=1,
        min_length=2,
        use_pmi=True,
        pmi_weight=2.0
    )

    summarizer = Summarizer(
        extractor=extractor,
        compression_threshold=0.1  # スコア0.1以上の重要フレーズを残す
    )

    # 圧縮モードを有効化
    summary_compressed = summarizer.summarize(
        text, 
        ratio=0.3, 
        compress=True
    )
    print("\n【圧縮要約結果】")
    print(summary_compressed)


def demo_top_n_summarization():
    """特定数の文を抽出するデモ"""
    print("\n" + "=" * 60)
    print("【デモ3】上位2文の抽出")
    print("=" * 60)
    
    extractor = PhraseExtracter(
        min_count=1,
        min_length=2,
        use_pmi=True,
        pmi_weight=2.0
    )

    summarizer = Summarizer(extractor=extractor)

    # top_nで文の数を指定
    summary_top2 = summarizer.summarize(text, top_n=2)
    print("\n【上位2文の抽出】")
    print(summary_top2)


if __name__ == "__main__":
    demo_basic_summarization()
    demo_compressed_summarization()
    demo_top_n_summarization()
