# coding:utf-8
"""
パラメータ最適化のデモ
"""

from japhrase import PhraseExtracter, UnsupervisedOptimizer
from japhrase.datasource import WikipediaSource, TextFileSource

def demo_optimization_with_wikipedia():
    """Wikipedia データで最適化デモ"""
    print("=" * 60)
    print("Wikipedia データでパラメータ最適化デモ")
    print("=" * 60)

    # Wikipediaからデータ取得
    print("\n1. Wikipediaからデータ取得中...")
    source = WikipediaSource()
    texts = source.fetch_pages([
        "機械学習",
        "深層学習",
        "自然言語処理",
        "人工知能",
        "ニューラルネットワーク"
    ])

    print(f"   取得したページ数: {len(texts)}")
    print(f"   総文字数: {sum(len(t) for t in texts):,} 文字")

    # パラメータ最適化
    print("\n2. パラメータ最適化中...")
    optimizer = UnsupervisedOptimizer(
        param_grid={
            'min_count': [3, 5],
            'max_length': [10, 15],
            'threshold_originality': [0.5, 0.7]
        },
        verbose=1
    )

    best_params, results = optimizer.optimize(texts, method='grid')

    # 結果表示
    print("\n" + "=" * 60)
    print("最適化結果")
    print("=" * 60)
    print(f"\n最適パラメータ:")
    for key, value in best_params.items():
        print(f"  {key}: {value}")

    print(f"\n全実験結果:")
    for i, result in enumerate(results, 1):
        print(f"  [{i}] Score: {result['score']:.4f}, Phrases: {result['n_phrases']}")
        print(f"      Params: {result['params']}")

    # 最適パラメータで実行
    print("\n3. 最適パラメータで抽出実行...")
    extractor = PhraseExtracter(verbose=0, **best_params)
    df = extractor.get_dfphrase(texts)

    print(f"\n抽出されたフレーズ（上位10件）:")
    print(df.head(10)[['seqchar', 'freq', 'length']])


def demo_optimization_with_local_file():
    """ローカルファイルで最適化デモ"""
    print("=" * 60)
    print("ローカルファイルでパラメータ最適化デモ")
    print("=" * 60)

    # サンプルテキスト生成
    sample_texts = [
        "機械学習は人工知能の一分野です。" * 5,
        "深層学習はニューラルネットワークを用いた学習方法です。" * 5,
        "自然言語処理は言語を扱う技術です。" * 5,
        "データサイエンスでは統計学と機械学習が重要です。" * 5,
        "Python は機械学習でよく使われるプログラミング言語です。" * 5,
    ]

    print(f"\nサンプルテキスト数: {len(sample_texts)}")

    # 小規模な最適化
    print("\n最適化実行中...")
    optimizer = UnsupervisedOptimizer(
        param_grid={
            'min_count': [2, 3, 4],
            'max_length': [10, 15],
        },
        verbose=0
    )

    best_params, results = optimizer.optimize(sample_texts, method='random', n_iterations=5)

    print(f"\n最適パラメータ: {best_params}")
    print(f"最高スコア: {max(r['score'] for r in results):.4f}")


def demo_evaluator():
    """評価器のデモ"""
    print("=" * 60)
    print("評価器デモ")
    print("=" * 60)

    from japhrase.evaluation import UnsupervisedEvaluator

    # サンプルデータ
    texts = ["機械学習はAIの一分野です"] * 10
    phrases = ["機械学習", "一分野"]

    evaluator = UnsupervisedEvaluator()

    # 詳細スコア取得
    scores = evaluator.get_detailed_scores(phrases, texts)

    print("\n評価スコア:")
    for metric, score in scores.items():
        print(f"  {metric}: {score:.4f}")


if __name__ == "__main__":
    print("\n" + "=" * 60)
    print("jphrase パラメータ最適化デモ")
    print("=" * 60)

    # デモ選択
    import sys

    if len(sys.argv) > 1:
        demo_type = sys.argv[1]
    else:
        demo_type = "local"

    if demo_type == "wikipedia":
        demo_optimization_with_wikipedia()
    elif demo_type == "local":
        demo_optimization_with_local_file()
    elif demo_type == "eval":
        demo_evaluator()
    else:
        print("\n使用方法:")
        print("  python optimization_demo.py [wikipedia|local|eval]")
        print("\nデフォルト: local")
        demo_optimization_with_local_file()

    print("\n" + "=" * 60)
    print("デモ終了")
    print("=" * 60)
