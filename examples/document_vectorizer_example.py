"""
DocumentVectorizer の使用例

NMFベースのドキュメント ベクトル化と差分分析の実装例です。
"""

import sys
import io
from pathlib import Path

# Windows環境でのUTF-8出力対応
if sys.platform == "win32":
    sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
    sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8')

# japhraseをインポート
sys.path.insert(0, str(Path(__file__).parent.parent))

from japhrase import DocumentVectorizer


def example_semantic_analysis():
    """例1: 意味的な差を分析（TF-IDF モード）"""
    print("=" * 70)
    print("例1: 意味的な差を分析（TF-IDF モード）")
    print("=" * 70)

    # サンプルテキスト
    texts = [
        """
        機械学習は人工知能の一分野で、データから自動的にパターンを学習します。
        深層学習はニューラルネットワークを用いた機械学習の手法です。
        自然言語処理は人間の言語を計算機で処理する技術です。
        テキスト分類は自然言語処理の重要な応用分野です。
        """,
        """
        データサイエンスはビジネスの意思決定に重要です。
        統計分析はデータから有用な情報を抽出します。
        ビジネスインテリジェンスは企業の競争力を高めます。
        顧客分析はマーケティング戦略の基本です。
        """,
        """
        プログラミングは現代の技術スキルです。
        ソフトウェア開発はチームで進められます。
        バグ修正はシステム品質の維持に必要です。
        テストは品質保証の重要なプロセスです。
        """
    ]

    # DocumentVectorizerを作成
    vectorizer = DocumentVectorizer(
        n_topics=5,
        feature_mode='tfidf',
        verbose=1
    )

    # ベクトル化
    result = vectorizer.from_texts(
        texts,
        labels=['AI_ML', 'Business', 'Programming']
    )

    print(f"\n✅ ベクトル化完了: {result.document_topic_matrix.shape}")

    # 各トピックの上位タームを表示
    print("\n📊 各トピックの上位ターム:")
    for topic_idx in range(result.topic_term_matrix.shape[0]):
        top_terms = result.get_topic_terms(topic_idx, n_terms=5)
        terms_str = ', '.join([f"{term}" for term, _ in top_terms])
        print(f"  Topic {topic_idx}: {terms_str}")

    # ドキュメント-トピック行列を表示
    print("\n📋 ドキュメント-トピック行列（正規化）:")
    df = result.to_dataframe(matrix_type='normalized')
    print(df.round(3))

    # ペアワイズ距離を計算
    print("\n📏 ドキュメント間の距離:")
    distances = vectorizer.calculate_pairwise_distances(result)
    print(distances.round(3))

    # 最初のドキュメントとの差分を計算
    print("\n🔄 差分分析（AI_ML を基準）:")
    diffs = vectorizer.calculate_differences(result, 0, [1, 2])
    print(diffs.round(3))


def example_stylistic_analysis():
    """例2: 文体・手癖の差を分析（低PMI モード）"""
    print("\n" + "=" * 70)
    print("例2: 文体・手癖の差を分析（低PMI モード）")
    print("=" * 70)

    # より長いサンプルテキスト
    text1 = """
    これはとても重要な文献です。本当に必要な情報が含まれています。
    実際のところ、この方法は非常に効果的です。実際のところ、多くの人が使用しています。
    実は、この技術は革新的です。実は、世界中で採用されています。
    要するに、これは優れたアプローチです。とても良い結果が得られました。
    実のところ、効果は確認されています。実のところ、多くの事例があります。
    """ * 3  # 頻度を高める

    text2 = """
    以下の内容をご説明させていただきたく存じます。
    まずもって、本件につきましてはご報告申し上げます。
    さらに申し上げますと、当該事項につきましては周知の通りです。
    念のため申し上げますと、本点は重要なポイントです。
    以上のとおり、本件に関しましてはご理解いただきたく存じます。
    """ * 3  # 頻度を高める

    texts = [text1, text2]

    # DocumentVectorizerを作成（低PMI モード）
    vectorizer = DocumentVectorizer(
        n_topics=3,
        feature_mode='low_pmi',
        pmi_threshold=3.0,
        min_count=6,
        verbose=1
    )

    print("\n📖 低PMI フレーズを抽出中（手癖・習慣的表現）...")

    # ベクトル化
    result = vectorizer.from_texts(
        texts,
        labels=['文体_A', '文体_B']
    )

    print(f"✅ ベクトル化完了: {result.document_topic_matrix.shape}")

    # 各トピックの上位タームを表示（文体的特徴）
    print("\n🎨 各トピックの上位ターム（文体的特徴）:")
    for topic_idx in range(result.topic_term_matrix.shape[0]):
        top_terms = result.get_topic_terms(topic_idx, n_terms=8)
        terms_str = ', '.join([f"{term}" for term, _ in top_terms])
        print(f"  Topic {topic_idx}: {terms_str}")

    # ドキュメント-トピック行列を表示
    print("\n📋 文体特性の分布:")
    df = result.to_dataframe(matrix_type='normalized')
    print(df.round(3))


def example_python_api():
    """例3: Python APIの包括的な例"""
    print("\n" + "=" * 70)
    print("例3: Python APIの包括的な例")
    print("=" * 70)

    # サンプルテキスト
    texts = [
        "自然言語処理は言語の意味を理解し、処理する技術です。テキストマイニング、感情分析、翻訳など、様々な応用があります。",
        "機械学習モデルのトレーニングには大量のデータが必要です。データの前処理が重要な役割を果たします。",
        "プログラミング言語の選択はプロジェクトの性質に依存します。Pythonは学習が容易で、実務でも人気があります。"
    ]

    # ベクトライザーの作成
    vectorizer = DocumentVectorizer(
        n_topics=3,
        feature_mode='tfidf',
        max_features=500,
        ngram_range=(2, 3),
        verbose=0
    )

    # ベクトル化の実行
    result = vectorizer.from_texts(texts, labels=['NLP', 'ML', 'Programming'])

    print("✅ ベクトル化完了")

    # 結果の検査
    print(f"\n📊 結果の構造:")
    print(f"  - ドキュメント数: {len(result.labels)}")
    print(f"  - トピック数: {result.document_topic_matrix.shape[1]}")
    print(f"  - 特徴数: {len(result.feature_names)}")
    print(f"  - メタデータ: {result.metadata}")

    # 個別ドキュメントのベクトルを取得
    print(f"\n🎯 個別ドキュメントのベクトル:")
    for i, label in enumerate(result.labels):
        vector = result.get_document_vector(i)
        print(f"  {label}: {vector.round(3)}")

    # トピック情報を取得
    print(f"\n📚 トピック情報:")
    for topic_idx in range(result.topic_term_matrix.shape[0]):
        top_terms = result.get_topic_terms(topic_idx, n_terms=3)
        terms_str = ' > '.join([f"{term}" for term, _ in top_terms])
        print(f"  Topic {topic_idx}: {terms_str}")

    # 差分分析
    print(f"\n🔀 NLPとMLの差分:")
    diffs = vectorizer.calculate_differences(result, 0, [1])
    print(diffs.round(3))

    # 距離分析
    print(f"\n📏 全ドキュメント間の距離:")
    distances = vectorizer.calculate_pairwise_distances(result, metric='cosine')
    print(distances.round(3))

    print("\n✨ すべての分析が完了しました！")


if __name__ == '__main__':
    print("\n🚀 DocumentVectorizer 使用例\n")

    # 各例を実行
    example_semantic_analysis()
    example_stylistic_analysis()
    example_python_api()

    print("\n" + "=" * 70)
    print("✅ すべての例が完了しました")
    print("=" * 70)
