"""
DocumentVectorizer - テキストベース出力の例

ドキュメント ベクトル化の結果をテキストフォーマットで表示する使用例です。
画像出力ではなく、コンソール上で理解しやすいテキスト形式で結果を表示します。
"""

import sys
import io
from pathlib import Path

# Windows環境でのUTF-8出力対応
if sys.platform == "win32":
    sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
    sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8')

sys.path.insert(0, str(Path(__file__).parent.parent))

from japhrase import DocumentVectorizer


def example_text_based_analysis():
    """テキストベースの差分分析例"""

    print("\n" + "=" * 80)
    print("📚 DocumentVectorizer - テキストベース出力例")
    print("=" * 80)

    # ============================================================================
    # サンプルドキュメント：異なるジャンルのテキスト
    # ============================================================================

    # 文書A: 技術系（機械学習について）
    text_ml = """
    機械学習は人工知能の一分野で、アルゴリズムがデータから自動的にパターンを学習します。
    ニューラルネットワークはこの学習をシミュレートする方法です。
    深層学習はニューラルネットワークを複数層重ねた技術で、画像認識や自然言語処理で
    大きな成功を収めています。機械学習モデルの精度は、大量の学習データと
    適切なパラメータチューニングに大きく依存します。
    """

    # 文書B: ビジネス系（経営戦略について）
    text_business = """
    ビジネス戦略は企業の競争力を決定する重要な要素です。
    市場分析を通じて顧客ニーズを理解することは戦略立案の基本です。
    デジタルトランスフォーメーションはビジネスモデルを根本から変える可能性があります。
    経営者は組織文化を構築し、チームの士気を高める必要があります。
    """

    # 文書C: 文学系（創作について）
    text_literature = """
    文学作品は読者の想像力を刺激し、新しい世界へと導きます。
    小説では登場人物の心理描写が作品の深さを決定します。
    物語の構成は起承転結のバランスが重要です。
    隠喩や比喩を効果的に用いることで、文章はより豊かな表現力を持つようになります。
    """

    print("\n【処理内容】")
    print("3つの異なるジャンルのテキストをNMFでベクトル化し、トピックごとの特徴を分析します。")
    print("- テキスト1（ML）: 機械学習・AI技術について")
    print("- テキスト2（Business）: ビジネス・経営戦略について")
    print("- テキスト3（Literature）: 創作・文学について")

    # ============================================================================
    # ステップ1: TF-IDF モード（意味的な差を検出）
    # ============================================================================

    print("\n\n" + "=" * 80)
    print("【実行1】TF-IDF モード（意味的な差を検出）")
    print("=" * 80)
    print("""
このモードでは、ドキュメント内で最も重要な概念を捉えます。
TF-IDF（Term Frequency - Inverse Document Frequency）は、
各ターム（単語句）の出現頻度と、複数ドキュメント全体でのレアリティを組み合わせて、
「その文書にとって特に重要な表現」を抽出します。
    """)

    vectorizer = DocumentVectorizer(
        n_topics=5,
        feature_mode='tfidf',
        verbose=0
    )

    result = vectorizer.from_texts(
        [text_ml, text_business, text_literature],
        labels=['ML', 'Business', 'Literature']
    )

    # ドキュメント-トピック プロファイル
    profile_text = vectorizer.format_document_profiles_as_text(result)
    print("\n" + profile_text)

    # トピック詳細
    topic_text = vectorizer.format_topic_summary_as_text(result, n_terms=8)
    print("\n" + topic_text)

    # ドキュメント間の距離
    distances = vectorizer.calculate_pairwise_distances(result, metric='cosine')
    print("\n" + "=" * 80)
    print("📏 ドキュメント間の類似度（コサイン距離）")
    print("=" * 80)
    print("""
コサイン距離は0～1の値で表されます。
- 0に近い: 2つのドキュメントのトピック構成が似ている
- 1に近い: 2つのドキュメントのトピック構成が大きく異なっている
    """)
    print(distances.round(3).to_string())

    # 差分分析
    print("\n\n" + "=" * 80)
    print("【差分分析】ML を基準とした場合")
    print("=" * 80)
    print("""
「ML」を参照文書とし、「Business」と「Literature」との差分を表示します。
- ▲（上向き矢印）: 比較文書の方がこのトピックに強い特徴を持つ
- ▼（下向き矢印）: 参照文書の方がこのトピックに強い特徴を持つ
- 正の値: 比較文書が参照文書より強く持つトピック
- 負の値: 参照文書が比較文書より強く持つトピック
    """)

    diffs = vectorizer.calculate_differences(result, 0, [1, 2])
    diff_text = vectorizer.format_differences_as_text(diffs)
    print("\n" + diff_text)

    # ============================================================================
    # ステップ2: 解釈
    # ============================================================================

    print("\n\n" + "=" * 80)
    print("【分析結果の解釈】")
    print("=" * 80)

    print("""
上記の分析結果から、以下のことが読み取れます：

1. 【ドキュメント-トピック プロファイル】
   各文書がどのトピックで特徴付けられるかを示します。
   - ヒストグラムバーが長いほど、そのトピックが文書の主要な要素
   - トピック寄与度（%）が高いほど、その文書の中心的な概念

2. 【トピック詳細分析】
   各トピックの内容を示す上位タームが表示されます。
   - Topic 0, 1, 2... それぞれが何を表現しているかの手がかり
   - タームのバーの長さ = そのトピック内での重要度

3. 【ドキュメント間距離】
   コサイン距離で各ドキュメント間の"近さ"を測定します。
   - MLとBusinessは異なる距離
   - MLとLiteratureはさらに異なる距離
   - これはジャンルの違いを数値で示しています

4. 【差分分析】
   基準文書からの「ズレ」を定量的に示します。
   - どのトピックで大きく異なるのか
   - 類似性と差異の両方を同時に把握できます
    """)


def example_habit_analysis():
    """文体・手癖分析の例"""

    print("\n\n" + "=" * 80)
    print("【実行2】Low-PMI モード（文体・手癖の差を検出）")
    print("=" * 80)
    print("""
このモードでは、「意味的には重要でないが、頻繁に出現する表現」を抽出します。
これにより、著者の書き癖や習慣的な表現パターンが浮かび上がります。

PMI（Pointwise Mutual Information）とは、2つのイベントの関連度を測る指標です。
- PMI が低い = 個々の要素は出現するが、組み合わせの意味は薄い（固定表現、癖）
- PMI が高い = 組み合わせに強い意味がある（専門用語、意味的フレーズ）
    """)

    # 著者Aの文体（実務的で簡潔）
    text_author_a = """
    本件に関しましては、以下の通りです。
    まずもって、重要な点をお伝えします。
    実際のところ、本方法は効果的です。
    要するに、本施策は推奨されます。
    念のため、ご確認ください。
    """ * 4  # 頻度を高める

    # 著者Bの文体（感情的で冗長）
    text_author_b = """
    本当に大事なことは、実は、心の持ち方です。
    実のところ、本当に大切なことを忘れがちです。
    実際のところ、本当に大事なのは信頼です。
    とても重要なのは、本当に、心の準備なのです。
    実は、本当に大事なのは、姿勢なのです。
    """ * 4  # 頻度を高める

    vectorizer = DocumentVectorizer(
        n_topics=3,
        feature_mode='low_pmi',
        pmi_threshold=2.5,
        min_count=5,
        verbose=0
    )

    result = vectorizer.from_texts(
        [text_author_a, text_author_b],
        labels=['著者A（実務的）', '著者B（感情的）']
    )

    # ドキュメント-トピック プロファイル
    profile_text = vectorizer.format_document_profiles_as_text(result)
    print("\n" + profile_text)

    # トピック詳細
    topic_text = vectorizer.format_topic_summary_as_text(result, n_terms=10)
    print("\n" + topic_text)

    # 差分分析
    print("\n" + "=" * 80)
    print("【著者スタイルの差異】")
    print("=" * 80)

    diffs = vectorizer.calculate_differences(result, 0, [1])
    diff_text = vectorizer.format_differences_as_text(diffs)
    print("\n" + diff_text)

    print("""

【解釈】
- 著者Aは「本件」「まずもって」「実際のところ」などの定型表現が多い
- 著者Bは「本当に」「実は」「実のところ」などの感情的表現が多い
- トピック寄与度の違いが、著者の文体的な特徴を定量化しています
    """)


def example_python_api_direct_access():
    """Python APIで直接アクセスする例"""

    print("\n\n" + "=" * 80)
    print("【Python API直接アクセス例】")
    print("=" * 80)

    texts = [
        "自然言語処理は言語の意味を理解する技術です。テキスト分析は重要です。",
        "機械学習モデルのトレーニングには多くのデータが必要です。",
    ]

    vectorizer = DocumentVectorizer(n_topics=3, feature_mode='tfidf', verbose=0)
    result = vectorizer.from_texts(texts, labels=['NLP', 'ML'])

    print("""
Python コードでの直接アクセス方法：

    # 1. 個別ドキュメントのトピック分布を取得
    vector = result.get_document_vector(0)  # doc_index=0 のベクトル
    print(vector)  # [0.45, 0.30, 0.25]

    # 2. トピックの上位ターム を取得
    terms = result.get_topic_terms(0, n_terms=5)  # Topic 0 の上位5ターム
    # [('言語', 0.85), ('処理', 0.72), ...]

    # 3. DataFrameとして取得
    df = result.to_dataframe(matrix_type='normalized')
    print(df)

    # 4. テキスト形式でのフォーマット出力
    text = vectorizer.format_document_profiles_as_text(result)
    print(text)

    # 5. 差分の計算
    diffs = vectorizer.calculate_differences(result, 0, [1])
    diff_text = vectorizer.format_differences_as_text(diffs)
    print(diff_text)
    """)

    # 実際に実行してみる
    print("\n【実際の出力】\n")

    vector = result.get_document_vector(0)
    print(f"ドキュメント 'NLP' のトピック分布:")
    print(f"  {vector.round(3)}")

    terms = result.get_topic_terms(0, n_terms=5)
    print(f"\nTopic 0 の上位5ターム:")
    for term, score in terms:
        print(f"  - {term}: {score:.3f}")

    print(f"\nDataFrame表示:")
    df = result.to_dataframe(matrix_type='normalized')
    print(df.round(3))


if __name__ == '__main__':
    example_text_based_analysis()
    example_habit_analysis()
    example_python_api_direct_access()

    print("\n" + "=" * 80)
    print("✅ すべての例が完了しました")
    print("=" * 80)
    print("""
【このスクリプトから学べること】

1. TF-IDF モードで意味的な差分を検出
   → ドキュメントの主要なトピックを理解できます

2. Low-PMI モードで文体・手癖の差を検出
   → 著者特性やスタイルの違いを定量化できます

3. テキストフォーマット出力で結果を解釈
   → グラフを描画せずにコンソール上で分析可能

4. Python API で結果にアクセス
   → 結果を他の処理に組み込めます

【さらに詳しく知るには】

- examples/document_vectorizer_example.py
  - より多くの例と詳細な使用方法

- japhrase/document_vectorizer.py
  - DocumentVectorizer クラスのメソッド一覧

- CLI から使用する場合
  - japhrase vectorize --help
    """)
