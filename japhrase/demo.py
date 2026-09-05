# coding: utf-8
"""
japhrase 自己デモンストレーション

全機能に .demo() を提供する。呼ぶだけで例文入り・分析結果付きの
説明が出力される。Python を触ったことがない人でも読める。

使い方:
    >>> import japhrase
    >>> japhrase.demo()           # 全機能のデモを順に実行

    >>> from japhrase import PhraseExtractor
    >>> PhraseExtractor.demo()    # フレーズ抽出だけデモ
"""

# ─── 共通の例文 ──────────────────────────────────────────

SAMPLE_SENTENCES = [
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
]

SAMPLE_TEXT_A = (
    '朝靄の中を歩いていると、足元で小さな花が揺れた。'
    '名前は知らない。淡い紫色の花弁が、露を含んで光っている。'
    '道の先に古い石橋が見えた。欄干には苔が生え、長い年月を感じさせる。'
    '風が吹いた。木々の葉がざわめき、遠くで鳥の声がした。'
)

SAMPLE_TEXT_B = (
    '男は歩いた。男は止まった。男はまた歩いた。男は見た。'
    '男は聞いた。男は歩いた。男は止まった。男はまた歩いた。'
)

SAMPLE_DIRTY = '正常な文章。\nâ€™ここに文字化け。\n「閉じない台詞\n出来ることをできる限りやる。'


def _header(title):
    """デモの区切り線とタイトルを標準出力へ表示する。"""
    print()
    print("=" * 60)
    print(f"  {title}")
    print("=" * 60)


def _explain(text):
    """説明文を行ごとにインデントして標準出力へ表示する。"""
    for line in text.strip().split('\n'):
        print(f"  {line}")
    print()


# ─── 個別デモ ────────────────────────────────────────────

def demo_phrase_extracter():
    """フレーズ抽出のデモ"""
    _header("フレーズ抽出 — PhraseExtractor")
    _explain("""
テキストから、辞書を使わずに頻出フレーズを統計で見つけます。
N-gram（連続するN文字の断片）の出現頻度を数え、
PMI（自己相互情報量）で「意味のある結合」だけを残します。
""")

    print("  【入力テキスト（10文 × 5 = 50文）】")
    for s in SAMPLE_SENTENCES[:3]:
        print(f"    {s}")
    print(f"    ...（他 {len(SAMPLE_SENTENCES) - 3} 文）× 5回繰り返し")
    print()

    from japhrase import PhraseExtractor
    ext = PhraseExtractor(min_count=2, max_length=10, min_length=2, verbose=0)
    df = ext.extract(SAMPLE_SENTENCES * 5)

    print("  【結果】")
    if not df.empty:
        for _, row in df.iterrows():
            print(f"    「{row['seqchar']}」 — {int(row['freq'])}回出現")
    else:
        print("    （この入力サイズではフレーズが抽出されませんでした）")
    print()

    _explain("""
「大規模言語モデル」のような辞書未収録の専門用語でも、
テキスト中で繰り返されていれば統計的に検出できます。

次のステップ:
  help(PhraseExtractor)          # 全パラメータの説明
  PhraseExtractor.preset('news') # プリセットを使う
""")


def demo_stylometry():
    """語彙多様性のデモ"""
    _header("語彙多様性 — StylometryAnalyzer")
    _explain("""
テキストの語彙がどれだけ豊かかを、複数の統計指標で測ります。
同じ長さの文章でも、使っている語彙の幅はまるで違います。
""")

    from japhrase import StylometryAnalyzer
    stylo = StylometryAnalyzer()

    for label, text in [("豊かな文章", SAMPLE_TEXT_A), ("単調な文章", SAMPLE_TEXT_B)]:
        adv = stylo.analyze_advanced_diversity(text)
        mattr = stylo.analyze_mattr(text, window_size=20)
        print(f"  【{label}】")
        print(f"    入力: {text[:40]}...")
        print(f"    Hapax比:    {adv['hapax_ratio']:.3f}  （一度しか使わなかった語の割合）")
        print(f"    Simpson's D: {adv['simpsons_d']:.4f}  （1に近いほど多様）")
        print(f"    MATTR:       {mattr['mattr']:.3f}  （テキスト長に依存しない多様性）")
        print(f"    → {adv['assessment']}")
        print()

    _explain("""
次のステップ:
  help(StylometryAnalyzer)                    # 全指標の説明
  stylo.analyze_vocabulary_growth(text)       # 語彙の増加速度（Heaps則）
""")


def demo_complexity():
    """テキスト複雑度のデモ"""
    _header("テキスト複雑度 — ComplexityAnalyzer")
    _explain("""
テキストの「難しさ」「情報の密度」「冗長さ」を測ります。
圧縮率が低い = 同じパターンの繰り返しが多い = 冗長。
""")

    from japhrase import ComplexityAnalyzer
    cx = ComplexityAnalyzer()

    for label, text in [("多様な文章", SAMPLE_TEXT_A), ("繰り返し文章", SAMPLE_TEXT_B)]:
        r = cx.analyze(text)
        print(f"  【{label}】")
        print(f"    圧縮率:   {r['compression_ratio']:.3f}  （低い = 繰り返しが多い）")
        print(f"    情報率:   {r['information_rate']:.3f}  （低い = 新情報が少ない）")
        print(f"    語彙密度: {r['lexical_density']:.3f}  （高い = 内容語が多い）")
        print()

    _explain("""
次のステップ:
  help(ComplexityAnalyzer)          # 全指標の説明
  cx.generate_report(text)          # 詳細レポート
""")


def demo_distribution():
    """分布比較のデモ"""
    _header("分布比較 — DistributionComparator")
    _explain("""
2つのテキストの語彙分布がどれだけ違うかを定量化します。
JS距離が 0 なら同一、1 なら完全に別の語彙です。
""")

    from japhrase import DistributionComparator
    from collections import Counter

    comp = DistributionComparator()
    freq_a = Counter({'騎士': 10, '剣': 8, '王城': 5, '訓練': 6})
    freq_b = Counter({'研究': 12, '実験': 9, 'データ': 7, '端末': 5})

    print("  【入力】")
    print(f"    テキストA の語彙: {dict(freq_a)}")
    print(f"    テキストB の語彙: {dict(freq_b)}")
    print()

    result = comp.compare(freq_a, freq_b)
    print("  【結果】")
    print(f"    JS距離: {result.jsd:.4f}")
    print(f"    共有語彙: {result.shared_terms}語")

    overuse = [kr for kr in result.keyness_results if kr.direction == 'overuse'][:3]
    underuse = [kr for kr in result.keyness_results if kr.direction == 'underuse'][:3]
    if overuse:
        print(f"    Aに多い語: {', '.join(kr.term for kr in overuse)}")
    if underuse:
        print(f"    Bに多い語: {', '.join(kr.term for kr in underuse)}")
    print()

    _explain("""
次のステップ:
  help(DistributionComparator)            # 全メソッドの説明
  comp.generate_report(freq_a, freq_b)    # キーネス分析付きレポート
""")


def demo_collocation():
    """コロケーション分析のデモ"""
    _header("コロケーション分析 — CollocationScorer")
    _explain("""
フレーズを構成する語同士がどれだけ強く結びついているかを測ります。
6つの指標がそれぞれ異なる角度から結合度を評価します。
""")

    from japhrase import CollocationScorer
    scorer = CollocationScorer()

    text = '生成AIが注目されている。生成AIの活用が進む。' * 30
    phrases = [('生成AI', 60), ('されている', 30), ('が注目', 30)]

    print("  【入力】 「生成AIが注目されている...」を30回繰り返したテキスト")
    print()

    for phrase, freq in phrases:
        cs = scorer.score_phrase(phrase, freq, text)
        ctype = scorer.classify_collocation(cs)
        print(f"  「{phrase}」:")
        print(f"    PMI={cs.pmi:.1f}  t-score={cs.t_score:.1f}  Log-Dice={cs.log_dice:.1f}  → {ctype}")
    print()

    _explain("""
PMI が高い = 意味的に結合（「生成AI」は偶然ではない）。
t-score が高い = 高頻度の結合。Log-Dice はコーパスサイズに依存しない。

次のステップ:
  help(CollocationScorer)         # 全指標の説明
  scorer.score_phrases(df, text)  # DataFrame で一括スコアリング
""")


def demo_contamination():
    """汚染検出のデモ"""
    _header("汚染検出 — scan()")
    _explain("""
テキストの破損（文字化け・コピペ重複・句読点の揺れ等）を
8つの検出器で多角的にチェックします。
""")

    from japhrase.contamination import scan

    print("  【入力（汚染あり）】")
    for line in SAMPLE_DIRTY.split('\n'):
        print(f"    {line}")
    print()

    profile = scan(SAMPLE_DIRTY)
    print("  【結果】")
    print(f"    汚染スコア: {profile.overall}/100  {'✅ クリーン' if profile.is_clean() else '⚠️ 汚染あり'}")
    print()

    for ax in profile.worst_axes:
        print(f"    {ax.name}: {ax.score}/100 ({ax.count}件)")
        for a in ax.anomalies[:2]:
            print(f"      {a.location}: {a.description}")
            print(f"      💡 {a.suggestion}")
    print()

    _explain("""
次のステップ:
  help(scan)                          # 全パラメータ
  scan(text).explain()                # 問題・場所・直し方を一発表示
  quick_check(text)                   # True/False だけ返す
  batch_scan({"ch1": t1, "ch2": t2})  # 複数テキスト一括
""")


def demo_preflight():
    """公開前プリフライトのデモ"""
    _header("公開前プリフライト — PreflightChecker")
    _explain("""
原稿を公開する前に、統計的な品質チェックを行います。
GO / WARN / NOGO の3段階で判定します。
""")

    from japhrase.applied import PreflightChecker
    checker = PreflightChecker()

    text_ok = SAMPLE_TEXT_A * 10
    text_ng = '体温は36.5度。心拍数は毎分90回。血圧は120mmHg。速度は10km/h。' * 10

    for label, text, lang in [("普通の原稿", text_ok, 'jp'), ("数値だらけ", text_ng, 'jp')]:
        r = checker.check(text, lang=lang)
        icon = {'GO': '✅', 'WARN': '⚠️', 'NOGO': '❌'}[r.verdict]
        print(f"  【{label}】 {icon} {r.verdict} ({r.quality_score:.0f}/100)")
        if r.skip_count:
            print(f"    SKIP={r.skip_count}件（数値+単位が多い → 読者が飛ばし読みする）")
    print()

    _explain("""
次のステップ:
  help(PreflightChecker)              # 全パラメータ
  checker.check(text, platform='sh')  # PF別の文字数制限チェック
""")


# ─── 統合デモ ────────────────────────────────────────────

ALL_DEMOS = [
    ('フレーズ抽出', demo_phrase_extracter),
    ('語彙多様性', demo_stylometry),
    ('テキスト複雑度', demo_complexity),
    ('分布比較', demo_distribution),
    ('コロケーション分析', demo_collocation),
    ('汚染検出', demo_contamination),
    ('公開前プリフライト', demo_preflight),
]


def demo_all():
    """全機能のデモを順に実行"""
    print()
    print("╔" + "═" * 58 + "╗")
    print("║" + "  japhrase デモンストレーション".center(52) + "║")
    print("║" + f"  全 {len(ALL_DEMOS)} 機能を順に実行します".center(48) + "║")
    print("╚" + "═" * 58 + "╝")

    for name, func in ALL_DEMOS:
        func()

    print()
    print("=" * 60)
    print("  デモ完了。")
    print()
    print("  各機能の詳細は help() で確認できます。")
    print("  例: help(PhraseExtractor), help(scan)")
    print("=" * 60)
