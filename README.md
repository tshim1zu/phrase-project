# japhrase

**辞書もLLMも使わずに、テキストからフレーズを見つける。**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Python 3.8+](https://img.shields.io/badge/python-3.8+-blue.svg)](https://www.python.org/downloads/)
[![PyPI](https://img.shields.io/pypi/v/japhrase)](https://pypi.org/project/japhrase/)

```bash
pip install japhrase
```

---

## 2行で全機能デモ

```python
import japhrase
japhrase.run_demo()
```

内蔵の例文でフレーズ抽出・語彙多様性・テキスト複雑度・分布比較・コロケーション分析・汚染検出・品質チェックが順に実行される。何も用意しなくていい。

---

## コピペで動く使用例

以下のコードは **すべて1ブロックで完結** する。コピペしてそのまま動く。ファイルもデータも不要。

### フレーズ抽出（辞書なし・形態素解析なし）

```python
from japhrase import PhraseExtractor

text = """ChatGPTの登場により生成AIが注目を集めている。
生成AIはテキストだけでなく画像生成にも使われる。
大規模言語モデルは生成AIの中核技術である。
生成AIの倫理的課題について議論が活発化している。
企業における生成AIの導入事例が増加している。""" * 5

sentences = [s.strip() for s in text.split("。") if s.strip()]
df = PhraseExtractor(min_count=2, min_length=2, max_length=10, selection=0, verbose=0).extract(sentences)
print(df[["seqchar", "freq"]].head(3))
```

```
        seqchar  freq
0          生成AI  25.0
2   ChatGPTの登場に   5.0
18  大規模言語モデルは生成   5.0
```

### 語彙の豊かさを比較

```python
from japhrase import StylometryAnalyzer

stylo = StylometryAnalyzer()

text_a = "朝靄の中を歩いていると、足元で小さな花が揺れた。名前は知らない。淡い紫色の花弁が露を含んで光っている。道の先に古い石橋が見えた。"
text_b = "男は歩いた。男は止まった。男はまた歩いた。男は見た。男は聞いた。男は歩いた。"

for label, t in [("豊かな文", text_a), ("単調な文", text_b)]:
    r = stylo.analyze_advanced_diversity(t)
    print(f"{label}: Hapax比={r['hapax_ratio']:.2f}  Simpson={r['simpsons_d']:.4f}")
```

```
豊かな文: Hapax比=0.97  Simpson=0.9995
単調な文: Hapax比=0.68  Simpson=0.9692
```

### 2つのテキストの語彙分布を比較

```python
from japhrase import DistributionComparator
from collections import Counter

comp = DistributionComparator()
result = comp.compare(
    Counter({"騎士": 10, "剣": 8, "王城": 5}),
    Counter({"研究": 12, "実験": 9, "データ": 7}),
)
print(f"JS距離: {result.jsd:.4f}  (0=同一, 1=完全に別)")
```

```
JS距離: 1.0000  (0=同一, 1=完全に別)
```

### 語の結びつきの強さを測る

```python
from japhrase import CollocationScorer

scorer = CollocationScorer()
cs = scorer.score_phrase("生成AI", 45, "生成AIが注目されている。" * 50)
print(f"PMI={cs.pmi:.1f}  t-score={cs.t_score:.1f}  Log-Dice={cs.log_dice:.1f}")
```

```
PMI=3.5  t-score=6.1  Log-Dice=13.8
```

### テキストの複雑度を測る

```python
from japhrase import ComplexityAnalyzer

cx = ComplexityAnalyzer()
for label, t in [("多様な文章", "朝靄の中を歩いていると、足元で小さな花が揺れた。名前は知らない。淡い紫色の花弁が露を含んで光っている。"),
                 ("繰り返し", "男は歩いた。" * 20)]:
    r = cx.analyze(t)
    print(f"{label}: 圧縮率={r['compression_ratio']:.3f}  情報率={r['information_rate']:.3f}")
```

```
多様な文章: 圧縮率=0.869  情報率=0.961
繰り返し:   圧縮率=0.092  情報率=0.050
```

### テキストの汚染を検出

```python
from japhrase.contamination import scan

profile = scan("正常な文章。\nâ€™文字化け。\n「閉じない台詞")
print(f"汚染スコア: {profile.overall}/100")
print(profile.explain())
```

```
汚染スコア: 15/100
⚠️ 汚染スコア: 15/100

■ エンコーディング (55/100, 1件)
  L2: mojibakeパターン: 'â€™'
  💡 文字化け箇所を正しい文字に置換してください。
```

### 複数テキストの一括汚染チェック

```python
from japhrase.contamination import batch_scan

texts = {
    "1章": "正常なテキスト。問題のない文章が続きます。特に異常はありません。",
    "2章": "â€™文字化け。「閉じない括弧。重複文。重複文。重複文。重複文。重複文。",
    "3章": "正常なテキスト。こちらも問題ありません。普通の文章です。",
}
result = batch_scan(texts)
print(f"全クリーン: {result.all_clean}")
for key, p in result.profiles.items():
    print(f"  {key}: スコア={p.overall}/100  クリーン={p.is_clean()}")
```

```
全クリーン: False
  1章: スコア=0/100  クリーン=True
  2章: スコア=23/100  クリーン=False
  3章: スコア=0/100  クリーン=True
```

### 時系列で語彙の変化を追跡

```python
from japhrase import TemporalAnalyzer

ch1 = "騎士が剣を抜いた。王城の門が開いた。騎士は走った。剣が光った。" * 10
ch2 = "研究室でデータを確認した。実験の結果を記録した。論文を読んだ。" * 10
ch3 = "騎士は剣を収めた。王城に平和が戻った。研究が進んだ。" * 10

ta = TemporalAnalyzer()
result = ta.analyze_series([ch1, ch2, ch3], labels=["1章", "2章", "3章"])
print(f"語彙飽和度: {result['vocab_saturation']:.2f}")
```

```
語彙飽和度: 1.00
```

### テキスト類似度（コピペ検出）

```python
from japhrase import SimilarityAnalyzer

texts = [
    "生成AIが注目を集めている。大規模言語モデルの進化が著しい。",
    "生成AIの技術が注目されている。言語モデルが急速に進化している。",
    "今日は天気がよい。散歩に出かけた。公園で花を見た。",
]
analyzer = SimilarityAnalyzer(method='jaccard')
matrix = analyzer.compare_texts(texts)
print(matrix)
```

```
          text_1    text_2  text_3
text_1  1.000000  0.166667     0.0
text_2  0.166667  1.000000     0.0
text_3  0.000000  0.000000     1.0
```

text_1 と text_2 は類似（0.17）、text_3 は完全に別（0.0）。

### パラメータ自動最適化（あなたのテキストに合わせてチューニング）

```python
from japhrase import PhraseExtractor

pe = PhraseExtractor()

# いつもの抽出
df = pe.extract(texts)

# テキストに合わせて自動チューニング → 抽出（フラグ1個追加するだけ）
df = pe.extract(texts, auto_tune=True)

# チューニング後のパラメータを確認（コピペ用コード付き）
pe.show_params()

# 気に入ったら保存（明示的 — 勝手にファイルは作らない）
pe.save_params("my_params.json")

# 次回はこれで復元
pe = PhraseExtractor.load_params("my_params.json")
df = pe.extract(new_texts)
```

`auto_tune=True` を付けるだけ。Optunaがあればベイズ最適化、なければヒューリスティック推定。ファイルシステムには一切触らない — 保存はユーザーが `save_params()` を呼んだ時だけ。

---

## 何をするツールか

日本語テキストの中から、繰り返し出現するフレーズを統計だけで検出する。MeCab も辞書ファイルも外部 AI も要らない。テキストを入れれば、そこに何度も現れているフレーズが出てくる——それが既知語であろうと、辞書に載っていない新語・造語・専門用語であろうと関係ない。

## N-gram による頻度抽出

テキストを「連続する N 文字」の断片（N-gram）に分解し、出現頻度を数える。

```python
from japhrase import PhraseExtractor

text = """ChatGPTの登場により生成AIが注目を集めている。
生成AIはテキストだけでなく画像生成にも使われる。
大規模言語モデルは生成AIの中核技術である。
生成AIの倫理的課題について議論が活発化している。
企業における生成AIの導入事例が増加している。""" * 5

sentences = [s.strip() for s in text.split("。") if s.strip()]
extractor = PhraseExtractor(min_count=2, max_length=10, min_length=2, verbose=0)
df = extractor.extract(sentences)
print(df[["seqchar", "freq"]].head(5))
```

「生成AI」「大規模言語モデル」は見つかる。しかし「ている」「である」のような、どんなテキストにも現れる意味のないフレーズも一緒に出てくる。N-gram の頻度だけでは、意味のある結合と偶然の並びを区別できない。

## PMI による洗練

PMI（自己相互情報量）は「2つの要素が偶然一緒に現れる確率」と「実際に一緒に現れた頻度」の比を測る。

```python
from japhrase import PhraseExtractor

text = """ChatGPTの登場により生成AIが注目を集めている。
生成AIはテキストだけでなく画像生成にも使われる。
大規模言語モデルは生成AIの中核技術である。
生成AIの倫理的課題について議論が活発化している。
企業における生成AIの導入事例が増加している。""" * 5

sentences = [s.strip() for s in text.split("。") if s.strip()]
extractor = PhraseExtractor(min_count=2, use_pmi=True, verbose=0)
df = extractor.extract(sentences)
print(df[["seqchar", "freq"]].head(5))
```

PMI が高いフレーズは、構成要素が偶然隣り合っただけでは説明できないほど頻繁に共起している。「生成」と「AI」がこれほど一緒に出現するのは、それが1つの意味単位だからだ。

## 分岐エントロピーによる境界検出

さらに分岐エントロピーを加えると、フレーズの自然な切れ目を特定できる。「あるフレーズの次にどんな文字が来るか」の多様性を測り、多様性が急に上がる位置をフレーズの境界とみなす。

```python
from japhrase import PhraseExtractor

text = """ChatGPTの登場により生成AIが注目を集めている。
生成AIはテキストだけでなく画像生成にも使われる。
大規模言語モデルは生成AIの中核技術である。
生成AIの倫理的課題について議論が活発化している。
企業における生成AIの導入事例が増加している。""" * 5

sentences = [s.strip() for s in text.split("。") if s.strip()]
extractor = PhraseExtractor(min_count=3, use_pmi=True, use_branching_entropy=True, verbose=0)
df = extractor.extract(sentences)
print(df[["seqchar", "freq"]].head(5))
```

N-gram が「繰り返されているもの」を広く拾い、PMI が「意味のある結合」だけを残し、分岐エントロピーが「切れ目」を正確にする。この3層が japhrase のコアである。

## プリセット

テキスト種別ごとに最適化されたパラメータセット（Optuna による実験的最適化済み）。

```python
from japhrase import PhraseExtractor

extractor = PhraseExtractor.preset('sns')     # SNS/Twitter（短文・高頻度）
extractor = PhraseExtractor.preset('news')    # ニュース（専門用語重視）
extractor = PhraseExtractor.preset('novel')   # 小説（繰り返し表現・長め）
extractor = PhraseExtractor.preset('report')  # 論文/レポート（定型・学術用語）
print(extractor)  # パラメータ確認
```

## デモで全機能を体験する

Python を起動して2行書くだけで、全7機能が内蔵例文で実行される。

```python
import japhrase
japhrase.run_demo()
```

個別にデモもできる:

```python
from japhrase import PhraseExtractor, StylometryAnalyzer, ComplexityAnalyzer
from japhrase import DistributionComparator, CollocationScorer

PhraseExtractor.demo()         # フレーズ抽出だけ
StylometryAnalyzer.demo()      # 語彙多様性
ComplexityAnalyzer.demo()      # テキスト複雑度
DistributionComparator.demo()  # 分布比較
CollocationScorer.demo()       # コロケーション分析
```

---

## コアの上に積み上げた分析機能

フレーズ抽出エンジンを土台として、以下の統計分析機能を提供する。すべて `pip install japhrase` だけで使える。

### 分布比較（2テキスト間の語彙の違い）

```python
from japhrase import DistributionComparator
from collections import Counter

comp = DistributionComparator()
result = comp.compare(
    Counter({"騎士": 10, "剣": 8, "王城": 5}),
    Counter({"研究": 12, "実験": 9, "データ": 7}),
)
print(f"JS距離: {result.jsd:.4f}")
for kr in sorted(result.keyness_results, key=lambda x: x.log_likelihood, reverse=True)[:3]:
    print(f"  {kr.term}: {kr.direction}  (log-likelihood={kr.log_likelihood:.1f})")
```

```
JS距離: 1.0000
  騎士: overuse  (log-likelihood=19.0)
  研究: underuse  (log-likelihood=17.4)
  剣: overuse  (log-likelihood=14.6)
```

JS距離（Jensen-Shannon Divergence）は対称的で有界（0〜1）。対数尤度比（G²）はカイ二乗より疎データに強い。Effect Size（Log Ratio）は「有意かどうか」ではなく「どれだけ違うか」を測る。

### コロケーション分析（語の結びつきの強さ）

```python
from japhrase import CollocationScorer

text = "生成AIが注目されている。生成AIの技術が進んでいる。" * 50
scorer = CollocationScorer()
cs = scorer.score_phrase("生成AI", 45, text)
print(f"PMI={cs.pmi:.1f}  t-score={cs.t_score:.1f}  Log-Dice={cs.log_dice:.1f}")
```

| 指標 | 何に強いか |
|------|----------|
| PMI | レアだが意味のある結合を発見する（低頻度に敏感） |
| t-score | 高頻度で安定した結合を発見する（PMI の逆） |
| Log-Dice | コーパスの大きさに影響されない（最も安定） |
| MI³ | PMI の低頻度バイアスを数学的に補正する |
| Delta-P | 方向性がある結合を測る（A→B と B→A は非対称） |

### 共起語分析

特定のキーワードの周辺に、統計的に特異な頻度で出現する語を抽出する（`CooccurrenceAnalyzer`）。キャラクター分析や製品レビュー分析に使える。大量テキスト向け。

### 語彙多様性（計量言語学）

```python
from japhrase import StylometryAnalyzer

stylo = StylometryAnalyzer()

text_a = "朝靄の中を歩いていると、足元で小さな花が揺れた。名前は知らない。淡い紫色の花弁が露を含んで光っている。道の先に古い石橋が見えた。"
text_b = "男は歩いた。男は止まった。男はまた歩いた。男は見た。男は聞いた。男は歩いた。"

for label, t in [("豊かな文章", text_a), ("単調な文章", text_b)]:
    adv = stylo.analyze_advanced_diversity(t)
    print(f"{label}: Hapax比={adv['hapax_ratio']:.2f}  MATTR={stylo.analyze_mattr(t)['mattr']:.3f}")
```

| 指標 | 何を測るか | 読み方 |
|------|----------|--------|
| Hapax比 | 一度しか使わなかった語の割合 | 高い = 同じ語に頼っていない |
| MATTR | テキスト長に依存しない語彙多様性 | 高い = 長文でも語彙が豊か |
| Simpson's D | 同じ語に2回当たる確率の逆数 | 1に近い = 多様 |
| Brunet's W / Honoré's R | 長文で崩壊しない多様性指標 | 長編小説の比較に使える |
| Heaps' Law (β) | 読み進めるほど新語がどれだけ出るか | 高い = 最後まで新語が出続ける |

### テキスト複雑度・情報密度

```python
from japhrase import ComplexityAnalyzer

cx = ComplexityAnalyzer()

for label, t in [("多様な文章", "朝靄の中を歩いていると、足元で小さな花が揺れた。名前は知らない。淡い紫色の花弁が露を含んで光っている。"),
                 ("繰り返し", "男は歩いた。" * 20)]:
    r = cx.analyze(t)
    print(f"{label}: 圧縮率={r['compression_ratio']:.3f}  情報率={r['information_rate']:.3f}")
```

| 指標 | 何を測るか | 読み方 |
|------|----------|--------|
| 圧縮率 | zlib で圧縮したサイズ比 | 低い = 同じパターンの繰り返しが多い |
| 情報率 | 各文が提供する新情報の割合 | 低い = 前の文と同じことを言っている |
| 語彙密度 | 内容語 / 全語の比率 | 低い = 機能語（助詞等）が多く冗長 |
| パープレキシティ | N-gram の予測困難度 | 高い = 多様で予測しにくい文章 |

### テキスト品質

| 機能 | 説明 | 使い方 |
|------|------|--------|
| **TextVariantDetector** | 表記ゆれ検出 | サーバー/サーバ、出来る/できる を発見 |
| **WritingHabitDetector** | 書き癖検出 | 高頻度×低PMI = 無意識の繰り返しを発見 |
| **Summarizer** | 統計的要約 | LLM不要・ハルシネーションなしでテキスト圧縮 |

### 汚染検出

テキストの破損（文字化け・コピペ重複・句読点の揺れ等）を8軸で検出する。

```python
from japhrase.contamination import scan, quick_check, batch_scan, compare

# ワンライナー（True=汚染あり, False=クリーン）
print(quick_check("â€™文字化け。「閉じない。重複文。重複文。重複文。重複文。"))  # → True
print(quick_check("正常なテキスト。問題のない文章です。"))                        # → False

# 詳細スキャン
profile = scan("â€™文字化け。「閉じない括弧。重複文。重複文。重複文。重複文。重複文。")
print(profile.explain())

# 2テキスト比較
r = compare("正常なテキスト。問題のない文章。", "â€™文字化け。「閉じない括弧")
print(r.report()[:100])
```

### その他

- **テキスト類似度**: Jaccard / Levenshtein / コサイン（`SimilarityAnalyzer`）
- **時系列分析**: 語彙飽和度・バースト検出・MATTRトレンド（`TemporalAnalyzer`）
- **執筆ワークフロー**: 公開前品質ゲート・話数間推移・キャラ文体指紋・A〜E健康診断（`japhrase.applied`）
- **NMF文書ベクトル化** / **ストリーミング処理** / **パラメータ自動最適化** / **自動インサイト生成**

---

## ファイルからの読み込み

```python
from japhrase import PhraseExtractor

# テキストファイル（エンコーディング自動検出: UTF-8 / Shift-JIS / EUC-JP）
df = PhraseExtractor.from_file("input.txt")

# CSV（列指定）
df = PhraseExtractor.from_file("data.csv", column="text")
```

## パラメータ一覧

```python
from japhrase import PhraseExtractor

extractor = PhraseExtractor(
    min_count=6,                # 最小出現回数（小さいと計算時間が増える）
    max_length=16,              # フレーズの最大文字数
    min_length=4,               # フレーズの最小文字数
    threshold_originality=0.5,  # 類似フレーズ除去の閾値（0.0〜1.0）
    use_pmi=True,               # PMI で意味的結合を評価
    use_branching_entropy=True, # 分岐エントロピーで境界を検出
    knowns=["既知語1"],         # 優先的に抽出したい既知語
    verbose=1,                  # 進捗表示（0:非表示, 1:表示）
)
```

## CLI

```bash
japhrase --help                     # コマンド一覧
japhrase extract --help             # フレーズ抽出の引数とオプション
japhrase stats --help               # 統計出力の形式指定
japhrase check --help               # 文書品質チェック
japhrase detect-habits --help       # 書き癖検出
```

## help() で調べる

全クラス・全関数に docstring がある。

```python
from japhrase import PhraseExtractor
help(PhraseExtractor.extract)       # 引数・戻り値・使用例

from japhrase import StylometryAnalyzer, ComplexityAnalyzer
help(StylometryAnalyzer)            # 語彙多様性（7指標）
help(ComplexityAnalyzer)            # テキスト複雑度

from japhrase.contamination import scan
help(scan)                          # 8軸汚染スキャン
```

## テスト

```bash
pytest                   # 290件以上
pytest --cov=japhrase    # カバレッジ付き
```

## English Summary

**japhrase** finds phrases in Japanese text without a dictionary or LLM. It scans for recurring N-gram patterns, then uses PMI (pointwise mutual information) to separate meaningful collocations from noise — "生成AI" ranks high because its components co-occur far more than chance predicts, while "である" ranks low. Branching entropy further refines phrase boundaries. On top of this core: text similarity, distribution comparison (JSD, G², keyness), collocation scoring (6 metrics), vocabulary richness (7 metrics), text complexity, and more. Pure math, no external AI, 290+ tests, numpy/scipy only.

## ライセンス

MIT License — Takeshi SHIMIZU
