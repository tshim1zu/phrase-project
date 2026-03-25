# japhrase

**辞書もLLMも使わずに、テキストからフレーズを見つける。**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Python 3.8+](https://img.shields.io/badge/python-3.8+-blue.svg)](https://www.python.org/downloads/)
[![PyPI](https://img.shields.io/pypi/v/japhrase)](https://pypi.org/project/japhrase/)

```bash
pip install japhrase
```

日本語テキストを入れると、繰り返し出現するフレーズを頻度で見つけ出す。MeCab も辞書も外部 AI も不要。

**2つの使い方がある:**

- **テキストを分析したい人**（研究者・マーケター）→ [テキスト分析として使う](#テキスト分析として使う)
- **文章を書く/直す人**（作家・編集者・ライター）→ [執筆支援として使う](#執筆支援として使う)

---

## 最初の一歩

```python
from japhrase import PhraseExtractor

text = """機械学習の応用が広がっている。
自然言語処理は機械学習の重要な分野だ。
深層学習は機械学習の手法の一つである。
機械学習エンジニアの需要が高まっている。
機械学習のモデルを訓練するにはデータが必要だ。
深層学習の計算コストは高い。
自然言語処理の精度が向上している。
機械学習の倫理的課題も議論されている。""" * 3

pe = PhraseExtractor(verbose=0)
df = pe.extract(text)
print(df[["seqchar", "freq"]].head(3).to_string(index=False))
```

```
  seqchar  freq
    機械学習の  15.0
   自然言語処理   6.0
      ている  12.0
```

テキストを渡すだけ。「機械学習」「自然言語処理」が辞書なしで見つかった。「ている」のようなノイズも混ざる — `use_pmi=True` を追加すると消える（→ [仕組み](#仕組み--n-gram--pmi--分岐エントロピー)）。

**次のステップ:** [デモで全機能を体験する](#デモ2行で全機能体験) → [自分の用途に合わせて使う](#テキスト分析として使う)

### 結果の使い方

`df` は pandas の DataFrame。

| 列 | 意味 |
|----|------|
| `seqchar` | 見つかったフレーズ |
| `freq` | 出現回数 |
| `length` | フレーズの文字数 |

```python
# 上位10フレーズ
print(df[["seqchar", "freq"]].head(10))

# 5回以上のフレーズだけ
frequent = df[df["freq"] >= 5]

# CSVに保存（Excelで開ける）
pe.export_csv(df, "results.csv")

# Pythonリストとして取り出す
phrases = df["seqchar"].tolist()
```

---

## テキスト分析として使う

**「大量のテキストの中身を数字で理解したい」人向け。** SNSの投稿、ニュース記事、論文、レビュー、アンケートの自由記述など。

### キーワード抽出

テキスト種別ごとにプリセットがある:

```python
from japhrase import PhraseExtractor

pe = PhraseExtractor.preset('sns')     # SNS/Twitter向け
pe = PhraseExtractor.preset('news')    # ニュース記事向け
pe = PhraseExtractor.preset('report')  # 論文/レポート向け

# テキストファイルから読み込んで抽出 → CSV保存
df = pe.extract("input.txt")           # ファイルパスを渡せば自動で読み込み
pe.export_csv(df, "keywords.csv")      # Excelで開ける
```

プリセットで足りないときは `auto_tune=True` で自動最適化:

```python
from japhrase import PhraseExtractor

text = "機械学習の応用が広がっている。自然言語処理は機械学習の重要な分野だ。深層学習は機械学習の手法の一つである。" * 10
pe = PhraseExtractor(verbose=0)
df = pe.extract(text, auto_tune=True)   # テキストに合わせて自動チューニング
pe.show_params()                         # 最適化されたパラメータを表示
pe.save_params("my_params.json")         # 保存して次回から再利用
```

### 2つのテキストの違いを定量化

「テキスト A にだけ多い語は何か？ B にだけ多い語は？」— レビュー比較や競合分析に。

```python
from japhrase import DistributionComparator
from collections import Counter

# PhraseExtractor の結果から Counter を作ることもできる:
# freq_a = Counter(dict(zip(df_a["seqchar"], df_a["freq"])))

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

「機械学習」は偶然の並びか、意味のある結合か？

```python
from japhrase import CollocationScorer

text = "機械学習の応用が広がっている。機械学習の技術が進んでいる。" * 50
scorer = CollocationScorer()
# score_phrase(フレーズ, 出現回数, テキスト全文)
# 出現回数は df["freq"] から取れる。ここでは直接指定:
cs = scorer.score_phrase("機械学習", 100, text)
print(f"PMI={cs.pmi:.1f}  t-score={cs.t_score:.1f}  Log-Dice={cs.log_dice:.1f}")
```

| 指標 | 得意なこと |
|------|----------|
| PMI | レアだが意味のある結合の発見 |
| t-score | 高頻度で安定した結合の発見 |
| Log-Dice | コーパスサイズに左右されない比較 |

### テキスト類似度（コピペ・重複検出）

```python
from japhrase import SimilarityAnalyzer

texts = [
    "機械学習の応用が広がっている。自然言語処理の進化が著しい。",
    "機械学習の技術が発展している。言語処理が急速に進化している。",
    "今日は天気がよい。散歩に出かけた。公園で花を見た。",
]
analyzer = SimilarityAnalyzer(method='jaccard')
matrix = analyzer.compare_texts(texts)
print(matrix)
```

text_1 と text_2 は類似、text_3 は完全に別。

### 時系列で語彙の変化を追う

章ごと・月ごとにテキストを並べて、語彙がどう変化しているか。

```python
from japhrase import TemporalAnalyzer

ch1 = "騎士が剣を抜いた。王城の門が開いた。騎士は走った。" * 10
ch2 = "研究室でデータを確認した。実験の結果を記録した。" * 10
ch3 = "騎士は剣を収めた。王城に平和が戻った。研究が進んだ。" * 10

ta = TemporalAnalyzer()
result = ta.analyze_series([ch1, ch2, ch3], labels=["1章", "2章", "3章"])
print(f"語彙飽和度: {result['vocab_saturation']:.2f}")
```

---

## 執筆支援として使う

**「自分の原稿を客観的に見たい」人向け。** 小説、エッセイ、ブログ、ビジネス文書など。

### 語彙の豊かさを測る

「同じ言葉ばかり使っていないか？」を数字で確認。

```python
from japhrase import StylometryAnalyzer

stylo = StylometryAnalyzer()
rich = stylo.analyze_advanced_diversity(
    "朝靄の中を歩いていると、足元で小さな花が揺れた。名前は知らない。淡い紫色の花弁が露を含んで光っている。")
poor = stylo.analyze_advanced_diversity(
    "男は歩いた。男は止まった。男はまた歩いた。男は見た。男は聞いた。")
print(f"豊かな文: Hapax比={rich['hapax_ratio']:.2f}")
print(f"単調な文: Hapax比={poor['hapax_ratio']:.2f}")
```

```
豊かな文: Hapax比=0.97
単調な文: Hapax比=0.76
```

Hapax比が高い = 一度しか使わなかった語が多い = 語彙が豊か。低い = 同じ語の繰り返し。

### 繰り返しの多さを測る

「同じことを何度も書いていないか？」を圧縮率で判定。

```python
from japhrase import ComplexityAnalyzer

cx = ComplexityAnalyzer()
r1 = cx.analyze("朝靄の中を歩いていると、足元で小さな花が揺れた。名前は知らない。淡い紫色の花弁が露を含んで光っている。")
r2 = cx.analyze("男は歩いた。" * 20)
print(f"多様な文章: 圧縮率={r1['compression_ratio']:.3f}")
print(f"繰り返し:   圧縮率={r2['compression_ratio']:.3f}")
```

```
多様な文章: 圧縮率=0.869
繰り返し:   圧縮率=0.092
```

圧縮率が低い = 同じパターンが多い = 冗長。

### 書き癖を見つける

無意識に繰り返している表現を検出する。

```python
from japhrase import WritingHabitDetector

text = """朝靄の中を歩いていた。しかし、名前は知らない。
しかし、気にはなった。しかし、立ち止まれなかった。
道の先に石橋が見えた。しかし、渡るのはためらわれた。
しかし、他に道はない。しかし、戻れない。
結局、渡った。しかし、足は震えていた。""" * 5

detector = WritingHabitDetector()
habits = detector.detect(text)
print(habits[["phrase", "count"]].to_string(index=False))
```

```
phrase  count
    。し     20
    い。     15
  た。しか     10
```

「。し」=「しかし」の文頭パターン、「た。しか」=「〜た。しかし」の接続パターン。N-gram ベースなので句読点を含む断片として検出される。繰り返しパターンが数字で見える。

### 原稿の汚染を検出

文字化け・コピペ重複・括弧の閉じ忘れを入稿前にチェック。

```python
from japhrase.contamination import scan

profile = scan("正常な文章。\nâ€™文字化け。\n「閉じない台詞")
print(f"汚染スコア: {profile.overall}/100")
print(profile.explain())
```

章ごとに一括チェック:

```python
from japhrase.contamination import batch_scan

result = batch_scan({
    "1章": "正常なテキスト。問題のない文章が続きます。特に異常はありません。",
    "2章": "â€™文字化け。「閉じない括弧。重複文。重複文。重複文。重複文。重複文。",
    "3章": "正常なテキスト。こちらも問題ありません。普通の文章です。",
})
for key, p in result.profiles.items():
    print(f"{key}: スコア={p.overall}/100")
```

```
1章: スコア=0/100
2章: スコア=23/100
3章: スコア=0/100
```

### その他の執筆支援

| 機能 | 何をするか |
|------|----------|
| **TextVariantDetector** | 表記ゆれ検出（サーバー/サーバ、出来る/できる） |
| **PhraseExtractor.preset('novel')** | 小説に最適化したフレーズ抽出 |
| **japhrase.applied** | 公開前品質ゲート・話数間推移・キャラ文体指紋 |

---

## デモ（2行で全機能体験）

```python
import japhrase
japhrase.run_demo()
```

内蔵の例文で全機能が順に実行される。何も用意しなくていい。

個別のデモ:

```python
from japhrase import PhraseExtractor
PhraseExtractor.demo()
```

---

## 仕組み — N-gram + PMI + 分岐エントロピー

japhrase は3層でフレーズを絞り込む。

### 第1層: N-gram（繰り返しを広く拾う）

テキストを「連続する N 文字」に分解し、出現頻度を数える。

```python
from japhrase import PhraseExtractor

text = """ChatGPTの登場により生成AIが注目を集めている。
生成AIはテキストだけでなく画像生成にも使われる。
大規模言語モデルは生成AIの中核技術である。
生成AIの倫理的課題について議論が活発化している。
企業における生成AIの導入事例が増加している。
生成AIの品質向上が求められている。
生成AIと人間の協調が進んでいる。
生成AIの応用範囲は広がり続けている。
生成AIによるコスト削減が期待されている。
生成AIの安全性が議論されている。""" * 5

pe = PhraseExtractor(min_count=3, min_length=2, max_length=8, verbose=0)
df = pe.extract(text)
print(df[["seqchar", "freq"]].head(4).to_string(index=False))
```

```
  seqchar  freq
     生成AI  50.0
      ている  35.0
ChatGPTの登   5.0
生成AIはテキスト   5.0
```

「生成AI」は見つかった。しかし「ている」(35回) も一緒に出てくる。N-gram だけでは意味のある結合とノイズを区別できない。

### 第2層: PMI（ノイズを落とす）

PMI（自己相互情報量）は「偶然一緒に現れる確率」と「実際の共起頻度」の比。フラグ1個追加するだけ:

```python
pe = PhraseExtractor(min_count=3, min_length=2, max_length=8, use_pmi=True, verbose=0)
df = pe.extract(text)  # ↑ 第1層と同じ text
print(df[["seqchar", "freq"]].head(4).to_string(index=False))
```

```
    seqchar  freq
    生成AIの  30.0
    されている  10.0
ChatGPTの登   5.0
生成AIはテキスト   5.0
```

「ている」(35回) が消えた。「生成AI」の構成文字は偶然以上に一緒に現れている — PMI が高い。「ている」はどんな文章にも現れるので PMI が低く、脱落する。

### 第3層: 分岐エントロピー（切れ目を正確にする）

「あるフレーズの次にどんな文字が来るか」の多様性を測る。多様性が急に上がる位置 = フレーズの境界。`use_branching_entropy=True` を追加:

```python
pe = PhraseExtractor(min_count=3, min_length=2, max_length=8,
                     use_pmi=True, use_branching_entropy=True, verbose=0)
df = pe.extract(text)  # ↑ 第1層と同じ text
print(df[["seqchar", "freq"]].head(4).to_string(index=False))
```

この例文では第2層と同じ結果になる。分岐エントロピーの効果は、フレーズの境界が曖昧な長文（小説など）で大きくなる。

**まとめ:** N-gram が広く拾い → PMI がノイズを落とし → 分岐エントロピーが切れ目を正確にする。この3層が japhrase のコア。

---

## ファイルから読み込み

```python
from japhrase import PhraseExtractor

df = PhraseExtractor.from_file("input.txt")                  # エンコーディング自動検出
df = PhraseExtractor.from_file("data.csv", column="text")    # CSV（列指定）
```

## パラメータ一覧

```python
from japhrase import PhraseExtractor

pe = PhraseExtractor(
    min_count=6,                # 最小出現回数
    max_length=16,              # フレーズの最大文字数
    min_length=4,               # フレーズの最小文字数
    threshold_originality=0.5,  # 類似フレーズ除去の閾値
    use_pmi=True,               # PMI で意味的結合を評価
    use_branching_entropy=True, # 分岐エントロピーで境界検出
    knowns=["既知語1"],         # 優先的に抽出したい語
    verbose=1,                  # 進捗表示（0:非表示）
)
```

## CLI

```bash
japhrase --help           # コマンド一覧
japhrase extract --help   # フレーズ抽出
japhrase check --help     # 文書品質チェック
```

## help() で調べる

```python
from japhrase import PhraseExtractor
help(PhraseExtractor.extract)

from japhrase.contamination import scan
help(scan)
```

## テスト

```bash
pytest
```

---

## English

**japhrase** extracts phrases from Japanese text using statistics alone — no dictionary, no morphological analyzer, no LLM.

```bash
pip install japhrase
```

```python
from japhrase import PhraseExtractor

pe = PhraseExtractor(verbose=0)
df = pe.extract("機械学習の応用が広がっている。自然言語処理は機械学習の重要な分野だ。" * 10)
print(df[["seqchar", "freq"]].head())
```

**How it works:** N-grams find recurring character sequences. PMI (pointwise mutual information) filters out meaningless combinations — "機械学習" scores high because its components co-occur far more than chance predicts, while "ている" scores low. Branching entropy then identifies natural phrase boundaries.

**Two use cases:**

- **Text analysis** (researchers, marketers): keyword extraction, distribution comparison (JSD), collocation scoring (6 metrics), vocabulary richness (7 metrics), temporal analysis
- **Writing support** (authors, editors): writing habit detection, contamination scanning (encoding errors, duplicates, bracket mismatches), complexity metrics, quality gates

Pure math, no external AI. Python 3.8+, numpy/scipy.

---

## ライセンス

MIT License — Takeshi SHIMIZU
