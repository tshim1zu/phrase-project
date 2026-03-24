# japhrase

**辞書もLLMも使わずに、テキストからフレーズを見つける。**

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Python 3.8+](https://img.shields.io/badge/python-3.8+-blue.svg)](https://www.python.org/downloads/)
[![PyPI](https://img.shields.io/pypi/v/japhrase)](https://pypi.org/project/japhrase/)
[![Tests](https://img.shields.io/badge/tests-290%2B%20passing-brightgreen)](https://github.com/tshim1zu/japhrase)

```bash
pip install japhrase
```

## 何をするツールか

日本語テキストの中から、繰り返し出現するフレーズを統計だけで検出する。MeCab も辞書ファイルも外部 AI も要らない。テキストを入れれば、そこに何度も現れているフレーズが出てくる——それが既知語であろうと、辞書に載っていない新語・造語・専門用語であろうと関係ない。

## N-gram による頻度抽出

テキストを「連続する N 文字」の断片（N-gram）に分解し、出現頻度を数える。

たとえば生成 AI に関する記事を入力すると：

```python
from japhrase import PhraseExtracter

extractor = PhraseExtracter(min_count=2, max_length=10, min_length=2)
df = extractor.extract(sentences)
```

```
     seqchar  freq
      ている    12   ← 意味のないフレーズも拾ってしまう
      生成AI    10
  大規模言語モデル     4
      である     4   ← これも意味がない
```

「生成AI」「大規模言語モデル」は見つかった。しかし「ている」「である」のような、どんなテキストにも現れる意味のないフレーズも一緒に出てくる。N-gram の頻度だけでは、意味のある結合と偶然の並びを区別できない。

## PMI による洗練

ここで PMI（自己相互情報量）を使う。PMI は「2つの要素が偶然一緒に現れる確率」と「実際に一緒に現れた頻度」の比を測る。

```python
extractor = PhraseExtracter(min_count=2, use_pmi=True)
df = extractor.extract(sentences)
```

```
     seqchar  freq    pmi
      生成AI    10   10.0   ← PMI が高い = 意味のある結合
  大規模言語モデル     4   10.0   ← 偶然ではない
      である     4    7.2   ← PMI が低い = ありふれた並び
```

PMI が高いフレーズは、構成要素が偶然隣り合っただけでは説明できないほど頻繁に共起している。「生成」と「AI」がこれほど一緒に出現するのは、それが1つの意味単位だからだ。一方「である」はどんな文脈にも現れるので PMI が低くなる。

PMI を有効にすることで、**意味的に結合したフレーズだけを上位に浮かび上がらせる**ことができる。

## 分岐エントロピーによる境界検出

さらに分岐エントロピーを加えると、フレーズの自然な切れ目を特定できる。「あるフレーズの次にどんな文字が来るか」の多様性を測り、多様性が急に上がる位置をフレーズの境界とみなす。

```python
extractor = PhraseExtracter(min_count=3, use_pmi=True, use_branching_entropy=True)
df = extractor.extract(sentences)
```

N-gram が「繰り返されているもの」を広く拾い、PMI が「意味のある結合」だけを残し、分岐エントロピーが「切れ目」を正確にする。この3層が japhrase のコアである。

## プリセット

テキスト種別ごとに最適化されたパラメータセット（Optuna による実験的最適化済み）。

```python
extractor = PhraseExtracter.preset('sns')     # SNS/Twitter（短文・高頻度）
extractor = PhraseExtracter.preset('news')    # ニュース（専門用語重視）
extractor = PhraseExtracter.preset('novel')   # 小説（繰り返し表現・長め）
extractor = PhraseExtracter.preset('report')  # 論文/レポート（定型・学術用語）
```

## ファイルからの読み込み

```python
df = PhraseExtracter.from_file("input.txt")
# エンコーディング自動検出（UTF-8 / Shift-JIS / EUC-JP）
```

## コアの上に積み上げた分析機能

フレーズ抽出エンジンを土台として、以下の統計分析機能を提供する。

### テキスト類似度

複数テキスト間の類似度を計算し、コピペ検出や重複分析を行う。

```python
from japhrase import SimilarityAnalyzer

analyzer = SimilarityAnalyzer(method='jaccard')
matrix = analyzer.compare_files(["doc1.txt", "doc2.txt", "doc3.txt"])
pairs = analyzer.find_similar_pairs(matrix, threshold=0.7)
# → [('doc1.txt', 'doc2.txt', 0.82)]  ← 82% 類似
```

Jaccard（N-gram の重なり）、Levenshtein（編集距離）、コサイン（TF-IDF）の3手法。`method='auto'` で自動選択。

### 分布比較（2テキスト間の語彙の違い）

2つのテキストの語彙分布がどれだけ違うかを定量化する。「テキスト A にだけ多い語」「テキスト B にだけ多い語」をキーネススコア付きで抽出できる。

```python
from japhrase import DistributionComparator
from collections import Counter

comp = DistributionComparator()
freq_a = Counter({'騎士': 10, '剣': 8, '王城': 5})
freq_b = Counter({'研究': 12, '実験': 9, 'データ': 7})
result = comp.compare(freq_a, freq_b)
```

```
JS距離: 1.0000  （0 = 同一の語彙、1 = 完全に別の語彙）
Aにだけ多い語: 騎士, 剣, 王城    ← 「テキストAらしさ」
Bにだけ多い語: 研究, 実験, データ  ← 「テキストBらしさ」
```

JS距離（Jensen-Shannon Divergence）は対称的で有界（0〜1）。対数尤度比（G²）はカイ二乗より疎データに強い。Effect Size（Log Ratio）は「有意かどうか」ではなく「どれだけ違うか」を測る。

### コロケーション分析（語の結びつきの強さ）

フレーズを構成する語同士の結びつきを6つの異なる角度から測る。

```python
from japhrase import CollocationScorer

scorer = CollocationScorer()
cs = scorer.score_phrase('生成AI', 45, text)
```

```
「生成AI」:
  PMI = 3.6      ← 意味的に結合している（偶然ではない）
  t-score = 6.1  ← 高頻度で安定した結合
  Log-Dice = 13.9 ← コーパスサイズに依存しない結合度
```

| 指標 | 何に強いか |
|------|----------|
| PMI | レアだが意味のある結合を発見する（低頻度に敏感） |
| t-score | 高頻度で安定した結合を発見する（PMI の逆） |
| Log-Dice | コーパスの大きさに影響されない（最も安定） |
| MI³ | PMI の低頻度バイアスを数学的に補正する |
| Delta-P | 方向性がある結合を測る（A→B と B→A は非対称） |

### 共起語分析

特定のキーワードの周辺に、統計的に特異な頻度で出現する語を抽出する。

```python
from japhrase import CooccurrenceAnalyzer

analyzer = CooccurrenceAnalyzer(window_size=50)
df = analyzer.analyze(text, "機械学習", top_n=10)
# → 機械学習の周辺50字以内に特異的に出現する語のランキング
```

キャラクター分析（あるキャラの周辺に頻出する語彙）、製品の評判分析（「画面」の周辺に「綺麗」があるか）等に使える。

### 時系列分析（複数テキスト間の推移）

複数のテキストを時系列として並べ、語彙がどう変化しているかを追跡する。

```python
from japhrase import TemporalAnalyzer

ta = TemporalAnalyzer()
result = ta.analyze_series([ch1, ch2, ch3, ch4, ch5], labels=["1章","2章","3章","4章","5章"])
```

```
語彙飽和度: 0.62  （後半で新語が減っている = 語彙が枯れ始めている）
MATTRトレンド: -0.008  （語彙の多様性が徐々に低下）
```

バースト検出: 特定の語が突然増えた区間を自動で見つける。伏線の使用状況やテーマの変化を追跡できる。

### 語彙多様性（計量言語学）

テキストの語彙がどれだけ豊かかを、7つの統計指標で定量化する。

```python
from japhrase import StylometryAnalyzer

stylo = StylometryAnalyzer()
adv = stylo.analyze_advanced_diversity(text)
```

```
豊かな文章 → Hapax比 0.97 / Simpson 0.9996 → 「語彙が非常に豊か」
単調な文章 → Hapax比 0.26 / Simpson 0.9676 → 「標準的」
```

| 指標 | 何を測るか | 読み方 |
|------|----------|--------|
| Hapax比 | 一度しか使わなかった語の割合 | 高い = 同じ語に頼っていない |
| MATTR | テキスト長に依存しない語彙多様性 | 高い = 長文でも語彙が豊か |
| Simpson's D | 同じ語に2回当たる確率の逆数 | 1に近い = 多様 |
| Brunet's W / Honoré's R | 長文で崩壊しない多様性指標 | 長編小説の比較に使える |
| Heaps' Law (β) | 読み進めるほど新語がどれだけ出るか | 高い = 最後まで新語が出続ける |

### テキスト複雑度・情報密度

テキストの「難しさ」「密度」「冗長さ」を複数の角度から測る。

```python
from japhrase import ComplexityAnalyzer

cx = ComplexityAnalyzer()
result = cx.analyze(text)
```

```
豊かな文章 → 圧縮率 0.71 / 情報率 0.98  ← 各文が新しい情報を提供
繰り返し   → 圧縮率 0.36 / 情報率 0.43  ← 同じことの言い直し
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
from japhrase.contamination import scan, quick_check

quick_check(text)       # → True（汚染あり）/ False（クリーン）

profile = scan(text)
print(profile.explain())
```

```
⚠️ 汚染スコア: 16/100

■ エンコーディング (55/100, 1件)
  L2: mojibakeパターン: 'â€™'
  💡 文字化け箇所を正しい文字に置換してください。

■ 構造 (39/100, 1件)
  L3: 開き括弧「が閉じていない
  💡 括弧の対応を確認してください。
```

複数テキストの一括チェックやテキスト間比較もできる:

```python
from japhrase.contamination import batch_scan, compare

batch_scan({"1章": t1, "2章": t2, "3章": t3}).contaminated_keys  # → ['2章']
compare(text_a, text_b).report()                                  # → 比較レポート
```

### その他

- **執筆ワークフロー**: 公開前品質ゲート、話数間推移、キャラ文体指紋、A〜E健康診断（`japhrase.applied`）
- **NMF文書ベクトル化** / **ストリーミング処理** / **パラメータ自動最適化** / **自動インサイト生成**
- **出力形式**: CSV、JSON、Excel、HTML レポート

## インストール

```bash
pip install japhrase                  # コア（numpy, pandas, scipy）
pip install japhrase[similarity]      # + sklearn, Levenshtein
pip install japhrase[viz]             # + matplotlib, seaborn
pip install japhrase[all]             # 全部入り
```

Python 3.8+

## 使い方を調べる

### CLI

```bash
japhrase --help                     # コマンド一覧
japhrase extract --help             # フレーズ抽出の引数とオプション
japhrase stats --help               # 統計出力の形式指定
japhrase check --help               # 文書品質チェック
japhrase detect-habits --help       # 書き癖検出
```

### Python

全クラス・全関数に docstring がある。`help()` で引数・戻り値・使用例が出る。

```python
# フレーズ抽出
from japhrase import PhraseExtracter
help(PhraseExtracter)               # クラス全体
help(PhraseExtracter.extract)       # 個別メソッド
help(PhraseExtracter.preset)        # プリセットの使い方

# 統計エンジン
from japhrase import DistributionComparator, CollocationScorer
from japhrase import StylometryAnalyzer, ComplexityAnalyzer, TemporalAnalyzer
help(DistributionComparator)        # 2テキスト間の分布比較
help(CollocationScorer)             # 語の結びつきの強さ（6指標）
help(StylometryAnalyzer)            # 語彙多様性（7指標）
help(ComplexityAnalyzer)            # テキスト複雑度
help(TemporalAnalyzer)              # 時系列分析

# 汚染検出
from japhrase.contamination import scan, quick_check, compare, batch_scan
help(scan)                          # 8軸汚染スキャン
help(quick_check)                   # True/False だけ返す最小API

# 執筆ワークフロー
from japhrase.applied import PreflightChecker, EPDashboard, PartHealthReport
help(PreflightChecker)              # 公開前品質ゲート
help(EPDashboard)                   # 話数間推移ダッシュボード
help(PartHealthReport)              # A〜E 健康診断
```

### デモ（例文入り・結果付き）

```python
import japhrase
japhrase.run_demo()              # 全7機能のデモを順に実行（例文・分析結果・次のステップ付き）
```

各クラス個別にデモもできる:

```python
from japhrase import PhraseExtracter, StylometryAnalyzer, ComplexityAnalyzer
from japhrase import DistributionComparator, CollocationScorer

PhraseExtracter.demo()           # フレーズ抽出のデモ
StylometryAnalyzer.demo()        # 語彙多様性のデモ
ComplexityAnalyzer.demo()        # テキスト複雑度のデモ
DistributionComparator.demo()    # 分布比較のデモ
CollocationScorer.demo()         # コロケーション分析のデモ
```

### サンプルスクリプト

```bash
python examples/quickstart.py    # 全機能の動作確認（コピペで動く）
```

### 使用ガイド

#### ファイルから抽出

```python
from japhrase import PhraseExtracter

df = PhraseExtracter.from_file("input.txt")                          # テキスト
df = PhraseExtracter.from_file("data.csv", column="text")            # CSV（列指定）
df = PhraseExtracter.from_file("input.txt", min_count=10, max_length=20)  # パラメータ指定
```

#### パラメータ

```python
extractor = PhraseExtracter(
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

#### 結果のエクスポート

```python
extractor.export_csv(df, "output.csv")      # CSV（Excel対応BOM付きUTF-8）
extractor.export_json(df, "output.json")    # JSON
extractor.export_excel(df, "output.xlsx")   # Excel（要 pip install japhrase[excel]）
```

#### 対応ファイル形式

テキスト（`.txt`）、CSV（`.csv`）、TSV（`.tsv`）。エンコーディングは自動検出（UTF-8 / Shift-JIS / EUC-JP）。

#### トラブルシューティング

```python
# メモリ不足 → バッチサイズを小さくする
extractor = PhraseExtracter(size_sentence=1000)

# 処理が遅い → min_count を上げる / max_length を下げる
extractor = PhraseExtracter(min_count=20, max_length=10)

# 結果が多すぎる → 類似度閾値を上げる
extractor = PhraseExtracter(threshold_originality=0.8)
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
