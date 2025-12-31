# 理論的考察：新語未知語抽出のロジックと課題

## 現在の手法の概要

jphrase は以下のアプローチで新語・未知語を抽出します：

1. **N-gram生成** - テキストから全ての連続部分文字列を生成
2. **頻度カウント** - 出現回数をカウント
3. **情報量スコア** - `freq × log(1+freq) + len × log(len)` で評価
4. **包含関係処理** - 上位スコアのフレーズに含まれる下位を除外
5. **パターンフィルタ** - 正規表現でカタカナ・漢字などを選別
6. **類似度除去** - レーベンシュタイン距離で類似フレーズを削除

---

## ⚠️ 理論的な問題点

### 1. **N-gramの根本的限界**

#### 問題
```
❌ 連続した文字列しか抽出できない
❌ 分散表現を捉えられない
❌ 構文構造を無視
```

#### 具体例
```python
# 抽出できるもの
"機械学習"  # ✅ OK

# 抽出できないもの
"お待ちください"  # "お〜ください" という構造
"読み書きできる"  # "読み" と "書き" の並列
```

#### 理論的背景
- **言語学的単位**: 言語の意味単位は必ずしも連続していない
- **構文依存**: 係り受け関係などの構文情報が必要
- **形態素境界**: 真の単語境界は形態素解析が必要

---

### 2. **頻度ベースの偏り（Zipfの法則）**

#### 問題
```
❌ 高頻度語に偏る
❌ 重要だが低頻度の専門用語を見逃す
❌ 文書間の重要度を考慮しない
```

#### Zipfの法則
自然言語では、単語の頻度分布は：
```
frequency(rank) ≈ 1/rank
```

つまり：
- 少数の語が圧倒的に多く出現
- 大多数の語は低頻度

#### 理論的問題
現在のスコア関数：
```python
score = freq * log(1 + freq) + len * log(len)
```

**問題点:**
1. 頻度が高いほど有利（当たり前の語も高スコア）
2. 希少だが重要な専門用語を見逃す
3. 統計的有意性を考慮していない

**より良いアプローチ:**
- **TF-IDF**: 文書間の頻度差を考慮
- **統計的有意性検定**: χ²検定、PMI (Pointwise Mutual Information)
- **C-value**: 専門用語抽出の標準手法

---

### 3. **包含関係の処理の曖昧さ**

#### 問題
```
"機械学習" vs "学習"
→ どちらが「正しい」単位？
```

#### 現在のロジック
```python
# 情報量スコアが高い方を残す
# しかし、これは必ずしも正しい単位とは限らない
```

#### 理論的問題

**例1: 両方が意味を持つ場合**
```
"深層学習モデル"  # 全体で1つの概念
"深層学習"        # 独立した概念
"学習モデル"      # 独立した概念
```
→ どれを残すべき？

**例2: 統計的共起**
```
"ニューラルネットワーク" が高頻度
→ "ネットワーク" は除外される
   しかし "ネットワーク" 自体も重要語かもしれない
```

#### より良いアプローチ
- **Nested NER**: 入れ子構造を許容
- **統計的独立性**: χ²検定で独立性を評価
- **意味的coherence**: 共起統計の活用

---

### 4. **「新語」判定の不在**

#### 致命的な問題
```
❌ 「新しい」かどうかを判定していない
❌ 既知語辞書との照合がない
❌ 全てのフレーズを「新語候補」として扱う
```

#### 理論的に必要なこと

**真の新語抽出には:**
1. **既知語辞書** - 既存の語彙との照合
2. **時系列分析** - 過去のコーパスとの比較
3. **分布の変化検出** - 急激な出現頻度の変化

**現状の問題:**
```python
# 「機械学習」も「学習」も同じように扱われる
# しかし「学習」は既知語、「機械学習」は新語（かもしれない）
```

---

### 5. **文脈情報の欠如**

#### 問題
```
❌ 同じ文字列でも文脈で意味が変わる
❌ 多義性を扱えない
```

#### 具体例
```
"バンク" の多義性:
- 銀行 (bank)
- 土手 (bank)
- データバンク
- 血液バンク
```

#### 理論的背景
- **分散意味論**: 単語の意味は文脈で決まる（word2vec, BERT）
- **コロケーション**: 前後の単語との関係が重要

---

### 6. **形態素解析なしの限界**

#### 問題
```
❌ 言語学的な単語境界が不明
❌ 活用形を別単語として扱う
❌ 複合語の内部構造が分からない
```

#### 具体例
```python
# 動詞の活用
"走る", "走った", "走って", "走れば"
→ 全て別のフレーズとして扱われる

# 複合語
"機械学習アルゴリズム"
→ どこで区切るべき？
   "機械" + "学習アルゴリズム"？
   "機械学習" + "アルゴリズム"？
```

#### より良いアプローチ
- **MeCab/Sudachi**: 形態素解析器の併用
- **複合語解析**: 複合語の内部構造を考慮
- **正規化**: 活用形を基本形に統一

---

### 7. **統計的根拠の弱さ**

#### 問題：スコア関数の恣意性

現在のスコア：
```python
score = weight_freq * log(1 + freq) + weight_len * log(len)
```

**理論的問題:**
- なぜこの関数？
- なぜlog？なぜ1+freq？
- weightの値は？

#### より理論的なアプローチ

**1. 統計的有意性検定**
```python
# χ²検定で共起の有意性を評価
χ² = Σ (observed - expected)² / expected
```

**2. Pointwise Mutual Information (PMI)**
```python
PMI(x,y) = log(P(x,y) / (P(x) * P(y)))
```

**3. C-value（専門用語抽出の標準）**
```python
C-value(a) = {
    log₂(|a|) × f(a)                     if a is not nested
    log₂(|a|) × (f(a) - 1/P(Tₐ) Σ f(b))  if a is nested
}
```

---

### 8. **評価基準の不在**

#### 根本的問題
```
何が「良い」新語抽出結果か？
```

#### 現状の問題
- **主観的**: "これは良さそう" という感覚
- **再現性**: 同じ結果が得られるか不明
- **比較**: 他の手法と比較できない

#### 必要なこと
1. **ゴールドスタンダード**: 正解データ
2. **評価指標**: Precision, Recall, F1
3. **ベンチマーク**: 標準的なデータセット

---

### 9. **言語特有の問題（日本語）**

#### 日本語固有の課題

**1. 単語境界の不在**
```
英語: "machine learning"  # スペースで区切られている
日本語: "機械学習"         # 境界が不明確
```

**2. 表記ゆれ**
```
"コンピュータ" vs "コンピューター"
"サーバ" vs "サーバー"
"アルゴリズム" vs "アルゴリズム"
```

**3. 漢字・カナ混じり**
```
"機械learning"
"AIアルゴリズム"
```

---

## 🔬 理論的に正しいアプローチ

### 1. **形態素解析ベース**

```python
# MeCabで形態素解析
tokens = mecab.parse("機械学習は人工知能の一分野です")
# → ["機械", "学習", "は", "人工", "知能", "の", "一", "分野", "です"]

# 複合語候補の抽出
candidates = extract_compound_words(tokens)
# → ["機械学習", "人工知能"]
```

### 2. **統計的専門用語抽出**

```python
# C-valueアルゴリズム
def c_value(term, corpus):
    freq = count_frequency(term, corpus)
    length = len(term)

    if not is_nested(term):
        return log2(length) * freq
    else:
        parent_terms = get_parent_terms(term)
        nested_freq = sum(count_frequency(p) for p in parent_terms)
        return log2(length) * (freq - nested_freq / len(parent_terms))
```

### 3. **新語検出（時系列分析）**

```python
# 過去のコーパスと比較
def detect_new_words(current_corpus, reference_corpus):
    current_vocab = extract_vocabulary(current_corpus)
    reference_vocab = extract_vocabulary(reference_corpus)

    # 新出語
    new_words = current_vocab - reference_vocab

    # 急増語（統計的検定）
    emerging_words = []
    for word in current_vocab & reference_vocab:
        if is_significantly_increased(word, current, reference):
            emerging_words.append(word)

    return new_words, emerging_words
```

### 4. **文脈埋め込み**

```python
# BERT等で文脈を考慮
from transformers import BertModel

# 文脈埋め込みを取得
embeddings = bert_model("この技術は機械学習を使っている")

# クラスタリングで類似表現をグループ化
clusters = cluster_by_context(embeddings)
```

---

## 💡 改善提案

### 短期的改善（すぐできる）

1. **TF-IDFの導入**
```python
from sklearn.feature_extraction.text import TfidfVectorizer

vectorizer = TfidfVectorizer()
tfidf = vectorizer.fit_transform(documents)
# 重要度の高い語を抽出
```

2. **統計的有意性検定**
```python
def chi_square_test(phrase, corpus):
    # 共起の有意性を検定
    observed = count_cooccurrence(phrase)
    expected = calculate_expected(phrase)
    chi2 = (observed - expected)**2 / expected
    return chi2
```

3. **既知語辞書との照合**
```python
import MeCab

mecab = MeCab.Tagger()
known_words = set(load_dictionary())

def is_new_word(word):
    return word not in known_words
```

### 中期的改善

4. **C-valueの実装**
5. **形態素解析の統合**
6. **時系列コーパスの構築**

### 長期的改善

7. **BERT等のトランスフォーマー活用**
8. **多言語対応**
9. **ドメイン適応**

---

## 📊 現実的な妥協点

完璧な新語抽出は難しいので、**用途に応じた妥協**が現実的：

### 用途1: ざっくりトレンド把握
```
→ 現在の手法で十分
→ 完璧でなくても "それらしい" ものが取れればOK
```

### 用途2: 専門用語抽出
```
→ 形態素解析 + C-value が必要
→ 精度が重要
```

### 用途3: 本当の新語発見
```
→ 時系列分析 + 辞書照合 が必須
→ ドメイン知識が必要
```

---

## 🎯 結論

### 現在の手法の位置づけ

**強み:**
- ✅ シンプルで実装が容易
- ✅ 特別なツール不要
- ✅ 計算コストが低い
- ✅ "それらしい" フレーズは取れる

**弱み:**
- ❌ 理論的根拠が弱い
- ❌ 真の新語判定はできない
- ❌ 言語学的に正しい単位とは限らない
- ❌ 評価が難しい

### 推奨される使い方

```python
# ✅ 良い使い方
"SNS投稿から話題のフレーズを探す"
"ニュースから注目キーワードを抽出"
"大まかなトレンド把握"

# ❌ 不適切な使い方
"辞書に載せる新語を決定的に判定"
"言語学的に正しい単位を抽出"
"低頻度の専門用語を漏れなく発見"
```

---

## 📚 参考文献

1. **専門用語自動抽出**
   - C-value: Frantzi, K., et al. (2000)
   - TermExtractor: 中川・湯本 (2003)

2. **統計的自然言語処理**
   - Manning & Schütze (1999)
   - Jurafsky & Martin (2020)

3. **新語検出**
   - Dunning, T. (1993) - Log-likelihood ratio
   - Church & Hanks (1990) - Mutual Information

4. **日本語形態素解析**
   - MeCab, Sudachi, Juman++
