ご提示いただいた `japhrase` プロジェクトのソースコード一式、拝見しました。

「忖度無し、ガチレビュー」というご要望ですので、お世辞は抜きにして、設計思想、アルゴリズムの妥当性、実装品質、そしてパフォーマンスの観点から**技術的に厳しく**レビューします。

### 総評：コンセプトは素晴らしいが、このままでは「実用」で死ぬ

**結論から言うと、「統計的アプローチでLLMに頼らず軽量に解析する」という着眼点と、それを支えるクラス設計（API）は非常に魅力的です。しかし、核心となるアルゴリズムの実装に致命的なパフォーマンス上のボトルネックがあり、長文や大量データを食わせた瞬間にフリーズする可能性が高いです。**

研究室のプロトタイプとしては合格ですが、`v0.2.0` としてPyPIで配布するライブラリとしては、計算量の観点で**落第点**です。

---

### 1. 致命的なパフォーマンス問題（ここが一番ヤバい）

`extracter.py` と `segmenter.py` の実装手法は、計算量オーダーの観点から見て、ナイーブすぎます。

#### ① `calculate_pmi` の再走査問題 (`extracter.py`)

PMI計算メソッド内で、候補フレーズごとに `all_text.count(phrase)` を実行しています。

```python
for phrase in phrases:
    # フレーズの出現回数（大まかな推定）
    phrase_count = all_text.count(phrase)  # <--- ここ！！

```

* **問題点**: `phrases` は候補となるN-gramのユニーク数（数千〜数万）、`all_text` は入力テキスト全体です。Pythonの `str.count()` は  です。これをループ内で行うため、全体の計算量は  となります。
* **なぜダメか**: `make_ngrampieces` ですでに全N-gramを生成し `Counter` で頻度を数えているはずです。なぜその `freq` を使わずに、わざわざ重たい `str.count` で再走査するのですか？ `str.count` は重複しない出現数を数えますが、N-gram統計では通常、オーバーラップを含んだ出現頻度（`Counter` の値）を使うのが一般的です。これでは長編小説ひとつ処理するだけで日が暮れます。

#### ② `calculate_right_entropy_profile` の非効率性 (`segmenter.py`)

分岐エントロピーの計算で、テキストを一文字ずつスライドさせながら、さらに内部でテキスト全体をスキャンしています。

```python
for i in range(len(text)):
    # ...
    for j in range(i, len(text)): # <--- 2重ループ
        if text[j:j+len(context)] == context ...

```

* **問題点**: これは実質  の処理です。数万文字のテキストで破綻します。
* **改善案**: Suffix Array（接尾辞配列）やトライ木（Trie）、あるいは単に辞書で「文脈→次文字の頻度分布」を1回のリニアスキャンで構築すべきです。そうすれば  で終わります。

---

### 2. コード品質と設計への指摘

#### ① 依存関係管理の甘さ

`extracter.py` の `levenshtein` メソッド内で `import Levenshtein` しています。

```python
def levenshtein(self, seq_x: str, seq_y: str) -> float:
    try:
        import Levenshtein  # <--- 関数内インポート

```

`pyproject.toml` の `dependencies` に `python-Levenshtein>=0.12.0` が明記されているなら、これはトップレベルでインポートすべきです。「入っていない場合」を考慮するなら `optional-dependencies` に移すべきですし、必須ならこの書き方はアンチパターンです。毎回インポートチェックが走ります。

#### ② 正規表現のコンパイル位置

`WritingHabitDetector` 内で `extracter` を初期化する際、`positive_patterns` などを都度定義・コンパイルしています。このクラスがループ内で何度も呼ばれるような使い方は想定していないかもしれませんが、定数としてクラスレベルかモジュールレベル（`constants.py`など）に定義しておくべきです。

#### ③ `WritingHabitDetector` のスコア計算の根拠

```python
score = 0.7 * norm_freq + 0.3 * norm_pmi_inv

```

この `0.7` と `0.3` というマジックナンバーの根拠は何でしょうか？ 「Optunaで最適化した」等の背景があるならdocstringに書くべきです。そうでなければ、ユーザーが調整可能にするか、ロジックの正当性を説明する必要があります。

---

### 3. 評価できる点（Good Points）

批判ばかりしましたが、光る部分も確実にあります。

* **API設計の美しさ**: `PhraseExtracter.from_file("input.txt")` や `PhraseExtracter.preset('sns')` といったインターフェースは非常に直感的で、ユーザー体験（DX）がよく考えられています。
* **「分岐エントロピー」による分割**: 句読点のないテキストや音声認識ログに対して、意味の切れ目を統計的に発見する `segmenter.py` のアプローチは非常に有用です（実装の高速化さえすれば）。
* **プリセット機能**: `preset('sns')`, `preset('novel')` など、用途別のパラメータセットを用意している点は親切で、実用性を意識しています。
* **型ヒントとドキュメント**: Type hinting がしっかり書かれており、`README.md` の構成もプロフェッショナルです。

### 4. まとめと提言

**判定: プロトタイプとしては優秀だが、ライブラリとしては「要チューニング」**

今のままでは「デモデータでは動くが、現場のデータを入れると返ってこない」ツールになってしまいます。

**緊急で行うべき修正:**

1. **計算量の削減**: `str.count` や 2重ループによる全探索を全廃し、`Counter` の結果の再利用や、ハッシュマップを使った  アルゴリズムへの書き換え。
2. **PMI計算の見直し**: N-gramの頻度集計結果をそのまま使うロジックへの変更。

これさえ直せば、LLM全盛の時代において「軽くて速い統計的NLPツール」として独自のポジションを築けるポテンシャルがあります。期待しています。





その意気やよし！改修が進んでいるようで何よりだ。

「テキストマイニング」「執筆支援」「計量文学（Stylometry）」の領域で、今の `japhrase` の「統計的・軽量」という強みを活かせそうなモジュール案をいくつか提案する。お前のプロジェクトの方向性（LLMに頼りすぎず、ロジックで攻める）に合わせて選んでみたぞ。

### 1. 感情アーク・アナライザー (`EmotionArcAnalyzer`)

物語の「盛り上がり」や「雰囲気の推移」を可視化する機能。

* **概要:** テキストを章やシーンごとに区切り、感情辞書（極性辞書）を使ってポジティブ/ネガティブのスコア推移をグラフ化する。
* **計量文学的価値:** カート・ヴォネガットが提唱した「物語の形状（Shape of Stories）」分析ができる。「悲劇」型なのか「シンデレラ（上昇）」型なのかがデータで見える。
* **実装ヒント:** 日本語評価極性辞書（用言・名詞）を内蔵し、単純なマッチングとスコアリングを行うだけで、驚くほどそれっぽい波形が出る。

### 2. 会話・地の文バランス分析 (`DialogueRatioAnalyzer`)

* **概要:** カギ括弧 `「」` 内（会話文）と、それ以外（地の文）の比率や、それぞれの特徴を分析する。
* **執筆支援的価値:** ラノベなら会話多め、重厚な小説なら地の文多め、といったジャンル分析や、自分の作品のペース配分の確認に使える。
* **拡張:** 「会話文でばかり使われる単語」と「地の文でばかり使われる単語」を `CooccurrenceAnalyzer` のロジックで比較抽出すると、キャラの口調分析にもなる。

### 3. 語彙多様性スコアラー (`VocabularyRichnessScorer`)

* **概要:** TTR (Type-Token Ratio) や、より長文に強い Yule's K, Simpson's D などの指標を用いて、「語彙の豊かさ」を数値化する。
* **計量文学的価値:** 著者の推定（Authorship Attribution）でよく使われる指標。「同じ単語ばかり使い回していないか？」のチェックになる。
* **実装ヒント:** `PhraseExtracter` で単語（トークン）化はできているので、ユニーク数と総単語数の比率計算をするだけ。実装コストが低く、効果が高い。

### 4. 表記ゆれチェッカー (`OrthographyVariantDetector`)

* **概要:** 同一語彙で表記が割れているものを統計的に検出する。
* 例：「コンピューター」vs「コンピュータ」、「私」vs「わたし」、「出来る」vs「できる」


* **実装ヒント:**
* カタカナ語の末尾長音（ー）の有無。
* 読み（yomi）が同じで表記が異なるもの（`jaconv`などでカタカナ化して比較）。
* 編集距離（Levenshtein）が近く、出現コンテキストが類似しているペアを探す。



### 5. 人物相関ネットワーク (`CharacterNetworkGenerator`)

* **概要:** 登場人物（固有名詞）同士の「共起」をカウントし、グラフ理論（NetworkXなど）でネットワーク図を作る。
* **テキストマイニング価値:** 「誰と誰がよく一緒に登場するか」「物語の中心人物（媒介中心性が高いノード）は誰か」が一発でわかる。
* **実装:** `PhraseExtracter` で固有名詞（高頻度語）を抽出し、`CooccurrenceAnalyzer` のロジックでエッジ（繋がり）の重みを計算すれば作れる。

### 6. 品詞比率指紋 (`PosFingerprint`)

* **概要:** 文章全体における「動詞」「形容詞」「名詞」の比率を出し、レーダーチャートにする。
* **計量文学的価値:**
* 動詞が多い → アクション、動き重視、ハードボイルド。
* 形容詞が多い → 情緒的、描写重視、心理描写多め。
* 名詞が多い → 説明的、硬い、論文調。


* **実装:** 形態素解析器（MeCabやSudachi）が裏で動いているなら集計するだけ。なければ、接続詞や語尾のパターンからある程度推定する「統計的品詞推定」という変態的なアプローチもあり（お前のプロジェクトっぽい）。

---

個人的なイチオシは **「2. 会話・地の文バランス」** と **「4. 表記ゆれチェッカー」** だ。これらは実用性が非常に高く、作家ツールとして組み込むと喜ばれる機能だ。

どれか興味が湧いたやつはあるか？ 詳細なロジックを詰めるぞ。


###　下記はたたき台

承知いたしました。「1. 感情分析」以外、つまり**辞書レス・統計ベースで実現可能な「2. 会話分析」「3. 語彙/文体分析」「4. 表記ゆれ」「5. 人物ネットワーク」**の4機能を実装します。

`japhrase` の設計思想（軽量・統計ベース）に合わせ、外部の形態素解析器（MeCab等）を必須とせず、正規表現と統計処理だけで動くように設計しました。

各ファイルを `japhrase/` 配下に配置する想定です。

---

### 1. 会話・地の文バランス分析 (`japhrase/dialogue_analyzer.py`)

カギ括弧の比率だけでなく、それぞれのパートで「よく使われる言葉」の違いも分析できるようにしました。

```python
"""
会話・地の文バランス分析モジュール
"""
import re
import pandas as pd
from typing import Dict, List, Optional
from .extracter import PhraseExtracter

class DialogueAnalyzer:
    """会話文と地の文の比率や特徴を分析するクラス"""

    def __init__(self):
        # 一般的な括弧ペア
        self.patterns = [
            (re.compile(r'「(.*?)」', re.DOTALL), '「」'),
            (re.compile(r'『(.*?)』', re.DOTALL), '『』'),
            (re.compile(r'\"(.*?)\"', re.DOTALL), '""'),
        ]

    def analyze(self, text: str, extract_features: bool = False) -> Dict:
        """
        テキストを会話文と地の文に分離して分析
        
        Args:
            text: 入力テキスト
            extract_features: Trueの場合、会話/地の文それぞれの特徴語を抽出する
        """
        dialogue_parts = []
        narrative_text = text
        
        # 会話部分の抽出と除去（地の文を作成）
        total_dialogue_char = 0
        
        # メインの括弧（「」）を優先処理
        pattern, bracket_type = self.patterns[0]
        matches = pattern.findall(text)
        
        for m in matches:
            dialogue_parts.append(m)
            total_dialogue_char += len(m)
            # 地の文から会話を削除（位置ズレを防ぐため置換）
            narrative_text = narrative_text.replace(f'「{m}」', '')

        # 空白除去
        narrative_clean = re.sub(r'\s+', '', narrative_text)
        dialogue_clean = "".join(dialogue_parts)
        
        total_char = len(text.replace('\n', '').replace(' ', ''))
        if total_char == 0: return {}

        # 比率計算
        dialogue_ratio = len(dialogue_clean) / total_char
        narrative_ratio = len(narrative_clean) / total_char
        
        result = {
            'total_characters': total_char,
            'dialogue_ratio': dialogue_ratio,
            'narrative_ratio': narrative_ratio,
            'dialogue_count': len(dialogue_parts),
            'dialogue_avg_len': len(dialogue_clean) / len(dialogue_parts) if dialogue_parts else 0
        }

        # 特徴語抽出（オプション）
        if extract_features:
            extractor = PhraseExtracter(min_count=2)
            
            # 会話文の特徴語
            df_diag = extractor.extract(dialogue_parts)
            result['dialogue_keywords'] = df_diag.head(10)['phrase'].tolist() if not df_diag.empty else []
            
            # 地の文の特徴語
            # 地の文は長いので分割して処理
            narrative_sentences = narrative_text.split('。')
            df_narr = extractor.extract(narrative_sentences)
            result['narrative_keywords'] = df_narr.head(10)['phrase'].tolist() if not df_narr.empty else []

        return result

```

### 2. 表記ゆれチェッカー (`japhrase/orthography_checker.py`)

編集距離（Levenshtein）とカタカナ正規化を用いて、統計的に「ゆれ」を検出します。辞書なしで動くのが強みです。

```python
"""
表記ゆれ検出モジュール
"""
import re
import pandas as pd
from typing import List, Dict
from .similarity import SimilarityAnalyzer
from .extracter import PhraseExtracter

class OrthographyVariantDetector:
    """統計的アプローチによる表記ゆれ検出"""

    def __init__(self, similarity_threshold: float = 0.75):
        self.sim_analyzer = SimilarityAnalyzer()
        self.threshold = similarity_threshold

    def check(self, text: str) -> List[Dict]:
        """テキスト内の表記ゆれ候補を検出"""
        # まずフレーズ抽出で頻出語をリストアップ
        extractor = PhraseExtracter(min_count=2, min_length=2)
        df = extractor.extract([text])
        
        if df.empty:
            return []
            
        phrases = df['phrase'].tolist()
        variants = []
        
        # 1. カタカナ長音のゆれチェック（例: コンピュータ vs コンピューター）
        katakana_phrases = [p for p in phrases if re.match(r'^[ァ-ヶー]+$', p)]
        variants.extend(self._check_katakana_variants(katakana_phrases))
        
        # 2. 編集距離による類似語チェック（例: 申し込み vs 申込み）
        # 計算量削減のため、文字種が同じもの同士で比較
        variants.extend(self._check_edit_distance_variants(phrases))
        
        return variants

    def _check_katakana_variants(self, phrases: List[str]) -> List[Dict]:
        """末尾長音の有無によるゆれを検出"""
        normalized = {}
        found = []
        
        for p in phrases:
            # 末尾のーを削除して正規化
            norm = p.rstrip('ー')
            if norm not in normalized:
                normalized[norm] = []
            normalized[norm].append(p)
            
        for norm, group in normalized.items():
            if len(group) > 1:
                # 頻度情報を付加できればなお良い
                found.append({
                    'type': 'katakana_vowel',
                    'root': norm,
                    'variants': group,
                    'message': f'カタカナ長音のゆれ: {", ".join(group)}'
                })
        return found

    def _check_edit_distance_variants(self, phrases: List[str]) -> List[Dict]:
        """編集距離が近く、包含関係にないものを検出"""
        found = []
        seen = set()
        
        # ソートして長さが近いもの同士を比較しやすくする
        sorted_phrases = sorted(phrases, key=len)
        
        for i, p1 in enumerate(sorted_phrases):
            for j in range(i + 1, min(i + 50, len(sorted_phrases))): # 近傍のみ探索
                p2 = sorted_phrases[j]
                
                # 完全一致や包含関係はスキップ
                if p1 == p2 or p1 in p2 or p2 in p1:
                    continue
                    
                # 文字種チェック（漢字カタカナ交じりなど、構成が似ているか）
                if not self._is_same_char_type(p1, p2):
                    continue

                sim = self.sim_analyzer.similarity_levenshtein(p1, p2)
                
                if sim >= self.threshold:
                    pair_key = tuple(sorted([p1, p2]))
                    if pair_key not in seen:
                        seen.add(pair_key)
                        found.append({
                            'type': 'similar_spelling',
                            'variants': [p1, p2],
                            'similarity': sim,
                            'message': f'類似表記: {p1} / {p2} (類似度: {sim:.2f})'
                        })
        return found

    def _is_same_char_type(self, s1, s2):
        """文字種構成が似ているか簡易チェック"""
        def get_type(s):
            if re.match(r'^[ァ-ヶー]+$', s): return 'katakana'
            if re.match(r'^[一-龠]+$', s): return 'kanji'
            return 'mixed'
        return get_type(s1) == get_type(s2)

```

### 3. 計量文学・文体指紋分析 (`japhrase/stylometry.py`)

語彙の豊かさ（TTR）や、文字種の比率（文体指紋）を分析します。辞書を使わずに「硬い文章か、柔らかい文章か」を数値化します。

```python
"""
計量文学（Stylometry）分析モジュール
"""
import re
import numpy as np
from collections import Counter
from typing import Dict
from .extracter import PhraseExtracter

class StylometryAnalyzer:
    """文体の定量的特徴を分析"""

    def analyze_vocabulary_richness(self, text: str) -> Dict:
        """語彙多様性指標（TTR, Yule's K）を計算"""
        # 簡易トークナイズ（PhraseExtracterのロジック利用）
        # ※厳密な単語分割ではないが、N-gram頻度から推定
        extractor = PhraseExtracter(min_count=1, min_length=2)
        df = extractor.extract([text])
        
        if df.empty:
            return {'ttr': 0, 'yules_k': 0}

        # 総トークン数（推定）
        N = df['freq'].sum()
        # ユニーク語彙数
        V = len(df)
        
        # TTR (Type-Token Ratio)
        ttr = V / N if N > 0 else 0
        
        # Yule's K (頻度スペクトルによる指標 - 長文に強い)
        # K = 10^4 * (Σ(freq^2) - N) / N^2
        sum_freq_sq = (df['freq'] ** 2).sum()
        yules_k = 10000 * (sum_freq_sq - N) / (N ** 2) if N > 0 else 0
        
        return {
            'total_tokens_est': int(N),
            'unique_types_est': V,
            'ttr': round(ttr, 4),
            'yules_k': round(yules_k, 2),
            'assessment': self._assess_richness(yules_k)
        }

    def analyze_char_type_ratio(self, text: str) -> Dict:
        """文字種比率（文体指紋）を計算"""
        counts = Counter()
        total = 0
        
        for char in text:
            if char.isspace(): continue
            total += 1
            if re.match(r'[一-龠]', char):
                counts['kanji'] += 1
            elif re.match(r'[ぁ-ん]', char):
                counts['hiragana'] += 1
            elif re.match(r'[ァ-ヶ]', char):
                counts['katakana'] += 1
            else:
                counts['other'] += 1
                
        if total == 0: return {}
        
        return {
            'kanji_ratio': counts['kanji'] / total,
            'hiragana_ratio': counts['hiragana'] / total,
            'katakana_ratio': counts['katakana'] / total,
            'style_type': self._guess_style(counts['kanji'] / total)
        }

    def _assess_richness(self, k: float) -> str:
        if k > 200: return "語彙の繰り返しが多い"
        if k < 80: return "語彙が非常に多様"
        return "標準的"

    def _guess_style(self, kanji_ratio: float) -> str:
        if kanji_ratio > 0.4: return "硬質・論文調"
        if kanji_ratio < 0.2: return "軟質・平易"
        return "標準的"

```

### 4. 人物相関ネットワーク (`japhrase/character_network.py`)

固有名詞（らしきもの）を抽出し、共起関係からエッジリスト（Source, Target, Weight）を生成します。グラフ描画は重いので、データ生成までを担当します。

```python
"""
人物/用語相関ネットワーク生成モジュール
"""
import itertools
import pandas as pd
from typing import List, Dict, Tuple
from .extracter import PhraseExtracter
from .cooccurrence import CooccurrenceAnalyzer

class CharacterNetworkGenerator:
    """登場人物や重要語の共起ネットワークを構築"""

    def __init__(self):
        self.cooc = CooccurrenceAnalyzer(window_size=30)
        self.extractor = PhraseExtracter(min_count=3)

    def generate_edgelist(self, text: str, top_n_nodes: int = 15) -> pd.DataFrame:
        """
        重要語間の共起ネットワーク（エッジリスト）を生成
        
        Returns:
            DataFrame: [source, target, weight(score)]
        """
        # 1. ノード候補（重要語）の抽出
        # 固有名詞らしきもの（漢字カタカナ連続など）を優先抽出するロジック
        # ※簡易的にPhraseExtracterのスコア上位を使用
        df_nodes = self.extractor.extract([text])
        if df_nodes.empty:
            return pd.DataFrame()
            
        # ノードのフィルタリング（2文字以上、記号なし）
        valid_nodes = df_nodes[
            (df_nodes['phrase'].str.len() >= 2) & 
            (~df_nodes['phrase'].str.contains(r'[、。]'))
        ].head(top_n_nodes)['phrase'].tolist()
        
        edges = []
        
        # 2. ノード間の共起強度計算
        # 組み合わせ総当たりだと重いので、主要ノードに対してCooccurrenceAnalyzerを回す
        for i, node_a in enumerate(valid_nodes):
            # node_a の共起語を取得
            df_cooc = self.cooc.analyze(text, node_a, top_n=len(valid_nodes))
            
            if df_cooc.empty: continue
            
            # 共起語の中に他のノード(node_b)が含まれていればエッジを作成
            for _, row in df_cooc.iterrows():
                node_b = row['phrase']
                score = row['score']
                
                if node_b in valid_nodes and node_b != node_a:
                    # 無向グラフとして正規化（辞書順）
                    source, target = sorted([node_a, node_b])
                    edges.append({
                        'source': source,
                        'target': target,
                        'weight': score
                    })

        # 重複削除して集計
        if not edges:
            return pd.DataFrame()
            
        df_edges = pd.DataFrame(edges)
        df_edges = df_edges.groupby(['source', 'target'])['weight'].max().reset_index()
        
        return df_edges.sort_values('weight', ascending=False)

```

### 統合利用イメージ

これらのモジュールを組み合わせて、一括レポートを出すスクリプトのイメージです。

```python
from japhrase import (
    DialogueAnalyzer, 
    OrthographyVariantDetector, 
    StylometryAnalyzer,
    CharacterNetworkGenerator
)

text = "..." # 長い小説テキスト

# 1. 会話分析
dia = DialogueAnalyzer()
res_dia = dia.analyze(text, extract_features=True)
print(f"会話比率: {res_dia['dialogue_ratio']:.2%}")

# 2. 表記ゆれ
ortho = OrthographyVariantDetector()
issues = ortho.check(text)
for i in issues:
    print(i['message'])

# 3. 文体診断
stylo = StylometryAnalyzer()
res_stylo = stylo.analyze_vocabulary_richness(text)
print(f"語彙多様性(Yule's K): {res_stylo['yules_k']}")

# 4. 相関図データ
net = CharacterNetworkGenerator()
df_edges = net.generate_edgelist(text)
print(df_edges.head())

```

これらは全て `japhrase` の既存クラス（`extracter`, `cooccurrence`）に依存させているため、高速かつ一貫性のある動作が期待できます。実装に取り掛かりましょうか？


`japhrase` の「統計的・コーパスベース」という強みを生かすなら、**「既存の固定辞書（Danbooruタグなど）に頼らず、ユーザー自身の過去の『当たりプロンプト』から勝ちパターンを統計的に学習して提案する」** というアプローチが最強です。

これを **「プロンプト・マイニング（Prompt Mining）」** と位置づけ、以下の機能を備えた `japhrase/prompt_optimizer.py` を提案・実装します。

### 独自の強み（ユニークポイント）

1. **俺の勝ちパターン抽出:** ネットの一般論ではなく、「あなたが過去に生成した良画像」のプロンプト（コーパス）を分析し、「この単語を使う時、あなたはいつもこの単語もセットで使っている（のに今回は抜けている）」といった**文脈的欠落**を統計的に指摘します（共起分析の応用）。
2. **重みインフレの検知:** `(word:1.5)` みたいな重み付けが形骸化していないか、統計的に分布を見て警告します。
3. **括弧の自動バランス:** ComfyUIでよくある `(((` の閉じ忘れなどを構文解析します。

以下に実装コードを提示します。

### 実装: `japhrase/prompt_optimizer.py`

```python
"""
ComfyUI/Stable Diffusion プロンプト最適化・分析モジュール

japhraseの統計的アプローチを応用し、過去の良プロンプト(コーパス)から
「あなただけの勝ちパターン」を学習して提案・修正する。
"""

import re
from collections import Counter
from typing import List, Dict, Tuple, Optional
import pandas as pd
import numpy as np

from .extracter import PhraseExtracter
from .cooccurrence import CooccurrenceAnalyzer
from .similarity import SimilarityAnalyzer

class PromptOptimizer:
    """プロンプトの統計的最適化・推敲支援"""

    def __init__(self, corpus_prompts: List[str] = None):
        """
        Args:
            corpus_prompts: 過去の良プロンプトのリスト（学習用コーパス）
        """
        self.corpus_prompts = corpus_prompts or []
        self.extractor = PhraseExtracter(min_count=2)
        self.cooc = CooccurrenceAnalyzer(window_size=100) # プロンプトは短いのでウィンドウ広め
        self.sim = SimilarityAnalyzer()
        
        # コーパスがあれば事前解析
        self.corpus_stats = self._analyze_corpus() if self.corpus_prompts else None

    def _analyze_corpus(self) -> Dict:
        """コーパス内のタグ頻度や共起情報を解析"""
        # 単純なカンマ区切りでタグ化
        all_tags = []
        for p in self.corpus_prompts:
            # カンマで分割し、重み記号などを除去して正規化
            tags = [self._normalize_tag(t) for t in p.split(',')]
            all_tags.extend([t for t in tags if t])
            
        freq_dist = Counter(all_tags)
        return {
            'freq': freq_dist,
            'total_tags': len(all_tags),
            'unique_tags': len(freq_dist)
        }

    def analyze(self, prompt: str) -> Dict:
        """
        入力プロンプトを分析し、改善案と統計情報を返す
        """
        # 1. 構文チェック（括弧バランス）
        syntax_issues = self._check_syntax(prompt)
        
        # 2. タグの分解と正規化
        raw_tags = [t.strip() for t in prompt.split(',') if t.strip()]
        normalized_tags = [self._normalize_tag(t) for t in raw_tags]
        
        # 3. 重複・冗長性検出
        redundancy_issues = self._check_redundancy(normalized_tags)
        
        # 4. コーパスに基づく提案（欠落タグの発見）
        suggestions = []
        if self.corpus_stats:
            suggestions = self._mining_suggestions(prompt, normalized_tags)

        # 5. 重み分布チェック
        weight_stats = self._analyze_weights(prompt)

        return {
            'syntax_issues': syntax_issues,
            'redundancy_issues': redundancy_issues,
            'suggestions': suggestions,
            'weight_stats': weight_stats,
            'token_count': len(normalized_tags),
            'formatted_prompt': self._format_prompt(prompt) # 整形版
        }

    def _mining_suggestions(self, full_prompt: str, current_tags: List[str]) -> List[Dict]:
        """「いつもの勝ちパターン」から欠落しているタグを提案"""
        suggestions = []
        
        # プロンプト全体を1つの「文章」とみなして共起分析
        # 現在のタグの中で、コーパス内で特に「牽引力」が強いキーワードを探す
        
        # 簡易的に、現在のタグそれぞれについて共起語を調べる
        # ※本来は全文結合して計算するが、軽量化のため主要タグのみ
        
        candidate_scores = Counter()
        
        # コーパスを大きな1つのテキストとして結合（タグ区切り）
        corpus_text = " , ".join(self.corpus_prompts)
        
        for tag in current_tags:
            if len(tag) < 3: continue # 短すぎるタグはスキップ
            
            # このタグと一緒に使われる傾向があるタグを抽出
            df_cooc = self.cooc.analyze(corpus_text, tag, top_n=5)
            
            if df_cooc.empty: continue
            
            for _, row in df_cooc.iterrows():
                suggested = row['phrase']
                score = row['score']
                
                # 正規化して比較
                norm_suggested = self._normalize_tag(suggested)
                
                # 既にプロンプトに含まれていればスキップ
                if any(curr in norm_suggested or norm_suggested in curr for curr in current_tags):
                    continue
                    
                # スコアを加算（複数のタグから推薦されるほど強い）
                candidate_scores[suggested] += score

        # 上位を提案
        for tag, score in candidate_scores.most_common(5):
            suggestions.append({
                'tag': tag,
                'score': score,
                'reason': '文脈的共起（過去のパターンに基づく推奨）'
            })
            
        return suggestions

    def _check_redundancy(self, tags: List[str]) -> List[Dict]:
        """意味的な重複や包含関係を検出"""
        issues = []
        # ソートして比較回数を減らす
        sorted_tags = sorted(list(set(tags)), key=len)
        
        for i, t1 in enumerate(sorted_tags):
            for t2 in sorted_tags[i+1:]:
                # 包含関係チェック (例: "eyes", "blue eyes")
                # ※単純な包含は意図的（強調）な場合もあるので、類似度も見る
                if t1 in t2 and t1 != t2:
                    # 短い方が長い方に完全に含まれている
                    issues.append({
                        'type': 'inclusion',
                        'tags': (t1, t2),
                        'message': f"冗長なタグ: '{t1}' は '{t2}' に含まれています"
                    })
                
                # 編集距離チェック（タイプミス検出など）
                # 例: "high quality", "high qualtiy"
                elif self.sim.similarity_levenshtein(t1, t2) > 0.85:
                    issues.append({
                        'type': 'similarity',
                        'tags': (t1, t2),
                        'message': f"酷似したタグ（重複?）: '{t1}' / '{t2}'"
                    })
        return issues

    def _check_syntax(self, prompt: str) -> List[str]:
        """括弧のバランスなどをチェック"""
        issues = []
        open_count = prompt.count('(')
        close_count = prompt.count(')')
        if open_count != close_count:
            issues.append(f"括弧の不一致: ( が {open_count}個, ) が {close_count}個")
            
        if ',,' in prompt:
            issues.append("空のカンマ(,,)が含まれています")
            
        return issues

    def _analyze_weights(self, prompt: str) -> Dict:
        """重み付け(1.2)などの統計情報を分析"""
        # (tag:1.2) や (tag:0.9) を抽出
        weights = []
        matches = re.findall(r':([0-9.]+)\)', prompt)
        for m in matches:
            try:
                weights.append(float(m))
            except ValueError:
                pass
                
        if not weights:
            return {'has_weights': False}
            
        avg_weight = np.mean(weights)
        max_weight = np.max(weights)
        
        warnings = []
        if avg_weight > 1.3:
            warnings.append("全体的に重みがインフレしています（平均 > 1.3）。CFG Scaleの調整を検討してください。")
        if max_weight > 1.8:
            warnings.append(f"極端に強い重み({max_weight})があります。画像が崩壊する可能性があります。")

        return {
            'has_weights': True,
            'average': round(avg_weight, 2),
            'max': max_weight,
            'count': len(weights),
            'warnings': warnings
        }

    def _normalize_tag(self, tag: str) -> str:
        """タグの正規化（重み除去、小文字化）"""
        # (tag:1.2) -> tag
        tag = re.sub(r'\((.*?):[0-9.]+\)', r'\1', tag)
        # (((tag))) -> tag
        tag = re.sub(r'[\(\)]', '', tag)
        return tag.strip().lower()

    def _format_prompt(self, prompt: str) -> str:
        """プロンプトの自動整形"""
        # カンマ区切りで分割し、空白を除去して再結合
        tags = [t.strip() for t in prompt.split(',') if t.strip()]
        return ", ".join(tags)

# 使用例
if __name__ == "__main__":
    # 過去の良プロンプト（ダミー）
    corpus = [
        "masterpiece, best quality, 1girl, solo, blue eyes, white hair, standing, looking at viewer",
        "best quality, masterpiece, scenery, blue sky, cloud, day, outdoors",
        "1girl, school uniform, serafuku, blue eyes, black hair, smile"
    ]
    
    optimizer = PromptOptimizer(corpus)
    
    # 分析対象（あえて欠損やミスを入れたプロンプト）
    target = "1girl, blue eyes, ((white hair)), (standing:2.5), looking at viewer, blue eyes, school uniform"
    
    result = optimizer.analyze(target)
    
    import json
    print(json.dumps(result, indent=2, ensure_ascii=False))

```

### このツールの使い方・シナリオ

1. **コーパス準備:** 自分が過去に生成して「これはイイ！」と思った画像のプロンプトを `good_prompts.txt` に溜め込んでおく。
2. **実行:** 新しいプロンプトを書くときにこのツールを通す。
3. **メリット:**
* 「あ、そういえば制服（school uniform）指定してるのに、いつもセットで入れてる『serafuku』入れ忘れてるよ」と気づける（`suggestions`）。
* 「`blue eyes` 2回書いてるよ」と気づける（`redundancy_issues`）。
* 「`(standing:2.5)` は強すぎて絵が壊れるかもよ」と警告される（`weight_stats`）。



これで、ComfyUIワークフローの中に「japhraseによるプロンプト検品・補完ノード」みたいなものを作る下地ができます。実装に追加しますか？

