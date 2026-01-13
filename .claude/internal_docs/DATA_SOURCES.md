# データソースガイド

日本語長文データを取得する方法をまとめています。

## 🚀 すぐに使える無料データソース

### 1. 青空文庫（最もおすすめ）

**概要：** 著作権が切れた日本の文学作品

```python
from japhrase.datasource import AozoraBunkoSource

source = AozoraBunkoSource()

# 著者から取得
texts = source.fetch_author("夏目漱石", limit=10)
texts = source.fetch_author("芥川龍之介", limit=5)

# ランダムに取得
texts = source.fetch_random(100)

# カテゴリから取得
texts = source.fetch_category("小説")
```

**特徴：**
- ✅ 完全無料・合法
- ✅ 高品質な日本語
- ✅ 15,000作品以上
- ✅ テキスト形式で取得可能

**URL:** https://www.aozora.gr.jp/

**実装方法:**
- 図書カードHTMLをパース
- テキストファイルをダウンロード
- robots.txt準拠

---

### 2. Wikipedia日本語版

**概要：** 百科事典的な最新情報

```python
from japhrase.datasource import WikipediaSource

source = WikipediaSource(language='ja')

# カテゴリから取得
texts = source.fetch_category("人工知能", limit=50)
texts = source.fetch_category("日本史", limit=100)

# ランダムページ取得
texts = source.fetch_random(1000)

# 特定の記事を取得
texts = source.fetch_pages(["機械学習", "深層学習", "自然言語処理"])
```

**特徴：**
- ✅ MediaWiki API完備
- ✅ 多様なジャンル
- ✅ 最新情報
- ✅ CC-BY-SAライセンス

**API:** https://ja.wikipedia.org/w/api.php

**実装例:**
```python
import requests

def fetch_wikipedia_page(title):
    url = "https://ja.wikipedia.org/w/api.php"
    params = {
        "action": "query",
        "format": "json",
        "titles": title,
        "prop": "extracts",
        "explaintext": True
    }
    response = requests.get(url, params=params)
    data = response.json()
    pages = data["query"]["pages"]
    for page_id, page_data in pages.items():
        return page_data.get("extract", "")
```

---

### 3. livedoor ニュースコーパス

**概要：** 機械学習用に公開されたニュース記事

```python
from japhrase.datasource import LivedoorNewsSource

source = LivedoorNewsSource()

# カテゴリ別取得
texts = source.fetch_category("it-life-hack")
texts = source.fetch_category("sports-watch")
texts = source.fetch_category("movie-enter")

# 全カテゴリ取得
texts = source.fetch_all()
```

**カテゴリ:**
- topic-news (トピックニュース)
- sports-watch (スポーツ)
- it-life-hack (IT)
- movie-enter (映画)
- kaden-channel (家電)
- livedoor-homme (ファッション)
- peachy (女性向け)
- smax (ガジェット)
- dokujo-tsushin (独女通信)

**ダウンロード:** https://www.rondhuit.com/download.html

**特徴:**
- ✅ すぐダウンロード可能
- ✅ カテゴリ分類済み
- ✅ 約7,400記事
- ✅ 商用利用可能

---

### 4. 日本語Wikipedia全文ダンプ

**概要：** Wikipediaの全データを一括ダウンロード

```bash
# ダウンロード（大容量：数GB）
wget https://dumps.wikimedia.org/jawiki/latest/jawiki-latest-pages-articles.xml.bz2
```

**処理:**
```python
from japhrase.datasource import WikiDumpSource

source = WikiDumpSource("jawiki-latest-pages-articles.xml.bz2")
texts = source.fetch(limit=10000)
```

**特徴:**
- ✅ 全記事を一括取得
- ✅ オフラインで利用可能
- ⚠️ 大容量（圧縮で3GB程度）

**URL:** https://dumps.wikimedia.org/jawiki/

---

## 💰 有料だが有用なデータソース

### 5. NewsAPI

**概要：** 世界中のニュースをAPI経由で取得

```python
from japhrase.datasource import NewsAPISource

source = NewsAPISource(
    api_key="YOUR_API_KEY",
    language="ja"
)

# キーワード検索
texts = source.fetch(
    query="人工知能",
    from_date="2025-01-01",
    to_date="2025-01-31"
)

# ソース指定
texts = source.fetch(sources="nhk-news,asahi-shimbun")
```

**料金:**
- 無料: 100リクエスト/日（開発用）
- 有料: $449/月〜（商用）

**URL:** https://newsapi.org/

---

### 6. Twitter/X API

**概要：** SNS投稿を取得

```python
from japhrase.datasource import TwitterSource

source = TwitterSource(bearer_token="YOUR_TOKEN")

# キーワード検索
texts = source.fetch(
    query="機械学習 lang:ja",
    max_results=100
)

# ユーザーのツイート取得
texts = source.fetch_user_tweets(user_id="123456789")
```

**料金:**
- Free: 機能制限あり
- Basic: $100/月
- Pro: $5,000/月

**URL:** https://developer.twitter.com/

---

## 🌐 Webスクレイピング（要注意）

### 注意事項

⚠️ **必ず確認すること:**
1. robots.txt を確認
2. 利用規約を読む
3. 著作権を尊重
4. アクセス頻度を制限（1秒以上の間隔）
5. User-Agentを設定

```python
from japhrase.datasource import WebScraperSource

source = WebScraperSource(
    urls=["https://example.com/article1"],
    respect_robots_txt=True,  # robots.txtを尊重
    delay=2.0,                 # 2秒待機
    user_agent="MyBot/1.0"
)

texts = source.fetch()
```

### robots.txtの確認方法

```python
import requests

def check_robots_txt(url):
    domain = '/'.join(url.split('/')[:3])
    robots_url = f"{domain}/robots.txt"
    response = requests.get(robots_url)
    print(response.text)

check_robots_txt("https://example.com")
```

---

## 📄 ローカルファイルから取得

### PDF文書

```python
from japhrase.datasource import PDFSource

source = PDFSource("documents/*.pdf")
texts = source.fetch()

# または個別に
source = PDFSource(["report1.pdf", "report2.pdf"])
texts = source.fetch()
```

**必要なライブラリ:**
```bash
pip install PyPDF2 pdfplumber
```

---

### Word文書

```python
from japhrase.datasource import WordSource

source = WordSource("documents/*.docx")
texts = source.fetch()
```

**必要なライブラリ:**
```bash
pip install python-docx
```

---

### テキストファイル

```python
from japhrase.datasource import TextFileSource

source = TextFileSource(["file1.txt", "file2.txt"])
texts = source.fetch()

# ディレクトリ全体
source = TextFileSource("texts/*.txt")
texts = source.fetch()
```

---

## 🗄️ データベースから取得

### PostgreSQL

```python
from japhrase.datasource import DatabaseSource

source = DatabaseSource(
    connection_string="postgresql://user:password@localhost/mydb",
    query="SELECT content FROM articles WHERE lang='ja'"
)

texts = source.fetch()
```

### MySQL

```python
source = DatabaseSource(
    connection_string="mysql://user:password@localhost/mydb",
    query="SELECT text FROM posts WHERE created_at > '2025-01-01'"
)

texts = source.fetch()
```

### SQLite

```python
source = DatabaseSource(
    connection_string="sqlite:///data.db",
    query="SELECT text FROM documents"
)

texts = source.fetch()
```

---

## 🎓 学術データベース

### CiNii（日本の学術論文）

```python
from japhrase.datasource import CiNiiSource

source = CiNiiSource(app_id="YOUR_APP_ID")

texts = source.fetch(
    query="自然言語処理",
    count=100
)
```

**URL:** https://cir.nii.ac.jp/
**特徴:** 日本の学術論文検索サービス

---

## 📊 推奨データソース（用途別）

| 用途 | おすすめデータソース |
|------|---------------------|
| **SNS分析** | Twitter API（有料） |
| **ニュース分析** | livedoorコーパス（無料）<br>NewsAPI（有料） |
| **文学作品** | 青空文庫（無料） |
| **百科事典的** | Wikipedia（無料） |
| **学術論文** | CiNii（無料） |
| **社内文書** | ローカルファイル（PDF/Word） |

---

## 🚀 実装の優先順位

### Phase 1: すぐ実装
1. ✅ テキストファイル読み込み（既存）
2. ✅ 青空文庫スクレイパー
3. ✅ Wikipedia API

### Phase 2: 中期
4. PDF/Word読み込み
5. データベース接続
6. livedoorコーパス対応

### Phase 3: 長期
7. NewsAPI統合
8. Twitter API統合
9. 汎用Webスクレイパー

---

## 🔒 法的・倫理的注意事項

1. **著作権を尊重**
   - 公開されていても著作権は存在
   - 研究・個人利用の範囲で

2. **利用規約を確認**
   - 各サービスの規約を必ず読む
   - 商用利用可否を確認

3. **robots.txtを尊重**
   - スクレイピング前に確認
   - Disallowされている場合は諦める

4. **適切なアクセス頻度**
   - サーバーに負荷をかけない
   - 1秒以上の間隔を空ける

5. **User-Agent設定**
   - 身元を明らかにする
   - 連絡先を含める

---

## 📚 参考リンク

- [青空文庫](https://www.aozora.gr.jp/)
- [Wikipedia API](https://www.mediawiki.org/wiki/API:Main_page)
- [livedoorコーパス](https://www.rondhuit.com/download.html)
- [NewsAPI](https://newsapi.org/)
- [Twitter API](https://developer.twitter.com/)
- [CiNii](https://cir.nii.ac.jp/)
