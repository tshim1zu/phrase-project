# coding:utf-8
"""
データソースモジュール
様々なソースから日本語テキストを取得
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023"

from abc import ABC, abstractmethod
from typing import List, Optional
import requests
import time
from pathlib import Path


class DataSource(ABC):
    """データソースの抽象基底クラス"""

    @abstractmethod
    def fetch(self) -> List[str]:
        """テキストのリストを取得"""
        pass


class TextFileSource(DataSource):
    """
    テキストファイルからデータを取得

    使用例:
        >>> source = TextFileSource(["file1.txt", "file2.txt"])
        >>> texts = source.fetch()
    """

    def __init__(self, filepaths: List[str], encoding: str = 'utf-8'):
        """
        Parameters:
            filepaths (List[str]): ファイルパスのリスト
            encoding (str): 文字エンコーディング
        """
        self.filepaths = filepaths if isinstance(filepaths, list) else [filepaths]
        self.encoding = encoding

    def fetch(self) -> List[str]:
        """ファイルからテキストを読み込む"""
        from .utils import read_files
        return read_files(self.filepaths, encoding=self.encoding)


class AozoraBunkoSource(DataSource):
    """
    青空文庫からテキストを取得

    使用例:
        >>> source = AozoraBunkoSource()
        >>> texts = source.fetch_random(10)
    """

    BASE_URL = "https://www.aozora.gr.jp"

    def __init__(self, delay: float = 1.0):
        """
        Parameters:
            delay (float): リクエスト間の待機時間（秒）
        """
        self.delay = delay
        self.session = requests.Session()
        self.session.headers.update({
            'User-Agent': 'jphrase/0.1.0 (https://github.com/tshim1zu/phrase-project)'
        })

    def fetch(self) -> List[str]:
        """デフォルト: ランダムに10作品取得"""
        return self.fetch_random(10)

    def fetch_random(self, n: int = 10) -> List[str]:
        """
        ランダムに作品を取得

        Parameters:
            n (int): 取得する作品数

        Returns:
            List[str]: テキストのリスト
        """
        texts = []

        # 簡易実装: 人気作品のIDリスト（実際はもっと増やす）
        # これらは青空文庫の実際の作品ID
        sample_ids = [
            '773',   # 夏目漱石「こころ」
            '752',   # 太宰治「人間失格」
            '43',    # 芥川龍之介「羅生門」
            '42',    # 芥川龍之介「鼻」
            '2253',  # 宮沢賢治「銀河鉄道の夜」
            '456',   # 夏目漱石「坊っちゃん」
            '1',     # 夏目漱石「吾輩は猫である」
            '160',   # 太宰治「走れメロス」
            '1235',  # 森鴎外「舞姫」
            '45',    # 芥川龍之介「蜘蛛の糸」
        ]

        # nの数だけランダムに取得（重複あり）
        import random
        selected_ids = random.choices(sample_ids, k=min(n, len(sample_ids)))

        for work_id in selected_ids:
            try:
                text = self._fetch_work(work_id)
                if text:
                    texts.append(text)
                time.sleep(self.delay)
            except Exception as e:
                print(f"Error fetching work {work_id}: {e}")
                continue

        return texts

    def _fetch_work(self, work_id: str) -> Optional[str]:
        """
        特定の作品を取得

        Parameters:
            work_id (str): 作品ID

        Returns:
            Optional[str]: テキスト
        """
        # 青空文庫のテキストファイルURL
        # 実際のURL構造に基づく（簡易版）
        url = f"{self.BASE_URL}/cards/000{work_id[:-2]}/{work_id}/files/{work_id}_ruby_{work_id}.zip"

        # 実装の簡易版: 実際はZIPを解凍してテキスト取得
        # ここでは概念的な実装
        try:
            # 実際の実装では、zipファイルをダウンロードして解凍
            # response = self.session.get(url)
            # ... ZIP処理 ...

            # プレースホルダー: 実装予定
            return None
        except Exception as e:
            return None

    def fetch_author(self, author_name: str, limit: int = 10) -> List[str]:
        """
        特定の著者の作品を取得（将来の実装）

        Parameters:
            author_name (str): 著者名
            limit (int): 取得する作品数

        Returns:
            List[str]: テキストのリスト
        """
        # TODO: 実装予定
        raise NotImplementedError("fetch_author is not yet implemented")


class WikipediaSource(DataSource):
    """
    Wikipedia日本語版からテキストを取得

    使用例:
        >>> source = WikipediaSource()
        >>> texts = source.fetch_random(100)
    """

    API_URL = "https://ja.wikipedia.org/w/api.php"

    def __init__(self, language: str = 'ja', delay: float = 0.5):
        """
        Parameters:
            language (str): 言語コード
            delay (float): リクエスト間の待機時間（秒）
        """
        self.language = language
        self.delay = delay
        self.session = requests.Session()
        self.session.headers.update({
            'User-Agent': 'jphrase/0.1.0 (https://github.com/tshim1zu/phrase-project)'
        })

    def fetch(self) -> List[str]:
        """デフォルト: ランダムに100ページ取得"""
        return self.fetch_random(100)

    def fetch_random(self, n: int = 100) -> List[str]:
        """
        ランダムにページを取得

        Parameters:
            n (int): 取得するページ数

        Returns:
            List[str]: テキストのリスト
        """
        texts = []

        # Wikipedia APIでランダムページを取得
        batch_size = 10  # 一度に取得する数

        for i in range(0, n, batch_size):
            try:
                params = {
                    'action': 'query',
                    'format': 'json',
                    'list': 'random',
                    'rnnamespace': 0,  # 記事のみ
                    'rnlimit': min(batch_size, n - i)
                }

                response = self.session.get(self.API_URL, params=params)
                data = response.json()

                # 各ページのコンテンツを取得
                for page in data['query']['random']:
                    title = page['title']
                    text = self._fetch_page_content(title)
                    if text:
                        texts.append(text)

                time.sleep(self.delay)

            except Exception as e:
                print(f"Error fetching random pages: {e}")
                continue

        return texts

    def _fetch_page_content(self, title: str) -> Optional[str]:
        """
        特定のページの本文を取得

        Parameters:
            title (str): ページタイトル

        Returns:
            Optional[str]: ページ本文
        """
        try:
            params = {
                'action': 'query',
                'format': 'json',
                'titles': title,
                'prop': 'extracts',
                'explaintext': True,
                'exsectionformat': 'plain'
            }

            response = self.session.get(self.API_URL, params=params)
            data = response.json()

            pages = data['query']['pages']
            for page_id, page_data in pages.items():
                return page_data.get('extract', '')

            return None

        except Exception as e:
            print(f"Error fetching page '{title}': {e}")
            return None

    def fetch_pages(self, titles: List[str]) -> List[str]:
        """
        特定のページを取得

        Parameters:
            titles (List[str]): ページタイトルのリスト

        Returns:
            List[str]: テキストのリスト
        """
        texts = []

        for title in titles:
            text = self._fetch_page_content(title)
            if text:
                texts.append(text)
            time.sleep(self.delay)

        return texts

    def fetch_category(self, category: str, limit: int = 100) -> List[str]:
        """
        カテゴリからページを取得（将来の実装）

        Parameters:
            category (str): カテゴリ名
            limit (int): 取得するページ数

        Returns:
            List[str]: テキストのリスト
        """
        # TODO: 実装予定
        raise NotImplementedError("fetch_category is not yet implemented")


class DatabaseSource(DataSource):
    """
    データベースからテキストを取得（将来の実装）

    使用例:
        >>> source = DatabaseSource(
        ...     connection_string="postgresql://localhost/mydb",
        ...     query="SELECT text FROM articles"
        ... )
        >>> texts = source.fetch()
    """

    def __init__(self, connection_string: str, query: str):
        """
        Parameters:
            connection_string (str): データベース接続文字列
            query (str): SQLクエリ
        """
        self.connection_string = connection_string
        self.query = query

    def fetch(self) -> List[str]:
        """データベースからテキストを取得"""
        # TODO: SQLAlchemyを使った実装
        raise NotImplementedError("DatabaseSource is not yet implemented")
