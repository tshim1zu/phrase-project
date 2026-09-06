"""datasource.py の回帰テスト

コードレビューで指摘された以下の点の再発防止:
- requests未インストール時にNoneType AttributeErrorではなく
  分かりやすいImportErrorになること
- WikipediaSource(language=...)が実際にAPIエンドポイントへ反映されること
- HTTPリクエストにtimeoutが設定されていること
"""
import pytest

from japhrase.datasource import WikipediaSource, AozoraBunkoSource, TextFileSource


class TestWikipediaSourceLanguage:
    def test_default_language_is_japanese(self):
        source = WikipediaSource()
        assert source.api_url == 'https://ja.wikipedia.org/w/api.php'

    def test_language_parameter_changes_api_url(self):
        source = WikipediaSource(language='en')
        assert source.api_url == 'https://en.wikipedia.org/w/api.php'
        assert 'ja.wikipedia' not in source.api_url


class TestRequestsGuard:
    def test_wikipedia_source_raises_clear_error_without_requests(self, monkeypatch):
        import japhrase.datasource as ds
        monkeypatch.setattr(ds, 'requests', None)

        with pytest.raises(ImportError):
            WikipediaSource()

    def test_aozora_source_raises_clear_error_without_requests(self, monkeypatch):
        import japhrase.datasource as ds
        monkeypatch.setattr(ds, 'requests', None)

        with pytest.raises(ImportError):
            AozoraBunkoSource()


class TestRequestTimeout:
    def test_wikipedia_requests_have_timeout(self, monkeypatch):
        """ネットワーク障害時に無期限に待たないよう、session.get()へ
        timeoutが渡されていること"""
        source = WikipediaSource()

        captured = {}

        class FakeResponse:
            def json(self):
                return {'query': {'random': []}}

        def fake_get(url, params=None, timeout=None):
            captured['timeout'] = timeout
            return FakeResponse()

        monkeypatch.setattr(source.session, 'get', fake_get)
        source.fetch_random(1)

        assert captured['timeout'] is not None
        assert captured['timeout'] > 0


class TestTextFileSourceUnaffected:
    """requestsに依存しないデータソースは今回の変更の影響を受けないこと"""

    def test_text_file_source_does_not_require_requests(self, tmp_path):
        f = tmp_path / "a.txt"
        f.write_text("hello\nworld", encoding='utf-8')
        source = TextFileSource([str(f)])
        texts = source.fetch()
        assert len(texts) > 0
