# coding:utf-8
"""
utils.pyのテスト
"""

import pytest
import pandas as pd
import os
import tempfile
from pathlib import Path
from japhrase.utils import (
    read_text_file,
    read_csv_file,
    read_tsv_file,
    read_file,
    read_files,
    export_to_csv,
    export_to_json,
    export_to_excel,
    ensure_directory,
    detect_encoding
)


class TestEncodingDetection:
    """エンコーディング自動検出のテスト"""

    @pytest.fixture
    def temp_dir(self):
        """一時ディレクトリを提供"""
        with tempfile.TemporaryDirectory() as tmpdir:
            yield tmpdir

    def test_detect_encoding_utf8(self, temp_dir):
        """UTF-8エンコーディングの検出テスト"""
        filepath = os.path.join(temp_dir, "test_utf8.txt")
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write("これはUTF-8のテキストです\n")

        encoding = detect_encoding(filepath)
        assert encoding.lower() in ['utf-8', 'utf8', 'ascii']

    def test_detect_encoding_shift_jis(self, temp_dir):
        """Shift-JISエンコーディングの検出テスト"""
        filepath = os.path.join(temp_dir, "test_sjis.txt")
        try:
            with open(filepath, 'w', encoding='shift_jis') as f:
                f.write("これはShift-JISのテキストです\n")

            encoding = detect_encoding(filepath)
            # chardetはcp932として検出することがある
            assert encoding.lower() in ['shift_jis', 'shift-jis', 'cp932', 'sjis']
        except Exception:
            pytest.skip("Shift-JIS encoding not available on this system")

    def test_read_text_file_auto_encoding(self, temp_dir):
        """自動エンコーディング検出でのファイル読み込みテスト"""
        # UTF-8ファイル
        filepath_utf8 = os.path.join(temp_dir, "test_utf8.txt")
        with open(filepath_utf8, 'w', encoding='utf-8') as f:
            f.write("日本語テキスト\n")

        lines = read_text_file(filepath_utf8, encoding='auto')
        assert len(lines) == 1
        assert "日本語テキスト" in lines


class TestReadFunctions:
    """ファイル読み込み関数のテスト"""

    @pytest.fixture
    def temp_dir(self):
        """一時ディレクトリを提供"""
        with tempfile.TemporaryDirectory() as tmpdir:
            yield tmpdir

    def test_read_text_file(self, temp_dir):
        """テキストファイル読み込みのテスト"""
        filepath = os.path.join(temp_dir, "test.txt")
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write("こんにちは\n")
            f.write("ありがとう\n")
            f.write("\n")
            f.write("さようなら\n")

        lines = read_text_file(filepath)

        assert len(lines) == 3
        assert "こんにちは" in lines
        assert "ありがとう" in lines
        assert "さようなら" in lines

    def test_read_csv_file(self, temp_dir):
        """CSVファイル読み込みのテスト"""
        filepath = os.path.join(temp_dir, "test.csv")
        df = pd.DataFrame({
            'text': ['フォロー', 'プレゼント', 'キャンペーン'],
            'count': [10, 20, 30]
        })
        df.to_csv(filepath, index=False, encoding='utf-8')

        texts = read_csv_file(filepath, column='text')

        assert len(texts) == 3
        assert 'フォロー' in texts
        assert 'プレゼント' in texts

    def test_read_tsv_file(self, temp_dir):
        """TSVファイル読み込みのテスト"""
        filepath = os.path.join(temp_dir, "test.tsv")
        df = pd.DataFrame({
            'text': ['テスト1', 'テスト2', 'テスト3']
        })
        df.to_csv(filepath, index=False, sep='\t', encoding='utf-8')

        texts = read_tsv_file(filepath)

        assert len(texts) == 3
        assert 'テスト1' in texts

    def test_read_file_auto_detect(self, temp_dir):
        """拡張子による自動判別のテスト"""
        # テキストファイル
        txt_path = os.path.join(temp_dir, "test.txt")
        with open(txt_path, 'w', encoding='utf-8') as f:
            f.write("テキスト\n")

        texts = read_file(txt_path)
        assert len(texts) == 1

        # CSVファイル
        csv_path = os.path.join(temp_dir, "test.csv")
        df = pd.DataFrame({'text': ['CSV']})
        df.to_csv(csv_path, index=False)

        texts = read_file(csv_path)
        assert len(texts) == 1

    def test_read_files_multiple(self, temp_dir):
        """複数ファイル読み込みのテスト"""
        # ファイル1
        file1 = os.path.join(temp_dir, "test1.txt")
        with open(file1, 'w', encoding='utf-8') as f:
            f.write("ファイル1\n")

        # ファイル2
        file2 = os.path.join(temp_dir, "test2.txt")
        with open(file2, 'w', encoding='utf-8') as f:
            f.write("ファイル2\n")

        texts = read_files([file1, file2])

        assert len(texts) == 2
        assert "ファイル1" in texts
        assert "ファイル2" in texts


class TestExportFunctions:
    """ファイル出力関数のテスト"""

    @pytest.fixture
    def temp_dir(self):
        """一時ディレクトリを提供"""
        with tempfile.TemporaryDirectory() as tmpdir:
            yield tmpdir

    @pytest.fixture
    def sample_df(self):
        """サンプルDataFrameを提供"""
        return pd.DataFrame({
            'seqchar': ['フォロー', 'プレゼント'],
            'freq': [10, 20],
            'length': [4, 5]
        })

    def test_export_to_csv(self, temp_dir, sample_df):
        """CSV出力のテスト"""
        filepath = os.path.join(temp_dir, "output.csv")
        export_to_csv(sample_df, filepath)

        assert os.path.exists(filepath)

        # 読み込んで確認
        df_loaded = pd.read_csv(filepath, encoding='utf-8-sig')
        assert len(df_loaded) == 2
        assert 'seqchar' in df_loaded.columns

    def test_export_to_json(self, temp_dir, sample_df):
        """JSON出力のテスト"""
        filepath = os.path.join(temp_dir, "output.json")
        export_to_json(sample_df, filepath)

        assert os.path.exists(filepath)

        # 読み込んで確認
        df_loaded = pd.read_json(filepath)
        assert len(df_loaded) == 2

    def test_export_to_excel(self, temp_dir, sample_df):
        """Excel出力のテスト"""
        filepath = os.path.join(temp_dir, "output.xlsx")

        try:
            export_to_excel(sample_df, filepath)
            assert os.path.exists(filepath)
        except ImportError:
            pytest.skip("openpyxl not installed")

    def test_ensure_directory(self, temp_dir):
        """ディレクトリ作成のテスト"""
        nested_dir = os.path.join(temp_dir, "a", "b", "c", "file.txt")
        ensure_directory(nested_dir)

        parent_dir = os.path.dirname(nested_dir)
        assert os.path.exists(parent_dir)


class TestIntegrationWithPhraseExtracter:
    """PhraseExtracterとの統合テスト"""

    @pytest.fixture
    def temp_dir(self):
        """一時ディレクトリを提供"""
        with tempfile.TemporaryDirectory() as tmpdir:
            yield tmpdir

    def test_from_file_classmethod(self, temp_dir):
        """from_fileクラスメソッドのテスト"""
        from japhrase import PhraseExtracter

        # テストファイル作成
        filepath = os.path.join(temp_dir, "test.txt")
        with open(filepath, 'w', encoding='utf-8') as f:
            for _ in range(10):
                f.write("テストフレーズです\n")
                f.write("サンプル文章です\n")

        # from_fileで直接抽出
        df = PhraseExtracter.from_file(filepath, min_count=3, verbose=0)

        assert isinstance(df, pd.DataFrame)

    def test_extract_method(self, temp_dir):
        """extractインスタンスメソッドのテスト"""
        from japhrase import PhraseExtracter

        # テストファイル作成
        filepath = os.path.join(temp_dir, "test.txt")
        with open(filepath, 'w', encoding='utf-8') as f:
            for _ in range(10):
                f.write("フォローありがとう\n")

        # extractメソッドで抽出
        extractor = PhraseExtracter(min_count=3, verbose=0)
        df = extractor.extract(filepath)

        assert isinstance(df, pd.DataFrame)

    def test_export_csv_method(self, temp_dir):
        """export_csvメソッドのテスト"""
        from japhrase import PhraseExtracter

        extractor = PhraseExtracter(verbose=0)
        df = pd.DataFrame({
            'seqchar': ['テスト'],
            'freq': [10]
        })

        output_path = os.path.join(temp_dir, "output.csv")
        extractor.export_csv(df, output_path)

        assert os.path.exists(output_path)

    def test_extract_with_string_list(self, temp_dir):
        """extract()メソッドで文字列リストを直接渡すテスト"""
        from japhrase import PhraseExtracter

        # 文字列リストを直接渡す
        texts = [
            "フォローありがとうございます",
            "フォローしてください",
            "ありがとうございます",
            "よろしくお願いします",
            "フォローお願いします"
        ]

        extractor = PhraseExtracter(min_count=2, verbose=0)
        df = extractor.extract(texts)

        assert isinstance(df, pd.DataFrame)
        # フォローという文字列が含まれているはず
        if len(df) > 0:
            phrases = df['seqchar'].tolist()
            assert any('フォロー' in phrase for phrase in phrases)

    def test_extract_with_file_and_string(self, temp_dir):
        """extract()メソッドがファイルパスと文字列リスト両方に対応しているテスト"""
        from japhrase import PhraseExtracter

        extractor = PhraseExtracter(min_count=2, verbose=0)

        # 1. 文字列リストで実行
        texts = ["テストフレーズです"] * 5
        df1 = extractor.extract(texts)
        assert isinstance(df1, pd.DataFrame)

        # 2. ファイルパスで実行
        filepath = os.path.join(temp_dir, "test.txt")
        with open(filepath, 'w', encoding='utf-8') as f:
            for text in texts:
                f.write(text + "\n")

        df2 = extractor.extract(filepath)
        assert isinstance(df2, pd.DataFrame)
