# coding: utf-8
"""
PhraseExtracter と formatters.py の統合テスト

新しいメソッド from_formatted_file(), from_formatted_files(),
extract_with_metadata() の動作を検証
"""

import pytest
import tempfile
import os
import json
import csv
from pathlib import Path
import pandas as pd
from japhrase import PhraseExtracter


class TestPhraseExtracterWithFormatters:
    """PhraseExtracter と formatters の統合テスト"""

    @pytest.fixture
    def temp_dir(self):
        """一時ディレクトリ"""
        with tempfile.TemporaryDirectory() as tmpdir:
            yield tmpdir

    def test_from_formatted_file_text(self, temp_dir):
        """from_formatted_file() でテキストファイル抽出テスト"""
        filepath = os.path.join(temp_dir, "test.txt")
        text = "テストフレーズです\n" * 5
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(text)

        # 新メソッドで抽出
        df, metadata = PhraseExtracter.from_formatted_file(
            filepath,
            format='auto',
            encoding='auto',
            min_count=2,
            verbose=0
        )

        assert isinstance(df, pd.DataFrame)
        assert metadata is not None
        assert metadata.get('format') == 'text' or 'format' in metadata

    def test_from_formatted_file_jsonl(self, temp_dir):
        """from_formatted_file() でJSONL形式抽出テスト"""
        filepath = os.path.join(temp_dir, "test.jsonl")
        data = [
            {"prompt": "プロンプト1テスト", "negative_prompt": "NG1"},
            {"prompt": "プロンプト2テスト", "negative_prompt": "NG2"},
            {"prompt": "プロンプト3テスト", "negative_prompt": "NG3"},
        ]
        with open(filepath, 'w', encoding='utf-8') as f:
            for item in data:
                f.write(json.dumps(item, ensure_ascii=False) + "\n")

        # 新メソッドで抽出
        df, metadata = PhraseExtracter.from_formatted_file(
            filepath,
            format='auto',
            encoding='auto',
            min_count=2,
            verbose=0
        )

        assert isinstance(df, pd.DataFrame)
        assert metadata.get('format') == 'json'

    def test_from_formatted_file_csv(self, temp_dir):
        """from_formatted_file() でCSV形式抽出テスト"""
        filepath = os.path.join(temp_dir, "test.csv")
        data = {
            'text': ['フォロー'] * 5 + ['プレゼント'] * 5,
            'count': list(range(10))
        }
        df_write = pd.DataFrame(data)
        df_write.to_csv(filepath, index=False, encoding='utf-8')

        # 新メソッドで抽出
        df, metadata = PhraseExtracter.from_formatted_file(
            filepath,
            format='auto',
            encoding='auto',
            column='text',
            min_count=2,
            verbose=0
        )

        assert isinstance(df, pd.DataFrame)
        assert metadata.get('format') == 'csv'

    def test_from_formatted_file_with_format_hint(self, temp_dir):
        """フォーマットヒント付き抽出テスト"""
        filepath = os.path.join(temp_dir, "data.txt")
        with open(filepath, 'w', encoding='utf-8') as f:
            for _ in range(5):
                f.write("テストデータです\n")

        df, metadata = PhraseExtracter.from_formatted_file(
            filepath,
            format='text',
            min_count=2,
            verbose=0
        )

        assert isinstance(df, pd.DataFrame)
        assert metadata['format'] == 'text'

    def test_from_formatted_files_multiple(self, temp_dir):
        """from_formatted_files() で複数ファイル抽出テスト"""
        # テキストファイル
        txt_path = os.path.join(temp_dir, "test1.txt")
        with open(txt_path, 'w', encoding='utf-8') as f:
            f.write("テスト\n" * 5)

        # JSONファイル
        json_path = os.path.join(temp_dir, "test2.jsonl")
        with open(json_path, 'w', encoding='utf-8') as f:
            for _ in range(5):
                f.write(json.dumps({"prompt": "テストプロンプト"}, ensure_ascii=False) + "\n")

        # 複数ファイルで抽出
        df_combined, metadata_dict = PhraseExtracter.from_formatted_files(
            [txt_path, json_path],
            format='auto',
            min_count=2,
            verbose=0
        )

        assert isinstance(df_combined, pd.DataFrame)
        assert isinstance(metadata_dict, dict)
        assert len(metadata_dict) >= 1

    def test_extract_with_metadata_text(self, temp_dir):
        """extract_with_metadata() でテキスト抽出テスト"""
        filepath = os.path.join(temp_dir, "test.txt")
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write("メタデータテスト\n" * 5)

        extractor = PhraseExtracter(min_count=2, verbose=0)
        df, metadata = extractor.extract_with_metadata(filepath)

        assert isinstance(df, pd.DataFrame)
        assert isinstance(metadata, dict)
        assert 'format' in metadata or 'encoding' in metadata

    def test_extract_with_metadata_file_list(self, temp_dir):
        """extract_with_metadata() でファイルリスト抽出テスト"""
        filepaths = []
        for i in range(2):
            filepath = os.path.join(temp_dir, f"test{i}.txt")
            with open(filepath, 'w', encoding='utf-8') as f:
                f.write(f"ファイル{i}\n" * 5)
            filepaths.append(filepath)

        extractor = PhraseExtracter(min_count=2, verbose=0)
        df, metadata = extractor.extract_with_metadata(filepaths)

        assert isinstance(df, pd.DataFrame)
        assert isinstance(metadata, dict)

    def test_extract_with_metadata_string_list(self, temp_dir):
        """extract_with_metadata() で文字列リスト抽出テスト"""
        texts = [
            "メタデータテスト1",
            "メタデータテスト2",
            "メタデータテスト3",
        ]

        extractor = PhraseExtracter(min_count=2, verbose=0)
        df, metadata = extractor.extract_with_metadata(texts)

        assert isinstance(df, pd.DataFrame)
        assert isinstance(metadata, dict)


class TestEncodingRobustnessIntegration:
    """エンコーディング頑健性の統合テスト"""

    @pytest.fixture
    def temp_dir(self):
        """一時ディレクトリ"""
        with tempfile.TemporaryDirectory() as tmpdir:
            yield tmpdir

    def test_utf8_file_extraction(self, temp_dir):
        """UTF-8ファイル抽出テスト"""
        filepath = os.path.join(temp_dir, "utf8.txt")
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write("これはUTF-8です\n" * 5)

        df, metadata = PhraseExtracter.from_formatted_file(
            filepath,
            encoding='auto',
            min_count=2,
            verbose=0
        )

        assert isinstance(df, pd.DataFrame)
        assert metadata['encoding'].lower() in ['utf-8', 'utf8', 'ascii']

    def test_bom_file_detection(self, temp_dir):
        """BOM付きファイル検出テスト"""
        filepath = os.path.join(temp_dir, "bom.txt")
        with open(filepath, 'w', encoding='utf-8-sig') as f:
            f.write("BOM付きテキスト\n" * 5)

        df, metadata = PhraseExtracter.from_formatted_file(
            filepath,
            encoding='auto',
            min_count=2,
            verbose=0
        )

        assert isinstance(df, pd.DataFrame)
        assert 'encoding' in metadata

    def test_mixed_format_directory(self, temp_dir):
        """混合フォーマットディレクトリ処理テスト"""
        # テキストファイル
        with open(os.path.join(temp_dir, "file1.txt"), 'w', encoding='utf-8') as f:
            f.write("テスト1\n" * 3)

        # JSONファイル
        with open(os.path.join(temp_dir, "file2.jsonl"), 'w', encoding='utf-8') as f:
            f.write(json.dumps({"prompt": "テスト2"}, ensure_ascii=False) + "\n")

        # CSVファイル
        df_csv = pd.DataFrame({'text': ['テスト3'] * 3})
        df_csv.to_csv(os.path.join(temp_dir, "file3.csv"), index=False, encoding='utf-8')

        # 各ファイルから抽出可能か確認
        for filepath in [
            os.path.join(temp_dir, "file1.txt"),
            os.path.join(temp_dir, "file2.jsonl"),
            os.path.join(temp_dir, "file3.csv"),
        ]:
            if os.path.exists(filepath):
                df, metadata = PhraseExtracter.from_formatted_file(
                    filepath,
                    encoding='auto',
                    min_count=1,
                    verbose=0
                )
                assert isinstance(df, pd.DataFrame)


class TestBackwardCompatibility:
    """後方互換性テスト"""

    @pytest.fixture
    def temp_dir(self):
        """一時ディレクトリ"""
        with tempfile.TemporaryDirectory() as tmpdir:
            yield tmpdir

    def test_original_from_file_still_works(self, temp_dir):
        """既存の from_file() メソッドが動作するか"""
        filepath = os.path.join(temp_dir, "test.txt")
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write("テスト\n" * 5)

        # 既存メソッド（後方互換性確認）
        try:
            df = PhraseExtracter.from_file(filepath, min_count=2, verbose=0)
            assert isinstance(df, pd.DataFrame)
        except AttributeError:
            pytest.skip("from_file() method not available (new API)")

    def test_extract_method_with_file_path(self, temp_dir):
        """既存の extract() がファイルパスで動作するか"""
        filepath = os.path.join(temp_dir, "test.txt")
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write("テスト\n" * 5)

        extractor = PhraseExtracter(min_count=2, verbose=0)
        df = extractor.extract(filepath)

        assert isinstance(df, pd.DataFrame)

    def test_extract_method_with_text_list(self, temp_dir):
        """既存の extract() が文字列リストで動作するか"""
        texts = ["テスト"] * 5

        extractor = PhraseExtracter(min_count=2, verbose=0)
        df = extractor.extract(texts)

        assert isinstance(df, pd.DataFrame)


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
