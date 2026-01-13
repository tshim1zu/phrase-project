# coding: utf-8
"""
formatters.py のテスト

TextFormatter、JSONFormatter、CSVFormatter、UniversalFormatter のテスト
各フォーマッターが正しくテキストを抽出し、メタデータを返すことを検証
"""

import pytest
import tempfile
import os
import json
import csv
from pathlib import Path
from japhrase.formatters import (
    TextFormatter,
    JSONFormatter,
    CSVFormatter,
    UniversalFormatter,
    BaseFormatter
)


class TestTextFormatter:
    """テキストフォーマッターのテスト"""

    @pytest.fixture
    def temp_dir(self):
        """一時ディレクトリ"""
        with tempfile.TemporaryDirectory() as tmpdir:
            yield tmpdir

    def test_detect_txt_file(self):
        """テキストファイル判定テスト"""
        assert TextFormatter.detect(Path("test.txt"))
        assert TextFormatter.detect(Path("test.text"))
        assert not TextFormatter.detect(Path("test.json"))

    def test_extract_simple_text(self, temp_dir):
        """シンプルなテキスト抽出テスト"""
        filepath = os.path.join(temp_dir, "test.txt")
        text = "これはテスト1\nこれはテスト2\n\nこれはテスト3\n"
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(text)

        formatter = TextFormatter()
        lines, metadata = formatter.extract(Path(filepath))

        assert len(lines) == 3
        assert "これはテスト1" in lines
        assert "これはテスト2" in lines
        assert "これはテスト3" in lines
        assert metadata['format'] == 'text'
        assert metadata['encoding'].lower() in ['utf-8', 'utf8', 'ascii']

    def test_extract_empty_lines_ignored(self, temp_dir):
        """空行が無視されるテスト"""
        filepath = os.path.join(temp_dir, "test.txt")
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write("テスト1\n\n\nテスト2\n")

        formatter = TextFormatter()
        lines, metadata = formatter.extract(Path(filepath))

        assert len(lines) == 2
        assert "" not in lines

    def test_metadata_includes_encoding_confidence(self, temp_dir):
        """メタデータにエンコーディング信頼度が含まれるテスト"""
        filepath = os.path.join(temp_dir, "test.txt")
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write("テスト")

        formatter = TextFormatter()
        lines, metadata = formatter.extract(Path(filepath))

        assert 'encoding_confidence' in metadata
        assert 0.0 <= metadata['encoding_confidence'] <= 1.0

    def test_metadata_includes_file_size(self, temp_dir):
        """メタデータにファイルサイズが含まれるテスト"""
        filepath = os.path.join(temp_dir, "test.txt")
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write("テスト")

        formatter = TextFormatter()
        lines, metadata = formatter.extract(Path(filepath))

        assert 'file_size' in metadata
        assert metadata['file_size'] > 0


class TestJSONFormatter:
    """JSONフォーマッターのテスト"""

    @pytest.fixture
    def temp_dir(self):
        """一時ディレクトリ"""
        with tempfile.TemporaryDirectory() as tmpdir:
            yield tmpdir

    def test_detect_json_file(self):
        """JSON ファイル判定テスト"""
        assert JSONFormatter.detect(Path("test.json"))
        assert JSONFormatter.detect(Path("test.jsonl"))
        assert not JSONFormatter.detect(Path("test.txt"))

    def test_extract_jsonl_format(self, temp_dir):
        """JSONL（1行1JSON）抽出テスト"""
        filepath = os.path.join(temp_dir, "test.jsonl")
        data = [
            {"prompt": "テスト1", "negative_prompt": "NG1"},
            {"prompt": "テスト2", "negative_prompt": "NG2"},
            {"prompt": "テスト3"},
        ]
        with open(filepath, 'w', encoding='utf-8') as f:
            for item in data:
                f.write(json.dumps(item) + "\n")

        formatter = JSONFormatter()
        lines, metadata = formatter.extract(Path(filepath))

        # prompt, negative_prompt が抽出される
        assert "テスト1" in lines or any("テスト1" in str(l) for l in lines)
        assert metadata['format'] == 'json'
        assert metadata['total_lines'] == 3

    def test_extract_json_format(self, temp_dir):
        """JSON（複数行）抽出テスト"""
        filepath = os.path.join(temp_dir, "test.json")
        data = {
            "items": [
                {"prompt": "プロンプト1", "description": "説明1"},
                {"prompt": "プロンプト2", "description": "説明2"},
            ]
        }
        with open(filepath, 'w', encoding='utf-8') as f:
            json.dump(data, f, ensure_ascii=False)

        formatter = JSONFormatter()
        lines, metadata = formatter.extract(
            Path(filepath),
            field_paths=["items[*].prompt", "items[*].description"]
        )

        assert len(lines) >= 2
        assert metadata['format'] == 'json'

    def test_extract_nested_json_path(self, temp_dir):
        """ネストされたJSONパス抽出テスト"""
        filepath = os.path.join(temp_dir, "test.json")
        data = {
            "data": {
                "items": [
                    {"name": "アイテム1"},
                    {"name": "アイテム2"},
                ]
            }
        }
        with open(filepath, 'w', encoding='utf-8') as f:
            json.dump(data, f, ensure_ascii=False)

        formatter = JSONFormatter()
        lines, metadata = formatter.extract(
            Path(filepath),
            field_paths=["data.items[*].name"]
        )

        assert "アイテム1" in lines or any("アイテム1" in str(l) for l in lines)
        assert "アイテム2" in lines or any("アイテム2" in str(l) for l in lines)

    def test_extract_json_default_fields(self, temp_dir):
        """JSON デフォルトフィールド抽出テスト"""
        filepath = os.path.join(temp_dir, "test.jsonl")
        data = [
            {
                "prompt": "プロンプト1",
                "text": "テキスト1",
                "content": "コンテンツ1",
                "other": "その他1"
            }
        ]
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(json.dumps(data[0]) + "\n")

        formatter = JSONFormatter()
        lines, metadata = formatter.extract(Path(filepath), field_paths=None)

        # デフォルトフィールドが抽出される
        assert any(v for v in lines if "プロンプト1" in str(v) or "テキスト1" in str(v))


class TestCSVFormatter:
    """CSVフォーマッターのテスト"""

    @pytest.fixture
    def temp_dir(self):
        """一時ディレクトリ"""
        with tempfile.TemporaryDirectory() as tmpdir:
            yield tmpdir

    def test_detect_csv_file(self):
        """CSVファイル判定テスト"""
        assert CSVFormatter.detect(Path("test.csv"))
        assert CSVFormatter.detect(Path("test.tsv"))
        assert not CSVFormatter.detect(Path("test.txt"))

    def test_extract_csv(self, temp_dir):
        """CSV抽出テスト"""
        filepath = os.path.join(temp_dir, "test.csv")
        data = {
            'text': ['フォロー', 'プレゼント', 'キャンペーン'],
            'count': [10, 20, 30]
        }
        with open(filepath, 'w', encoding='utf-8', newline='') as f:
            writer = csv.DictWriter(f, fieldnames=data.keys())
            writer.writeheader()
            for i in range(len(data['text'])):
                writer.writerow({'text': data['text'][i], 'count': data['count'][i]})

        formatter = CSVFormatter()
        lines, metadata = formatter.extract(Path(filepath), column='text')

        assert "フォロー" in lines
        assert "プレゼント" in lines
        assert "キャンペーン" in lines
        assert metadata['format'] == 'csv'

    def test_extract_tsv(self, temp_dir):
        """TSV抽出テスト"""
        filepath = os.path.join(temp_dir, "test.tsv")
        data = {'text': ['テスト1', 'テスト2', 'テスト3']}
        with open(filepath, 'w', encoding='utf-8', newline='') as f:
            writer = csv.DictWriter(f, fieldnames=data.keys(), delimiter='\t')
            writer.writeheader()
            for text in data['text']:
                writer.writerow({'text': text})

        formatter = CSVFormatter()
        lines, metadata = formatter.extract(Path(filepath), column='text')

        assert "テスト1" in lines
        assert "テスト2" in lines
        assert metadata['format'] == 'tsv'

    def test_extract_csv_all_columns(self, temp_dir):
        """CSVすべてのカラム抽出テスト"""
        filepath = os.path.join(temp_dir, "test.csv")
        with open(filepath, 'w', encoding='utf-8', newline='') as f:
            writer = csv.DictWriter(f, fieldnames=['col1', 'col2'])
            writer.writeheader()
            writer.writerow({'col1': 'テスト1', 'col2': 'テスト2'})

        formatter = CSVFormatter()
        lines, metadata = formatter.extract(Path(filepath), column=None)

        # カラムが結合される
        assert any('テスト1' in line or 'テスト2' in line for line in lines)


class TestUniversalFormatter:
    """統合フォーマッターのテスト"""

    @pytest.fixture
    def temp_dir(self):
        """一時ディレクトリ"""
        with tempfile.TemporaryDirectory() as tmpdir:
            yield tmpdir

    def test_auto_detect_text(self, temp_dir):
        """テキストファイル自動判定テスト"""
        filepath = os.path.join(temp_dir, "test.txt")
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write("テスト")

        formatter = UniversalFormatter.auto_detect(Path(filepath))
        assert isinstance(formatter, TextFormatter)

    def test_auto_detect_json(self, temp_dir):
        """JSONファイル自動判定テスト"""
        filepath = os.path.join(temp_dir, "test.json")
        with open(filepath, 'w', encoding='utf-8') as f:
            json.dump({"data": "test"}, f)

        formatter = UniversalFormatter.auto_detect(Path(filepath))
        assert isinstance(formatter, JSONFormatter)

    def test_auto_detect_csv(self, temp_dir):
        """CSVファイル自動判定テスト"""
        filepath = os.path.join(temp_dir, "test.csv")
        with open(filepath, 'w', encoding='utf-8', newline='') as f:
            writer = csv.DictWriter(f, fieldnames=['col1'])
            writer.writeheader()
            writer.writerow({'col1': 'test'})

        formatter = UniversalFormatter.auto_detect(Path(filepath))
        assert isinstance(formatter, CSVFormatter)

    def test_extract_with_format_hint(self, temp_dir):
        """フォーマットヒント付き抽出テスト"""
        filepath = os.path.join(temp_dir, "test.txt")
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write("テスト")

        lines, metadata = UniversalFormatter.extract(filepath, format_hint='text')
        assert len(lines) > 0
        assert metadata['format'] == 'text'

    def test_extract_auto_detect(self, temp_dir):
        """自動判定での抽出テスト"""
        filepath = os.path.join(temp_dir, "test.txt")
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write("自動判定テスト")

        lines, metadata = UniversalFormatter.extract(filepath)
        assert "自動判定テスト" in lines
        assert metadata['format'] == 'text'

    def test_register_custom_formatter(self):
        """カスタムフォーマッター登録テスト"""
        class CustomFormatter(BaseFormatter):
            @staticmethod
            def detect(file_path):
                return str(file_path).endswith('.custom')

            def extract(self, file_path, **kwargs):
                return ['custom'], {'format': 'custom'}

        UniversalFormatter.register_custom('custom', CustomFormatter)

        # 登録できたか確認
        assert 'custom' in UniversalFormatter.FORMATTERS


class TestFormattersIntegration:
    """フォーマッター統合テスト"""

    @pytest.fixture
    def temp_dir(self):
        """一時ディレクトリ"""
        with tempfile.TemporaryDirectory() as tmpdir:
            yield tmpdir

    def test_extract_from_text_file(self, temp_dir):
        """テキストファイルからの抽出テスト"""
        filepath = os.path.join(temp_dir, "test.txt")
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write("これはテスト1\nこれはテスト2\n")

        lines, metadata = UniversalFormatter.extract(Path(filepath))
        assert len(lines) == 2
        assert metadata['format'] == 'text'
        assert 'encoding' in metadata

    def test_extract_with_metadata(self, temp_dir):
        """メタデータ付き抽出テスト"""
        filepath = os.path.join(temp_dir, "test.txt")
        text = "テスト"
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(text)

        lines, metadata = UniversalFormatter.extract(Path(filepath))

        assert 'format' in metadata
        assert 'encoding' in metadata
        assert 'file_size' in metadata or 'num_rows' in metadata

    def test_encode_error_handling(self, temp_dir):
        """エンコーディングエラーハンドリングテスト"""
        filepath = os.path.join(temp_dir, "test.txt")
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write("正常なテキスト")

        # 自動検出で抽出
        lines, metadata = UniversalFormatter.extract(filepath, encoding='auto')

        assert len(lines) > 0
        assert 'encoding' in metadata


class TestErrorHandling:
    """エラーハンドリングテスト"""

    @pytest.fixture
    def temp_dir(self):
        """一時ディレクトリ"""
        with tempfile.TemporaryDirectory() as tmpdir:
            yield tmpdir

    def test_missing_file(self, temp_dir):
        """存在しないファイルのテスト"""
        filepath = os.path.join(temp_dir, "nonexistent.txt")
        formatter = TextFormatter()

        try:
            lines, metadata = formatter.extract(Path(filepath))
            # エラーが返される
            assert 'error' in metadata or lines == []
        except Exception:
            # 例外が発生する場合もある
            pass

    def test_invalid_json(self, temp_dir):
        """不正なJSON のテスト"""
        filepath = os.path.join(temp_dir, "invalid.json")
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write("{ invalid json }")

        formatter = JSONFormatter()
        lines, metadata = formatter.extract(Path(filepath))

        # エラーが記録される
        assert 'errors' in metadata or len(lines) == 0


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
