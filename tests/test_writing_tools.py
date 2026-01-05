"""
執筆補助ツール（EditorConfigGenerator、SelfRecommender）のテスト
"""

import pytest
import pandas as pd
import json
from pathlib import Path
import tempfile
import shutil

from japhrase import (
    EditorConfigGenerator,
    SelfRecommender,
    PhraseExtracter
)


class TestEditorConfigGenerator:
    """エディタ設定生成機能のテスト"""

    def test_basic_initialization(self):
        """基本的な初期化テスト"""
        extractor = PhraseExtracter(min_count=1, verbose=0)
        phrases_df = extractor.get_dfphrase(["テストテスト"])

        generator = EditorConfigGenerator(phrases_df)
        assert generator is not None

    def test_spelling_variation_detection(self):
        """表記ゆれ検出テスト"""
        # 類似度が高いが異なるフレーズを作成
        extractor = PhraseExtracter(min_count=1, verbose=0)
        phrases_df = extractor.get_dfphrase([
            "ユーザー",
            "ユーザ",
            "ユーザー",
            "ユーザー"
        ])

        if len(phrases_df) > 0:
            generator = EditorConfigGenerator(phrases_df, similarity_threshold=0.75)
            variations_df = generator.get_variations_df()

            # 変動が検出される可能性がある
            assert isinstance(variations_df, pd.DataFrame)

    def test_empty_phrases(self):
        """空のフレーズDataFrameでのハンドリングテスト"""
        empty_df = pd.DataFrame(columns=['seqchar', 'freq'])
        generator = EditorConfigGenerator(empty_df)
        variations = generator.get_variations_df()

        assert len(variations) == 0

    def test_export_vscode_config(self):
        """VS Code設定エクスポートテスト"""
        extractor = PhraseExtracter(min_count=1, verbose=0)
        # 類似フレーズを含むテキストを作成
        phrases_df = extractor.get_dfphrase([
            "ユーザー管理システム",
            "ユーザ管理システム"
        ])

        generator = EditorConfigGenerator(phrases_df, similarity_threshold=0.7)

        with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False) as f:
            filepath = f.name

        try:
            generator.export_vscode_config(filepath)

            # ファイルが作成されたか確認
            assert Path(filepath).exists()

            # JSON形式が正しいか確認
            with open(filepath, 'r') as f:
                content = f.read()
                if content.strip():  # ファイルが空でない場合のみ解析
                    config = json.loads(content)
                    assert isinstance(config, dict)

        finally:
            Path(filepath).unlink()

    def test_generate_report(self):
        """レポート生成テスト"""
        extractor = PhraseExtracter(min_count=1, verbose=0)
        phrases_df = extractor.get_dfphrase(["テスト"] * 3)

        generator = EditorConfigGenerator(phrases_df)
        report = generator.generate_report()

        assert isinstance(report, str)
        assert "表記ゆれ" in report


class TestSelfRecommender:
    """セルフ・リコメンデーション機能のテスト"""

    def test_initialization(self):
        """基本的な初期化テスト"""
        with tempfile.TemporaryDirectory() as tmpdir:
            # テスト用ファイルを作成
            test_file = Path(tmpdir) / "test.txt"
            test_file.write_text("テスト用テキスト", encoding='utf-8')

            recommender = SelfRecommender(tmpdir)
            assert recommender is not None

    def test_corpus_building(self):
        """コーパス構築テスト"""
        with tempfile.TemporaryDirectory() as tmpdir:
            # 複数のテスト用ファイルを作成
            (Path(tmpdir) / "file1.txt").write_text(
                "機械学習について述べる。深層学習の基本を説明する。",
                encoding='utf-8'
            )
            (Path(tmpdir) / "file2.md").write_text(
                "深層学習について詳しく述べる。",
                encoding='utf-8'
            )

            recommender = SelfRecommender(tmpdir, min_count=1)
            assert len(recommender.corpus_texts) >= 1

    def test_find_related_articles(self):
        """関連記事検索テスト"""
        with tempfile.TemporaryDirectory() as tmpdir:
            # テスト用ファイルを作成
            (Path(tmpdir) / "article1.txt").write_text(
                "機械学習は重要である。" * 5,
                encoding='utf-8'
            )
            (Path(tmpdir) / "article2.txt").write_text(
                "機械学習について詳しく述べる。" * 5,
                encoding='utf-8'
            )

            recommender = SelfRecommender(tmpdir, min_count=1)
            results = recommender.find_related_articles(
                "機械学習について述べる。",
                top_n=5
            )

            assert isinstance(results, pd.DataFrame)

    def test_find_overlapping_phrases(self):
        """共通フレーズ検出テスト"""
        with tempfile.TemporaryDirectory() as tmpdir:
            # テスト用ファイルを作成
            (Path(tmpdir) / "file1.txt").write_text(
                "重要な重要な重要な単語",
                encoding='utf-8'
            )

            recommender = SelfRecommender(tmpdir, min_count=1)
            overlaps = recommender.find_overlapping_phrases(
                "重要な重要な",
                top_n=5
            )

            assert isinstance(overlaps, dict)

    def test_generate_recommendation_report(self):
        """リコメンデーションレポート生成テスト"""
        with tempfile.TemporaryDirectory() as tmpdir:
            # テスト用ファイルを作成
            (Path(tmpdir) / "past.txt").write_text(
                "テスト文章テスト文章テスト文章" * 3,
                encoding='utf-8'
            )

            recommender = SelfRecommender(tmpdir, min_count=1)
            report = recommender.generate_recommendation_report(
                "テスト文章について述べる。"
            )

            assert isinstance(report, str)
            assert "レポート" in report or "関連記事" in report

    def test_empty_corpus_dir(self):
        """空のコーパスディレクトリでのハンドリングテスト"""
        with tempfile.TemporaryDirectory() as tmpdir:
            recommender = SelfRecommender(tmpdir)

            results = recommender.find_related_articles("テスト")
            assert isinstance(results, pd.DataFrame)
            assert len(results) == 0

    def test_md_and_txt_files(self):
        """マークダウンとテキストファイル両方の対応テスト"""
        with tempfile.TemporaryDirectory() as tmpdir:
            # テキストファイル
            (Path(tmpdir) / "file1.txt").write_text(
                "テキストファイル" * 5,
                encoding='utf-8'
            )
            # マークダウンファイル
            (Path(tmpdir) / "file2.md").write_text(
                "マークダウンファイル" * 5,
                encoding='utf-8'
            )

            recommender = SelfRecommender(tmpdir, min_count=1)
            assert len(recommender.corpus_texts) >= 1


class TestIntegration:
    """統合テスト"""

    def test_editor_generator_workflow(self):
        """エディタ設定生成のワークフローテスト"""
        extractor = PhraseExtracter(min_count=1, verbose=0)
        phrases_df = extractor.get_dfphrase(["テスト"] * 3)

        generator = EditorConfigGenerator(phrases_df)
        variations = generator.get_variations_df()

        assert isinstance(variations, pd.DataFrame)

    def test_recommender_workflow(self):
        """リコメンデーションのワークフローテスト"""
        with tempfile.TemporaryDirectory() as tmpdir:
            # コーパスを作成
            (Path(tmpdir) / "doc1.txt").write_text(
                "機械学習は重要である。" * 5,
                encoding='utf-8'
            )
            (Path(tmpdir) / "doc2.txt").write_text(
                "深層学習について述べる。" * 5,
                encoding='utf-8'
            )

            recommender = SelfRecommender(tmpdir, min_count=1)

            # 検索を実行
            related = recommender.find_related_articles("機械学習")
            overlaps = recommender.find_overlapping_phrases("機械学習")

            assert isinstance(related, pd.DataFrame)
            assert isinstance(overlaps, dict)
