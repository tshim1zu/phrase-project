"""
ユースケース駆動インターフェースのテスト
"""

import pytest
import tempfile
from pathlib import Path

from japhrase import WritingWorkflow


class TestWritingWorkflow:
    """WritingWorkflowのテスト"""

    def test_list_usecases(self):
        """利用可能なユースケース一覧"""
        usecases = WritingWorkflow.list_usecases()
        assert len(usecases) == 5
        assert 'academic_writing' in usecases
        assert 'novel_revision' in usecases
        assert 'blog_writing' in usecases
        assert 'sns_content' in usecases
        assert 'editing' in usecases

    def test_create_academic_workflow(self):
        """学位論文ワークフローの作成"""
        workflow = WritingWorkflow.for_use_case('academic_writing')
        assert workflow.use_case == 'academic_writing'
        assert workflow.config['required_files'] == ['body_file']

    def test_create_novel_workflow(self):
        """小説推敲ワークフローの作成"""
        workflow = WritingWorkflow.for_use_case('novel_revision')
        assert workflow.use_case == 'novel_revision'
        assert 'v1' in workflow.config['required_files']

    def test_create_blog_workflow(self):
        """ブログワークフローの作成"""
        workflow = WritingWorkflow.for_use_case('blog_writing')
        assert workflow.use_case == 'blog_writing'

    def test_create_sns_workflow(self):
        """SNS投稿ワークフローの作成"""
        workflow = WritingWorkflow.for_use_case('sns_content')
        assert workflow.use_case == 'sns_content'

    def test_create_editing_workflow(self):
        """編集者向けワークフローの作成"""
        workflow = WritingWorkflow.for_use_case('editing')
        assert workflow.use_case == 'editing'

    def test_invalid_usecase(self):
        """無効なユースケース"""
        with pytest.raises(ValueError):
            WritingWorkflow('invalid_usecase')

    def test_run_academic_writing(self):
        """学位論文ワークフローの実行"""
        with tempfile.TemporaryDirectory() as tmpdir:
            # テストファイルを作成
            body_file = Path(tmpdir) / 'body.txt'
            body_file.write_text(
                '機械学習は重要な技術です\n'
                '機械学習を使うと分析ができます\n'
                '機械学習の応用は広いです\n'
                '深層学習も機械学習の一種です\n'
                '機械学習のモデル開発が必要です\n'
                '機械学習のアルゴリズムは複雑です\n',
                encoding='utf-8'
            )

            abstract_file = Path(tmpdir) / 'abstract.txt'
            abstract_file.write_text(
                '本論文は機械学習について説明する\n'
                '機械学習の応用を示す\n',
                encoding='utf-8'
            )

            workflow = WritingWorkflow.for_use_case('academic_writing')
            report = workflow.run(
                body_file=str(body_file),
                abstract_file=str(abstract_file),
                output_dir=tmpdir
            )

            assert isinstance(report, str)
            assert '学位論文' in report or '論文' in report
            assert len(report) > 100

    def test_run_blog_writing(self):
        """ブログワークフローの実行"""
        with tempfile.TemporaryDirectory() as tmpdir:
            # テストファイルを作成
            body_file = Path(tmpdir) / 'blog.txt'
            body_file.write_text(
                'Pythonプログラミングは楽しいです\n'
                'Pythonは人気の言語です\n'
                'Pythonで効率的なコードを書けます\n'
                'Pythonのライブラリが豊富です\n'
                'Pythonを学ぶことをお勧めします\n',
                encoding='utf-8'
            )

            workflow = WritingWorkflow.for_use_case('blog_writing')
            report = workflow.run(
                body_file=str(body_file),
                output_dir=tmpdir
            )

            assert isinstance(report, str)
            assert 'ブログ' in report
            assert len(report) > 50

    def test_run_sns_content(self):
        """SNS投稿ワークフローの実行"""
        with tempfile.TemporaryDirectory() as tmpdir:
            body_file = Path(tmpdir) / 'sns.txt'
            body_file.write_text(
                '今日は良い天気です\n'
                '今日のお天気は最高\n'
                '今日も頑張ります\n'
                '今日のお弁当は美味しい\n'
                '今日も楽しい一日でした\n',
                encoding='utf-8'
            )

            workflow = WritingWorkflow.for_use_case('sns_content')
            report = workflow.run(
                body_file=str(body_file),
                output_dir=tmpdir
            )

            assert isinstance(report, str)
            assert 'SNS' in report
            assert len(report) > 50

    def test_run_editing(self):
        """編集者向けワークフローの実行"""
        with tempfile.TemporaryDirectory() as tmpdir:
            body_file = Path(tmpdir) / 'body.txt'
            body_file.write_text(
                'この記事について説明します\n'
                'この記事は重要です\n'
                'この記事の内容を理解するには\n'
                'この記事が参考になるでしょう\n'
                'この記事を読むことをお勧めします\n',
                encoding='utf-8'
            )

            workflow = WritingWorkflow.for_use_case('editing')
            report = workflow.run(
                body_file=str(body_file),
                output_dir=tmpdir
            )

            assert isinstance(report, str)
            assert '編集' in report or '口癖' in report
            assert len(report) > 50

    def test_run_novel_revision(self):
        """小説推敲ワークフローの実行"""
        with tempfile.TemporaryDirectory() as tmpdir:
            # 2つのバージョンを作成
            v1_file = Path(tmpdir) / 'v1.txt'
            v1_file.write_text(
                'キャラクターが歩いています\n'
                'キャラクターは思います\n'
                'キャラクターが言いました\n'
                'キャラクターは走ります\n'
                'キャラクターが見ています\n',
                encoding='utf-8'
            )

            v2_file = Path(tmpdir) / 'v2.txt'
            v2_file.write_text(
                'キャラクターが歩いています\n'
                'キャラクターは思い考えています\n'
                'キャラクターが声を上げました\n'
                'キャラクターは速く走ります\n'
                'キャラクターが景色を見ています\n',
                encoding='utf-8'
            )

            workflow = WritingWorkflow.for_use_case('novel_revision')
            report = workflow.run(
                v1=str(v1_file),
                v2=str(v2_file),
                output_dir=tmpdir
            )

            assert isinstance(report, str)
            assert '小説' in report or '推敲' in report
            assert len(report) > 50
