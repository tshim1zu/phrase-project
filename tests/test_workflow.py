"""
ワークフローエンジンのテスト
"""

import pytest
import tempfile
from pathlib import Path
import yaml

from japhrase import (
    WorkflowDefinition,
    WorkflowEngine,
    TaskRegistry,
    TaskStatus
)


class TestWorkflowDefinition:
    """ワークフロー定義のテスト"""

    def test_create_workflow(self):
        """ワークフロー作成テスト"""
        workflow = WorkflowDefinition(
            name="test_workflow",
            description="テスト用ワークフロー"
        )
        assert workflow.name == "test_workflow"
        assert len(workflow.tasks) == 0

    def test_add_task(self):
        """タスク追加テスト"""
        workflow = WorkflowDefinition(name="test")
        workflow.add_task({
            'id': 'task1',
            'type': 'extract',
            'input': 'input.txt'
        })

        assert 'task1' in workflow.tasks
        assert workflow.tasks['task1'].type == 'extract'

    def test_validate_workflow(self):
        """ワークフロー検証テスト"""
        workflow = WorkflowDefinition(name="test")
        workflow.add_task({
            'id': 'task1',
            'type': 'extract',
            'input': 'input.txt'
        })

        valid, errors = workflow.validate()
        assert valid

    def test_validate_missing_dependency(self):
        """依存タスク不在の検証テスト"""
        workflow = WorkflowDefinition(name="test")
        workflow.add_task({
            'id': 'task1',
            'type': 'extract',
            'depends_on': ['nonexistent']
        })

        valid, errors = workflow.validate()
        assert not valid
        assert len(errors) > 0

    def test_from_dict(self):
        """辞書からのワークフロー生成テスト"""
        data = {
            'name': 'test_workflow',
            'description': 'テスト',
            'tasks': [
                {
                    'id': 'task1',
                    'type': 'extract',
                    'input': 'input.txt'
                }
            ]
        }

        workflow = WorkflowDefinition.from_dict(data)
        assert workflow.name == 'test_workflow'
        assert 'task1' in workflow.tasks

    def test_to_dict(self):
        """ワークフローを辞書に変換テスト"""
        workflow = WorkflowDefinition(
            name="test",
            description="test workflow"
        )
        workflow.add_task({
            'id': 'task1',
            'type': 'extract',
            'input': 'input.txt'
        })

        data = workflow.to_dict()
        assert data['name'] == 'test'
        assert len(data['tasks']) == 1

    def test_get_execution_order(self):
        """実行順序の計算テスト"""
        workflow = WorkflowDefinition(name="test")
        workflow.add_task({'id': 'task1', 'type': 'extract'})
        workflow.add_task({'id': 'task2', 'type': 'kwic', 'depends_on': ['task1']})
        workflow.add_task({'id': 'task3', 'type': 'extract', 'depends_on': ['task1']})

        order = workflow.get_execution_order()
        assert order[0] == 'task1'
        assert set(order[1:]) == {'task2', 'task3'}

    def test_circular_dependency_detection(self):
        """循環依存検出テスト"""
        workflow = WorkflowDefinition(name="test")
        workflow.add_task({'id': 'task1', 'type': 'extract', 'depends_on': ['task2']})
        workflow.add_task({'id': 'task2', 'type': 'extract', 'depends_on': ['task1']})

        valid, errors = workflow.validate()
        assert not valid


class TestTaskRegistry:
    """タスクレジストリのテスト"""

    def test_register_task(self):
        """タスク登録テスト"""
        registry = TaskRegistry()

        def dummy_task(**kwargs):
            return "result"

        registry.register('dummy', dummy_task)
        assert registry.get('dummy') is not None

    def test_builtin_tasks(self):
        """ビルトインタスク登録テスト"""
        registry = TaskRegistry()
        registry.register_builtin_tasks()

        assert registry.get('extract') is not None
        assert registry.get('kwic') is not None
        assert registry.get('check_divergence') is not None
        assert registry.get('detect_habits') is not None


class TestWorkflowEngine:
    """ワークフローエンジンのテスト"""

    def test_execute_simple_workflow(self):
        """シンプルなワークフロー実行テスト"""
        registry = TaskRegistry()

        call_count = {'count': 0}

        def test_task(**kwargs):
            call_count['count'] += 1
            return "test_result"

        registry.register('test', test_task)
        engine = WorkflowEngine(registry)

        workflow = WorkflowDefinition(name="test")
        workflow.add_task({'id': 'task1', 'type': 'test'})

        results = engine.execute(workflow)
        assert 'task1' in results
        assert results['task1'].status == TaskStatus.COMPLETED

    def test_execute_dependent_workflow(self):
        """依存関係のあるワークフロー実行テスト"""
        registry = TaskRegistry()
        execution_order = []

        def task1_func(**kwargs):
            execution_order.append('task1')
            return "result1"

        def task2_func(**kwargs):
            execution_order.append('task2')
            return "result2"

        registry.register('task1_type', task1_func)
        registry.register('task2_type', task2_func)
        engine = WorkflowEngine(registry)

        workflow = WorkflowDefinition(name="test")
        workflow.add_task({'id': 'task1', 'type': 'task1_type'})
        workflow.add_task({'id': 'task2', 'type': 'task2_type', 'depends_on': ['task1']})

        results = engine.execute(workflow)

        # 実行順序をチェック
        assert execution_order == ['task1', 'task2']

    def test_execute_with_error_handling(self):
        """エラーハンドリングテスト"""
        registry = TaskRegistry()

        def failing_task(**kwargs):
            raise ValueError("Test error")

        registry.register('failing', failing_task)
        engine = WorkflowEngine(registry)

        workflow = WorkflowDefinition(name="test")
        workflow.add_task({'id': 'task1', 'type': 'failing'})

        results = engine.execute(workflow)
        assert results['task1'].status == TaskStatus.FAILED
        assert results['task1'].error is not None

    def test_get_report(self):
        """レポート生成テスト"""
        registry = TaskRegistry()

        def dummy_task(**kwargs):
            return "result"

        registry.register('dummy', dummy_task)
        engine = WorkflowEngine(registry)

        workflow = WorkflowDefinition(name="test")
        workflow.add_task({'id': 'task1', 'type': 'dummy'})

        engine.execute(workflow)
        report = engine.get_report()

        assert isinstance(report, str)
        assert "ワークフロー実行レポート" in report


class TestWorkflowIntegration:
    """統合テスト"""

    def test_workflow_with_yaml_file(self):
        """YAMLファイルベースのワークフロー実行テスト"""
        with tempfile.TemporaryDirectory() as tmpdir:
            # YAMLファイルを作成
            workflow_yaml = {
                'name': 'test_workflow',
                'description': 'テスト用',
                'tasks': [
                    {
                        'id': 'task1',
                        'type': 'test_task'
                    },
                    {
                        'id': 'task2',
                        'type': 'test_task',
                        'depends_on': ['task1']
                    }
                ]
            }

            yaml_file = Path(tmpdir) / 'workflow.yaml'
            with open(yaml_file, 'w') as f:
                yaml.dump(workflow_yaml, f)

            # ワークフローを読込
            workflow = WorkflowDefinition.from_yaml(str(yaml_file))
            assert workflow.name == 'test_workflow'
            assert len(workflow.tasks) == 2

    def test_complete_workflow_example(self):
        """完全なワークフロー例テスト"""
        registry = TaskRegistry()

        # タスク1: データ準備
        def prepare_data(**kwargs):
            return {'data': [1, 2, 3]}

        # タスク2: 処理
        def process_data(**kwargs):
            results = kwargs.get('results', {})
            data = results.get('task1', {}).result
            return sum(data['data']) if data else 0

        registry.register('prepare', prepare_data)
        registry.register('process', process_data)
        engine = WorkflowEngine(registry)

        workflow = WorkflowDefinition(
            name="data_pipeline",
            description="データ処理パイプライン"
        )
        workflow.add_task({'id': 'task1', 'type': 'prepare'})
        workflow.add_task({'id': 'task2', 'type': 'process', 'depends_on': ['task1']})

        results = engine.execute(workflow)

        assert results['task1'].status == TaskStatus.COMPLETED
        assert results['task2'].status == TaskStatus.COMPLETED
