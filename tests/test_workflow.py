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

    def test_missing_networkx_raises_clear_error(self, monkeypatch):
        """networkxが無い環境では、AttributeError('NoneType' has no 'DiGraph')
        ではなく分かりやすいImportErrorになること"""
        import japhrase.workflow as workflow_module

        monkeypatch.setattr(workflow_module, '_HAS_NETWORKX', False)

        workflow = WorkflowDefinition(name="test")
        workflow.add_task({'id': 'task1', 'type': 'extract'})

        with pytest.raises(ImportError):
            workflow.validate()

    def test_duplicate_task_id_detection(self):
        """同じIDのタスクを2つ追加した場合、後者が前者を静かに上書きせず
        validate()が重複として検出すること"""
        workflow = WorkflowDefinition(name="test")
        workflow.add_task({'id': 'preprocess', 'type': 'extract', 'input': 'a.txt'})
        workflow.add_task({'id': 'preprocess', 'type': 'extract', 'input': 'b.txt'})

        valid, errors = workflow.validate()
        assert not valid
        assert any('preprocess' in e for e in errors)


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

    def test_check_divergence_task_wired_to_multi_input_convention(self, tmp_path):
        """check_divergenceは `inputs:` (複数入力) タスクとしてYAMLに書かれるため、
        _execute_task()が実際に渡すキーワード引数(input_0, input_1, ...)で
        呼び出せること（abstract_file/body_fileという名前では渡されない）"""
        registry = TaskRegistry()
        registry.register_builtin_tasks()
        func = registry.get('check_divergence')

        abstract_file = tmp_path / "abstract.txt"
        body_file = tmp_path / "body.txt"
        abstract_file.write_text("猫が好きだ", encoding='utf-8')
        body_file.write_text("猫が好きだ。犬も好きだ。", encoding='utf-8')

        result = func(input_0=str(abstract_file), input_1=str(body_file), params={})
        assert 'divergence_score' in result

    def test_check_divergence_via_engine_with_inputs(self, tmp_path):
        """check_divergenceタスクをWorkflowEngine経由（inputs:形式）で実行し、
        引数名の不一致でTypeError/FAILEDにならないこと"""
        abstract_file = tmp_path / "abstract.txt"
        body_file = tmp_path / "body.txt"
        abstract_file.write_text("猫が好きだ", encoding='utf-8')
        body_file.write_text("猫が好きだ。犬も好きだ。", encoding='utf-8')

        engine = WorkflowEngine()
        workflow = WorkflowDefinition(name="test")
        workflow.add_task({
            'id': 'check',
            'type': 'check_divergence',
            'inputs': [str(abstract_file), str(body_file)],
        })

        results = engine.execute(workflow)
        assert results['check'].status == TaskStatus.COMPLETED, results['check'].error


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

    def test_failed_dependency_skips_downstream_task_sequential(self):
        """依存タスクが失敗した場合、下流タスクは実行されずSKIPPEDになること
        （逐次実行）"""
        registry = TaskRegistry()
        downstream_ran = {'called': False}

        def failing_task(**kwargs):
            raise ValueError("boom")

        def downstream_task(**kwargs):
            downstream_ran['called'] = True
            return "should not run"

        registry.register('failing', failing_task)
        registry.register('downstream', downstream_task)
        engine = WorkflowEngine(registry)

        workflow = WorkflowDefinition(name="test")
        workflow.add_task({'id': 'task1', 'type': 'failing'})
        workflow.add_task({'id': 'task2', 'type': 'downstream', 'depends_on': ['task1']})

        results = engine.execute(workflow)

        assert results['task1'].status == TaskStatus.FAILED
        assert results['task2'].status == TaskStatus.SKIPPED
        assert not downstream_ran['called']

    def test_failed_dependency_skips_downstream_task_parallel(self):
        """依存タスクが失敗した場合、並列実行でも下流タスクがSKIPPEDになること"""
        registry = TaskRegistry()
        downstream_ran = {'called': False}

        def failing_task(**kwargs):
            raise ValueError("boom")

        def downstream_task(**kwargs):
            downstream_ran['called'] = True
            return "should not run"

        registry.register('failing', failing_task)
        registry.register('downstream', downstream_task)
        engine = WorkflowEngine(registry)

        workflow = WorkflowDefinition(name="test")
        workflow.add_task({'id': 'task1', 'type': 'failing'})
        workflow.add_task({'id': 'task2', 'type': 'downstream', 'depends_on': ['task1']})

        results = engine.execute(workflow, parallel=True)

        assert results['task1'].status == TaskStatus.FAILED
        assert results['task2'].status == TaskStatus.SKIPPED
        assert not downstream_ran['called']

    @pytest.mark.parametrize("parallel", [False, True])
    def test_skip_cascades_through_three_level_dag(self, parallel):
        """A(FAILED) -> B(依存失敗によりSKIPPED) -> C(Bに依存)という
        3段DAGで、SKIPPEDのさらに下流もSKIPPEDになる（COMPLETEDにならない）
        こと。逐次実行・並列実行の両方で確認する。"""
        registry = TaskRegistry()
        ran = {'B': False, 'C': False}

        def a_func(**kwargs):
            raise ValueError("boom")

        def b_func(**kwargs):
            ran['B'] = True
            return "b"

        def c_func(**kwargs):
            ran['C'] = True
            return "c"

        registry.register('a_type', a_func)
        registry.register('b_type', b_func)
        registry.register('c_type', c_func)
        engine = WorkflowEngine(registry)

        workflow = WorkflowDefinition(name="test")
        workflow.add_task({'id': 'A', 'type': 'a_type'})
        workflow.add_task({'id': 'B', 'type': 'b_type', 'depends_on': ['A']})
        workflow.add_task({'id': 'C', 'type': 'c_type', 'depends_on': ['B']})

        results = engine.execute(workflow, parallel=parallel)

        assert results['A'].status == TaskStatus.FAILED
        assert results['B'].status == TaskStatus.SKIPPED
        assert results['C'].status == TaskStatus.SKIPPED
        assert not ran['B']
        assert not ran['C']

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
