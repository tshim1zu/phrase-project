"""
ワークフロー/パイプラインエンジン

YAML定義に基づいて、複数タスクを依存関係を解決しながら実行するエンジン
"""

import yaml
import json
import logging
from typing import Dict, List, Any, Optional, Callable
from pathlib import Path
from dataclasses import dataclass, field
from enum import Enum
try:
    import networkx as nx
    _HAS_NETWORKX = True
except ImportError:
    nx = None
    _HAS_NETWORKX = False
from concurrent.futures import ThreadPoolExecutor, as_completed

logger = logging.getLogger(__name__)


class TaskStatus(Enum):
    """タスク実行状態"""
    PENDING = "pending"
    RUNNING = "running"
    COMPLETED = "completed"
    FAILED = "failed"
    SKIPPED = "skipped"


@dataclass
class TaskDefinition:
    """タスク定義"""
    id: str
    type: str
    input: Optional[str] = None
    inputs: Optional[List[str]] = None
    output: Optional[str] = None
    depends_on: Optional[List[str]] = None
    params: Dict[str, Any] = field(default_factory=dict)

    def __post_init__(self):
        """依存タスクが未指定の場合は空のリストへ正規化する。"""
        if self.depends_on is None:
            self.depends_on = []


@dataclass
class TaskResult:
    """タスク実行結果"""
    task_id: str
    status: TaskStatus
    result: Any = None
    error: Optional[str] = None
    duration: float = 0.0


class WorkflowDefinition:
    """ワークフロー定義"""

    def __init__(
        self,
        name: str,
        description: str = "",
        tasks: Optional[List[Dict[str, Any]]] = None
    ):
        """名前と説明を保持し、指定されたタスク定義を登録する。"""
        self.name = name
        self.description = description
        self.tasks: Dict[str, TaskDefinition] = {}

        if tasks:
            for task_data in tasks:
                self.add_task(task_data)

    def add_task(self, task_data: Dict[str, Any]):
        """タスクを追加"""
        task = TaskDefinition(
            id=task_data['id'],
            type=task_data['type'],
            input=task_data.get('input'),
            inputs=task_data.get('inputs'),
            output=task_data.get('output'),
            depends_on=task_data.get('depends_on', []),
            params=task_data.get('params', {})
        )
        self.tasks[task.id] = task

    @staticmethod
    def from_yaml(filepath: str) -> 'WorkflowDefinition':
        """YAMLファイルからワークフローを読込"""
        with open(filepath, 'r', encoding='utf-8') as f:
            data = yaml.safe_load(f)

        return WorkflowDefinition(
            name=data['name'],
            description=data.get('description', ''),
            tasks=data.get('tasks', [])
        )

    @staticmethod
    def from_dict(data: Dict[str, Any]) -> 'WorkflowDefinition':
        """辞書からワークフローを生成"""
        return WorkflowDefinition(
            name=data['name'],
            description=data.get('description', ''),
            tasks=data.get('tasks', [])
        )

    def to_dict(self) -> Dict[str, Any]:
        """辞書に変換"""
        return {
            'name': self.name,
            'description': self.description,
            'tasks': [
                {
                    'id': t.id,
                    'type': t.type,
                    'input': t.input,
                    'inputs': t.inputs,
                    'output': t.output,
                    'depends_on': t.depends_on,
                    'params': t.params
                }
                for t in self.tasks.values()
            ]
        }

    def validate(self) -> tuple[bool, List[str]]:
        """ワークフロー定義の妥当性をチェック"""
        errors = []

        # タスク0件チェック（0件は「検証を通過して何もせず成功する」fail-openになるため拒否する）
        if not self.tasks:
            errors.append("タスクが1件も定義されていません")

        # タスクIDの重複チェック
        if len(set(self.tasks.keys())) != len(self.tasks):
            errors.append("タスクIDが重複しています")

        # 依存関係の妥当性チェック
        for task in self.tasks.values():
            for dep in task.depends_on:
                if dep not in self.tasks:
                    errors.append(f"タスク '{task.id}' の依存タスク '{dep}' が見つかりません")

        # 循環依存チェック
        if self._has_circular_dependency():
            errors.append("循環依存が検出されました")

        return len(errors) == 0, errors

    def _has_circular_dependency(self) -> bool:
        """循環依存をチェック"""
        g = nx.DiGraph()

        for task in self.tasks.values():
            g.add_node(task.id)
            for dep in task.depends_on:
                g.add_edge(dep, task.id)

        # DAGでない場合は循環依存がある
        return not nx.is_directed_acyclic_graph(g)

    def get_execution_order(self) -> List[str]:
        """実行順序を計算（トポロジカルソート）"""
        g = nx.DiGraph()

        for task in self.tasks.values():
            g.add_node(task.id)
            for dep in task.depends_on:
                g.add_edge(dep, task.id)

        return list(nx.algorithms.dag.topological_sort(g))


class TaskRegistry:
    """タスク実行関数のレジストリ"""

    def __init__(self):
        """タスク種別から実行関数を引く空のレジストリを初期化する。"""
        self.tasks: Dict[str, Callable] = {}

    def register(self, task_type: str, func: Callable):
        """タスク実行関数を登録"""
        self.tasks[task_type] = func
        logger.info(f"タスク '{task_type}' を登録しました")

    def get(self, task_type: str) -> Optional[Callable]:
        """タスク実行関数を取得"""
        return self.tasks.get(task_type)

    def register_builtin_tasks(self):
        """ビルトインタスク（japhrase統合機能）を登録"""
        from .extracter import PhraseExtracter
        from .writing_assistant import (
            KWICAnalyzer,
            AbstractBodyChecker,
            HabitDetector
        )

        def extract_task(input_file: str, params: Dict[str, Any], **kwargs) -> Any:
            """フレーズ抽出タスク"""
            extractor = PhraseExtracter(**params)
            texts = self._read_file(input_file)
            return extractor.get_dfphrase(texts)

        def kwic_task(input_file: str, params: Dict[str, Any], **kwargs) -> Any:
            """KWIC検索タスク"""
            kwic = KWICAnalyzer(
                self._read_file(input_file),
                context_lines=params.get('context_lines', 1)
            )
            phrase = params.get('phrase', '')
            return kwic.find_phrase(phrase)

        def check_divergence_task(
            abstract_file: str,
            body_file: str,
            params: Dict[str, Any],
            **kwargs
        ) -> Any:
            """あらすじ vs 本文 チェックタスク"""
            abstract = '\n'.join(self._read_file(abstract_file))
            body = '\n'.join(self._read_file(body_file))
            checker = AbstractBodyChecker(abstract, body)
            return {
                'divergence_score': checker.get_divergence_score(),
                'missing': checker.get_missing_phrases(),
                'added': checker.get_added_phrases()
            }

        def detect_habits_task(input_file: str, params: Dict[str, Any], **kwargs) -> Any:
            """口癖検出タスク"""
            detector = HabitDetector(
                self._read_file(input_file),
                **params
            )
            return detector.detect_habits()

        self.register('extract', extract_task)
        self.register('kwic', kwic_task)
        self.register('check_divergence', check_divergence_task)
        self.register('detect_habits', detect_habits_task)

    @staticmethod
    def _read_file(filepath: str) -> List[str]:
        """ファイルを読込"""
        from .utils import read_file
        return read_file(filepath, encoding='auto')


class WorkflowEngine:
    """ワークフロー実行エンジン"""

    def __init__(self, registry: Optional[TaskRegistry] = None):
        """タスクレジストリへ組み込み処理を登録し、結果領域を初期化する。"""
        self.registry = registry or TaskRegistry()
        self.registry.register_builtin_tasks()
        self.results: Dict[str, TaskResult] = {}

    def execute(
        self,
        workflow: WorkflowDefinition,
        parallel: bool = False,
        max_workers: int = 4
    ) -> Dict[str, TaskResult]:
        """ワークフローを実行"""
        # バリデーション
        valid, errors = workflow.validate()
        if not valid:
            raise ValueError(f"ワークフロー検証エラー: {errors}")

        logger.info(f"ワークフロー '{workflow.name}' を実行開始します")

        self.results = {}
        execution_order = workflow.get_execution_order()

        if parallel:
            self._execute_parallel(workflow, execution_order, max_workers)
        else:
            self._execute_sequential(workflow, execution_order)

        logger.info(f"ワークフロー実行完了")
        return self.results

    def _execute_sequential(
        self,
        workflow: WorkflowDefinition,
        execution_order: List[str]
    ):
        """順序実行"""
        for task_id in execution_order:
            self._execute_task(workflow, task_id)

    def _execute_parallel(
        self,
        workflow: WorkflowDefinition,
        execution_order: List[str],
        max_workers: int
    ):
        """並列実行"""
        with ThreadPoolExecutor(max_workers=max_workers) as executor:
            futures = {}
            completed = set()

            while len(completed) < len(execution_order):
                # 実行可能なタスクを検出
                ready_tasks = []
                for task_id in execution_order:
                    if task_id not in completed:
                        task = workflow.tasks[task_id]
                        deps_satisfied = all(d in completed for d in task.depends_on)
                        if deps_satisfied and task_id not in futures:
                            ready_tasks.append(task_id)

                # 実行可能なタスクを送信
                for task_id in ready_tasks:
                    future = executor.submit(self._execute_task, workflow, task_id)
                    futures[task_id] = future

                # 完了を待つ
                if futures:
                    done, _ = as_completed(futures.values()), None
                    for future in done:
                        for task_id, f in list(futures.items()):
                            if f == future:
                                completed.add(task_id)
                                del futures[task_id]
                                break

    def _execute_task(self, workflow: WorkflowDefinition, task_id: str):
        """タスクを実行"""
        task = workflow.tasks[task_id]

        logger.info(f"[{task_id}] 実行開始 (type={task.type})")

        result = TaskResult(
            task_id=task_id,
            status=TaskStatus.RUNNING
        )

        try:
            # タスク実行関数を取得
            func = self.registry.get(task.type)
            if not func:
                raise ValueError(f"未知のタスクタイプ: {task.type}")

            # 入力を準備
            kwargs = {
                'params': task.params,
                'workflow': workflow,
                'results': self.results
            }

            if task.input:
                kwargs['input_file'] = task.input
            if task.inputs:
                kwargs.update({f'input_{i}': f for i, f in enumerate(task.inputs)})

            # タスクを実行
            task_result = func(**kwargs)

            result.status = TaskStatus.COMPLETED
            result.result = task_result

            # 出力をファイルに保存
            if task.output and task_result is not None:
                self._save_output(task.output, task_result)

            logger.info(f"[{task_id}] 実行完了")

        except Exception as e:
            result.status = TaskStatus.FAILED
            result.error = str(e)
            logger.error(f"[{task_id}] 実行失敗: {e}")

        self.results[task_id] = result

    @staticmethod
    def _save_output(filepath: str, result: Any):
        """結果をファイルに保存"""
        Path(filepath).parent.mkdir(parents=True, exist_ok=True)

        if hasattr(result, 'to_csv'):  # DataFrame
            result.to_csv(filepath, index=False, encoding='utf-8-sig')
        elif isinstance(result, (dict, list)):
            with open(filepath, 'w', encoding='utf-8') as f:
                json.dump(result, f, indent=2, ensure_ascii=False)
        else:
            with open(filepath, 'w', encoding='utf-8') as f:
                f.write(str(result))

        logger.info(f"結果を保存しました: {filepath}")

    def get_report(self) -> str:
        """実行レポートを生成"""
        report = []
        report.append("=" * 70)
        report.append("ワークフロー実行レポート")
        report.append("=" * 70)
        report.append("")

        # 統計情報
        total = len(self.results)
        completed = sum(1 for r in self.results.values() if r.status == TaskStatus.COMPLETED)
        failed = sum(1 for r in self.results.values() if r.status == TaskStatus.FAILED)

        report.append(f"実行結果: {completed}/{total} 成功")
        if failed > 0:
            report.append(f"⚠️ {failed}件の失敗")
        report.append("")

        # タスク詳細
        report.append("タスク実行詳細:")
        for task_id, result in self.results.items():
            status_icon = {
                TaskStatus.COMPLETED: "✅",
                TaskStatus.FAILED: "❌",
                TaskStatus.SKIPPED: "⏭️"
            }.get(result.status, "❓")

            report.append(f"  {status_icon} {task_id}: {result.status.value}")
            if result.error:
                report.append(f"      エラー: {result.error}")

        report.append("")
        return "\n".join(report)
