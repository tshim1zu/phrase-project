"""
japhrase - 日本語テキストから頻出フレーズを検出

N-gramベースの頻度分析により、テキスト中で頻繁に出現するフレーズを検出します。
SNSトレンド分析、ニュース話題抽出、頻出キーワード発見などに適しています。

使用例:
    >>> from japhrase import PhraseExtracter
    >>> extractor = PhraseExtracter(min_count=6, max_length=16)
    >>> df_result = extractor.get_dfphrase(sentences)

詳細は POSITIONING.md を参照してください。
"""

__version__ = "0.1.3"
__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023"

from .extracter import PhraseExtracter, extracter, PRESETS
from .constants import FIRST_KANJI, DEFAULT_REMOVES, DEFAULT_UNNECESSARY
from .patterns import get_positive_patterns, get_negative_patterns

# 新機能
from .evaluation import UnsupervisedEvaluator, SupervisedEvaluator
from .optimization import UnsupervisedOptimizer, SupervisedOptimizer
from .similarity import SimilarityAnalyzer
from .writing_assistant import (
    KWICAnalyzer,
    AbstractBodyChecker,
    HabitDetector,
    RevisionHeatmap,
    RankingTrajectory
)
from .writing_tools import (
    EditorConfigGenerator,
    SelfRecommender
)
from .workflow import (
    WorkflowDefinition,
    WorkflowEngine,
    TaskRegistry,
    TaskDefinition,
    TaskResult,
    TaskStatus
)
from .use_cases import WritingWorkflow
from .config import JaphraseConfig
from .checker import QualityChecker

__all__ = [
    'PhraseExtracter',
    'extracter',  # 後方互換性のため
    'PRESETS',
    'FIRST_KANJI',
    'DEFAULT_REMOVES',
    'DEFAULT_UNNECESSARY',
    'get_positive_patterns',
    'get_negative_patterns',
    # 評価・最適化
    'UnsupervisedEvaluator',
    'SupervisedEvaluator',
    'UnsupervisedOptimizer',
    'SupervisedOptimizer',
    # 類似度分析
    'SimilarityAnalyzer',
    # 執筆支援ツール
    'KWICAnalyzer',
    'AbstractBodyChecker',
    'HabitDetector',
    'RevisionHeatmap',
    'RankingTrajectory',
    # 執筆補助ツール
    'EditorConfigGenerator',
    'SelfRecommender',
    # ワークフロー/パイプライン
    'WorkflowDefinition',
    'WorkflowEngine',
    'TaskRegistry',
    'TaskDefinition',
    'TaskResult',
    'TaskStatus',
    # ユースケース駆動インターフェース
    'WritingWorkflow',
    # 設定ファイル管理
    'JaphraseConfig',
    # 品質チェック
    'QualityChecker',
]
