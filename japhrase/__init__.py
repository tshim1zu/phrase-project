"""
japhrase - 日本語テキストから頻出フレーズを検出

N-gramベースの頻度分析とPMI（自己相互情報量）により、
辞書やLLMに依存せずテキスト中の頻出フレーズを検出します。

使用例:
    >>> from japhrase import PhraseExtractor
    >>> pe = PhraseExtractor()
    >>> df = pe.extract('テキスト文字列、またはファイルパス')
"""

__version__ = "0.3.13"
__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023-2026"
__license__ = "MIT"
__email__ = "shim1zu@hotmail.com"

from .extracter import PhraseExtracter, extracter, PRESETS
# 正しい綴りをメインに、旧綴りも後方互換で残す
PhraseExtractor = PhraseExtracter
from .constants import FIRST_KANJI, DEFAULT_REMOVES, DEFAULT_UNNECESSARY
from .patterns import get_positive_patterns, get_negative_patterns

# 新機能
from .evaluation import UnsupervisedEvaluator, SupervisedEvaluator
from .optimization import UnsupervisedOptimizer, SupervisedOptimizer
try:
    from .similarity import SimilarityAnalyzer
except ImportError:
    SimilarityAnalyzer = None
from .segmenter import TextSegmenter, segment_text
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
try:
    from .workflow import (
        WorkflowDefinition,
        WorkflowEngine,
        TaskRegistry,
        TaskDefinition,
        TaskResult,
        TaskStatus
    )
except ImportError:
    WorkflowDefinition = WorkflowEngine = TaskRegistry = None
    TaskDefinition = TaskResult = TaskStatus = None
from .use_cases import WritingWorkflow
from .config import JaphraseConfig
from .checker import QualityChecker
from .summarizer import Summarizer
from .cooccurrence import CooccurrenceAnalyzer
# 汎用フォーマット対応モジュール
from .formatters import (
    UniversalFormatter,
    TextFormatter,
    JSONFormatter,
    CSVFormatter,
    BaseFormatter
)
from .encoding import (
    EncodingDetector,
    BOMHandler,
    EncodingValidator
)
from .text_variant_detector import (
    TextVariantDetector,
    VariantCandidate
)
from .phrase_unifier import (
    PhraseUnifier,
    InteractiveUnifier
)
# Phase B: 統計的有意性評価
from .statistical_scorer import (
    StatisticalScorer,
    StatisticalScore,
    PhraseTrustMetric
)
# Phase B: パラメータ自動最適化
from .parameter_optimizer import (
    ParameterOptimizer,
    TextCharacteristics,
    ParameterRecommendation
)
# Phase B: ストリーミング処理
from .streaming_processor import (
    StreamingPhraseExtracter,
    ChunkedTextReader,
    IncrementalAggregator,
    StreamingAnalyzer
)
# Incremental state
from .incremental import IncrementalPhraseState
# Phase B: 自動インサイト生成
from .insight_generator import (
    InsightGenerator,
    Insight
)
# 執筆支援: てにをはLint
from .teniwoha_lint import TeniwohaLinter
# 執筆支援: 同文反復検出とバリエーション生成
from .sentence_variation import SentenceVariationGenerator
# 執筆支援: 主語/視点ブレ検出
from .subject_pov_detector import SubjectPOVDetector
# 執筆支援: 冗長語尾のヒートマップ
from .ending_heatmap import EndingHeatmapGenerator
# 執筆支援: 比喩・専門語の過密警告
from .metaphor_density_detector import MetaphorDensityDetector
# 執筆支援: 文章テンポスコア
from .tempo_analyzer import TempoAnalyzer
# 執筆支援: 場面転換の弱さ検出
from .scene_transition_detector import SceneTransitionDetector
# Evidence and reports
from .evidence import build_phrase_evidence
from .stats_utils import compute_stats_data
from .stats_report import render_stats_html
# 書き癖検出
from .writing_habit_detector import WritingHabitDetector
# ドキュメント ベクトル化（sklearn が必要）
try:
    from .document_vectorizer import DocumentVectorizer, VectorizationResult
except ImportError:
    DocumentVectorizer = VectorizationResult = None
# テキストマイニング・計量文学モジュール
from .dialogue_analyzer import DialogueAnalyzer, analyze_dialogue
from .orthography_checker import OrthographyVariantDetector, check_orthography
from .stylometry import StylometryAnalyzer, analyze_stylometry, get_stylometry_summary
from .character_network import CharacterNetworkGenerator, generate_character_network
from .prompt_optimizer import PromptOptimizer, optimize_prompt
# 執筆支援: 高度な分析機能
from .ending_repetition_police import EndingRepetitionPolice, EndingRepetitionIssue
from .entropy_pacing import EntropyPacing, PacingIssue
from .chekhov_gun_detector import ChekhofGunDetector, UnfiredGun
# Phase 2 強化: 分布比較エンジン
from .distribution_comparator import (
    DistributionComparator,
    DistributionComparison,
    KeynessResult,
)
# Phase 2 強化: コロケーション結合度
from .collocation_scorer import (
    CollocationScorer,
    CollocationScore,
)
# Phase 2 強化: テキスト複雑度
from .complexity_metrics import (
    ComplexityAnalyzer,
    ComplexityProfile,
)
# Phase 2 強化: 時系列分析
from .temporal_analyzer import (
    TemporalAnalyzer,
    TemporalSnapshot,
    TermTrend,
    BurstInterval,
)
# Phase 3: 適応型パラメータチューナー
from .adaptive_tuner import AdaptiveTuner

__all__ = [
    'PhraseExtracter',
    'PhraseExtractor',  # エイリアス（Extractor でも Extracter でも動く）
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
    # テキストセグメンテーション
    'TextSegmenter',
    'segment_text',
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
    # 統計的要約
    'Summarizer',
    # 共起語分析
    'CooccurrenceAnalyzer',
    # 汎用フォーマット対応
    'UniversalFormatter',
    'TextFormatter',
    'JSONFormatter',
    'CSVFormatter',
    'BaseFormatter',
    'EncodingDetector',
    'BOMHandler',
    'EncodingValidator',
    # 表記ゆれ検出・統一
    'TextVariantDetector',
    'VariantCandidate',
    'PhraseUnifier',
    'InteractiveUnifier',
    # Phase B: 統計的有意性
    'StatisticalScorer',
    'StatisticalScore',
    'PhraseTrustMetric',
    # Phase B: パラメータ最適化
    'ParameterOptimizer',
    'TextCharacteristics',
    'ParameterRecommendation',
    # Phase B: ストリーミング処理
    'StreamingPhraseExtracter',
    'ChunkedTextReader',
    'IncrementalAggregator',
    'StreamingAnalyzer',
    'IncrementalPhraseState',
    # Phase B: インサイト生成
    'InsightGenerator',
    'Insight',
    # 執筆支援: てにをはLint
    'TeniwohaLinter',
    # 執筆支援: 同文反復検出とバリエーション生成
    'SentenceVariationGenerator',
    # 執筆支援: 主語/視点ブレ検出
    'SubjectPOVDetector',
    # 執筆支援: 冗長語尾のヒートマップ
    'EndingHeatmapGenerator',
    # 執筆支援: 比喩・専門語の過密警告
    'MetaphorDensityDetector',
    # 執筆支援: 文章テンポスコア
    'TempoAnalyzer',
    # 執筆支援: 場面転換の弱さ検出
    'SceneTransitionDetector',
    'build_phrase_evidence',
    'compute_stats_data',
    'render_stats_html',
    # 書き癖検出
    'WritingHabitDetector',
    # ドキュメント ベクトル化
    'DocumentVectorizer',
    'VectorizationResult',
    # テキストマイニング・計量文学モジュール
    'DialogueAnalyzer',
    'analyze_dialogue',
    'OrthographyVariantDetector',
    'check_orthography',
    'StylometryAnalyzer',
    'analyze_stylometry',
    'get_stylometry_summary',
    'CharacterNetworkGenerator',
    'generate_character_network',
    'PromptOptimizer',
    'optimize_prompt',
    # 執筆支援: 高度な分析機能（Sharp Features）
    'EndingRepetitionPolice',
    'EndingRepetitionIssue',
    'EntropyPacing',
    'PacingIssue',
    'ChekhofGunDetector',
    'UnfiredGun',
    # Phase 2 強化: 分布比較エンジン
    'DistributionComparator',
    'DistributionComparison',
    'KeynessResult',
    # Phase 2 強化: コロケーション結合度
    'CollocationScorer',
    'CollocationScore',
    # Phase 2 強化: テキスト複雑度
    'ComplexityAnalyzer',
    'ComplexityProfile',
    # Phase 2 強化: 時系列分析
    'TemporalAnalyzer',
    'TemporalSnapshot',
    'TermTrend',
    'BurstInterval',
    # デモ
    'run_demo',
    # 適応型チューナー
    'AdaptiveTuner',
]


def run_demo():
    """全機能のデモを実行。使い方: import japhrase; japhrase.run_demo()"""
    from .demo import demo_all
    demo_all()


# 各クラスに .demo() クラスメソッドを付与
def _attach_demos():
    from .demo import (
        demo_phrase_extracter, demo_stylometry, demo_complexity,
        demo_distribution, demo_collocation, demo_contamination,
        demo_preflight,
    )
    _pairs = [
        # PhraseExtracter は独自の demo(**kwargs) メソッドを持つため除外
        (StylometryAnalyzer, demo_stylometry),
        (ComplexityAnalyzer, demo_complexity),
        (DistributionComparator, demo_distribution),
        (CollocationScorer, demo_collocation),
    ]
    for cls, func in _pairs:
        if cls is not None:
            cls.demo = classmethod(lambda cls_, _f=func: _f())

_attach_demos()
