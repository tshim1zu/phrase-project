# coding: utf-8
"""
E2E: まっさらな環境での動作確認

pip install japhrase だけで入るコア依存（numpy, pandas, scipy, chardet, click, PyYAML）
のみがある環境を模擬し、全公開APIが import + 基本実行できることを検証する。

optional 依存（sklearn, matplotlib, networkx 等）をブロックした状態でテストする。
"""

import sys
import pytest


# optional 依存をブロックするフック
class _BlockOptionalImports:
    """optional な外部ライブラリの import を失敗させる

    networkx/openpyxl は pyproject.toml のコア依存であり（Workflow検証や
    export_excel()が無条件に使うため）、"clean install"を模擬する上で
    ブロックしてはいけない。ブロックすると「importは通るが機能実行時に
    死ぬ」欠陥（過去に実際発生: nx=Noneのままvalidate()がnx.DiGraph()を
    呼びクラッシュ）を再現できなくなり、テストが偽の安心感を与える。
    """
    BLOCKED = [
        'sklearn', 'matplotlib', 'seaborn',
        'Levenshtein', 'optuna', 'tqdm',
        'requests', 'plotly',
    ]

    def find_module(self, name, path=None):
        if any(name == b or name.startswith(b + '.') for b in self.BLOCKED):
            return self
        return None

    def load_module(self, name):
        raise ImportError(f'[test_clean_install] {name} is blocked')


@pytest.fixture(autouse=True)
def block_optional_deps():
    """テスト関数ごとに optional 依存をブロック"""
    blocker = _BlockOptionalImports()
    sys.meta_path.insert(0, blocker)
    yield
    sys.meta_path.remove(blocker)


class TestCoreImport:
    """コアモジュールが import できる"""

    def test_japhrase_top_level(self):
        import japhrase
        assert japhrase.__version__

    def test_phrase_extracter(self):
        from japhrase import PhraseExtracter
        assert PhraseExtracter is not None

    def test_statistical_scorer(self):
        from japhrase import StatisticalScorer
        assert StatisticalScorer is not None

    def test_stylometry(self):
        from japhrase import StylometryAnalyzer
        assert StylometryAnalyzer is not None

    def test_complexity(self):
        from japhrase import ComplexityAnalyzer
        assert ComplexityAnalyzer is not None

    def test_distribution_comparator(self):
        from japhrase import DistributionComparator
        assert DistributionComparator is not None

    def test_collocation_scorer(self):
        from japhrase import CollocationScorer
        assert CollocationScorer is not None

    def test_temporal_analyzer(self):
        from japhrase import TemporalAnalyzer
        assert TemporalAnalyzer is not None


class TestContaminationImport:
    """contamination パッケージの高水準 API が import できる"""

    def test_scan(self):
        from japhrase.contamination import scan
        assert callable(scan)

    def test_quick_check(self):
        from japhrase.contamination import quick_check
        assert callable(quick_check)

    def test_compare(self):
        from japhrase.contamination import compare
        assert callable(compare)

    def test_batch_scan(self):
        from japhrase.contamination import batch_scan
        assert callable(batch_scan)


class TestAppliedImport:
    """applied パッケージが import できる"""

    def test_preflight(self):
        from japhrase.applied import PreflightChecker
        assert PreflightChecker is not None

    def test_dashboard(self):
        from japhrase.applied import EPDashboard
        assert EPDashboard is not None

    def test_habit_drift(self):
        from japhrase.applied import HabitDriftDetector
        assert HabitDriftDetector is not None

    def test_jpen_divergence(self):
        from japhrase.applied import JPENDivergenceChecker
        assert JPENDivergenceChecker is not None

    def test_character_stylometry(self):
        from japhrase.applied import CharacterStylometry
        assert CharacterStylometry is not None

    def test_part_health(self):
        from japhrase.applied import PartHealthReport
        assert PartHealthReport is not None


class TestCoreExecution:
    """import だけでなく、基本的な実行ができる"""

    def test_phrase_extract(self):
        from japhrase import PhraseExtracter
        ext = PhraseExtracter(min_count=2, max_length=10, min_length=2, verbose=0)
        df = ext.extract(['テスト文章です。テスト文章です。テスト。'] * 5)
        assert df is not None

    def test_stylometry_analyze(self):
        from japhrase import StylometryAnalyzer
        stylo = StylometryAnalyzer()
        result = stylo.analyze_full('朝靄の中を歩いていると、足元で小さな花が揺れた。')
        assert 'vocabulary' in result
        assert 'advanced_diversity' in result
        assert 'mattr' in result

    def test_complexity_analyze(self):
        from japhrase import ComplexityAnalyzer
        cx = ComplexityAnalyzer()
        result = cx.analyze('朝靄の中を歩いていると、足元で小さな花が揺れた。')
        assert 'perplexity' in result
        assert 'compression_ratio' in result

    def test_distribution_compare(self):
        from japhrase import DistributionComparator
        from collections import Counter
        comp = DistributionComparator()
        result = comp.compare(Counter({'a': 5, 'b': 3}), Counter({'a': 2, 'c': 7}))
        assert 0 <= result.jsd <= 1

    def test_collocation_score(self):
        from japhrase import CollocationScorer
        scorer = CollocationScorer()
        cs = scorer.score_phrase('テスト', 10, 'テスト文章のテスト。テスト。' * 10)
        assert hasattr(cs, 'pmi')
        assert hasattr(cs, 't_score')
        assert hasattr(cs, 'log_dice')

    def test_contamination_scan(self):
        from japhrase.contamination import scan
        profile = scan('正常なテキスト。問題なし。')
        assert profile.is_clean()
        assert profile.overall <= 10

    def test_contamination_quick_check(self):
        from japhrase.contamination import quick_check
        assert quick_check('正常なテキスト。') is False

    def test_contamination_explain(self):
        from japhrase.contamination import scan
        profile = scan('正常なテキスト。')
        explanation = profile.explain()
        assert '問題なし' in explanation

    def test_contamination_batch(self):
        from japhrase.contamination import batch_scan
        result = batch_scan({'a': 'テスト。', 'b': 'テスト。'})
        assert result.all_clean

    def test_preflight_check(self):
        from japhrase.applied import PreflightChecker
        checker = PreflightChecker()
        result = checker.check('テスト文章。' * 50, lang='jp')
        assert result.verdict in ('GO', 'WARN', 'NOGO')
        assert 0 <= result.quality_score <= 100

    def test_workflow_validate_and_execute(self, tmp_path):
        """WorkflowDefinition.validate()/WorkflowEngine.execute()は
        importできるだけでなく、コア依存(networkx)がある通常のclean install
        環境で実際に実行できること。

        過去、この経路はimportテストだけがあり実行を通していなかったため、
        `nx=None`のままvalidate()が無条件にnx.DiGraph()を呼びクラッシュする
        不具合（networkxが未インストールの環境で発生）を見逃していた。
        """
        from japhrase import WorkflowDefinition, WorkflowEngine

        input_file = tmp_path / "input.txt"
        input_file.write_text("テスト文章です。テスト文章です。", encoding='utf-8')

        workflow = WorkflowDefinition(name="clean_install_smoke")
        workflow.add_task({
            'id': 'extract',
            'type': 'extract',
            'input': str(input_file),
            'params': {'min_count': 1},
        })

        valid, errors = workflow.validate()
        assert valid, errors

        order = workflow.get_execution_order()
        assert order == ['extract']

        engine = WorkflowEngine()
        results = engine.execute(workflow)
        assert results['extract'].status.value == 'completed'

    def test_export_excel(self, tmp_path):
        """export_excel()はopenpyxlがコア依存として入っている
        clean install環境で実際に動作すること。"""
        from japhrase import PhraseExtracter

        extractor = PhraseExtracter(min_count=1, verbose=0)
        df = extractor.extract(['テスト文章です。テスト文章です。テスト。'] * 5)

        out_path = tmp_path / "output.xlsx"
        extractor.export_excel(df, str(out_path))

        assert out_path.is_file()
        assert out_path.stat().st_size > 0
