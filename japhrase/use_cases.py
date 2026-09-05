"""
ユースケース駆動のワークフローインターフェース

特定の執筆シナリオに最適化されたプリセットワークフローを提供します。
"""

from typing import Dict, List, Optional, Any
from pathlib import Path
import json

from .workflow import WorkflowDefinition, WorkflowEngine, TaskRegistry
from .extracter import PhraseExtracter
from .writing_assistant import (
    KWICAnalyzer,
    AbstractBodyChecker,
    HabitDetector
)
from .writing_tools import EditorConfigGenerator, SelfRecommender
from .utils import read_file


class WritingWorkflow:
    """ユースケース別のワークフロー"""

    # ユースケース定義
    USECASES = {
        'academic_writing': {
            'name': '学位論文・学術論文',
            'description': '論文の品質をチェック（あらすじ確認 + 口癖検出 + 過去論文検索）',
            'required_files': ['body_file'],
            'optional_files': ['abstract_file', 'past_corpus_dir'],
        },
        'novel_revision': {
            'name': '小説推敲',
            'description': '複数版の原稿を比較（ランキング変動 + 推敲偏り + KWIC検索）',
            'required_files': ['v1'],
            'optional_files': ['v2', 'v3', 'v4'],
        },
        'blog_writing': {
            'name': 'ブログ記事執筆',
            'description': 'ブログ記事の最適化（フレーズ抽出 + 表記ゆれ + 推敲偏り）',
            'required_files': ['body_file'],
            'optional_files': ['past_corpus_dir'],
        },
        'sns_content': {
            'name': 'SNS投稿',
            'description': 'SNS記事の表記を統一（短いフレーズ抽出 + 表記ゆれ検出）',
            'required_files': ['body_file'],
            'optional_files': [],
        },
        'editing': {
            'name': '編集者向けチェック',
            'description': '編集業務の効率化（乖離検出 + 口癖 + 過去記事検索）',
            'required_files': ['body_file'],
            'optional_files': ['abstract_file', 'past_corpus_dir'],
        },
    }

    def __init__(self, use_case: str):
        """
        ユースケースに基づいてワークフローを初期化

        Args:
            use_case: ユースケース ID
        """
        if use_case not in self.USECASES:
            raise ValueError(
                f"未知のユースケース: {use_case}\n"
                f"利用可能: {list(self.USECASES.keys())}"
            )

        self.use_case = use_case
        self.config = self.USECASES[use_case]
        self.inputs = {}

    @classmethod
    def for_use_case(cls, use_case: str) -> 'WritingWorkflow':
        """ユースケースからワークフローを生成"""
        return cls(use_case)

    @classmethod
    def list_usecases(cls) -> Dict[str, str]:
        """利用可能なユースケース一覧を取得"""
        return {
            uc: info['description'] for uc, info in cls.USECASES.items()
        }

    def run(
        self,
        body_file: Optional[str] = None,
        abstract_file: Optional[str] = None,
        v1: Optional[str] = None,
        v2: Optional[str] = None,
        v3: Optional[str] = None,
        v4: Optional[str] = None,
        past_corpus_dir: Optional[str] = None,
        output_dir: str = 'results',
        **kwargs
    ) -> str:
        """
        ワークフローを実行

        Args:
            body_file: 本文ファイル
            abstract_file: あらすじファイル
            v1-v4: 複数版ファイル（novel_revision用）
            past_corpus_dir: 過去原稿ディレクトリ
            output_dir: 出力ディレクトリ
            **kwargs: その他パラメータ

        Returns:
            実行レポート
        """
        # 出力ディレクトリを作成
        Path(output_dir).mkdir(parents=True, exist_ok=True)

        if self.use_case == 'academic_writing':
            return self._run_academic_writing(
                body_file, abstract_file, past_corpus_dir, output_dir
            )
        elif self.use_case == 'novel_revision':
            return self._run_novel_revision(v1, v2, v3, v4, output_dir)
        elif self.use_case == 'blog_writing':
            return self._run_blog_writing(body_file, past_corpus_dir, output_dir)
        elif self.use_case == 'sns_content':
            return self._run_sns_content(body_file, output_dir)
        elif self.use_case == 'editing':
            return self._run_editing(
                body_file, abstract_file, past_corpus_dir, output_dir
            )

    def _run_academic_writing(
        self,
        body_file: str,
        abstract_file: Optional[str],
        past_corpus_dir: Optional[str],
        output_dir: str
    ) -> str:
        """学位論文・学術論文の品質チェック"""
        report = []
        report.append("=" * 70)
        report.append("学位論文・学術論文 品質チェック")
        report.append("=" * 70)
        report.append("")

        texts = read_file(body_file, encoding='auto')
        body_text = '\n'.join(texts)

        # 1. フレーズ抽出
        report.append("[1/3] フレーズ抽出\n")
        extractor = PhraseExtracter(min_count=5, max_length=30, verbose=0)
        phrases_df = extractor.get_dfphrase(texts)
        report.append(f"検出フレーズ数: {len(phrases_df)}個\n")
        if len(phrases_df) > 0:
            report.append("上位フレーズ:")
            for idx, row in phrases_df.head(10).iterrows():
                report.append(f"  {row['seqchar']}: {int(row['freq'])}回")
        report.append("")

        # 2. あらすじチェック
        if abstract_file:
            report.append("[2/3] あらすじ vs 本文\n")
            abstract_text = '\n'.join(read_file(abstract_file, encoding='auto'))
            checker = AbstractBodyChecker(abstract_text, body_text)

            divergence = checker.get_divergence_score()
            report.append(f"乖離度: {divergence:.1%}\n")

            missing = checker.get_missing_phrases()
            added = checker.get_added_phrases()
            report.append(f"あらすじにない表現: {len(missing)}個")
            report.append(f"あらすじにない本文表現: {len(added)}個")
        else:
            report.append("[2/3] あらすじチェック - スキップ\n")
        report.append("")

        # 3. 口癖検出
        report.append("[3/3] 口癖・習癖検出\n")
        detector = HabitDetector(texts)
        habits = detector.detect_habits(limit=5)
        if len(habits) > 0:
            report.append(f"検出数: {len(habits)}個\n")
            for idx, row in habits.iterrows():
                report.append(f"  {row['phrase']}: {int(row['freq_target'])}回 (Z={row['z_score']:.2f})")
        else:
            report.append("特別な習癖は見つかりません\n")
        report.append("")

        # 4. 関連論文検索
        if past_corpus_dir:
            report.append("[4/4] 過去論文との関連性\n")
            try:
                recommender = SelfRecommender(past_corpus_dir, min_count=1)
                related = recommender.find_related_articles(body_text, top_n=3)
                if len(related) > 0:
                    report.append(f"関連論文: {len(related)}件")
                else:
                    report.append("関連論文が見つかりません")
            except Exception as e:
                report.append(f"関連論文検索エラー: {e}")
        else:
            report.append("[4/4] 過去論文検索 - スキップ\n")

        report.append("\n" + "=" * 70)
        return "\n".join(report)

    def _run_novel_revision(
        self,
        v1: str,
        v2: Optional[str],
        v3: Optional[str],
        v4: Optional[str],
        output_dir: str
    ) -> str:
        """小説推敲ワークフロー"""
        from .writing_assistant import RankingTrajectory, RevisionHeatmap

        report = []
        report.append("=" * 70)
        report.append("小説原稿推敲分析")
        report.append("=" * 70)
        report.append("")

        # バージョンリストを作成
        versions = [v1]
        if v2:
            versions.append(v2)
        if v3:
            versions.append(v3)
        if v4:
            versions.append(v4)

        # 各版をテキストに変換
        version_texts = {}
        for i, version_file in enumerate(versions, 1):
            texts = read_file(version_file, encoding='auto')
            version_texts[f'v{i}'] = '\n'.join(texts)

        # 1. ランキング推移分析
        report.append("[1/2] フレーズランキング推移\n")
        extractor = PhraseExtracter(min_count=2, max_length=20, verbose=0)

        all_phrases = {}
        for version_name, text in version_texts.items():
            texts = text.split('\n')
            phrases_df = extractor.get_dfphrase(texts)
            all_phrases[version_name] = set(phrases_df['seqchar'].tolist())

        if len(versions) >= 2:
            try:
                trajectory = RankingTrajectory(version_texts)
                movers = trajectory.get_biggest_movers()
                report.append(f"検出フレーズ総数: {len(all_phrases.get('v1', []))}個")
                if len(movers) > 0:
                    report.append(f"\n順位が大きく変わったフレーズ:")
                    for phrase, change in movers.items():
                        report.append(f"  {phrase}: {change:+d}位")
            except Exception as e:
                report.append(f"ランキング分析エラー: {e}")
        report.append("")

        # 2. 推敲偏り分析
        report.append("[2/2] 推敲領域の偏り\n")
        try:
            heatmap = RevisionHeatmap(version_texts)
            report.append("推敲が集中している領域:")
        except Exception as e:
            report.append(f"推敲偏り分析エラー: {e}")

        v1_text = version_texts.get('v1', '')
        report.append(f"全体行数: {len(v1_text.split(chr(10)))}行\n")

        report.append("\n" + "=" * 70)
        return "\n".join(report)

    def _run_blog_writing(
        self,
        body_file: str,
        past_corpus_dir: Optional[str],
        output_dir: str
    ) -> str:
        """ブログ記事執筆用ワークフロー"""
        report = []
        report.append("=" * 70)
        report.append("ブログ記事 最適化分析")
        report.append("=" * 70)
        report.append("")

        texts = read_file(body_file, encoding='auto')
        body_text = '\n'.join(texts)

        # 1. フレーズ抽出（短め）
        report.append("[1/2] フレーズ抽出\n")
        extractor = PhraseExtracter(min_count=3, max_length=15, verbose=0)
        phrases_df = extractor.get_dfphrase(texts)
        report.append(f"検出フレーズ数: {len(phrases_df)}個\n")
        if len(phrases_df) > 0:
            report.append("頻出フレーズTop 5:")
            for idx, row in phrases_df.head(5).iterrows():
                report.append(f"  {row['seqchar']}: {int(row['freq'])}回")
        report.append("")

        # 2. 表記ゆれ検出
        report.append("[2/2] 表記ゆれ検出\n")
        try:
            generator = EditorConfigGenerator(phrases_df)
            similar_groups = generator.detect_spelling_variations(0.7)
            if similar_groups:
                report.append(f"表記ゆれグループ: {len(similar_groups)}個\n")
                for group in similar_groups[:5]:
                    report.append(f"  {' / '.join(group)}")
            else:
                report.append("表記ゆれは見つかりませんでした")
        except Exception as e:
            report.append(f"表記ゆれ検出エラー: {e}")
        report.append("")

        # 3. 過去記事検索
        if past_corpus_dir:
            report.append("[3/3] 過去記事との関連性\n")
            try:
                recommender = SelfRecommender(past_corpus_dir, min_count=1)
                related = recommender.find_related_articles(body_text, top_n=3)
                if len(related) > 0:
                    report.append(f"関連記事: {len(related)}件\n")
                    for article in related[:3]:
                        report.append(f"  {article}")
                else:
                    report.append("関連記事が見つかりません")
            except Exception as e:
                report.append(f"関連記事検索エラー: {e}")
        else:
            report.append("[3/3] 過去記事検索 - スキップ\n")

        report.append("\n" + "=" * 70)
        return "\n".join(report)

    def _run_sns_content(self, body_file: str, output_dir: str) -> str:
        """SNS投稿最適化ワークフロー"""
        report = []
        report.append("=" * 70)
        report.append("SNS投稿 最適化分析")
        report.append("=" * 70)
        report.append("")

        texts = read_file(body_file, encoding='auto')

        # フレーズ抽出（短め）
        report.append("[1/1] フレーズ抽出 & 表記ゆれ検出\n")
        extractor = PhraseExtracter(min_count=2, max_length=10, verbose=0)
        phrases_df = extractor.get_dfphrase(texts)
        report.append(f"検出フレーズ数: {len(phrases_df)}個\n")

        if len(phrases_df) > 0:
            # 表記ゆれ検出
            try:
                generator = EditorConfigGenerator(phrases_df)
                similar_groups = generator.detect_spelling_variations(0.65)
                if similar_groups:
                    report.append(f"表記ゆれグループ: {len(similar_groups)}個\n")
                    for group in similar_groups[:10]:
                        report.append(f"  {' ⇄ '.join(group)}")
                else:
                    report.append("表記ゆれは見つかりませんでした")
            except Exception as e:
                report.append(f"検出エラー: {e}")

        report.append("\n" + "=" * 70)
        return "\n".join(report)

    def _run_editing(
        self,
        body_file: str,
        abstract_file: Optional[str],
        past_corpus_dir: Optional[str],
        output_dir: str
    ) -> str:
        """編集者向けチェックワークフロー"""
        report = []
        report.append("=" * 70)
        report.append("編集チェック")
        report.append("=" * 70)
        report.append("")

        texts = read_file(body_file, encoding='auto')
        body_text = '\n'.join(texts)

        # 1. フレーズ抽出
        report.append("[1/3] フレーズ分析\n")
        extractor = PhraseExtracter(min_count=4, max_length=25, verbose=0)
        phrases_df = extractor.get_dfphrase(texts)
        report.append(f"検出フレーズ数: {len(phrases_df)}個\n")

        # 2. 乖離検出
        if abstract_file:
            report.append("[2/3] あらすじ vs 本文の乖離\n")
            abstract_text = '\n'.join(read_file(abstract_file, encoding='auto'))
            checker = AbstractBodyChecker(abstract_text, body_text)

            divergence = checker.get_divergence_score()
            report.append(f"乖離度: {divergence:.1%}")
            if divergence > 0.3:
                report.append(" ⚠️ 高い乖離度 - あらすじと本文に大きな違いがあります")
        else:
            report.append("[2/3] あらすじ確認 - スキップ\n")
        report.append("")

        # 3. 口癖検出
        report.append("[3/3] 個人の口癖\n")
        detector = HabitDetector(texts)
        habits = detector.detect_habits(limit=5)
        if len(habits) > 0:
            report.append(f"検出数: {len(habits)}個\n")
            for idx, row in habits.iterrows():
                report.append(f"  {row['phrase']}: {int(row['freq_target'])}回")
        else:
            report.append("特別な習癖は見つかりません")
        report.append("")

        # 4. 過去記事検索
        if past_corpus_dir:
            report.append("[4/4] 過去記事との関連性\n")
            try:
                recommender = SelfRecommender(past_corpus_dir, min_count=1)
                related = recommender.find_related_articles(body_text, top_n=3)
                if len(related) > 0:
                    report.append(f"関連記事: {len(related)}件")
                else:
                    report.append("関連記事が見つかりません")
            except Exception as e:
                report.append(f"関連記事検索エラー: {e}")
        else:
            report.append("[4/4] 過去記事検索 - スキップ\n")

        report.append("\n" + "=" * 70)
        return "\n".join(report)

    def generate_editor_config(
        self,
        texts: List[str],
        output_format: str = 'vscode',
        output_file: Optional[str] = None
    ) -> Dict[str, Any]:
        """
        エディタ設定を生成（SNS/ブログ向け）

        Args:
            texts: テキストファイルのリスト
            output_format: 出力形式 (vscode, sublime, etc.)
            output_file: 出力ファイルパス

        Returns:
            エディタ設定
        """
        all_phrases = []
        for text_file in texts:
            file_texts = read_file(text_file, encoding='auto')
            extractor = PhraseExtracter(min_count=2, verbose=0)
            phrases_df = extractor.get_dfphrase(file_texts)
            all_phrases.append(phrases_df)

        import pandas as pd
        combined_df = pd.concat(all_phrases, ignore_index=True)

        generator = EditorConfigGenerator(combined_df)
        config = generator.generate_vscode_config()

        if output_file:
            with open(output_file, 'w', encoding='utf-8') as f:
                json.dump(config, f, indent=2, ensure_ascii=False)

        return config
