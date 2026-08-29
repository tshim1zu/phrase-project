"""
執筆支援ツール - 原稿の品質管理と分析機能

あらすじと本文の乖離検出、フレーズの逆引き検索、推敲の偏り可視化など、
執筆プロセスを支援する各種機能を提供します。
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023"

import numpy as np
import pandas as pd
from typing import List, Dict, Tuple, Optional, Union
from collections import Counter
import logging
from pathlib import Path

from .extracter import PhraseExtracter
from .similarity import SimilarityAnalyzer

logger = logging.getLogger(__name__)


class KWICAnalyzer:
    """
    逆引き検索（KWIC: Keyword In Context）機能

    抽出されたフレーズが原稿のどこにあるかを検索し、
    行番号と前後の文脈を表示します。

    使用例:
        >>> kwic = KWICAnalyzer(sentences)
        >>> results = kwic.find_phrase('表記ゆれ')
        >>> print(results)
    """

    def __init__(self, sentences: List[str], context_lines: int = 1):
        """
        Parameters:
            sentences (List[str]): 対象となる文章のリスト
            context_lines (int): 前後の文脈行数（デフォルト: 1行）
        """
        self.sentences = sentences
        self.context_lines = context_lines
        self._build_index()

    def _build_index(self):
        """文章の行インデックスを構築"""
        self.line_map = {}  # phrase -> list of (line_num, char_pos)

        for line_num, sentence in enumerate(self.sentences):
            # 同じフレーズが複数出現する場合に対応
            for phrase_len in range(1, len(sentence) + 1):
                for char_pos in range(len(sentence) - phrase_len + 1):
                    phrase = sentence[char_pos:char_pos + phrase_len]
                    if phrase not in self.line_map:
                        self.line_map[phrase] = []
                    self.line_map[phrase].append((line_num, char_pos))

    def find_phrase(self, phrase: str) -> pd.DataFrame:
        """
        フレーズが出現する箇所をすべて検索

        Parameters:
            phrase (str): 検索対象フレーズ

        Returns:
            pd.DataFrame: 出現箇所の詳細情報
        """
        if phrase not in self.line_map:
            return pd.DataFrame(columns=[
                'line_num', 'char_pos', 'context',
                'occurrence_num', 'total_occurrences'
            ])

        occurrences = self.line_map[phrase]
        total_count = len(occurrences)

        results = []
        for occurrence_num, (line_num, char_pos) in enumerate(occurrences, 1):
            context = self._get_context(line_num, char_pos, len(phrase))

            results.append({
                'line_num': line_num + 1,  # 1-indexed
                'char_pos': char_pos,
                'context': context,
                'occurrence_num': occurrence_num,
                'total_occurrences': total_count
            })

        return pd.DataFrame(results)

    def _get_context(self, line_num: int, char_pos: int, phrase_len: int) -> str:
        """フレーズの前後の文脈を取得"""
        context_lines = []

        # 前の行を追加
        for i in range(max(0, line_num - self.context_lines), line_num):
            context_lines.append(f"  {self.sentences[i]}")

        # 対象行を追加（フレーズをハイライト）
        line = self.sentences[line_num]
        highlighted = (
            line[:char_pos] +
            f"【{line[char_pos:char_pos + phrase_len]}】" +
            line[char_pos + phrase_len:]
        )
        context_lines.append(f"> {highlighted}")

        # 後の行を追加
        for i in range(line_num + 1, min(len(self.sentences), line_num + self.context_lines + 1)):
            context_lines.append(f"  {self.sentences[i]}")

        return "\n".join(context_lines)

    def find_multiple_phrases(self, phrases: List[str]) -> Dict[str, pd.DataFrame]:
        """
        複数のフレーズをまとめて検索

        Parameters:
            phrases (List[str]): 検索対象フレーズのリスト

        Returns:
            Dict[str, pd.DataFrame]: フレーズ -> 出現結果のマッピング
        """
        return {phrase: self.find_phrase(phrase) for phrase in phrases}

    def export_kwic_results(self, phrase: str, filepath: str):
        """
        KWIC検索結果をファイルに出力

        Parameters:
            phrase (str): 検索対象フレーズ
            filepath (str): 出力先ファイルパス
        """
        results = self.find_phrase(phrase)

        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(f"フレーズ「{phrase}」の検索結果\n")
            f.write(f"{'=' * 60}\n\n")

            for idx, row in results.iterrows():
                f.write(f"【出現 {row['occurrence_num']}/{row['total_occurrences']}】\n")
                f.write(f"行番号: {row['line_num']}, 文字位置: {row['char_pos']}\n")
                f.write(f"{row['context']}\n")
                f.write(f"\n{'-' * 60}\n\n")

        logger.info(f"KWIC結果を保存しました: {filepath}")


class AbstractBodyChecker:
    """
    あらすじと本文の乖離チェッカー

    あらすじと本文のフレーズを比較し、内容のズレを検出します。
    - あらすじにあるのに本文に出てこない重要語
    - 本文で頻出するのにあらすじにない言葉
    などを警告します。

    使用例:
        >>> checker = AbstractBodyChecker(abstract_text, body_text)
        >>> missing = checker.get_missing_phrases()
        >>> added = checker.get_added_phrases()
    """

    def __init__(
        self,
        abstract_text: Union[str, List[str]],
        body_text: Union[str, List[str]],
        min_count_abstract: int = 2,
        min_count_body: int = 3
    ):
        """
        Parameters:
            abstract_text: あらすじのテキスト（文字列またはリスト）
            body_text: 本文のテキスト（文字列またはリスト）
            min_count_abstract: あらすじ内での最小出現回数
            min_count_body: 本文内での最小出現回数
        """
        self.abstract_extractor = PhraseExtracter(
            min_count=min_count_abstract,
            min_length=3,
            max_length=16,
            verbose=0
        )
        self.body_extractor = PhraseExtracter(
            min_count=min_count_body,
            min_length=3,
            max_length=16,
            verbose=0
        )

        # テキストの正規化
        self.abstract_sentences = self._normalize_text(abstract_text)
        self.body_sentences = self._normalize_text(body_text)

        # フレーズを抽出
        self.abstract_phrases = self.abstract_extractor.get_dfphrase(self.abstract_sentences)
        self.body_phrases = self.body_extractor.get_dfphrase(self.body_sentences)

    def _normalize_text(self, text: Union[str, List[str]]) -> List[str]:
        """テキストをリストに正規化"""
        if isinstance(text, str):
            # 句点で分割
            for delim in ['。', '．', '\n']:
                text = text.replace(delim, '\n')
            return [s.strip() for s in text.split('\n') if s.strip()]
        return text

    def get_missing_phrases(self) -> pd.DataFrame:
        """
        あらすじにあるのに本文に出てこないフレーズを取得

        Returns:
            pd.DataFrame: 欠落フレーズの詳細
        """
        if len(self.abstract_phrases) == 0:
            return pd.DataFrame(columns=['phrase', 'freq_abstract', 'status'])

        abstract_phrase_set = set(self.abstract_phrases['seqchar'].values)
        body_phrase_set = set(self.body_phrases['seqchar'].values)

        missing = abstract_phrase_set - body_phrase_set

        if not missing:
            return pd.DataFrame(columns=['phrase', 'freq_abstract', 'status'])

        results = []
        for phrase in missing:
            abstract_freq = float(
                self.abstract_phrases[self.abstract_phrases['seqchar'] == phrase]['freq'].values[0]
            )
            results.append({
                'phrase': phrase,
                'freq_abstract': abstract_freq,
                'status': '⚠️ あらすじにあるが本文に未出現'
            })

        return pd.DataFrame(results).sort_values('freq_abstract', ascending=False)

    def get_added_phrases(self) -> pd.DataFrame:
        """
        本文で頻出するのにあらすじにない新規フレーズを取得

        Returns:
            pd.DataFrame: 新規フレーズの詳細
        """
        if len(self.body_phrases) == 0:
            return pd.DataFrame(columns=['phrase', 'freq_body', 'status'])

        abstract_phrase_set = (
            set(self.abstract_phrases['seqchar'].values)
            if 'seqchar' in self.abstract_phrases.columns else set()
        )
        body_phrase_set = (
            set(self.body_phrases['seqchar'].values)
            if 'seqchar' in self.body_phrases.columns else set()
        )

        added = body_phrase_set - abstract_phrase_set

        if not added:
            return pd.DataFrame(columns=['phrase', 'freq_body', 'status'])

        results = []
        for phrase in added:
            body_freq = float(
                self.body_phrases[self.body_phrases['seqchar'] == phrase]['freq'].values[0]
            )
            results.append({
                'phrase': phrase,
                'freq_body': body_freq,
                'status': '⚠️ 本文で新規出現'
            })

        return pd.DataFrame(results).sort_values('freq_body', ascending=False)

    def get_divergence_score(self) -> float:
        """
        あらすじと本文の乖離スコアを計算（0-1、1に近いほど乖離が大きい）

        Returns:
            float: 乖離スコア
        """
        analyzer = SimilarityAnalyzer(method='jaccard')

        abstract_text = ' '.join(self.abstract_sentences)
        body_text = ' '.join(self.body_sentences)

        similarity = analyzer.similarity_jaccard(abstract_text, body_text)
        return 1.0 - similarity

    def generate_report(self) -> str:
        """
        乖離チェックの総合レポートを生成

        Returns:
            str: レポートテキスト
        """
        report = []
        report.append("=" * 70)
        report.append("あらすじ vs 本文 乖離チェックレポート")
        report.append("=" * 70)
        report.append("")

        # 乖離スコア
        divergence = self.get_divergence_score()
        report.append(f"乖離スコア: {divergence:.2%}")
        report.append(f"判定: {'大きく乖離' if divergence > 0.3 else '概ね一致' if divergence < 0.1 else '中程度の乖離'}")
        report.append("")

        # 欠落フレーズ
        missing = self.get_missing_phrases()
        report.append(f"⚠️ あらすじにあるが本文に未出現: {len(missing)}件")
        if len(missing) > 0:
            report.append(missing.head(5).to_string(index=False))
        report.append("")

        # 新規フレーズ
        added = self.get_added_phrases()
        report.append(f"⚠️ 本文で新規出現: {len(added)}件")
        if len(added) > 0:
            report.append(added.head(5).to_string(index=False))
        report.append("")

        return "\n".join(report)

    def export_report(self, filepath: str):
        """
        レポートをファイルに保存

        Parameters:
            filepath (str): 出力先ファイルパス
        """
        with open(filepath, 'w', encoding='utf-8') as f:
            f.write(self.generate_report())

        logger.info(f"乖離チェックレポートを保存しました: {filepath}")


class HabitDetector:
    """
    個人の口癖検出器

    作家やライターが無意識に多用している「手癖のようなフレーズ」を検出します。
    参照コーパス（一般的なテキスト）と比較して、当該文書で異常に頻度が高い
    非名詞フレーズを特定します。

    使用例:
        >>> detector = HabitDetector(target_text, reference_texts)
        >>> habits = detector.detect_habits()
    """

    def __init__(
        self,
        target_text: Union[str, List[str]],
        reference_texts: Optional[List[str]] = None,
        z_score_threshold: float = 2.0
    ):
        """
        Parameters:
            target_text: 対象テキスト
            reference_texts: 参照コーパス（Noneの場合はデフォルト使用）
            z_score_threshold: 異常度の閾値（高いほどレアな習癖を検出）
        """
        self.target_extractor = PhraseExtracter(
            min_count=2,
            min_length=2,
            max_length=12,
            verbose=0
        )
        self.reference_extractor = PhraseExtracter(
            min_count=2,
            min_length=2,
            max_length=12,
            verbose=0
        )

        self.target_sentences = self._normalize_text(target_text)
        self.reference_texts = reference_texts or self._get_default_reference()

        # フレーズを抽出（空の場合はデフォルトを返す）
        try:
            self.target_phrases = self.target_extractor.get_dfphrase(self.target_sentences)
        except ValueError:
            self.target_phrases = pd.DataFrame(columns=['seqchar', 'freq'])

        reference_sentences = []
        for ref in self.reference_texts:
            reference_sentences.extend(self._normalize_text(ref))

        try:
            self.reference_phrases = self.reference_extractor.get_dfphrase(reference_sentences)
        except ValueError:
            self.reference_phrases = pd.DataFrame(columns=['seqchar', 'freq'])

    def _normalize_text(self, text: Union[str, List[str]]) -> List[str]:
        """テキストをリストに正規化"""
        if isinstance(text, str):
            for delim in ['。', '．', '\n']:
                text = text.replace(delim, '\n')
            return [s.strip() for s in text.split('\n') if s.strip()]
        return text

    def _get_default_reference(self) -> List[str]:
        """デフォルト参照コーパスを取得"""
        return [
            "基本的に良い点である。",
            "概して良好である。",
            "一般的にはそうである。",
            "例えば良い例がある。",
            "つまりは重要である。",
            "その結果として得られた。",
            "以上の通り述べた。",
        ]

    def detect_habits(self, limit: int = 10) -> pd.DataFrame:
        """
        頻度異常なフレーズを検出

        Parameters:
            limit (int): 結果の件数上限

        Returns:
            pd.DataFrame: 検出された習癖フレーズ
        """
        if len(self.target_phrases) == 0:
            return pd.DataFrame(columns=['phrase', 'freq_target', 'freq_reference', 'z_score', 'habit_score'])

        target_freq = self.target_phrases.set_index('seqchar')['freq'].to_dict()
        reference_freq = self.reference_phrases.set_index('seqchar')['freq'].to_dict()

        # 参照コーパスの統計量を計算
        all_freqs = list(reference_freq.values())
        ref_mean = np.mean(all_freqs)
        ref_std = np.std(all_freqs) if np.std(all_freqs) > 0 else 1.0

        habits = []
        for phrase, freq in target_freq.items():
            ref_freq = reference_freq.get(phrase, 0)

            # Z-scoreを計算
            z_score = (freq - ref_mean) / ref_std

            # 習癖スコア（ターゲット頻度 / 参照頻度の比）
            habit_score = freq / (ref_freq + 1)  # ゼロ除算を避ける

            if z_score > 2.0:  # 統計的に有意
                habits.append({
                    'phrase': phrase,
                    'freq_target': float(freq),
                    'freq_reference': float(ref_freq),
                    'z_score': float(z_score),
                    'habit_score': float(habit_score)
                })

        if not habits:
            return pd.DataFrame(columns=['phrase', 'freq_target', 'freq_reference', 'z_score', 'habit_score'])

        result = pd.DataFrame(habits).sort_values('habit_score', ascending=False)
        return result.head(limit)


class RevisionHeatmap:
    """
    推敲の偏りヒートマップ

    原稿の複数バージョン間で、セクションごとのフレーズ分布の変化を分析し、
    「推敲が集中している箇所」を可視化します。

    使用例:
        >>> heatmap = RevisionHeatmap([v1, v2, v3])
        >>> changes = heatmap.analyze_section_changes()
    """

    def __init__(
        self,
        versions: List[List[str]],
        version_labels: Optional[List[str]] = None,
        section_size: int = 1000
    ):
        """
        Parameters:
            versions: 複数バージョンの文章（各バージョンは文のリスト）
            version_labels: バージョンラベル
            section_size: 1セクションの文字数
        """
        self.versions = versions
        self.section_size = section_size
        self.num_versions = len(versions)

        if version_labels is None:
            version_labels = [f"v{i+1}" for i in range(self.num_versions)]
        self.version_labels = version_labels

        self.extractor = PhraseExtracter(min_count=1, min_length=2, verbose=0)
        self._analyze_versions()

    def _normalize_text(self, sentences: List[str]) -> str:
        """センテンスを統合してテキストにする"""
        return '\n'.join(sentences)

    def _analyze_versions(self):
        """各バージョンのセクション分析を実行"""
        self.section_analyses = []

        for version in self.versions:
            text = self._normalize_text(version)
            sections = self._split_into_sections(text)

            section_analysis = []
            for sec_idx, section in enumerate(sections):
                phrases = self.extractor.get_dfphrase([section])
                section_analysis.append({
                    'section': sec_idx,
                    'char_count': len(section),
                    'phrase_count': len(phrases),
                    'avg_freq': phrases['freq'].mean() if len(phrases) > 0 else 0
                })

            self.section_analyses.append(section_analysis)

    def _split_into_sections(self, text: str) -> List[str]:
        """テキストをセクションに分割"""
        sections = []
        current_section = ""

        for char in text:
            current_section += char
            if len(current_section) >= self.section_size:
                sections.append(current_section)
                current_section = ""

        if current_section:
            sections.append(current_section)

        return sections

    def get_section_changes(self) -> pd.DataFrame:
        """
        セクションごとの変化を分析

        Returns:
            pd.DataFrame: セクションの変化情報
        """
        changes = []

        # 全セクション数を取得
        max_sections = max(
            len(analysis) for analysis in self.section_analyses
        )

        for sec_idx in range(max_sections):
            row_data = {'section': sec_idx}

            for v_idx, analysis in enumerate(self.section_analyses):
                if sec_idx < len(analysis):
                    row_data[self.version_labels[v_idx]] = analysis[sec_idx]['avg_freq']
                else:
                    row_data[self.version_labels[v_idx]] = 0

            changes.append(row_data)

        return pd.DataFrame(changes)

    def export_heatmap_data(self, filepath: str):
        """
        ヒートマップ用データをCSVで出力

        Parameters:
            filepath (str): 出力先ファイルパス
        """
        changes = self.get_section_changes()
        changes.to_csv(filepath, index=False, encoding='utf-8-sig')
        logger.info(f"ヒートマップデータを保存しました: {filepath}")


class RankingTrajectory:
    """
    フレーズランキングの推移グラフ

    複数バージョン間でのキーワードランキングの変化を追跡し、
    どのフレーズが重要度を増したり減したりしたかを可視化します。

    使用例:
        >>> traj = RankingTrajectory([v1, v2, v3])
        >>> history = traj.get_ranking_history()
    """

    def __init__(
        self,
        versions: List[List[str]],
        version_labels: Optional[List[str]] = None,
        top_n: int = 10
    ):
        """
        Parameters:
            versions: 複数バージョンの文章
            version_labels: バージョンラベル
            top_n: 追跡するトップNフレーズ
        """
        self.versions = versions
        self.top_n = top_n
        self.num_versions = len(versions)

        if version_labels is None:
            version_labels = [f"v{i+1}" for i in range(self.num_versions)]
        self.version_labels = version_labels

        self.extractor = PhraseExtracter(
            min_count=1,
            min_length=2,
            max_length=16,
            verbose=0
        )
        self._extract_rankings()

    def _extract_rankings(self):
        """各バージョンのトップフレーズランキングを抽出"""
        self.rankings = []

        for version in self.versions:
            phrases = self.extractor.get_dfphrase(version)

            if len(phrases) > 0:
                # 頻度でソートしてトップNを取得
                top_phrases = phrases.nlargest(self.top_n, 'freq')[['seqchar', 'freq']]

                ranking = {
                    phrase: idx + 1
                    for idx, phrase in enumerate(top_phrases['seqchar'].values)
                }
            else:
                ranking = {}

            self.rankings.append(ranking)

    def get_ranking_history(self) -> pd.DataFrame:
        """
        全フレーズのランキング履歴を取得

        Returns:
            pd.DataFrame: フレーズとバージョンごとのランク
        """
        # 全フレーズを集約
        all_phrases = set()
        for ranking in self.rankings:
            all_phrases.update(ranking.keys())

        history = []
        for phrase in sorted(all_phrases):
            row = {'phrase': phrase}
            for v_idx, ranking in enumerate(self.rankings):
                rank = ranking.get(phrase, None)
                row[self.version_labels[v_idx]] = rank
            history.append(row)

        return pd.DataFrame(history)

    def get_biggest_movers(self) -> pd.DataFrame:
        """
        ランク変化が最も大きいフレーズを取得

        Returns:
            pd.DataFrame: フレーズとランク変化
        """
        history = self.get_ranking_history()

        # 最初と最後のランクで差分を計算
        first_col = self.version_labels[0]
        last_col = self.version_labels[-1]

        history['first_rank'] = history[first_col]
        history['last_rank'] = history[last_col]
        history['rank_change'] = history['first_rank'] - history['last_rank']

        # 上昇と下降を分離
        risers = history[history['rank_change'] > 0].nlargest(5, 'rank_change')
        fallers = history[history['rank_change'] < 0].nsmallest(5, 'rank_change')

        result = pd.concat([risers, fallers])
        return result[['phrase', 'first_rank', 'last_rank', 'rank_change']]

    def export_ranking_data(self, filepath: str):
        """
        ランキング推移データをCSVで出力

        Parameters:
            filepath (str): 出力先ファイルパス
        """
        history = self.get_ranking_history()
        history.to_csv(filepath, index=False, encoding='utf-8-sig')
        logger.info(f"ランキング推移データを保存しました: {filepath}")
