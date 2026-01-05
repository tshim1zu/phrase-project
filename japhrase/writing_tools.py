"""
執筆補助ツール - エディタ連携とセルフ・リコメンデーション機能

エディタ用の設定自動生成やコーパス内検索などの、より高度な執筆支援機能を提供します。
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023"

import json
import pandas as pd
from typing import List, Dict, Optional, Union
from pathlib import Path
import logging

from .extracter import PhraseExtracter
from .similarity import SimilarityAnalyzer

logger = logging.getLogger(__name__)


class EditorConfigGenerator:
    """
    エディタ用「表記ゆれ修正辞書」の自動生成

    SimilarityAnalyzerで検出した類似フレーズペアを、
    VS Code などのエディタ設定に変換します。

    使用例:
        >>> extractor = PhraseExtracter()
        >>> phrases_df = extractor.get_dfphrase(texts)
        >>> generator = EditorConfigGenerator(phrases_df)
        >>> generator.export_vscode_config("settings.json")
    """

    def __init__(self, phrases_df: pd.DataFrame, similarity_threshold: float = 0.75):
        """
        Parameters:
            phrases_df: PhraseExtracterの出力（seqcharとfreqを含むDataFrame）
            similarity_threshold: 類似度の閾値（高いほど厳密）
        """
        self.phrases_df = phrases_df
        self.similarity_threshold = similarity_threshold
        self.analyzer = SimilarityAnalyzer(method='levenshtein')

        self._detect_spelling_variations()

    def _detect_spelling_variations(self):
        """類似フレーズペアを検出"""
        if len(self.phrases_df) == 0:
            self.variations = []
            return

        phrase_list = self.phrases_df['seqchar'].tolist()
        self.variations = []

        # 全ペアについて類似度を計算
        for i, phrase1 in enumerate(phrase_list):
            for j in range(i + 1, len(phrase_list)):
                phrase2 = phrase_list[j]

                similarity = self.analyzer.similarity_levenshtein(phrase1, phrase2)

                # 類似度が高く、完全一致でないペアを記録
                if (self.similarity_threshold <= similarity < 1.0 and
                    phrase1 != phrase2):
                    # 優先度：頻度の高い方を標準形とする
                    freq1 = self.phrases_df[
                        self.phrases_df['seqchar'] == phrase1
                    ]['freq'].values[0]
                    freq2 = self.phrases_df[
                        self.phrases_df['seqchar'] == phrase2
                    ]['freq'].values[0]

                    if freq1 >= freq2:
                        standard = phrase1
                        variant = phrase2
                    else:
                        standard = phrase2
                        variant = phrase1

                    self.variations.append({
                        'standard': standard,
                        'variant': variant,
                        'similarity': float(similarity)
                    })

    def get_variations_df(self) -> pd.DataFrame:
        """
        検出された表記ゆれを DataFrame で取得

        Returns:
            pd.DataFrame: 標準形と異形の対応表
        """
        if not self.variations:
            return pd.DataFrame(columns=['standard', 'variant', 'similarity'])

        return pd.DataFrame(self.variations).drop_duplicates().sort_values(
            'similarity', ascending=False
        )

    def export_vscode_config(self, filepath: str, config_key: str = "cSpell.words"):
        """
        VS Code の settings.json 用に設定をエクスポート

        Parameters:
            filepath: 出力先ファイルパス
            config_key: 設定キー（デフォルト: cSpell.words）
        """
        if not self.variations:
            logger.warning("表記ゆれが検出されませんでした")
            return

        variations_df = self.get_variations_df()

        # VS Code 設定として出力
        vscode_config = {
            config_key: list(variations_df['standard'].unique()),
            "cSpell.ignoreWords": list(variations_df['variant'].unique())
        }

        with open(filepath, 'w', encoding='utf-8') as f:
            json.dump(vscode_config, f, indent=2, ensure_ascii=False)

        logger.info(f"VS Code設定を保存しました: {filepath}")

    def export_replacement_rules(self, filepath: str):
        """
        置換ルール（JSON形式）をエクスポート

        Parameters:
            filepath: 出力先ファイルパス
        """
        if not self.variations:
            logger.warning("表記ゆれが検出されませんでした")
            return

        variations_df = self.get_variations_df()

        # 置換ルール: 異形 -> 標準形
        replacement_rules = {
            row['variant']: row['standard']
            for _, row in variations_df.iterrows()
        }

        with open(filepath, 'w', encoding='utf-8') as f:
            json.dump(replacement_rules, f, indent=2, ensure_ascii=False)

        logger.info(f"置換ルールを保存しました: {filepath}")

    def generate_report(self) -> str:
        """
        表記ゆれ検出レポートを生成

        Returns:
            str: レポートテキスト
        """
        variations_df = self.get_variations_df()

        report = []
        report.append("=" * 70)
        report.append("表記ゆれ修正辞書レポート")
        report.append("=" * 70)
        report.append("")

        if len(variations_df) == 0:
            report.append("表記ゆれは検出されませんでした。")
        else:
            report.append(f"検出された表記ゆれ: {len(variations_df)}件")
            report.append("")
            report.append(variations_df.to_string(index=False))

        report.append("")

        return "\n".join(report)


class SelfRecommender:
    """
    過去原稿からの「セルフ・リコメンデーション」

    現在の原稿から抽出したキーワードを使って、過去の原稿フォルダを検索し、
    重複や再利用可能なコンテンツを発見します。

    使用例:
        >>> recommender = SelfRecommender("past_articles_dir")
        >>> matches = recommender.find_related_articles(current_text)
    """

    def __init__(self, corpus_dir: Union[str, Path], min_count: int = 2):
        """
        Parameters:
            corpus_dir: 過去原稿ディレクトリ
            min_count: フレーズの最小出現回数
        """
        self.corpus_dir = Path(corpus_dir)
        self.extractor = PhraseExtracter(min_count=min_count, verbose=0)
        self.analyzer = SimilarityAnalyzer(method='jaccard')

        self._build_corpus_index()

    def _build_corpus_index(self):
        """過去原稿ファイルのインデックスを構築"""
        self.corpus_texts = {}
        self.corpus_phrases = {}

        # サポートされているファイル拡張子
        supported_ext = ['.txt', '.md']

        for filepath in self.corpus_dir.glob('**/*'):
            if filepath.is_file() and filepath.suffix in supported_ext:
                try:
                    with open(filepath, 'r', encoding='utf-8') as f:
                        text = f.read()
                        self.corpus_texts[str(filepath)] = text

                        # フレーズを抽出
                        phrases = self.extractor.get_dfphrase([text])
                        self.corpus_phrases[str(filepath)] = phrases

                except Exception as e:
                    logger.warning(f"ファイル読み込み失敗: {filepath} - {e}")

        logger.info(f"コーパスインデックス完成: {len(self.corpus_texts)}個のファイルを登録")

    def find_related_articles(
        self,
        current_text: Union[str, List[str]],
        top_n: int = 5,
        similarity_threshold: float = 0.3
    ) -> pd.DataFrame:
        """
        現在の原稿と関連度が高い過去の記事を検索

        Parameters:
            current_text: 現在の原稿（文字列またはリスト）
            top_n: 返す結果の上限
            similarity_threshold: 類似度の最小閾値

        Returns:
            pd.DataFrame: マッチした記事の詳細
        """
        # 現在の原稿をテキストに統一
        if isinstance(current_text, list):
            current_text_str = '\n'.join(current_text)
        else:
            current_text_str = current_text

        matches = []

        for filepath, corpus_text in self.corpus_texts.items():
            similarity = self.analyzer.calculate_similarity(
                current_text_str, corpus_text, method='jaccard'
            )

            if similarity >= similarity_threshold:
                matches.append({
                    'filepath': filepath,
                    'similarity': float(similarity),
                    'file_size': len(corpus_text)
                })

        if not matches:
            return pd.DataFrame(columns=['filepath', 'similarity', 'file_size'])

        result = pd.DataFrame(matches).sort_values('similarity', ascending=False)
        return result.head(top_n)

    def find_overlapping_phrases(
        self,
        current_text: Union[str, List[str]],
        top_n: int = 10
    ) -> Dict[str, pd.DataFrame]:
        """
        現在の原稿と過去の原稿で共通するキーフレーズを検出

        Parameters:
            current_text: 現在の原稿
            top_n: 返すフレーズの上限

        Returns:
            Dict: ファイルパス -> 共通フレーズの対応表
        """
        # 現在のテキストからフレーズを抽出
        if isinstance(current_text, list):
            text_list = current_text
        else:
            text_list = current_text.split('\n')

        current_phrases = self.extractor.get_dfphrase(text_list)

        if len(current_phrases) == 0:
            return {}

        current_phrase_set = set(current_phrases['seqchar'].values)

        overlaps = {}

        for filepath, corpus_phrases in self.corpus_phrases.items():
            if len(corpus_phrases) == 0:
                continue

            corpus_phrase_set = set(corpus_phrases['seqchar'].values)

            # 共通フレーズを検出
            common = current_phrase_set & corpus_phrase_set

            if common:
                # 共通フレーズの詳細を取得
                common_details = []
                for phrase in common:
                    curr_freq = float(
                        current_phrases[current_phrases['seqchar'] == phrase]['freq'].values[0]
                    )
                    corp_freq = float(
                        corpus_phrases[corpus_phrases['seqchar'] == phrase]['freq'].values[0]
                    )

                    common_details.append({
                        'phrase': phrase,
                        'freq_current': curr_freq,
                        'freq_corpus': corp_freq,
                        'overlap_score': min(curr_freq, corp_freq)
                    })

                overlaps[filepath] = pd.DataFrame(common_details).sort_values(
                    'overlap_score', ascending=False
                ).head(top_n)

        return overlaps

    def generate_recommendation_report(
        self,
        current_text: Union[str, List[str]],
        top_n: int = 5
    ) -> str:
        """
        リコメンデーションレポートを生成

        Parameters:
            current_text: 現在の原稿
            top_n: レポートに含める上限

        Returns:
            str: レポートテキスト
        """
        report = []
        report.append("=" * 70)
        report.append("セルフ・リコメンデーション レポート")
        report.append("=" * 70)
        report.append("")

        # 関連記事を検索
        related = self.find_related_articles(current_text, top_n=top_n)

        if len(related) > 0:
            report.append(f"関連記事（上位{min(top_n, len(related))}件）:")
            report.append(related.to_string(index=False))
            report.append("")
        else:
            report.append("関連記事が見つかりませんでした。")
            report.append("")

        # 共通フレーズを検出
        overlaps = self.find_overlapping_phrases(current_text, top_n=top_n)

        if overlaps:
            report.append(f"共通フレーズを含むファイル（上位{len(overlaps)}件）:")
            report.append("")

            for filepath, phrases_df in overlaps.items():
                report.append(f"📄 {Path(filepath).name}")
                report.append(f"  共通フレーズ: {len(phrases_df)}個")
                if len(phrases_df) > 0:
                    report.append(f"  {phrases_df.head(3).to_string(index=False)}")
                report.append("")
        else:
            report.append("共通フレーズが見つかりませんでした。")
            report.append("")

        return "\n".join(report)
