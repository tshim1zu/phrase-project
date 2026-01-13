# coding: utf-8
"""
テキストマイニング自動インサイト生成

抽出されたフレーズから自動的に「何が重要か」を発見・提示
"""

import logging
from dataclasses import dataclass
from typing import List, Dict, Optional, Tuple
from collections import Counter
import pandas as pd
from datetime import datetime

logger = logging.getLogger(__name__)


@dataclass
class Insight:
    """インサイト"""
    category: str                    # カテゴリ（topic, trend, anomaly等）
    title: str                       # タイトル
    description: str                 # 説明
    phrases: List[Tuple[str, float]]  # 関連フレーズと信頼度
    confidence: float                # インサイトの確信度（0-1）


class InsightGenerator:
    """自動インサイト生成"""

    def __init__(self):
        self.insights = []

    def generate_insights(
        self,
        df_phrases: pd.DataFrame,
        texts: Optional[List[str]] = None,
        statistical_scores: Optional[pd.DataFrame] = None
    ) -> List[Insight]:
        """
        自動的にインサイトを生成

        Parameters:
            df_phrases: フレーズDataFrame
            texts: 元テキスト
            statistical_scores: 統計スコアDataFrame

        Returns:
            インサイトのリスト
        """
        self.insights = []

        if df_phrases.empty:
            logger.warning("フレーズがありません")
            return self.insights

        phrase_col = 'seqchar' if 'seqchar' in df_phrases.columns else 'phrase'
        freq_col = 'freq' if 'freq' in df_phrases.columns else 'frequency'

        # 1. ドミナントトピックを検出
        dominant_insights = self._detect_dominant_topics(df_phrases, phrase_col, freq_col)
        self.insights.extend(dominant_insights)

        # 2. 新規・浮上テーマを検出
        if texts:
            emerging_insights = self._detect_emerging_themes(df_phrases, texts, phrase_col, freq_col)
            self.insights.extend(emerging_insights)

        # 3. 異常値を検出
        anomaly_insights = self._detect_anomalies(df_phrases, statistical_scores, phrase_col, freq_col)
        self.insights.extend(anomaly_insights)

        # 4. ノイズ比率を評価
        noise_insight = self._analyze_noise_ratio(df_phrases, phrase_col, freq_col)
        if noise_insight:
            self.insights.append(noise_insight)

        # 5. テキスト品質を評価
        quality_insight = self._evaluate_text_quality(df_phrases, phrase_col, freq_col)
        if quality_insight:
            self.insights.append(quality_insight)

        # 6. 推奨アクションを生成
        recommendations = self._generate_recommendations(df_phrases, self.insights, phrase_col)
        self.insights.extend(recommendations)

        logger.info(f"{len(self.insights)} 個のインサイトを生成しました")
        return self.insights

    def _detect_dominant_topics(
        self,
        df_phrases: pd.DataFrame,
        phrase_col: str,
        freq_col: str
    ) -> List[Insight]:
        """ドミナントトピックを検出"""
        insights = []

        # TOP フレーズ
        top_phrases = df_phrases.nlargest(5, freq_col)

        top_phrases_list = [
            (row[phrase_col], row[freq_col] / df_phrases[freq_col].sum())
            for _, row in top_phrases.iterrows()
        ]

        insight = Insight(
            category='dominant_topic',
            title='ドミナントトピック',
            description=f"全フレーズの {top_phrases_list[0][1]*100:.1f}% が '{top_phrases_list[0][0]}' に集中しています。",
            phrases=top_phrases_list,
            confidence=0.95,
        )
        insights.append(insight)

        return insights

    def _detect_emerging_themes(
        self,
        df_phrases: pd.DataFrame,
        texts: List[str],
        phrase_col: str,
        freq_col: str
    ) -> List[Insight]:
        """新規・浮上テーマを検出"""
        insights = []

        if len(texts) < 10:
            return insights

        # 前半と後半のテキストを分割
        mid_point = len(texts) // 2
        first_half = texts[:mid_point]
        second_half = texts[mid_point:]

        # 後半で多く出現するフレーズを抽出
        emerging = []

        for _, row in df_phrases.iterrows():
            phrase = row[phrase_col]
            freq_first = sum(1 for text in first_half if phrase in text)
            freq_second = sum(1 for text in second_half if phrase in text)

            if freq_second > freq_first and freq_second >= 3:
                growth_rate = freq_second / max(freq_first, 1)
                emerging.append((phrase, growth_rate))

        if emerging:
            emerging.sort(key=lambda x: x[1], reverse=True)

            phrases_str = ', '.join([f"'{p}'" for p, _ in emerging[:3]])

            insight = Insight(
                category='emerging_theme',
                title='浮上テーマ',
                description=f"最近になって増加したテーマ: {phrases_str}",
                phrases=emerging[:5],
                confidence=0.80,
            )
            insights.append(insight)

        return insights

    def _detect_anomalies(
        self,
        df_phrases: pd.DataFrame,
        statistical_scores: Optional[pd.DataFrame],
        phrase_col: str,
        freq_col: str
    ) -> List[Insight]:
        """異常値を検出"""
        insights = []

        if statistical_scores is None or statistical_scores.empty:
            return insights

        # 統計的に有意でないが頻出のフレーズ
        freq_col_scores = 'frequency' if 'frequency' in statistical_scores.columns else freq_col

        insignificant_frequent = statistical_scores[
            (statistical_scores['is_significant'] == False) &
            (statistical_scores[freq_col_scores] > statistical_scores[freq_col_scores].median() * 2)
        ]

        if not insignificant_frequent.empty:
            phrases_list = [
                (row['phrase'], row[freq_col_scores] / df_phrases[freq_col].sum())
                for _, row in insignificant_frequent.head(3).iterrows()
            ]

            insight = Insight(
                category='anomaly',
                title='異常フレーズ',
                description=f"頻出だが統計的には有意でないフレーズ: {', '.join([p[0] for p in phrases_list])}",
                phrases=phrases_list,
                confidence=0.70,
            )
            insights.append(insight)

        return insights

    def _analyze_noise_ratio(
        self,
        df_phrases: pd.DataFrame,
        phrase_col: str,
        freq_col: str
    ) -> Optional[Insight]:
        """ノイズ比率を分析"""
        # 1文字フレーズ、記号のみフレーズをノイズと判定
        noise_count = 0
        total_freq = df_phrases[freq_col].sum()

        for _, row in df_phrases.iterrows():
            phrase = row[phrase_col]
            freq = row[freq_col]

            # 1文字
            if len(phrase) == 1:
                noise_count += freq
            # 記号のみ
            elif all(not c.isalnum() for c in phrase):
                noise_count += freq

        noise_ratio = noise_count / total_freq if total_freq > 0 else 0

        if noise_ratio > 0.05:
            confidence = min(0.95, 0.5 + noise_ratio)

            insight = Insight(
                category='noise_analysis',
                title='ノイズ分析',
                description=f"フレーズの {noise_ratio*100:.1f}% がノイズと判定されました。min_count を上げるか、min_length を2以上にすることを推奨します。",
                phrases=[],
                confidence=confidence,
            )
            return insight

        return None

    def _evaluate_text_quality(
        self,
        df_phrases: pd.DataFrame,
        phrase_col: str,
        freq_col: str
    ) -> Optional[Insight]:
        """テキスト品質を評価"""
        # 品質指標: 語彙多様性と頻度分布

        # 語彙多様性（Gini係数で計算）
        freqs = df_phrases[freq_col].tolist()
        total = sum(freqs)

        # Gini係数
        sorted_freqs = sorted(freqs)
        gini = (2 * sum((i + 1) * f for i, f in enumerate(sorted_freqs))) / (len(freqs) * total) - (len(freqs) + 1) / len(freqs)

        if gini < 0.5:
            quality_level = "低"
            description = "テキストが単純で、同じフレーズが何度も出現しています。"
        elif gini < 0.7:
            quality_level = "中"
            description = "テキストは適度な多様性を持っています。"
        else:
            quality_level = "高"
            description = "テキストが多様で、様々なフレーズが出現しています。"

        confidence = 0.75

        insight = Insight(
            category='text_quality',
            title='テキスト品質評価',
            description=f"{quality_level}: {description}",
            phrases=[],
            confidence=confidence,
        )

        return insight

    def _generate_recommendations(
        self,
        df_phrases: pd.DataFrame,
        insights: List[Insight],
        phrase_col: str
    ) -> List[Insight]:
        """推奨アクションを生成"""
        recommendations = []

        # ノイズが多い場合
        noise_insight = next((i for i in insights if i.category == 'noise_analysis'), None)
        if noise_insight:
            insight = Insight(
                category='recommendation',
                title='推奨アクション',
                description="1. min_count を上げてノイズを削減してください\n2. min_length を2以上に設定して1文字フレーズを除外してください",
                phrases=[],
                confidence=0.85,
            )
            recommendations.append(insight)

        return recommendations

    def cluster_related_phrases(
        self,
        df_phrases: pd.DataFrame,
        similarity_threshold: float = 0.7
    ) -> Dict[str, List[str]]:
        """
        関連フレーズをクラスタリング

        Returns:
            {代表フレーズ: [関連フレーズリスト]}
        """
        phrase_col = 'seqchar' if 'seqchar' in df_phrases.columns else 'phrase'

        clusters = {}

        for _, row in df_phrases.iterrows():
            phrase = row[phrase_col]

            # 既に何かのクラスタに属しているか
            assigned = False
            for cluster_key in clusters:
                if self._phrases_similar(phrase, cluster_key, similarity_threshold):
                    clusters[cluster_key].append(phrase)
                    assigned = True
                    break

            if not assigned:
                clusters[phrase] = [phrase]

        return clusters

    @staticmethod
    def _phrases_similar(phrase1: str, phrase2: str, threshold: float) -> bool:
        """フレーズの類似度を判定"""
        from difflib import SequenceMatcher

        ratio = SequenceMatcher(None, phrase1, phrase2).ratio()
        return ratio >= threshold

    def export_insights(self, filepath: str) -> None:
        """インサイトを CSV に出力"""
        if not self.insights:
            logger.warning("エクスポートするインサイトがありません")
            return

        data = []
        for insight in self.insights:
            data.append({
                'category': insight.category,
                'title': insight.title,
                'description': insight.description,
                'confidence': f"{insight.confidence:.1%}",
                'related_phrases': ', '.join([p[0] for p in insight.phrases[:3]])
            })

        df_export = pd.DataFrame(data)
        df_export.to_csv(filepath, index=False, encoding='utf-8-sig')
        logger.info(f"インサイトを {filepath} に出力しました")

    def generate_report(self) -> str:
        """インサイトレポートを生成"""
        if not self.insights:
            return "インサイトがありません"

        report = "【自動インサイトレポート】\n\n"
        report += f"生成日時: {datetime.now().strftime('%Y-%m-%d %H:%M:%S')}\n"
        report += f"インサイト数: {len(self.insights)}\n\n"

        for i, insight in enumerate(self.insights, 1):
            report += f"【{i}】{insight.title}\n"
            report += f"カテゴリ: {insight.category}\n"
            report += f"内容: {insight.description}\n"
            report += f"確信度: {insight.confidence:.0%}\n"

            if insight.phrases:
                report += "関連フレーズ:\n"
                for phrase, score in insight.phrases[:3]:
                    report += f"  • {phrase} ({score:.2f})\n"

            report += "\n"

        return report
