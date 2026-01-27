"""
ComfyUI/Stable Diffusion プロンプト最適化・分析モジュール

japhraseの統計的アプローチを応用し、過去の良プロンプト（コーパス）から
「あなただけの勝ちパターン」を学習して提案・修正する。

概要:
- 括弧のバランスチェック
- 重複・冗長タグの検出
- 重み分布の分析
- 過去のプロンプトから欠落タグの提案（コーパスベース学習）
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023-2026"

import re
from collections import Counter
from typing import List, Dict, Tuple, Optional
import pandas as pd
import numpy as np
import logging

logger = logging.getLogger(__name__)


class PromptOptimizer:
    """
    プロンプトの統計的最適化・推敲支援

    過去の良プロンプト（コーパス）を学習し、
    新しいプロンプトの品質向上を提案します。

    使用例:
        >>> corpus = ["masterpiece, best quality, 1girl, ...", ...]
        >>> optimizer = PromptOptimizer(corpus)
        >>> result = optimizer.analyze("1girl, blue eyes, ...")
        >>> print(result['suggestions'])
    """

    def __init__(self, corpus_prompts: List[str] = None):
        """
        Parameters:
            corpus_prompts (List[str]): 過去の良プロンプトのリスト（学習用コーパス）
        """
        self.corpus_prompts = corpus_prompts or []
        self.corpus_stats = self._analyze_corpus() if self.corpus_prompts else None

    def _analyze_corpus(self) -> Dict:
        """コーパス内のタグ頻度や共起情報を解析"""
        all_tags = []

        for p in self.corpus_prompts:
            # カンマで分割し、重み記号などを除去して正規化
            tags = [self._normalize_tag(t) for t in p.split(',')]
            all_tags.extend([t for t in tags if t])

        freq_dist = Counter(all_tags)
        return {
            'freq': freq_dist,
            'total_tags': len(all_tags),
            'unique_tags': len(freq_dist),
            'avg_tags_per_prompt': len(all_tags) / len(self.corpus_prompts)
            if self.corpus_prompts
            else 0,
        }

    def analyze(self, prompt: str) -> Dict:
        """
        入力プロンプトを分析し、改善案と統計情報を返す

        Parameters:
            prompt (str): 入力プロンプト

        Returns:
            Dict: 分析結果
                {
                    'syntax_issues': List[str],      # 構文エラー
                    'redundancy_issues': List[Dict], # 冗長性
                    'suggestions': List[Dict],       # 改善提案
                    'weight_stats': Dict,            # 重み分布統計
                    'token_count': int,              # トークン数
                    'quality_score': float           # 品質スコア (0-100)
                }
        """
        # 1. 構文チェック
        syntax_issues = self._check_syntax(prompt)

        # 2. タグの分解と正規化
        raw_tags = [t.strip() for t in prompt.split(',') if t.strip()]
        normalized_tags = [self._normalize_tag(t) for t in raw_tags]

        # 3. 重複・冗長性検出
        redundancy_issues = self._check_redundancy(normalized_tags)

        # 4. コーパスに基づく提案
        suggestions = []
        if self.corpus_stats:
            suggestions = self._mining_suggestions(prompt, normalized_tags)

        # 5. 重み分布チェック
        weight_stats = self._analyze_weights(prompt)

        # 6. 品質スコア計算
        quality_score = self._calculate_quality_score(
            syntax_issues, redundancy_issues, weight_stats, normalized_tags
        )

        return {
            'syntax_issues': syntax_issues,
            'redundancy_issues': redundancy_issues,
            'suggestions': suggestions,
            'weight_stats': weight_stats,
            'token_count': len(normalized_tags),
            'quality_score': round(quality_score, 1),
            'analysis_summary': self._generate_summary(
                syntax_issues, redundancy_issues, suggestions, quality_score
            ),
        }

    def _mining_suggestions(
        self, full_prompt: str, current_tags: List[str]
    ) -> List[Dict]:
        """「いつもの勝ちパターン」から欠落しているタグを提案"""
        if not self.corpus_stats:
            return []

        suggestions = []
        candidate_scores = Counter()

        # コーパス統計から、頻出しているが未使用のタグを検出
        freq_threshold = 3  # 最低出現回数

        for tag, freq in self.corpus_stats['freq'].most_common(50):
            # 既にプロンプトに含まれているかチェック
            if any(curr == tag or tag in curr for curr in current_tags):
                continue

            # 頻出しているタグを提案
            if freq >= freq_threshold:
                # スコア: 頻度 + タグの一般性
                score = freq / len(self.corpus_prompts)
                candidate_scores[tag] = score

        # 上位を提案
        for tag, score in candidate_scores.most_common(5):
            suggestions.append(
                {
                    'tag': tag,
                    'score': round(score, 3),
                    'reason': f'コーパスで頻出（出現率: {score:.1%}）',
                }
            )

        return suggestions

    def _check_redundancy(self, tags: List[str]) -> List[Dict]:
        """意味的な重複や包含関係を検出"""
        issues = []
        sorted_tags = sorted(list(set(tags)), key=len)

        for i, t1 in enumerate(sorted_tags):
            for t2 in sorted_tags[i + 1 :]:
                # 包含関係チェック
                if t1 in t2 and t1 != t2:
                    issues.append(
                        {
                            'type': 'inclusion',
                            'tags': (t1, t2),
                            'message': f"冗長: '{t1}' は '{t2}' に含まれています",
                        }
                    )

                # 完全一致チェック
                elif t1 == t2:
                    issues.append(
                        {
                            'type': 'exact_duplicate',
                            'tag': t1,
                            'message': f"重複: '{t1}' が複数回出現しています",
                        }
                    )

        return issues

    def _check_syntax(self, prompt: str) -> List[str]:
        """括弧のバランスなどをチェック"""
        issues = []

        open_count = prompt.count('(')
        close_count = prompt.count(')')
        if open_count != close_count:
            issues.append(f"括弧の不一致: ( が {open_count}個, ) が {close_count}個")

        if ',,' in prompt:
            issues.append("カンマの連続(,,)が含まれています")

        if prompt.startswith(',') or prompt.endswith(','):
            issues.append("先頭または末尾にカンマがあります")

        return issues

    def _analyze_weights(self, prompt: str) -> Dict:
        """重み付け(1.2)などの統計情報を分析"""
        weights = []
        matches = re.findall(r':([0-9.]+)\)', prompt)

        for m in matches:
            try:
                weights.append(float(m))
            except ValueError:
                pass

        if not weights:
            return {'has_weights': False, 'count': 0}

        avg_weight = np.mean(weights)
        std_weight = np.std(weights)
        max_weight = np.max(weights)
        min_weight = np.min(weights)

        warnings = []
        if avg_weight > 1.3:
            warnings.append(
                "重みがインフレ気味（平均 > 1.3）。CFG Scale調整を検討。"
            )
        if max_weight > 2.0:
            warnings.append(f"極端に強い重み({max_weight})。画像崩壊のリスク。")
        if std_weight > 0.5:
            warnings.append("重みのばらつきが大きい。バランスを統一することを推奨。")

        return {
            'has_weights': True,
            'count': len(weights),
            'average': round(avg_weight, 2),
            'std': round(std_weight, 2),
            'min': min_weight,
            'max': max_weight,
            'warnings': warnings,
        }

    def _calculate_quality_score(
        self, syntax_issues, redundancy_issues, weight_stats, tags
    ) -> float:
        """プロンプトの品質スコアを計算 (0-100)"""
        score = 100.0

        # 構文エラーで減点
        score -= len(syntax_issues) * 10

        # 冗長性で減点
        score -= len(redundancy_issues) * 5

        # 重みエラーで減点
        if weight_stats.get('warnings'):
            score -= len(weight_stats['warnings']) * 3

        # トークン数チェック（50～150が理想）
        token_count = len(tags)
        if token_count < 10:
            score -= 5
        elif token_count > 200:
            score -= 10
        elif 50 <= token_count <= 150:
            score += 5  # ボーナス

        return max(0.0, min(100.0, score))

    def _generate_summary(
        self, syntax_issues, redundancy_issues, suggestions, quality_score
    ) -> str:
        """分析結果の簡潔なサマリー生成"""
        summary = []

        # 品質評価
        if quality_score >= 80:
            rating = "優秀"
        elif quality_score >= 60:
            rating = "良好"
        elif quality_score >= 40:
            rating = "改善推奨"
        else:
            rating = "要修正"

        summary.append(f"品質スコア: {quality_score}/100 ({rating})")

        if syntax_issues:
            summary.append(f"構文エラー: {len(syntax_issues)}件")
        if redundancy_issues:
            summary.append(f"冗長性エラー: {len(redundancy_issues)}件")
        if suggestions:
            summary.append(f"改善提案: {len(suggestions)}件")

        return " | ".join(summary)

    def _normalize_tag(self, tag: str) -> str:
        """タグの正規化（重み除去、括弧除去、小文字化）"""
        # (tag:1.2) -> tag
        tag = re.sub(r'\((.*?):[0-9.]+\)', r'\1', tag)
        # (((tag))) -> tag
        tag = re.sub(r'[\(\)]', '', tag)
        return tag.strip().lower()

    def get_report(self, prompt: str) -> str:
        """分析結果の詳細レポートを生成"""
        result = self.analyze(prompt)

        lines = []
        lines.append("=" * 70)
        lines.append("【プロンプト分析レポート】")
        lines.append("=" * 70)

        # サマリー
        lines.append(f"\n{result['analysis_summary']}")

        # 構文エラー
        if result['syntax_issues']:
            lines.append("\n【構文エラー】")
            for issue in result['syntax_issues']:
                lines.append(f"  ⚠ {issue}")

        # 冗長性
        if result['redundancy_issues']:
            lines.append("\n【冗長性エラー】")
            for issue in result['redundancy_issues']:
                lines.append(f"  ⚠ {issue['message']}")

        # 重み統計
        if result['weight_stats'].get('has_weights'):
            lines.append("\n【重み分布統計】")
            ws = result['weight_stats']
            lines.append(
                f"  平均: {ws['average']}, 標準偏差: {ws['std']}, 範囲: [{ws['min']}, {ws['max']}]"
            )
            if ws['warnings']:
                for warn in ws['warnings']:
                    lines.append(f"  ⚠ {warn}")

        # 改善提案
        if result['suggestions']:
            lines.append("\n【改善提案】")
            for i, sug in enumerate(result['suggestions'], 1):
                lines.append(
                    f"  {i}. {sug['tag']} (スコア: {sug['score']}) - {sug['reason']}"
                )

        lines.append("\n" + "=" * 70)
        return "\n".join(lines)


# 便利関数
def optimize_prompt(prompt: str, corpus: List[str] = None) -> Dict:
    """
    単一関数でのプロンプト最適化

    Parameters:
        prompt (str): 入力プロンプト
        corpus (List[str]): 学習用コーパス（オプション）

    Returns:
        Dict: 分析結果
    """
    optimizer = PromptOptimizer(corpus)
    return optimizer.analyze(prompt)
