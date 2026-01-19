"""
主語/視点ブレ検出モジュール

連続文内で主語が頻繁に変わったり、視点（POV）が乱れる箇所を検出します。

使用例:
    >>> from japhrase import SubjectPOVDetector
    >>> detector = SubjectPOVDetector()
    >>> issues = detector.check("私は走った。彼は歩いた。彼女は飛んだ。")
    >>> for issue in issues:
    ...     print(issue['message'])
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2026"

import re
import json
import logging
from typing import List, Dict, Tuple, Optional
from collections import defaultdict

from .utils_robustness import ScoreValidator, ConfigValidator, TextProcessor, ErrorHandler
from .utils_advanced import MetricsCollector, PositionTracker

logger = logging.getLogger(__name__)


class SubjectPOVDetector:
    """主語と視点（POV）のブレを検出するクラス"""

    # 人称代名詞のパターン
    FIRST_PERSON = ['私', '僕', '俺', '我', '当方', '筆者', '著者', '自分']
    SECOND_PERSON = ['あなた', 'あんた', 'お前', '君', '貴方', '貴殿', 'そちら']
    THIRD_PERSON = ['彼', '彼女', '彼ら', '彼女ら', 'その人', 'あの人']

    # 主語を示す助詞
    SUBJECT_PARTICLES = ['は', 'が', 'も']

    def __init__(self, sensitivity: str = 'medium'):
        """
        Args:
            sensitivity: 検出感度（'low', 'medium', 'high'）
        """
        # 感度のバリデーション
        validation = ConfigValidator.validate_sensitivity(sensitivity)
        if not validation.is_valid:
            logger.warning(f"{validation.message}、'medium' を使用")
            sensitivity = 'medium'

        self.sensitivity = sensitivity

        # 感度に応じた閾値設定
        self.thresholds = {
            'low': {'subject_changes': 4, 'pov_window': 5},
            'medium': {'subject_changes': 3, 'pov_window': 4},
            'high': {'subject_changes': 2, 'pov_window': 3}
        }[sensitivity]

    def check(self, text: str) -> List[Dict[str, any]]:
        """
        テキストの主語と視点のブレをチェック

        Args:
            text: チェック対象のテキスト

        Returns:
            問題のリスト
        """
        issues = []

        # 段落単位で分割
        paragraphs = self._split_paragraphs(text)
        position = 0

        for para in paragraphs:
            if not para.strip():
                position += len(para)
                continue

            # 段落内での主語変化をチェック
            issues.extend(self._check_subject_changes(para, position))

            # 視点の一貫性をチェック
            issues.extend(self._check_pov_consistency(para, position))

            position += len(para)

        return sorted(issues, key=lambda x: x['position'][0])

    def _split_paragraphs(self, text: str) -> List[str]:
        """テキストを段落単位に分割"""
        # 空行または連続する改行で分割
        paragraphs = re.split(r'\n\s*\n', text)
        return paragraphs

    def _split_sentences(self, text: str) -> List[str]:
        """テキストを文単位に分割"""
        sentences = re.split(r'([。！？])', text)

        result = []
        for i in range(0, len(sentences) - 1, 2):
            if i + 1 < len(sentences):
                result.append(sentences[i] + sentences[i + 1])
            else:
                result.append(sentences[i])

        if len(sentences) % 2 == 1:
            result.append(sentences[-1])

        return [s for s in result if s.strip()]

    def _extract_subject(self, sentence: str) -> Optional[str]:
        """
        文から主語を抽出

        Args:
            sentence: 文

        Returns:
            主語（見つからない場合はNone）
        """
        # 「〜は」「〜が」「〜も」のパターンで主語を抽出
        for particle in self.SUBJECT_PARTICLES:
            pattern = f'([^\\s。！？、，]{{1,10}}){particle}'
            match = re.search(pattern, sentence)
            if match:
                return match.group(1)

        return None

    def _get_pov(self, text: str) -> Optional[str]:
        """
        テキストから視点（POV）を判定

        Args:
            text: テキスト

        Returns:
            視点（'first', 'second', 'third', None）
        """
        # 一人称の検出
        if any(re.search(f'{word}[はがも]', text) for word in self.FIRST_PERSON):
            return 'first'

        # 二人称の検出
        if any(re.search(f'{word}[はがも]', text) for word in self.SECOND_PERSON):
            return 'second'

        # 三人称の検出
        if any(re.search(f'{word}[はがも]', text) for word in self.THIRD_PERSON):
            return 'third'

        return None

    def _check_subject_changes(self, paragraph: str, offset: int) -> List[Dict[str, any]]:
        """段落内での主語の頻繁な変化をチェック"""
        issues = []
        sentences = self._split_sentences(paragraph)

        if len(sentences) < 3:
            return issues

        subjects = []
        positions_map = []
        current_pos = offset

        for sentence in sentences:
            subject = self._extract_subject(sentence)
            if subject:
                subjects.append(subject)
                positions_map.append((current_pos, current_pos + len(sentence)))
            current_pos += len(sentence)

        # 主語の変化回数をカウント
        if len(subjects) >= 3:
            changes = 0
            for i in range(1, len(subjects)):
                if subjects[i] != subjects[i-1]:
                    changes += 1

            # 閾値を超えた場合
            if changes >= self.thresholds['subject_changes']:
                start = positions_map[0][0]
                end = positions_map[-1][1]

                issues.append({
                    'type': 'subject_changes',
                    'message': f'段落内で主語が{changes}回変化しています（{len(subjects)}文中）',
                    'position': (start, end),
                    'severity': 'warning',
                    'subjects': subjects,
                    'change_count': changes,
                    'suggestion': '文を並べ替えるか、段落を分割して主語を統一してください'
                })

        return issues

    def _check_pov_consistency(self, paragraph: str, offset: int) -> List[Dict[str, any]]:
        """視点の一貫性をチェック"""
        issues = []
        sentences = self._split_sentences(paragraph)

        if len(sentences) < self.thresholds['pov_window']:
            return issues

        # スライディングウィンドウで視点の変化を検出
        for i in range(len(sentences) - self.thresholds['pov_window'] + 1):
            window = sentences[i:i + self.thresholds['pov_window']]
            povs = []

            for sentence in window:
                pov = self._get_pov(sentence)
                if pov:
                    povs.append(pov)

            # ウィンドウ内で複数の視点が混在している場合
            if len(set(povs)) >= 2:
                # 位置を計算
                start = offset + sum(len(s) for s in sentences[:i])
                end = start + sum(len(s) for s in window)

                issues.append({
                    'type': 'pov_inconsistency',
                    'message': f'視点が混在しています: {", ".join(set(povs))}',
                    'position': (start, end),
                    'severity': 'warning',
                    'povs': povs,
                    'suggestion': '一貫した視点で記述してください（一人称か三人称のどちらかに統一）'
                })

        return issues

    def analyze_text(self, text: str) -> Dict[str, any]:
        """
        テキスト全体の主語と視点の統計を分析

        Args:
            text: 分析対象のテキスト

        Returns:
            統計情報の辞書
        """
        sentences = self._split_sentences(text)
        subjects = []
        povs = []

        for sentence in sentences:
            subject = self._extract_subject(sentence)
            if subject:
                subjects.append(subject)

            pov = self._get_pov(sentence)
            if pov:
                povs.append(pov)

        # 統計を計算
        subject_freq = defaultdict(int)
        for subj in subjects:
            subject_freq[subj] += 1

        pov_freq = defaultdict(int)
        for pov in povs:
            pov_freq[pov] += 1

        return {
            'total_sentences': len(sentences),
            'subject_count': len(subjects),
            'subject_variety': len(set(subjects)),
            'most_common_subjects': sorted(subject_freq.items(), key=lambda x: x[1], reverse=True)[:5],
            'pov_distribution': dict(pov_freq),
            'dominant_pov': max(pov_freq.items(), key=lambda x: x[1])[0] if pov_freq else None
        }

    def format_issues(self, issues: List[Dict[str, any]], text: str = None) -> str:
        """
        問題リストをフォーマット

        Args:
            issues: check() の戻り値
            text: 元のテキスト

        Returns:
            フォーマットされた文字列
        """
        if not issues:
            return "✓ 主語や視点のブレは見つかりませんでした。"

        lines = [f"\n{len(issues)}件の問題が見つかりました:\n"]

        for i, issue in enumerate(issues, 1):
            severity_icon = {
                'error': '❌',
                'warning': '⚠️',
                'info': 'ℹ️'
            }.get(issue['severity'], '•')

            lines.append(f"{i}. {severity_icon} {issue['message']}")

            if text and 'position' in issue:
                start, end = issue['position']
                context_start = max(0, start - 20)
                context_end = min(len(text), end + 20)
                context = text[context_start:context_end]

                lines.append(f"   位置 {start}-{end}: ...{context}...")

            if 'suggestion' in issue:
                lines.append(f"   💡 提案: {issue['suggestion']}")

            if 'subjects' in issue:
                lines.append(f"   主語の変化: {' → '.join(issue['subjects'])}")

            lines.append("")

        return "\n".join(lines)

    def check_and_report(self, text: str) -> str:
        """チェックを実行し、結果をフォーマットして返す便利メソッド"""
        issues = self.check(text)
        return self.format_issues(issues, text)

    def export_issues_json(self, issues: List[Dict[str, any]], 
                          filepath: str) -> None:
        """
        検出された問題をJSON形式で出力

        Args:
            issues: check() の戻り値
            filepath: 出力ファイルパス
        """
        output = {
            'metadata': {
                'total_issues': len(issues),
                'by_type': self._count_by_type(issues),
            },
            'issues': issues
        }

        with open(filepath, 'w', encoding='utf-8') as f:
            json.dump(output, f, ensure_ascii=False, indent=2)

    def _count_by_type(self, issues: List[Dict[str, any]]) -> Dict[str, int]:
        """問題を種類別にカウント"""
        counts = defaultdict(int)
        for issue in issues:
            counts[issue['type']] += 1
        return dict(counts)

    def get_statistics(self, text: str) -> Dict[str, any]:
        """
        テキスト全体の統計を返す

        Args:
            text: 分析対象テキスト

        Returns:
            統計情報を含む辞書
        """
        analysis = self.analyze_text(text)
        issues = self.check(text)

        return {
            'analysis': analysis,
            'issues': issues,
            'issue_count': len(issues),
            'dominant_pov_ratio': self._calculate_pov_dominance(analysis),
            'subject_stability_score': self._calculate_subject_stability(text),
        }

    def _calculate_pov_dominance(self, analysis: Dict[str, any]) -> float:
        """視点の支配度を計算（0-1）"""
        pov_dist = analysis.get('pov_distribution', {})
        if not pov_dist:
            return 0.0

        total = sum(pov_dist.values())
        if total == 0:
            return 0.0

        max_pov_count = max(pov_dist.values())
        
        result = ScoreValidator.safe_divide(max_pov_count, total, default=0.0)
        is_valid, normalized, msg = ScoreValidator.validate_score(result)
        return normalized

    def _calculate_subject_stability(self, text: str) -> float:
        """主語の安定性を計算（0-1、高いほど安定）"""
        sentences = self._split_sentences(text)
        subjects = []

        for sentence in sentences:
            subject = self._extract_subject(sentence)
            if subject:
                subjects.append(subject)

        if len(subjects) < 2:
            return 1.0

        # 同じ主語が連続する割合を計算
        consecutive_count = 0
        for i in range(1, len(subjects)):
            if subjects[i] == subjects[i - 1]:
                consecutive_count += 1

        stability = consecutive_count / (len(subjects) - 1)
        return min(stability, 1.0)

    def suggest_improvements(self, text: str) -> List[Dict[str, any]]:
        """
        検出された問題に対する改善提案を生成

        Args:
            text: 分析対象テキスト

        Returns:
            改善提案のリスト
        """
        issues = self.check(text)
        suggestions = []

        for issue in issues:
            if issue['type'] == 'subject_changes':
                suggestions.append({
                    'issue_type': 'subject_changes',
                    'severity': issue['severity'],
                    'problem': f"主語が{issue['change_count']}回変化しています",
                    'suggestions': [
                        '同じ主語で複数の行動を述べる場合は、文を並べ替える',
                        '段落を分割して、主語ごとにグループ化する',
                        '「〜は」「〜は」で統一し、主語の一貫性を保つ',
                        '主語を明示的に記述し、曖昧性を減らす'
                    ],
                    'subjects_involved': issue.get('subjects', [])
                })

            elif issue['type'] == 'pov_inconsistency':
                suggestions.append({
                    'issue_type': 'pov_inconsistency',
                    'severity': issue['severity'],
                    'problem': f"視点が混在しています: {', '.join(set(issue.get('povs', [])))}",
                    'suggestions': [
                        '一人称 (I/We) で統一するか、三人称 (He/She/They) で統一する',
                        '一貫した視点で全文を書き直す',
                        '段落単位で視点を統一する（章ごとに視点を変えるなど）',
                        '視点の切り替わりが意図的でないか確認する'
                    ],
                    'povs_involved': issue.get('povs', [])
                })

        return suggestions
