# coding: utf-8
"""
重複検出・削除ロジック

JSON出力やスコア候補の重複を統一的に検出・削除
"""

import logging
from typing import List, Dict, Any, Set, Tuple
from collections import defaultdict

logger = logging.getLogger(__name__)


class DuplicateHandler:
    """重複検出・削除の共通ロジック"""

    @staticmethod
    def deduplicate_candidates(candidates: List[Dict[str, Any]], 
                              key_func=None,
                              merge_func=None) -> List[Dict[str, Any]]:
        """
        候補リストから重複を削除

        Args:
            candidates: 候補リスト
            key_func: 重複判定のキーを生成する関数
            merge_func: 重複した候補をマージする関数

        Returns:
            重複排除後の候補リスト
        """
        if not candidates:
            return []

        if key_func is None:
            # デフォルト: 'primary' フィールドで判定
            key_func = lambda c: c.get('primary', '')

        if merge_func is None:
            # デフォルト: 最初の候補を保持、confidence が高い方を優先
            merge_func = lambda c1, c2: c1 if c1.get('confidence', 0) >= c2.get('confidence', 0) else c2

        seen = {}
        duplicates = defaultdict(list)

        for candidate in candidates:
            key = key_func(candidate)
            if key in seen:
                duplicates[key].append(candidate)
            else:
                seen[key] = candidate

        # 重複候補をマージ
        for key, dup_list in duplicates.items():
            original = seen[key]
            for dup in dup_list:
                merged = merge_func(original, dup)
                seen[key] = merged

        result = list(seen.values())
        if len(result) < len(candidates):
            logger.info(f"重複検出: {len(candidates)} → {len(result)} ({len(candidates) - len(result)} 件削除)")

        return result

    @staticmethod
    def find_duplicate_positions(issues: List[Dict[str, Any]]) -> List[List[int]]:
        """
        位置情報が重複している問題を検出

        Args:
            issues: 問題リスト（'position' フィールド必須）

        Returns:
            重複グループのインデックスリスト
        """
        if not issues:
            return []

        position_map = defaultdict(list)
        for i, issue in enumerate(issues):
            if 'position' in issue:
                pos = issue['position']
                # (start, end) をキーに
                key = (pos[0], pos[1])
                position_map[key].append(i)

        # 同じ位置の問題グループを抽出
        duplicates = [indices for indices in position_map.values() if len(indices) > 1]
        
        if duplicates:
            logger.debug(f"位置重複検出: {len(duplicates)} グループ")

        return duplicates

    @staticmethod
    def merge_issues_at_position(issues: List[Dict[str, Any]]) -> List[Dict[str, Any]]:
        """
        同じ位置の問題をマージ

        Args:
            issues: 問題リスト

        Returns:
            マージ後の問題リスト
        """
        duplicates = DuplicateHandler.find_duplicate_positions(issues)

        if not duplicates:
            return issues

        # 重複する位置のインデックスセット
        dup_indices = set()
        for group in duplicates:
            dup_indices.update(group)

        # マージ対象を整理
        merged_issues = []
        processed = set()

        for i, issue in enumerate(issues):
            if i in processed:
                continue

            if i in dup_indices:
                # 同じ位置のすべての問題を集計
                pos = issue['position']
                same_pos_issues = [issues[j] for j in dup_indices if issues[j]['position'] == pos]

                # マージ問題を作成
                merged = {
                    'position': pos,
                    'type': 'merged_issues',
                    'count': len(same_pos_issues),
                    'original_types': list(set(i.get('type', 'unknown') for i in same_pos_issues)),
                    'original_issues': same_pos_issues,
                    'messages': [i.get('message', '') for i in same_pos_issues if i.get('message')]
                }

                merged_issues.append(merged)
                processed.update([j for j, iss in enumerate(issues) if iss['position'] == pos])
            else:
                merged_issues.append(issue)
                processed.add(i)

        if len(merged_issues) < len(issues):
            logger.info(f"位置重複マージ: {len(issues)} → {len(merged_issues)} ({len(issues) - len(merged_issues)} 件統合)")

        return merged_issues

    @staticmethod
    def filter_duplicate_suggestions(suggestions: List[Dict[str, Any]],
                                     position_tolerance: int = 0) -> List[Dict[str, Any]]:
        """
        修正提案から近い位置の重複を削除

        Args:
            suggestions: 修正提案リスト（'position' フィールド必須）
            position_tolerance: 同一視する位置の許容範囲（文字数）

        Returns:
            重複排除後の提案リスト
        """
        if not suggestions:
            return []

        # 位置でグループ化
        position_groups = defaultdict(list)
        for sugg in suggestions:
            if 'position' not in sugg:
                continue

            # 許容範囲内の位置をキーにグループ化
            pos = sugg['position']
            key = (
                pos // (position_tolerance + 1) if position_tolerance > 0 else pos
            )
            position_groups[key].append(sugg)

        # グループごとに信頼度が最も高いものを保持
        result = []
        for key, group in position_groups.items():
            if len(group) > 1:
                logger.debug(f"位置 {key} で {len(group)} 件の提案が重複")
                # confidence が最も高いものを選択
                best = max(group, 
                          key=lambda s: s.get('confidence', 0))
                result.append(best)
            else:
                result.append(group[0])

        if len(result) < len(suggestions):
            logger.info(f"提案重複排除: {len(suggestions)} → {len(result)} ({len(suggestions) - len(result)} 件削除)")

        return result

    @staticmethod
    def detect_score_conflicts(candidates: List[Dict[str, Any]]) -> List[Tuple[int, int, float]]:
        """
        スコアに矛盾がある候補対を検出

        Args:
            candidates: 候補リスト（'confidence', 'primary', 'variants' フィールド必須）

        Returns:
            (index1, index2, conflict_score) のリスト
        """
        conflicts = []

        for i, cand1 in enumerate(candidates):
            for j, cand2 in enumerate(candidates[i+1:], i+1):
                # 基準フレーズが重複している場合
                if cand1.get('primary') == cand2.get('primary'):
                    conf1 = cand1.get('confidence', 0)
                    conf2 = cand2.get('confidence', 0)
                    conflict_score = abs(conf1 - conf2)

                    if conflict_score > 0.1:  # 信頼度の差が10%以上
                        conflicts.append((i, j, conflict_score))
                        logger.warning(
                            f"スコア矛盾検出: {cand1.get('primary')} - "
                            f"conf1={conf1:.2f}, conf2={conf2:.2f}"
                        )

        return conflicts
