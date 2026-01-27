"""
チェーホフの銃検知器（Chekhov's Gun Detector）

登場したのに何もしなかった要素（未回収の伏線）を検出します。
「銃が壁に掛かっていたら、それは必ず発射されなければならない」という文学の鉄則を適用。
"""

import pandas as pd
import numpy as np
from typing import List, Dict, Tuple, Optional
from dataclasses import dataclass
from collections import Counter
import logging

logger = logging.getLogger(__name__)


@dataclass
class UnfiredGun:
    """未発火の銃（未回収の要素）"""
    node: str                        # ノード（要素名）
    first_appearance: int            # 初出現位置（文字番号）
    appearances: int                 # 総出現回数
    final_appearance: int            # 最終出現位置
    total_length: int                # テキスト全体の長さ
    edge_count: int                  # 他の要素との関連性数（エッジ数）
    unexpectedness: float            # 予期しなさスコア (0-1)
    severity: str                    # 'critical' / 'warning' / 'info'
    message: str                     # メッセージ


class ChekhofGunDetector:
    """
    チェーホフの銃を検知

    ストーリー構成における「張られた伏線」の回収度を検証。
    - 序盤で導入されたが、終盤で完全にフェードアウトした要素
    - 出現数は少ないが、重要そうに見える要素
    - を検出します。
    """

    # 基準値
    EARLY_PHASE = 0.33              # 序盤判定 (全体の33%まで)
    LATE_PHASE = 0.67               # 後期判定 (全体の67%以降)
    FADEOUT_THRESHOLD = 0.8         # フェードアウト判定 (最終出現がここまで)
    MIN_APPEARANCES = 2             # 最小出現回数

    def __init__(self):
        pass

    def _calculate_unexpectedness(
        self,
        first_pos: int,
        final_pos: int,
        total_length: int,
        appearances: int,
        edge_count: int
    ) -> float:
        """
        「予期しなさ」スコアを計算

        序盤で出現 + 最終出現が早い + エッジが少ない = スコア高

        Returns:
            0.0-1.0 のスコア
        """
        # 序盤出現度
        first_ratio = 1.0 - (first_pos / total_length)  # 早いほど高い

        # 最終出現の早さ
        final_ratio = final_pos / total_length
        fadeout_score = max(0.0, (self.FADEOUT_THRESHOLD - final_ratio) / self.FADEOUT_THRESHOLD)

        # エッジの少なさ（孤立度）
        # 他の要素と絡みが少ないほど「置き去り」感がある
        isolation_score = 1.0 / (1.0 + edge_count)

        # 複合スコア
        unexpectedness = (
            first_ratio * 0.4 +      # 序盤出現が大切
            fadeout_score * 0.4 +    # 早期フェードアウトが問題
            isolation_score * 0.2    # 孤立度
        )

        return min(1.0, unexpectedness)

    def detect_unfired_guns(
        self,
        text: str,
        df_cooccurrence: Optional[pd.DataFrame] = None,
        important_nodes: Optional[List[str]] = None
    ) -> List[UnfiredGun]:
        """
        未発火の銃を検知

        Parameters:
            text: 元テキスト
            df_cooccurrence: 共起DataFrame (source, target, weight 列)
            important_nodes: 重要ノードのリスト（Noneなら全ノード）

        Returns:
            未発火の銃のリスト（unexpectedness の降順）
        """
        if not text:
            return []

        if df_cooccurrence is not None and df_cooccurrence.empty:
            return []

        unfired = []
        total_length = len(text)

        # エッジ数を集計
        edge_counts = Counter()
        if df_cooccurrence is not None and not df_cooccurrence.empty:
            for _, row in df_cooccurrence.iterrows():
                edge_counts[row['source']] += 1
                edge_counts[row['target']] += 1

        # 分析対象ノードを決定
        if important_nodes:
            nodes_to_check = important_nodes
        else:
            # df_cooccurrence から全ノードを抽出
            if df_cooccurrence is None or df_cooccurrence.empty:
                return []
            nodes = set(df_cooccurrence['source'].tolist() + df_cooccurrence['target'].tolist())
            nodes_to_check = list(nodes)

        # 各ノードをチェック
        for node in nodes_to_check:
            # テキスト内の出現位置を全て検出
            appearances_list = []
            start = 0
            while True:
                pos = text.find(node, start)
                if pos == -1:
                    break
                appearances_list.append(pos)
                start = pos + 1

            if len(appearances_list) < self.MIN_APPEARANCES:
                continue

            first_pos = appearances_list[0]
            final_pos = appearances_list[-1]
            appearance_count = len(appearances_list)

            # 序盤出現 & 早期フェードアウト の判定
            is_early_intro = first_pos < total_length * self.EARLY_PHASE
            is_fadeout = final_pos < total_length * self.FADEOUT_THRESHOLD
            has_few_edges = edge_counts.get(node, 0) <= 2

            # 条件チェック
            if not (is_early_intro and is_fadeout):
                continue

            # 予期しなさスコア
            unexpectedness = self._calculate_unexpectedness(
                first_pos, final_pos, total_length, appearance_count, edge_counts.get(node, 0)
            )

            # 重大度判定
            if unexpectedness > 0.7 and has_few_edges:
                severity = 'critical'
                severity_label = '🚨'
            elif unexpectedness > 0.5:
                severity = 'warning'
                severity_label = '⚠️'
            else:
                severity = 'info'
                severity_label = 'ℹ️'

            message = (
                f"{severity_label} 【チェーホフの銃（未発火）】\n"
                f"要素: 「{node}」\n"
                f"初出現: 位置 {first_pos} (全体の {first_pos/total_length*100:.1f}%)\n"
                f"最終出現: 位置 {final_pos} (全体の {final_pos/total_length*100:.1f}%)\n"
                f"出現回数: {appearance_count}回\n"
                f"関連性: {edge_counts.get(node, 0)}件\n"
                f"予期しなさ: {unexpectedness:.2f}/1.0\n"
                f"\n→ 序盤で導入されたこの要素は、中盤でフェードアウトしています。\n"
                f"→ ストーリーの伏線として機能していません。\n"
                f"→ 後半で活躍させるか、削除を検討してください。"
            )

            unfired.append(
                UnfiredGun(
                    node=node,
                    first_appearance=first_pos,
                    appearances=appearance_count,
                    final_appearance=final_pos,
                    total_length=total_length,
                    edge_count=edge_counts.get(node, 0),
                    unexpectedness=unexpectedness,
                    severity=severity,
                    message=message
                )
            )

        # unexpectedness の降順でソート
        unfired.sort(key=lambda x: x.unexpectedness, reverse=True)

        return unfired

    def get_gun_report(
        self,
        text: str,
        df_cooccurrence: Optional[pd.DataFrame] = None,
        important_nodes: Optional[List[str]] = None
    ) -> str:
        """
        チェーホフの銃レポート生成

        Parameters:
            text: 元テキスト
            df_cooccurrence: 共起DataFrame
            important_nodes: 重要ノード

        Returns:
            レポート文字列
        """
        unfired = self.detect_unfired_guns(text, df_cooccurrence, important_nodes)

        if not unfired:
            return "✅ チェーホフの銃は検出されませんでした。伏線の回収が良好です。"

        critical = [u for u in unfired if u.severity == 'critical']
        warnings = [u for u in unfired if u.severity == 'warning']

        report = f"""
{'='*70}
【チェーホフの銃検知レポート】
{'='*70}

未発火の銃: {len(unfired)}件
  - 致命的: {len(critical)}件
  - 警告: {len(warnings)}件

{'-'*70}
"""

        for gun in unfired[:10]:  # Top 10
            report += gun.message + "\n\n"

        if len(unfired) > 10:
            report += f"... ほか {len(unfired) - 10}件\n\n"

        report += f"""
{'-'*70}
【文学的解釈】
チェーホフの銃の法則:
  「舞台上に銃が掛かっていたら、それは必ず発射されなければならない」

→ 導入した要素は必ずストーリー内で活躍させる必要があります
→ 不要な要素は早めに削除するか、後半で活躍させるべきです

{'='*70}
"""
        return report
