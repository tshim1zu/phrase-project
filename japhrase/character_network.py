"""
人物/用語相関ネットワーク生成モジュール

テキスト中の重要語（固有名詞のようなもの）を抽出し、
それらの共起関係からネットワーク構造（エッジリスト）を生成します。

生成されたエッジリストは NetworkX や可視化ツールで利用できます。
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023-2026"

import itertools
import pandas as pd
from typing import List, Dict, Tuple, Optional
import logging

logger = logging.getLogger(__name__)


class CharacterNetworkGenerator:
    """
    登場人物や重要語の共起ネットワークを構築

    テキスト中の頻出語（登場人物や重要な概念）を「ノード」と見なし、
    それらが同じ文脈で現れる「共起」を「エッジ」として、
    グラフデータ（エッジリスト）を生成します。

    使用例:
        >>> generator = CharacterNetworkGenerator()
        >>> df_edges = generator.generate_edgelist(text, top_n_nodes=10)
        >>> print(df_edges.head())
    """

    def __init__(self, window_size: int = 50):
        """
        Parameters:
            window_size (int): 共起判定のウィンドウサイズ（文字数）
                              この文字数内に含まれるノードは「共起している」と判定
        """
        self.window_size = window_size

    def generate_edgelist(self, text: str, top_n_nodes: int = 15) -> pd.DataFrame:
        """
        重要語間の共起ネットワーク（エッジリスト）を生成

        Parameters:
            text (str): 入力テキスト
            top_n_nodes (int): 抽出する重要語の数（ノード数）

        Returns:
            pd.DataFrame: エッジリスト
                [
                    {
                        'source': str,      # ノードA
                        'target': str,      # ノードB
                        'weight': float     # エッジの強度（共起スコア）
                    },
                    ...
                ]
        """
        try:
            from .extracter import PhraseExtracter

            # ノード候補（重要語）の抽出
            extractor = PhraseExtracter(min_count=3, verbose=0)
            df_nodes = extractor.extract([text])

            if df_nodes.empty:
                logger.warning("重要語が抽出されませんでした")
                return pd.DataFrame(columns=['source', 'target', 'weight'])

            # 列名の互換性処理
            phrase_col = 'seqchar' if 'seqchar' in df_nodes.columns else 'phrase'

            # ノードのフィルタリング（2文字以上、記号なし）
            valid_nodes = df_nodes[
                (df_nodes[phrase_col].str.len() >= 2)
                & (~df_nodes[phrase_col].str.contains(r'[、。\s]'))
            ].head(top_n_nodes)[phrase_col].tolist()

            if not valid_nodes:
                logger.warning("条件を満たすノードがありません")
                return pd.DataFrame(columns=['source', 'target', 'weight'])

            # エッジ生成
            edges = []
            edge_map = {}  # 重複排除用

            for node_a in valid_nodes:
                # node_aの出現位置を全て取得
                occurrences_a = self._find_all_positions(text, node_a)

                if not occurrences_a:
                    continue

                # 各出現位置の周辺でnode_bと共起しているか調べる
                for pos_a in occurrences_a:
                    # ウィンドウ内をチェック
                    window_start = max(0, pos_a - self.window_size)
                    window_end = min(len(text), pos_a + len(node_a) + self.window_size)
                    window_text = text[window_start:window_end]

                    for node_b in valid_nodes:
                        if node_a == node_b:
                            continue

                        # node_bがウィンドウ内にあるか
                        if node_b in window_text:
                            # 無向グラフとして正規化（辞書順）
                            source, target = sorted([node_a, node_b])
                            key = (source, target)

                            if key not in edge_map:
                                edge_map[key] = 0
                            edge_map[key] += 1

            # エッジリストに変換
            for (source, target), count in edge_map.items():
                edges.append({'source': source, 'target': target, 'weight': float(count)})

            if not edges:
                logger.warning("共起が検出されませんでした")
                return pd.DataFrame(columns=['source', 'target', 'weight'])

            df_edges = pd.DataFrame(edges)
            # 重みでソート
            df_edges = df_edges.sort_values('weight', ascending=False).reset_index(
                drop=True
            )

            return df_edges

        except ImportError:
            logger.warning("CharacterNetworkGenerator: PhraseExtracterが必要です")
            return pd.DataFrame(columns=['source', 'target', 'weight'])

    def _find_all_positions(self, text: str, substring: str) -> List[int]:
        """
        テキスト内のsubstringの全出現位置を取得

        Parameters:
            text (str): テキスト
            substring (str): 検索文字列

        Returns:
            List[int]: 出現位置のリスト
        """
        positions = []
        start = 0
        while True:
            pos = text.find(substring, start)
            if pos == -1:
                break
            positions.append(pos)
            start = pos + 1  # オーバーラップを許可
        return positions

    def get_network_stats(self, df_edges: pd.DataFrame) -> Dict:
        """
        ネットワークの統計情報を計算

        Parameters:
            df_edges (pd.DataFrame): generate_edgelist()の戻り値

        Returns:
            Dict: ネットワーク統計
                {
                    'node_count': int,          # ノード数
                    'edge_count': int,          # エッジ数
                    'avg_weight': float,        # 平均エッジ重み
                    'max_weight': float,        # 最大エッジ重み
                    'density_est': float        # ネットワーク密度（推定）
                }
        """
        if df_edges.empty:
            return {
                'node_count': 0,
                'edge_count': 0,
                'avg_weight': 0.0,
                'max_weight': 0.0,
                'density_est': 0.0,
            }

        # ノード数（unique な source と target）
        unique_nodes = set(df_edges['source'].unique()) | set(
            df_edges['target'].unique()
        )
        node_count = len(unique_nodes)
        edge_count = len(df_edges)

        avg_weight = df_edges['weight'].mean()
        max_weight = df_edges['weight'].max()

        # ネットワーク密度の推定
        # 完全グラフのエッジ数: n*(n-1)/2
        # 実際のエッジ数 / 完全グラフのエッジ数
        if node_count > 1:
            max_edges = node_count * (node_count - 1) / 2
            density = edge_count / max_edges
        else:
            density = 0.0

        return {
            'node_count': node_count,
            'edge_count': edge_count,
            'avg_weight': round(avg_weight, 2),
            'max_weight': max_weight,
            'density_est': round(density, 3),
        }

    def get_important_nodes(self, df_edges: pd.DataFrame, top_n: int = 10) -> pd.DataFrame:
        """
        ネットワーク内の「中心的なノード」を検出

        各ノードの接続度（degree）を計算し、ランキングします。

        Parameters:
            df_edges (pd.DataFrame): generate_edgelist()の戻り値
            top_n (int): 上位N件を返す

        Returns:
            pd.DataFrame: ノードのランキング
                [
                    {'node': str, 'degree': int, 'weighted_degree': float},
                    ...
                ]
        """
        if df_edges.empty:
            return pd.DataFrame(columns=['node', 'degree', 'weighted_degree'])

        node_stats = {}

        for _, row in df_edges.iterrows():
            source, target, weight = row['source'], row['target'], row['weight']

            # source ノードの統計
            if source not in node_stats:
                node_stats[source] = {'degree': 0, 'weighted_degree': 0.0}
            node_stats[source]['degree'] += 1
            node_stats[source]['weighted_degree'] += weight

            # target ノードの統計
            if target not in node_stats:
                node_stats[target] = {'degree': 0, 'weighted_degree': 0.0}
            node_stats[target]['degree'] += 1
            node_stats[target]['weighted_degree'] += weight

        # DataFrame化してソート
        df_nodes = pd.DataFrame(
            [{'node': node, **stats} for node, stats in node_stats.items()]
        )
        df_nodes['weighted_degree'] = df_nodes['weighted_degree'].round(2)
        df_nodes = df_nodes.sort_values(
            'weighted_degree', ascending=False
        ).reset_index(drop=True)

        return df_nodes.head(top_n)

    def to_gexf(self, df_edges: pd.DataFrame, filepath: str) -> None:
        """
        エッジリストをGEXF形式で出力（Gephi等で利用可能）

        Parameters:
            df_edges (pd.DataFrame): generate_edgelist()の戻り値
            filepath (str): 出力ファイルパス
        """
        try:
            import networkx as nx

            G = nx.Graph()

            # ノードとエッジを追加
            for _, row in df_edges.iterrows():
                G.add_edge(row['source'], row['target'], weight=row['weight'])

            # GEXF形式で保存
            nx.write_gexf(G, filepath)
            logger.info(f"ネットワークグラフを保存: {filepath}")

        except ImportError:
            logger.warning("GEXFエクスポート: networkxが必要です")

    def to_csv(self, df_edges: pd.DataFrame, filepath: str) -> None:
        """
        エッジリストをCSV形式で出力

        Parameters:
            df_edges (pd.DataFrame): generate_edgelist()の戻り値
            filepath (str): 出力ファイルパス
        """
        df_edges.to_csv(filepath, index=False, encoding='utf-8-sig')
        logger.info(f"エッジリストを保存: {filepath}")


# 便利関数
def generate_character_network(text: str, top_n: int = 15) -> pd.DataFrame:
    """
    単一関数でのネットワーク生成

    Parameters:
        text (str): 入力テキスト
        top_n (int): ノード数

    Returns:
        pd.DataFrame: エッジリスト
    """
    generator = CharacterNetworkGenerator()
    return generator.generate_edgelist(text, top_n_nodes=top_n)
