# coding: utf-8
"""
ストリーミング・インクリメンタル処理

大規模テキスト（100GB+）をメモリ効率的に処理
"""

import logging
from typing import List, Dict, Tuple, Optional, Generator
from collections import defaultdict, Counter
import pandas as pd

logger = logging.getLogger(__name__)


class StreamingPhraseExtracter:
    """ストリーミング処理対応のフレーズ抽出"""

    def __init__(self, min_count: int = 3, min_length: int = 1, max_length: int = 10):
        """
        Parameters:
            min_count: 最小出現回数
            min_length: 最小フレーズ長
            max_length: 最大フレーズ長
        """
        self.min_count = min_count
        self.min_length = min_length
        self.max_length = max_length

        # ストリーミング状態
        self.phrase_counter = defaultdict(int)
        self.total_texts = 0
        self.chunks_processed = 0

    def add_chunk(self, texts: List[str]) -> Tuple[pd.DataFrame, Dict]:
        """
        新しいテキストチャンク（例：1000行）を追加

        Parameters:
            texts: テキストリスト

        Returns:
            (現在のフレーズ結果DataFrame, 処理統計)
        """
        from .extracter import PhraseExtracter

        # フレーズを抽出（ここから import されているはずなので使用）
        try:
            extractor = PhraseExtracter(
                min_count=1,  # 一時的に1でカウント
                min_length=self.min_length,
                max_length=self.max_length,
                verbose=0
            )
            df_chunk = extractor.extract(texts)
        except Exception as e:
            logger.warning(f"チャンク処理エラー: {e}")
            df_chunk = pd.DataFrame()

        if df_chunk is not None and not df_chunk.empty:
            phrase_col = 'seqchar' if 'seqchar' in df_chunk.columns else 'phrase'
            freq_col = 'freq' if 'freq' in df_chunk.columns else 'frequency'

            # フレーズカウンターを更新
            for idx, row in df_chunk.iterrows():
                phrase = row[phrase_col]
                freq = row[freq_col]
                self.phrase_counter[phrase] += freq

        # 処理統計
        self.total_texts += len(texts)
        self.chunks_processed += 1

        stats = {
            'total_texts': self.total_texts,
            'chunks_processed': self.chunks_processed,
            'unique_phrases': len(self.phrase_counter),
            'chunk_size': len(texts),
        }

        logger.info(
            f"チャンク {self.chunks_processed} 処理完了: "
            f"テキスト {len(texts)} 行, "
            f"ユニークフレーズ {len(self.phrase_counter)} 個"
        )

        return self.get_current_results(), stats

    def get_current_results(self, top_k: Optional[int] = None) -> pd.DataFrame:
        """
        現在のところの暫定結果を返す

        Parameters:
            top_k: 上位K件を返す（Noneなら全件）

        Returns:
            フレーズDataFrame
        """
        # min_count でフィルタ
        filtered = {
            phrase: count
            for phrase, count in self.phrase_counter.items()
            if count >= self.min_count
        }

        if not filtered:
            return pd.DataFrame(columns=['seqchar', 'freq'])

        # DataFrame に変換
        df = pd.DataFrame([
            {'seqchar': phrase, 'freq': count}
            for phrase, count in filtered.items()
        ])

        # 頻度でソート
        df = df.sort_values('freq', ascending=False)

        if top_k is not None:
            df = df.head(top_k)

        return df.reset_index(drop=True)

    def finalize(self) -> pd.DataFrame:
        """全チャンク処理後の最終結果を返す"""
        return self.get_current_results()

    def get_statistics(self) -> Dict:
        """ストリーミング処理の統計を取得"""
        total_count = sum(self.phrase_counter.values())

        return {
            'total_texts': self.total_texts,
            'chunks_processed': self.chunks_processed,
            'unique_phrases': len(self.phrase_counter),
            'total_phrase_count': total_count,
            'avg_chunk_size': self.total_texts / self.chunks_processed if self.chunks_processed > 0 else 0,
        }

    def memory_estimate(self) -> Dict:
        """メモリ使用量の推定"""
        # Counter のメモリ: フレーズ * (50 + フレーズ長)
        phrase_memory = sum(
            50 + len(phrase)
            for phrase in self.phrase_counter.keys()
        ) / (1024 * 1024)  # MB

        return {
            'phrase_counter_mb': phrase_memory,
            'estimated_total_mb': phrase_memory * 1.5,  # オーバーヘッド込み
        }


class ChunkedTextReader:
    """テキストファイルをチャンク単位で読み込み"""

    def __init__(self, filepath: str, chunk_size: int = 1000):
        """
        Parameters:
            filepath: テキストファイルパス
            chunk_size: チャンクサイズ（行数）
        """
        self.filepath = filepath
        self.chunk_size = chunk_size

    def read_chunks(self) -> Generator[List[str], None, None]:
        """
        チャンク単位でテキストを読み込み

        Yields:
            テキストリスト（chunk_size 行）
        """
        chunk = []

        try:
            with open(self.filepath, 'r', encoding='utf-8') as f:
                for line in f:
                    line = line.strip()
                    if line:
                        chunk.append(line)

                    if len(chunk) >= self.chunk_size:
                        yield chunk
                        chunk = []

                # 残りのデータ
                if chunk:
                    yield chunk

        except Exception as e:
            logger.error(f"ファイル読み込みエラー: {e}")
            raise


class IncrementalAggregator:
    """インクリメンタル集約"""

    def __init__(self):
        self.aggregated = {}
        self.updates = []

    def add_phrase(self, phrase: str, count: int) -> None:
        """フレーズをカウンター"""
        if phrase not in self.aggregated:
            self.aggregated[phrase] = 0

        old_count = self.aggregated[phrase]
        new_count = old_count + count

        self.aggregated[phrase] = new_count

        # 更新履歴を記録（オプション）
        self.updates.append({
            'phrase': phrase,
            'old_count': old_count,
            'new_count': new_count,
            'delta': count,
        })

    def get_snapshot(self) -> Dict[str, int]:
        """現在のスナップショット"""
        return self.aggregated.copy()

    def get_changes_since(self, last_update: int = 0) -> Dict[str, int]:
        """最後の更新以降の変更"""
        recent_updates = self.updates[last_update:]

        changes = {}
        for update in recent_updates:
            phrase = update['phrase']
            if phrase not in changes:
                changes[phrase] = 0
            changes[phrase] += update['delta']

        return changes


class StreamingAnalyzer:
    """ストリーミング処理の統合分析"""

    def __init__(self, min_count: int = 3, chunk_size: int = 1000):
        self.extractor = StreamingPhraseExtracter(min_count=min_count)
        self.reader = None
        self.aggregator = IncrementalAggregator()
        self.chunk_size = chunk_size

    def process_file(
        self,
        filepath: str,
        callback: Optional[callable] = None
    ) -> Tuple[pd.DataFrame, Dict]:
        """
        ファイルをストリーミング処理

        Parameters:
            filepath: ファイルパス
            callback: 各チャンク処理後のコールバック関数
                    (df_results, stats) を受け取る

        Returns:
            (最終結果DataFrame, 処理統計)
        """
        self.reader = ChunkedTextReader(filepath, self.chunk_size)

        stats_total = {
            'total_chunks': 0,
            'total_texts': 0,
            'processing_started': True,
        }

        for chunk in self.reader.read_chunks():
            # チャンクを処理
            df_results, chunk_stats = self.extractor.add_chunk(chunk)

            # コールバック実行
            if callback:
                callback(df_results, chunk_stats)

            # 統計を集約
            stats_total['total_chunks'] += 1
            stats_total['total_texts'] += chunk_stats['total_texts']

            logger.info(
                f"進捗: チャンク {stats_total['total_chunks']}, "
                f"テキスト {stats_total['total_texts']} 行"
            )

        # 最終結果
        final_results = self.extractor.finalize()
        final_stats = self.extractor.get_statistics()

        return final_results, final_stats

    def generate_streaming_report(self) -> str:
        """ストリーミング処理のレポート"""
        stats = self.extractor.get_statistics()
        mem = self.extractor.memory_estimate()

        report = "【ストリーミング処理レポート】\n\n"
        report += f"処理済みテキスト: {stats['total_texts']} 行\n"
        report += f"処理チャンク数: {stats['chunks_processed']}\n"
        report += f"ユニークフレーズ: {stats['unique_phrases']} 個\n"
        report += f"総フレーズ数: {stats['total_phrase_count']}\n"
        report += f"平均チャンクサイズ: {stats['avg_chunk_size']:.1f} 行\n\n"

        report += "【メモリ使用量】\n"
        report += f"フレーズカウンター: {mem['phrase_counter_mb']:.2f} MB\n"
        report += f"推定総メモリ: {mem['estimated_total_mb']:.2f} MB\n"

        return report
