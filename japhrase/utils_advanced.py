# coding: utf-8
"""
高度な分析機能：コンテキスト、位置情報、キャッシング、ストリーミング、メトリクス
"""

import logging
import time
from functools import lru_cache
from typing import List, Dict, Any, Optional, Tuple
from collections import defaultdict, Counter
import hashlib

logger = logging.getLogger(__name__)


class ContextAnalyzer:
    """⓹ コンテキスト分析の深化"""

    @staticmethod
    def analyze_section_baseline(text: str, section_markers: List[str] = None) -> Dict[str, Any]:
        """
        セクション単位での「正常性ベースライン」を算出

        Args:
            text: テキスト
            section_markers: セクション区切り（例: ['##', '【', '章']）

        Returns:
            セクション別の統計
        """
        if section_markers is None:
            section_markers = ['##', '【', '章', '\n\n']

        sections = []
        current_section = {'title': 'intro', 'text': '', 'start': 0}

        lines = text.split('\n')
        char_pos = 0

        for line in lines:
            is_marker = any(marker in line for marker in section_markers)

            if is_marker and current_section['text'].strip():
                sections.append(current_section)
                current_section = {
                    'title': line[:30],
                    'text': '',
                    'start': char_pos
                }
            else:
                current_section['text'] += line + '\n'

            char_pos += len(line) + 1

        if current_section['text'].strip():
            sections.append(current_section)

        # 各セクションの統計
        section_stats = []
        for section in sections:
            stats = {
                'title': section['title'],
                'length': len(section['text']),
                'sentence_count': len([s for s in section['text'].split('。') if s.strip()]),
                'baseline': _calculate_baseline(section['text'])
            }
            section_stats.append(stats)

        return {
            'total_sections': len(sections),
            'sections': section_stats,
            'overall_baseline': _calculate_baseline(text)
        }

    @staticmethod
    def detect_genre_pattern(text: str) -> str:
        """テキストのジャンルを推定（小説 vs. 技術文書など）"""
        from .utils_robustness import TextProcessor

        normalized = TextProcessor.normalize_whitespace(text)
        lines = normalized.split('.')
        avg_line_length = sum(len(l) for l in lines) / len(lines) if lines else 0

        # 簡易的なジャンル判定
        if '関数' in text or 'API' in text or 'return' in text:
            return 'technical'
        elif '〜だった' in text or '〜である' in text:
            return 'narrative'
        elif avg_line_length > 80:
            return 'academic'
        else:
            return 'general'


def _calculate_baseline(text: str) -> Dict[str, float]:
    """テキストの基本統計"""
    sentences = [s.strip() for s in text.split('。') if s.strip()]
    if not sentences:
        return {'avg_length': 0, 'variety': 0}

    avg_length = sum(len(s) for s in sentences) / len(sentences)
    unique_endings = len(set(s[-3:] for s in sentences if len(s) > 3))
    variety = unique_endings / max(len(sentences), 1)

    return {
        'avg_sentence_length': avg_length,
        'ending_variety': variety,
        'total_sentences': len(sentences)
    }


class PositionTracker:
    """⓶ 位置情報の信頼性強化"""

    def __init__(self):
        self.modification_map = {}  # {original_pos: new_pos}
        self.history = []

    def record_modification(self, original_pos: Tuple[int, int], 
                           replacement_text: str,
                           replacement_length: int) -> Tuple[int, int]:
        """
        修正を記録し、新しい位置を計算

        Args:
            original_pos: 元の位置 (start, end)
            replacement_text: 置換後のテキスト
            replacement_length: 置換後の長さ

        Returns:
            新しい位置
        """
        start, end = original_pos
        length_diff = replacement_length - (end - start)

        # 履歴に記録
        record = {
            'original_pos': original_pos,
            'new_length': replacement_length,
            'length_diff': length_diff,
            'timestamp': time.time()
        }
        self.history.append(record)

        # 後続の位置をすべて調整
        self.modification_map[original_pos] = (start, start + replacement_length)

        logger.debug(f"位置修正: {original_pos} → ({start}, {start + replacement_length}), 差分: {length_diff}")

        return (start, start + replacement_length)

    def get_modification_history(self) -> List[Dict[str, Any]]:
        """修正履歴を返す"""
        return self.history

    def validate_positions(self, text: str, positions: List[Tuple[int, int]]) -> bool:
        """位置情報がテキストと矛盾していないか検証"""
        for start, end in positions:
            if start < 0 or end > len(text) or start >= end:
                logger.warning(f"位置情報が不正: ({start}, {end}), テキスト長: {len(text)}")
                return False
        return True


class CachingAnalyzer:
    """⓪ キャッシング機構"""

    def __init__(self, cache_size: int = 128):
        self.cache_size = cache_size
        self.cache = {}
        self.access_count = Counter()

    def _get_cache_key(self, text: str, method_name: str) -> str:
        """テキストとメソッド名からキャッシュキーを生成"""
        content_hash = hashlib.md5(text.encode()).hexdigest()[:8]
        return f"{method_name}:{content_hash}"

    def get_cached(self, text: str, method_name: str) -> Optional[Any]:
        """キャッシュから結果を取得"""
        key = self._get_cache_key(text, method_name)
        if key in self.cache:
            self.access_count[key] += 1
            logger.debug(f"キャッシュヒット: {key}")
            return self.cache[key]
        return None

    def set_cache(self, text: str, method_name: str, result: Any) -> None:
        """結果をキャッシュに保存"""
        key = self._get_cache_key(text, method_name)

        # キャッシュサイズ制限
        if len(self.cache) >= self.cache_size:
            # 最もアクセスが少ないキーを削除
            least_used = min(self.cache.keys(), 
                           key=lambda k: self.access_count.get(k, 0))
            del self.cache[least_used]
            logger.debug(f"キャッシュ削除: {least_used}")

        self.cache[key] = result
        self.access_count[key] = 0
        logger.debug(f"キャッシュ設定: {key}")

    def clear_cache(self) -> None:
        """キャッシュをクリア"""
        self.cache.clear()
        self.access_count.clear()
        logger.info("キャッシュをクリアしました")

    def get_cache_stats(self) -> Dict[str, Any]:
        """キャッシュの統計情報"""
        return {
            'cache_size': len(self.cache),
            'max_size': self.cache_size,
            'total_accesses': sum(self.access_count.values()),
            'hit_rate': sum(self.access_count.values()) / max(len(self.cache), 1)
        }


class StreamingAnalyzer:
    """⓫ ストリーミング処理対応"""

    def __init__(self, chunk_size: int = 5000):
        self.chunk_size = chunk_size

    def stream_text_chunks(self, text: str) -> List[Dict[str, Any]]:
        """
        テキストをチャンクに分割してストリーム

        Args:
            text: テキスト

        Returns:
            チャンク情報のリスト
        """
        chunks = []
        char_count = 0

        # 文単位で分割（チャンクサイズを超えないように）
        sentences = text.split('。')
        current_chunk = []
        current_length = 0

        for i, sentence in enumerate(sentences):
            if current_length + len(sentence) > self.chunk_size and current_chunk:
                chunks.append({
                    'index': len(chunks),
                    'text': '。'.join(current_chunk) + '。',
                    'start_pos': char_count,
                    'sentence_count': len(current_chunk)
                })
                char_count += sum(len(s) + 1 for s in current_chunk)
                current_chunk = []
                current_length = 0

            current_chunk.append(sentence)
            current_length += len(sentence) + 1

        # 最後のチャンク
        if current_chunk:
            chunks.append({
                'index': len(chunks),
                'text': '。'.join(current_chunk) + ('。' if current_chunk[-1] else ''),
                'start_pos': char_count,
                'sentence_count': len(current_chunk)
            })

        logger.info(f"ストリーム分割: {len(chunks)} チャンク")
        return chunks


class MetricsCollector:
    """⓬ 構造化ログ・メトリクス"""

    def __init__(self):
        self.metrics = defaultdict(list)
        self.start_times = {}

    def start_timer(self, operation_name: str) -> None:
        """操作のタイマーを開始"""
        self.start_times[operation_name] = time.time()

    def end_timer(self, operation_name: str) -> float:
        """操作のタイマーを終了し、経過時間を記録"""
        if operation_name not in self.start_times:
            return 0.0

        elapsed = time.time() - self.start_times.pop(operation_name)
        self.metrics[operation_name].append(elapsed)
        logger.debug(f"{operation_name}: {elapsed:.3f}秒")
        return elapsed

    def get_metrics(self) -> Dict[str, Dict[str, Any]]:
        """メトリクスの統計情報を取得"""
        result = {}
        for op_name, times in self.metrics.items():
            if times:
                result[op_name] = {
                    'count': len(times),
                    'total_time': sum(times),
                    'avg_time': sum(times) / len(times),
                    'min_time': min(times),
                    'max_time': max(times)
                }
        return result

    def log_score_calculation(self, operation_name: str, 
                             score_name: str, score_value: float,
                             reasoning: str) -> None:
        """スコア計算の根拠を記録"""
        logger.debug(f"スコア計算: {operation_name}/{score_name} = {score_value:.3f} ({reasoning})")

    def export_metrics(self, filepath: str) -> None:
        """メトリクスをJSON形式で出力"""
        import json

        metrics = self.get_metrics()
        with open(filepath, 'w', encoding='utf-8') as f:
            json.dump(metrics, f, ensure_ascii=False, indent=2)
        logger.info(f"メトリクスを {filepath} に出力しました")
