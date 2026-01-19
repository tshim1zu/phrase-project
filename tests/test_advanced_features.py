# coding: utf-8
"""
高度な機能テスト：キャッシング、位置情報、メトリクス、ストリーミング、コンテキスト分析
"""

import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

from japhrase.utils_advanced import (
    ContextAnalyzer, PositionTracker, CachingAnalyzer,
    StreamingAnalyzer, MetricsCollector
)


def test_context_analysis():
    """コンテキスト分析テスト"""
    text = """
    ## 第1章 導入
    これは導入部分です。
    ここで主な概念を説明します。
    
    ## 第2章 詳細
    ここは詳細な説明です。
    より具体的な内容を含みます。
    """

    analyzer = ContextAnalyzer()

    # セクションベースライン分析
    baseline = analyzer.analyze_section_baseline(text, section_markers=['##'])
    print(f"✓ セクション数: {baseline['total_sections']}")
    assert baseline['total_sections'] >= 2

    # ジャンル検出
    genre = analyzer.detect_genre_pattern(text)
    print(f"✓ 推定ジャンル: {genre}")
    assert genre in ['technical', 'narrative', 'academic', 'general']


def test_position_tracking():
    """位置情報追跡テスト"""
    tracker = PositionTracker()

    # 修正を記録
    original_pos = (10, 20)
    new_pos = tracker.record_modification(original_pos, "新しいテキスト", 5)
    print(f"✓ 位置修正: {original_pos} → {new_pos}")
    assert new_pos[1] - new_pos[0] == 5

    # 履歴を取得
    history = tracker.get_modification_history()
    print(f"✓ 修正履歴: {len(history)}件")
    assert len(history) == 1

    # 位置検証
    text = "これはテストテキストです。" * 5
    positions = [(0, 5), (10, 15)]
    is_valid = tracker.validate_positions(text, positions)
    print(f"✓ 位置検証: {is_valid}")
    assert is_valid


def test_caching():
    """キャッシング機構テスト"""
    cache = CachingAnalyzer(cache_size=3)

    text1 = "テキスト1"
    result1 = {"data": [1, 2, 3]}

    # キャッシュに保存
    cache.set_cache(text1, 'method1', result1)
    print(f"✓ キャッシュ設定: method1")

    # キャッシュから取得
    cached = cache.get_cached(text1, 'method1')
    assert cached == result1
    print(f"✓ キャッシュヒット")

    # 統計情報
    stats = cache.get_cache_stats()
    print(f"✓ キャッシュサイズ: {stats['cache_size']}/{stats['max_size']}")

    # キャッシュクリア
    cache.clear_cache()
    assert len(cache.cache) == 0
    print(f"✓ キャッシュクリア")


def test_streaming():
    """ストリーミング処理テスト"""
    text = "これはサンプルです。" * 100  # 長いテキスト

    analyzer = StreamingAnalyzer(chunk_size=100)

    # チャンク分割
    chunks = analyzer.stream_text_chunks(text)
    print(f"✓ ストリーム分割: {len(chunks)}チャンク")
    assert len(chunks) > 1

    # 各チャンク情報を確認
    for i, chunk in enumerate(chunks[:2]):
        print(f"  チャンク{i}: {chunk['sentence_count']}文, 開始位置: {chunk['start_pos']}")


def test_metrics():
    """メトリクス収集テスト"""
    metrics = MetricsCollector()

    # タイマー測定
    import time

    metrics.start_timer('operation1')
    time.sleep(0.1)
    elapsed = metrics.end_timer('operation1')
    print(f"✓ 操作時間: {elapsed:.3f}秒")

    # スコア計算ログ
    metrics.log_score_calculation('analysis', 'variant_score', 0.85, 'frequency-based calculation')
    print(f"✓ スコア計算ログ記録")

    # メトリクス取得
    stats = metrics.get_metrics()
    print(f"✓ メトリクス: {len(stats)}項目")
    assert 'operation1' in stats
    assert stats['operation1']['count'] == 1


if __name__ == '__main__':
    print("=" * 50)
    print("高度な機能テスト開始")
    print("=" * 50)

    test_context_analysis()
    print()

    test_position_tracking()
    print()

    test_caching()
    print()

    test_streaming()
    print()

    test_metrics()
    print()

    print("=" * 50)
    print("✓ すべてのテスト成功")
    print("=" * 50)
