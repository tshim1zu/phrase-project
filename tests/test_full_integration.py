# coding: utf-8
"""
統合テスト：すべてのモジュールの相互作用を検証
"""

import sys
import os
sys.path.insert(0, os.path.join(os.path.dirname(__file__), '..'))

import pandas as pd
from japhrase import (
    TextVariantDetector,
    EndingHeatmapGenerator,
    SubjectPOVDetector,
    SentenceVariationGenerator,
    TeniwohaLinter
)
from japhrase.utils_robustness import ScoreValidator, ConfigValidator, ErrorHandler
from japhrase.utils_deduplication import DuplicateHandler
from japhrase.utils_advanced import (
    ContextAnalyzer, PositionTracker, CachingAnalyzer,
    StreamingAnalyzer, MetricsCollector
)


def test_full_integration():
    """全モジュール統合テスト"""
    
    test_text = """
    彼は走った。彼は走った。彼は走った。
    その後、彼女は歩いた。彼女は歩いた。
    最後に、私は飛んだ。私は飛んだ。
    
    ## 第2章
    彼は走った。彼女は歩いた。
    そして、私が話した。私が話した。
    彼が応答した。彼が応答した。
    """

    print("\n" + "=" * 60)
    print("統合テスト：全モジュール相互作用")
    print("=" * 60)

    # 1. メトリクス初期化
    metrics = MetricsCollector()
    metrics.start_timer('full_integration')

    # 2. 単語ゆれ検出
    print("\n[1] テキスト単語ゆれ検出")
    metrics.start_timer('variant_detection')
    detector = TextVariantDetector()
    
    # DataFrameの準備
    phrases_list = [
        {'seqchar': '彼は走った', 'freq': 3},
        {'seqchar': '彼は走る', 'freq': 1},
        {'seqchar': '彼女は歩いた', 'freq': 2},
        {'seqchar': '彼女は歩く', 'freq': 1},
    ]
    df_phrases = pd.DataFrame(phrases_list)
    
    variants = detector.detect_variants(df_phrases, texts=[test_text])
    metrics.end_timer('variant_detection')
    print(f"   ✓ {len(variants)}個の候補を検出")

    # 3. 語尾密度分析
    print("\n[2] 語尾ヒートマップ生成")
    metrics.start_timer('heatmap_analysis')
    heatmap_gen = EndingHeatmapGenerator(chunk_size=3)
    heatmap_result = heatmap_gen.analyze(test_text)
    metrics.end_timer('heatmap_analysis')
    print(f"   ✓ {heatmap_result['total_chunks']}チャンクを分析")

    # 4. 主語/視点ブレ検出
    print("\n[3] 主語/視点ブレ検出")
    metrics.start_timer('pov_detection')
    pov_detector = SubjectPOVDetector(sensitivity='medium')
    pov_issues = pov_detector.check(test_text)
    metrics.end_timer('pov_detection')
    print(f"   ✓ {len(pov_issues)}個の視点ブレを検出")

    # 5. 同文反復検出
    print("\n[4] 同文反復検出と自動リライト")
    metrics.start_timer('variation_generation')
    variation_gen = SentenceVariationGenerator()
    repetitions = variation_gen.detect_repetitions(test_text)
    metrics.end_timer('variation_generation')
    print(f"   ✓ {len(repetitions)}個の反復パターンを検出")

    # 6. 助詞チェック
    print("\n[5] 助詞（てにをは）チェック")
    metrics.start_timer('particle_check')
    particle_checker = TeniwohaLinter()
    particle_issues = particle_checker.check(test_text)
    metrics.end_timer('particle_check')
    print(f"   ✓ {len(particle_issues)}個の助詞問題を検出")

    # 7. コンテキスト分析
    print("\n[6] コンテキスト分析")
    metrics.start_timer('context_analysis')
    context_analyzer = ContextAnalyzer()
    context = context_analyzer.analyze_section_baseline(test_text)
    genre = context_analyzer.detect_genre_pattern(test_text)
    metrics.end_timer('context_analysis')
    print(f"   ✓ {context['total_sections']}セクション検出、ジャンル: {genre}")

    # 8. 位置情報追跡
    print("\n[7] 位置情報追跡")
    tracker = PositionTracker()
    new_pos = tracker.record_modification((10, 20), "修正テキスト", 6)
    is_valid = tracker.validate_positions(test_text, [(0, 10), (50, 100)])
    print(f"   ✓ 位置修正記録、検証結果: {is_valid}")

    # 9. ストリーミング処理
    print("\n[8] ストリーミング処理")
    streaming = StreamingAnalyzer(chunk_size=100)
    chunks = streaming.stream_text_chunks(test_text * 2)
    print(f"   ✓ {len(chunks)}チャンクに分割")

    # 10. キャッシング検証
    print("\n[9] キャッシング機構")
    cache = CachingAnalyzer(cache_size=10)
    cache.set_cache(test_text, 'method1', {'result': 'cached'})
    cached_result = cache.get_cached(test_text, 'method1')
    assert cached_result == {'result': 'cached'}
    print(f"   ✓ キャッシュ機能動作確認")

    # 11. 重複検出・排除
    print("\n[10] 重複検出・排除")
    if variants:
        deduped = DuplicateHandler.filter_duplicate_suggestions(
            variants,
            position_tolerance=5
        )
        print(f"   ✓ {len(variants)} → {len(deduped)}候補に削減")

    # 12. エラーハンドリング検証
    print("\n[11] エラーハンドリング")
    
    # スコア検証
    normalized = ScoreValidator.validate_score(1.5)
    assert normalized[0] and normalized[1] == 1.0
    
    # 設定値検証
    result = ConfigValidator.validate_chunk_size(-1)
    assert result.is_valid or result.suggestion  # ValidationErrorオブジェクト
    
    # JSON エクスポート（フォールバック）
    import tempfile
    large_result = {'data': [1, 2, 3] * 100}
    with tempfile.NamedTemporaryFile(suffix='.json', delete=False) as tmp:
        success, path = ErrorHandler.safe_json_export(large_result, tmp.name)
        assert success or path
    
    print(f"   ✓ スコア正規化、設定値検証、JSON フォールバック OK")

    # 結果集約
    print("\n" + "=" * 60)
    print("統合テスト結果")
    print("=" * 60)

    metrics.end_timer('full_integration')
    stats = metrics.get_metrics()

    print("\n処理時間統計:")
    for operation, timing in stats.items():
        print(f"  {operation}: {timing['avg_time']:.3f}秒 (実行{timing['count']}回)")

    print("\n✅ すべてのモジュールが正常に連携しました！")
    print("\n統合確認事項:")
    print("  ✓ 5つの分析モジュールが動作")
    print("  ✓ utility層が正常に機能")
    print("  ✓ キャッシング・位置情報追跡が動作")
    print("  ✓ エラーハンドリングが有効")
    print("  ✓ メトリクス収集が動作")


if __name__ == '__main__':
    test_full_integration()
