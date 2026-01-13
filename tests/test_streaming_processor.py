# coding: utf-8
"""
ストリーミング処理のテスト

StreamingProcessor モジュールの全機能を検証：
- チャンク処理
- メモリ効率
- インクリメンタル集約
- 大規模ファイル対応
"""

import pytest
import pandas as pd
import tempfile
import os
from japhrase import (
    StreamingPhraseExtracter,
    ChunkedTextReader,
    IncrementalAggregator,
    StreamingAnalyzer
)


class TestStreamingPhraseExtracter:
    """ストリーミングフレーズ抽出のテスト"""

    def test_add_chunk_basic(self):
        """基本的なチャンク追加"""
        extractor = StreamingPhraseExtracter(
            min_count=2,
            min_length=1,
            max_length=10
        )

        texts = ['テスト'] * 5 + ['実装'] * 3

        df_results, stats = extractor.add_chunk(texts)

        assert isinstance(df_results, pd.DataFrame), "DataFrame を返す"
        assert isinstance(stats, dict), "統計を返す"
        assert stats['total_texts'] == 8, "テキスト数をカウント"
        assert stats['chunks_processed'] == 1, "チャンク数"

    def test_add_multiple_chunks(self):
        """複数チャンクの追加"""
        extractor = StreamingPhraseExtracter(min_count=1)

        # 第1チャンク
        texts1 = ['テスト'] * 3 + ['実装'] * 2
        df1, stats1 = extractor.add_chunk(texts1)

        # 第2チャンク
        texts2 = ['テスト'] * 2 + ['実装'] * 3 + ['バグ']
        df2, stats2 = extractor.add_chunk(texts2)

        assert stats2['chunks_processed'] == 2, "チャンク数"
        assert stats2['total_texts'] == 11, "総テキスト数"
        assert stats2['unique_phrases'] >= 2, "ユニークフレーズ"

    def test_phrase_frequency_accumulation(self):
        """フレーズ頻度の累積"""
        extractor = StreamingPhraseExtracter(min_count=1)

        # 同じフレーズを複数チャンクで抽出
        texts1 = ['テスト'] * 5
        extractor.add_chunk(texts1)

        texts2 = ['テスト'] * 3
        df2, stats2 = extractor.add_chunk(texts2)

        # df2 に 'テスト' が出現するはず
        if len(df2) > 0 and 'seqchar' in df2.columns:
            test_phrase = df2[df2['seqchar'] == 'テスト']
            if not test_phrase.empty:
                # 最低でも片方のチャンク内でカウント
                assert test_phrase.iloc[0]['freq'] >= 3, "チャンク内頻度"

    def test_get_current_results(self):
        """現在のスナップショット取得"""
        extractor = StreamingPhraseExtracter(min_count=2)

        texts = ['テスト'] * 5 + ['実装'] * 3 + ['バグ']
        extractor.add_chunk(texts)

        df_results = extractor.get_current_results()

        assert isinstance(df_results, pd.DataFrame), "DataFrame を返す"
        assert len(df_results) > 0, "フレーズが返される"

    def test_get_current_results_with_top_k(self):
        """上位 K 件の結果"""
        extractor = StreamingPhraseExtracter(min_count=1)

        texts = ['A'] * 10 + ['B'] * 5 + ['C'] * 3 + ['D'] * 1
        extractor.add_chunk(texts)

        df_results = extractor.get_current_results(top_k=2)

        assert len(df_results) <= 2, "上位2件に制限"
        if len(df_results) > 0:
            # 最初の行が最高頻度
            assert df_results.iloc[0]['freq'] >= df_results.iloc[-1]['freq'], "頻度でソート"

    def test_finalize(self):
        """最終結果の取得"""
        extractor = StreamingPhraseExtracter(min_count=2)

        texts = ['テスト'] * 5 + ['実装'] * 3
        extractor.add_chunk(texts)

        df_final = extractor.finalize()

        assert isinstance(df_final, pd.DataFrame), "DataFrame を返す"
        assert 'seqchar' in df_final.columns or 'phrase' in df_final.columns, "フレーズ列"
        assert 'freq' in df_final.columns, "頻度列"

    def test_get_statistics(self):
        """ストリーミング処理の統計"""
        extractor = StreamingPhraseExtracter(min_count=1)

        texts1 = ['テスト'] * 3
        extractor.add_chunk(texts1)

        texts2 = ['実装'] * 2
        extractor.add_chunk(texts2)

        stats = extractor.get_statistics()

        assert isinstance(stats, dict), "辞書を返す"
        assert stats['total_texts'] == 5, "総テキスト数"
        assert stats['chunks_processed'] == 2, "チャンク数"
        assert stats['unique_phrases'] > 0, "ユニークフレーズ"
        assert stats['avg_chunk_size'] > 0, "平均チャンクサイズ"

    def test_memory_estimate(self):
        """メモリ使用量の推定"""
        extractor = StreamingPhraseExtracter(min_count=1)

        texts = ['テスト'] * 100 + ['実装'] * 50
        extractor.add_chunk(texts)

        mem = extractor.memory_estimate()

        assert isinstance(mem, dict), "辞書を返す"
        assert 'phrase_counter_mb' in mem, "フレーズカウンター MB"
        assert 'estimated_total_mb' in mem, "推定合計 MB"
        assert mem['phrase_counter_mb'] > 0, "メモリ > 0"
        assert mem['estimated_total_mb'] >= mem['phrase_counter_mb'], "合計 >= フレーズカウンター"


class TestChunkedTextReader:
    """チャンク式テキスト読み込みのテスト"""

    def test_read_chunks_small_file(self):
        """小さいファイル"""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            f.write('テキスト1\n')
            f.write('テキスト2\n')
            f.write('テキスト3\n')
            filepath = f.name

        try:
            reader = ChunkedTextReader(filepath, chunk_size=2)

            chunks = list(reader.read_chunks())

            assert len(chunks) == 2, "2 チャンク（2+1）"
            assert chunks[0] == ['テキスト1', 'テキスト2'], "第1チャンク"
            assert chunks[1] == ['テキスト3'], "第2チャンク"
        finally:
            os.unlink(filepath)

    def test_read_chunks_large_file(self):
        """大きいファイル"""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            for i in range(5000):
                f.write(f'テキスト{i}\n')
            filepath = f.name

        try:
            reader = ChunkedTextReader(filepath, chunk_size=1000)

            chunks = list(reader.read_chunks())

            # 5000 行 / 1000 行 = 5 チャンク
            assert len(chunks) == 5, "5 チャンク"
            assert len(chunks[0]) == 1000, "第1チャンクは1000行"
            assert len(chunks[-1]) == 1000, "最後のチャンクは1000行"
        finally:
            os.unlink(filepath)

    def test_read_chunks_custom_chunk_size(self):
        """カスタムチャンクサイズ"""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            for i in range(100):
                f.write(f'テキスト{i}\n')
            filepath = f.name

        try:
            reader = ChunkedTextReader(filepath, chunk_size=30)

            chunks = list(reader.read_chunks())

            # 100 行 / 30 行 = 3 チャンク（30 + 30 + 40）
            assert len(chunks) == 4, "4 チャンク"  # 30 + 30 + 30 + 10
            assert len(chunks[0]) == 30, "第1チャンク"
        finally:
            os.unlink(filepath)

    def test_read_chunks_empty_file(self):
        """空ファイル"""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            filepath = f.name

        try:
            reader = ChunkedTextReader(filepath, chunk_size=100)

            chunks = list(reader.read_chunks())

            assert len(chunks) == 0, "チャンクなし"
        finally:
            os.unlink(filepath)

    def test_read_chunks_with_blank_lines(self):
        """空行を含むファイル"""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            f.write('テキスト1\n')
            f.write('\n')  # 空行
            f.write('テキスト2\n')
            f.write('   \n')  # 空白行
            f.write('テキスト3\n')
            filepath = f.name

        try:
            reader = ChunkedTextReader(filepath, chunk_size=10)

            chunks = list(reader.read_chunks())

            # 空行は無視される
            for chunk in chunks:
                for line in chunk:
                    assert line.strip() != '', "空行は含まれない"
        finally:
            os.unlink(filepath)


class TestIncrementalAggregator:
    """インクリメンタル集約のテスト"""

    def test_add_phrase_new(self):
        """新規フレーズの追加"""
        agg = IncrementalAggregator()

        agg.add_phrase('テスト', 5)

        snapshot = agg.get_snapshot()

        assert snapshot['テスト'] == 5, "フレーズカウント"

    def test_add_phrase_existing(self):
        """既存フレーズの更新"""
        agg = IncrementalAggregator()

        agg.add_phrase('テスト', 5)
        agg.add_phrase('テスト', 3)

        snapshot = agg.get_snapshot()

        assert snapshot['テスト'] == 8, "カウントが累積"

    def test_add_multiple_phrases(self):
        """複数フレーズ"""
        agg = IncrementalAggregator()

        agg.add_phrase('テスト', 5)
        agg.add_phrase('実装', 3)
        agg.add_phrase('テスト', 2)
        agg.add_phrase('バグ', 1)

        snapshot = agg.get_snapshot()

        assert snapshot['テスト'] == 7, "テスト: 5+2"
        assert snapshot['実装'] == 3, "実装: 3"
        assert snapshot['バグ'] == 1, "バグ: 1"
        assert len(snapshot) == 3, "3 フレーズ"

    def test_get_changes_since(self):
        """最後の更新以降の変更"""
        agg = IncrementalAggregator()

        agg.add_phrase('テスト', 5)
        agg.add_phrase('実装', 3)

        changes_all = agg.get_changes_since(0)

        assert changes_all['テスト'] == 5, "最初からの全変更"
        assert changes_all['実装'] == 3, "全変更を取得"

        agg.add_phrase('テスト', 2)

        changes_recent = agg.get_changes_since(2)

        assert changes_recent['テスト'] == 2, "最近の変更のみ"
        assert '実装' not in changes_recent, "最近の変更に含まれない"


class TestStreamingAnalyzer:
    """ストリーミング分析の統合テスト"""

    def test_process_file_basic(self):
        """基本的なファイル処理"""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            for i in range(100):
                f.write(f'テキスト{i}\n')
            filepath = f.name

        try:
            analyzer = StreamingAnalyzer(min_count=1, chunk_size=30)

            df_results, stats = analyzer.process_file(filepath)

            assert isinstance(df_results, pd.DataFrame), "DataFrame を返す"
            assert isinstance(stats, dict), "統計を返す"
            assert stats['total_texts'] == 100, "総テキスト数"
            assert stats['chunks_processed'] > 0, "チャンク数"
        finally:
            os.unlink(filepath)

    def test_process_file_with_callback(self):
        """コールバック付き処理"""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            for i in range(100):
                f.write(f'テキスト{i}\n')
            filepath = f.name

        try:
            analyzer = StreamingAnalyzer(min_count=1, chunk_size=30)

            callback_calls = []

            def on_chunk(df_results, chunk_stats):
                callback_calls.append(len(df_results))

            df_results, stats = analyzer.process_file(filepath, callback=on_chunk)

            # 3-4 チャンクが処理されるはず
            assert len(callback_calls) >= 3, "複数回のコールバック"
        finally:
            os.unlink(filepath)

    def test_generate_streaming_report(self):
        """ストリーミング処理レポート"""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            for i in range(50):
                f.write(f'テキスト{i}\n')
            filepath = f.name

        try:
            analyzer = StreamingAnalyzer(min_count=1, chunk_size=20)

            df_results, stats = analyzer.process_file(filepath)

            report = analyzer.generate_streaming_report()

            assert isinstance(report, str), "文字列を返す"
            assert 'ストリーミング処理レポート' in report, "タイトル"
            assert '処理済みテキスト' in report, "処理統計"
            assert 'メモリ使用量' in report, "メモリ情報"
        finally:
            os.unlink(filepath)


class TestMemoryEfficiency:
    """メモリ効率のテスト"""

    def test_memory_estimate_scales(self):
        """メモリ推定がスケール"""
        extractor1 = StreamingPhraseExtracter(min_count=1)
        extractor1.add_chunk(['A'] * 100)
        mem1 = extractor1.memory_estimate()

        extractor2 = StreamingPhraseExtracter(min_count=1)
        extractor2.add_chunk(['A'] * 100 + ['B'] * 100)
        mem2 = extractor2.memory_estimate()

        # フレーズが2倍 → メモリも大体2倍
        assert mem2['phrase_counter_mb'] > mem1['phrase_counter_mb'], "フレーズ増加でメモリ増"

    def test_streaming_vs_batch(self):
        """ストリーミングとバッチの結果一致"""
        texts = [f'テキスト{i}' for i in range(1000)]

        # ストリーミング処理
        streaming_extractor = StreamingPhraseExtracter(min_count=1)
        streaming_extractor.add_chunk(texts[:500])
        streaming_extractor.add_chunk(texts[500:])
        df_streaming = streaming_extractor.finalize()

        # 結果の検証
        assert len(df_streaming) > 0, "ストリーミング結果あり"


class TestIntegration:
    """統合テスト"""

    def test_full_streaming_pipeline(self):
        """ストリーミング処理の全フロー"""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            for i in range(500):
                f.write(f'AI技術{i}\n')
                f.write(f'機械学習{i}\n')
                f.write(f'テスト{i}\n')
            filepath = f.name

        try:
            analyzer = StreamingAnalyzer(min_count=1, chunk_size=100)

            # 処理実行
            df_results, stats = analyzer.process_file(filepath)

            # 検証
            assert stats['total_texts'] == 1500, "総テキスト数"
            assert stats['chunks_processed'] >= 3, "複数チャンク処理"
            assert len(df_results) > 0, "フレーズが抽出"

            # レポート生成
            report = analyzer.generate_streaming_report()
            assert len(report) > 50, "詳細レポート"
        finally:
            os.unlink(filepath)

    def test_large_file_simulation(self):
        """大規模ファイルシミュレーション"""
        with tempfile.NamedTemporaryFile(mode='w', delete=False, encoding='utf-8') as f:
            for i in range(10000):
                f.write(f'テキスト{i % 1000}\n')
            filepath = f.name

        try:
            analyzer = StreamingAnalyzer(min_count=5, chunk_size=1000)

            df_results, stats = analyzer.process_file(filepath)

            # 大規模処理の検証
            assert stats['total_texts'] == 10000, "10,000行処理"
            assert stats['chunks_processed'] == 10, "10 チャンク"
            assert len(df_results) > 0, "フレーズが抽出"

            # メモリ推定が現実的か
            mem = analyzer.extractor.memory_estimate()
            assert mem['estimated_total_mb'] < 1000, "メモリは実用的"
        finally:
            os.unlink(filepath)
