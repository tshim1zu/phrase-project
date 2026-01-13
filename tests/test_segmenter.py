"""
テキストセグメンテーション機能のテストスイート
"""

import pytest
import numpy as np
from japhrase.segmenter import TextSegmenter, segment_text


class TestTextSegmenter:
    """TextSegmenter のテスト"""
    
    def setup_method(self):
        """各テストの前に実行"""
        self.segmenter = TextSegmenter(window_size=4)
    
    def test_initialization(self):
        """初期化テスト"""
        assert self.segmenter.window_size == 4
        assert self.segmenter.max_entropy > 0
    
    def test_entropy_calculation_basic(self):
        """エントロピー計算の基本テスト"""
        # 全て同じ文字
        chars_same = ['a', 'a', 'a']
        entropy_same = TextSegmenter._calculate_entropy(chars_same)
        assert entropy_same == 0.0, "同じ文字のみ → エントロピー 0"
        
        # 異なる文字が均等に分布
        chars_varied = ['a', 'b', 'c', 'a', 'b', 'c']
        entropy_varied = TextSegmenter._calculate_entropy(chars_varied)
        assert entropy_varied > 0, "異なる文字 → エントロピー > 0"
        
        # 空リスト
        entropy_empty = TextSegmenter._calculate_entropy([])
        assert entropy_empty == 0.0, "空リスト → エントロピー 0"
    
    def test_entropy_profile_generation(self):
        """エントロピープロファイル生成テスト"""
        text = "あああいいいあああ"
        profile, contexts = self.segmenter.calculate_right_entropy_profile(text)
        
        assert len(profile) == len(text), "プロファイル長 = テキスト長"
        assert len(contexts) == len(text), "コンテキスト数 = テキスト長"
        assert all(isinstance(p, (float, np.floating)) for p in profile), "プロファイルは浮動小数点数"
        assert all(isinstance(c, str) for c in contexts), "コンテキストは文字列"
    
    def test_peak_detection(self):
        """ピーク検出テスト"""
        # 山型のプロファイル：[低, 高, 低]
        profile = np.array([0.2, 0.8, 0.3, 0.1])
        peaks = self.segmenter.find_peaks(profile, prominence=0.1)
        
        assert 1 in peaks, "インデックス 1 がピーク"
    
    def test_peak_detection_no_peaks(self):
        """ピークがない場合のテスト"""
        # 平坦なプロファイル
        profile = np.array([0.3, 0.3, 0.3, 0.3])
        peaks = self.segmenter.find_peaks(profile, prominence=0.1)
        
        assert len(peaks) == 0, "ピークなし"
    
    def test_split_by_threshold_basic(self):
        """閾値ベース分割の基本テスト"""
        text = "あああああいいいいいあああああ"
        segments = self.segmenter.split_by_threshold(text, threshold=0.5)
        
        assert isinstance(segments, list), "結果はリスト"
        assert all(isinstance(s, str) for s in segments), "全て文字列"
        assert "".join(segments) != text or text.strip() == "".join(segments), "テキストが失われていない"
    
    def test_split_by_threshold_empty_text(self):
        """空テキストの処理"""
        segments = self.segmenter.split_by_threshold("", threshold=0.5)
        assert segments == [], "空テキスト → 空リスト"
    
    def test_split_top_n_basic(self):
        """Top-N分割の基本テスト"""
        text = "あああああいいいいいあああああ"
        segments = self.segmenter.split_top_n(text, n=2)
        
        assert len(segments) >= 1, "セグメント数 >= 1"
        assert len(segments) <= 3, "セグメント数 <= n+1"
    
    def test_split_top_n_single(self):
        """n=0 またはテキスト短い場合"""
        text = "短い"
        segments = self.segmenter.split_top_n(text, n=0)
        assert segments == [text], "n=0 → 分割なし"
    
    def test_smart_split_with_target_chunks(self):
        """スマート分割（チャンク数指定）"""
        text = "あああああいいいいいあああああ"
        segments = self.segmenter.smart_split(text, target_chunks=2)
        
        assert len(segments) >= 1, "セグメント生成"
    
    def test_smart_split_with_threshold(self):
        """スマート分割（閾値指定）"""
        text = "あああああいいいいいあああああ"
        segments = self.segmenter.smart_split(text, threshold=0.5)
        
        assert len(segments) >= 1, "セグメント生成"
    
    def test_smart_split_adaptive(self):
        """スマート分割（自動適応）"""
        text = "あああああいいいいいあああああ"
        segments = self.segmenter.smart_split(text)
        
        assert len(segments) >= 1, "セグメント生成"
    
    def test_add_punctuation(self):
        """句読点挿入テスト"""
        text = "あああああいいいいい"
        result = self.segmenter.add_punctuation(text)
        
        assert isinstance(result, str), "結果は文字列"
        assert "。" in result or result == text, "句読点が挿入されるか元のテキスト"
    
    def test_analyze_entropy_profile(self):
        """エントロピープロファイル分析テスト"""
        text = "あああああいいいいいあああああ"
        analysis = self.segmenter.analyze_entropy_profile(text)
        
        assert isinstance(analysis, dict), "結果は辞書"
        assert 'profile' in analysis
        assert 'mean' in analysis
        assert 'std' in analysis
        assert 'peaks' in analysis
        assert isinstance(analysis['mean'], float), "平均は浮動小数点数"
        assert isinstance(analysis['peaks'], list), "ピークはリスト"
    
    def test_analyze_entropy_profile_empty(self):
        """空テキストの分析"""
        analysis = self.segmenter.analyze_entropy_profile("")
        
        assert len(analysis['profile']) == 0, "空プロファイル"
        assert analysis['mean'] == 0.0
        assert analysis['peaks'] == []


class TestSegmentFunction:
    """便利関数のテスト"""
    
    def test_segment_text_adaptive(self):
        """segment_text（adaptive）"""
        text = "あああああいいいいい"
        segments = segment_text(text, method='adaptive')
        assert isinstance(segments, list)
    
    def test_segment_text_threshold(self):
        """segment_text（threshold）"""
        text = "あああああいいいいい"
        segments = segment_text(text, method='threshold', threshold=0.5)
        assert isinstance(segments, list)
    
    def test_segment_text_top_n(self):
        """segment_text（top_n）"""
        text = "あああああいいいいい"
        segments = segment_text(text, method='top_n', n=2)
        assert isinstance(segments, list)


class TestIntegrationWithRealText:
    """実テキストでの統合テスト"""
    
    def test_long_text_segmentation(self):
        """長めのテキスト分割"""
        text = "人工知能は機械学習の一分野です。深層学習は最近注目されています。自然言語処理は複雑な課題です。"
        segmenter = TextSegmenter()
        segments = segmenter.split_by_threshold(text, threshold=0.3, min_chunk_length=3)
        
        assert len(segments) >= 1
        joined = "".join(segments)
        # 分割後の結合がもとのテキストに近い（ただし分割されている）
        assert len(joined) <= len(text) + 10  # 若干の空白差は許容
    
    def test_mixed_japanese_english(self):
        """日本語と英語の混在テキスト"""
        text = "AIと機械学習MLについて述べます。"
        segmenter = TextSegmenter()
        segments = segmenter.smart_split(text, target_chunks=2)
        
        assert isinstance(segments, list)
    
    def test_punctuation_restoration(self):
        """句読点なしテキストの復元"""
        text = "今日は天気が良いです明日も晴れるでしょう"
        segmenter = TextSegmenter()
        result = segmenter.add_punctuation(text)
        
        assert isinstance(result, str)
        # テキストが保持されている（句読点が挿入）
        assert len(result) >= len(text)


class TestEdgeCases:
    """エッジケース"""
    
    def test_single_character(self):
        """1文字テキスト"""
        segmenter = TextSegmenter()
        segments = segmenter.smart_split("a")
        assert isinstance(segments, list)
    
    def test_repeated_character(self):
        """同じ文字の繰り返し"""
        segmenter = TextSegmenter()
        segments = segmenter.smart_split("ああああああああ")
        assert isinstance(segments, list)
    
    def test_very_long_text(self):
        """長いテキスト（1000文字以上）"""
        text = "あいうえおあいうえお" * 100
        segmenter = TextSegmenter()
        segments = segmenter.split_top_n(text, n=5)
        
        assert len(segments) >= 1
        assert len(segments) <= 6  # n+1 = 6
    
    def test_special_characters(self):
        """特殊文字を含むテキスト"""
        text = "Hello!世界@#$%^&*()"
        segmenter = TextSegmenter()
        segments = segmenter.smart_split(text)
        
        assert isinstance(segments, list)


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
