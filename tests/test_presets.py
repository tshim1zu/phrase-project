"""
Tests for preset functionality
"""

import pytest
from japhrase import PhraseExtracter, PRESETS


class TestPresets:
    """Test preset functionality"""

    def test_presets_constant_exists(self):
        """Test that PRESETS constant is available"""
        assert PRESETS is not None
        assert isinstance(PRESETS, dict)

    def test_presets_has_required_presets(self):
        """Test that required presets exist"""
        assert 'sns' in PRESETS
        assert 'news' in PRESETS
        assert 'novel' in PRESETS
        assert 'report' in PRESETS
        assert 'default' in PRESETS

    def test_preset_structure(self):
        """Test that each preset has required keys"""
        for preset_name, config in PRESETS.items():
            assert 'min_count' in config
            assert 'max_length' in config
            assert 'min_length' in config
            assert 'threshold_originality' in config
            assert 'description' in config

    def test_preset_values_are_valid(self):
        """Test that preset values are valid"""
        for preset_name, config in PRESETS.items():
            assert isinstance(config['min_count'], int)
            assert config['min_count'] > 0
            assert isinstance(config['max_length'], int)
            assert config['max_length'] > 0
            assert isinstance(config['min_length'], int)
            assert config['min_length'] > 0
            assert isinstance(config['threshold_originality'], (int, float))
            assert 0 <= config['threshold_originality'] <= 1
            assert isinstance(config['description'], str)

    def test_preset_method_sns(self):
        """Test creating extractor with sns preset"""
        extractor = PhraseExtracter.preset('sns')
        assert extractor.min_count == 6
        assert extractor.max_length == 10  # +1 added internally
        assert extractor.min_length == 5
        assert extractor.threshold_originality == 0.52

    def test_preset_method_news(self):
        """Test creating extractor with news preset"""
        extractor = PhraseExtracter.preset('news')
        assert extractor.min_count == 5
        assert extractor.max_length == 11  # +1 added internally
        assert extractor.min_length == 3
        assert extractor.threshold_originality == 0.64

    def test_preset_method_default(self):
        """Test creating extractor with default preset"""
        extractor = PhraseExtracter.preset('default')
        assert extractor.min_count == 6
        assert extractor.max_length == 17  # +1 added internally
        assert extractor.min_length == 4
        assert extractor.threshold_originality == 0.5

    def test_preset_method_novel(self):
        """Test creating extractor with novel preset"""
        extractor = PhraseExtracter.preset('novel')
        assert extractor.min_count == 4
        assert extractor.max_length == 17  # +1 added internally
        assert extractor.min_length == 3
        assert extractor.threshold_originality == 0.6

    def test_preset_method_report(self):
        """Test creating extractor with report preset"""
        extractor = PhraseExtracter.preset('report')
        assert extractor.min_count == 10
        assert extractor.max_length == 25  # +1 added internally
        assert extractor.min_length == 4
        assert extractor.threshold_originality == 0.78

    def test_preset_with_override(self):
        """Test overriding preset parameters"""
        extractor = PhraseExtracter.preset('sns', min_count=10)
        assert extractor.min_count == 10  # overridden
        assert extractor.max_length == 10  # from preset (+1)
        assert extractor.min_length == 5  # from preset

    def test_preset_invalid_name(self):
        """Test error on invalid preset name"""
        with pytest.raises(ValueError) as exc_info:
            PhraseExtracter.preset('invalid_preset')
        assert 'Unknown preset' in str(exc_info.value)
        assert 'invalid_preset' in str(exc_info.value)

    def test_list_presets_method(self):
        """Test list_presets method runs without error"""
        # Just verify it doesn't crash
        PhraseExtracter.list_presets()

    def test_preset_with_extraction(self):
        """Test that preset extractor can be used for extraction"""
        import pandas as pd

        sample_texts = [
            "ありがとうございます",
            "よろしくお願いします",
            "キャンペーン開催中",
        ] * 10  # Repeat many times

        # Use sns preset with adjusted parameters for small test dataset
        extractor = PhraseExtracter.preset('sns', min_count=3, min_length=2)
        df = extractor.get_dfphrase(sample_texts)

        # Should return a DataFrame (even if empty)
        assert isinstance(df, pd.DataFrame)
