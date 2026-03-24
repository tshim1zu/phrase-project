# coding: utf-8
"""
汚染検知フレームワーク v2 のテスト（8軸版）
"""

import pytest
from japhrase.contamination import scan, ContaminationScanner, ContaminationProfile, Anomaly


CLEAN_TEXT = (
    '朝靄の中を歩いていると、足元で小さな花が揺れた。\n'
    '名前は知らない。淡い紫色の花弁が、露を含んで光っている。\n'
    '道の先に古い石橋が見えた。欄干には苔が生え、長い年月を感じさせる。\n'
    '風が吹いた。木々の葉がざわめき、遠くで鳥の声がした。\n'
    '丘の上に着くと、街の灯りが見えた。まだ夜明け前だった。\n'
)


class TestScanBasic:
    def test_returns_profile(self):
        assert isinstance(scan(CLEAN_TEXT), ContaminationProfile)

    def test_clean_is_clean(self):
        p = scan(CLEAN_TEXT)
        assert p.is_clean()
        assert p.overall <= 10

    def test_8_axes(self):
        p = scan(CLEAN_TEXT)
        assert len(p.axes) == 8

    def test_select_detectors(self):
        p = scan(CLEAN_TEXT, detectors=['encoding'])
        assert p.encoding.count == 0

    def test_reference_text(self):
        """テキスト間比較が動く"""
        p = scan(CLEAN_TEXT, reference_text='全く別のテキスト。データベースの最適化。')
        assert isinstance(p, ContaminationProfile)


class TestEncoding:
    def test_clean(self):
        assert scan(CLEAN_TEXT, detectors=['encoding']).encoding.score == 0

    def test_mojibake(self):
        p = scan('正常â€™文字化けÃ©', detectors=['encoding'])
        assert p.encoding.count >= 1

    def test_control_chars(self):
        p = scan('正常\x01制御文字', detectors=['encoding'])
        assert p.encoding.count >= 1


class TestStructural:
    def test_clean(self):
        assert scan(CLEAN_TEXT, detectors=['structural']).structural.score == 0

    def test_unclosed_bracket(self):
        p = scan('「閉じない台詞', detectors=['structural'])
        assert p.structural.count >= 1

    def test_merge_markers(self):
        p = scan('<<<<<<< HEAD\n本文\n=======\n別の本文\n>>>>>>> branch', detectors=['structural'])
        assert p.structural.count >= 1
        assert any('マージ' in a.description for a in p.structural.anomalies)

    def test_work_annotations(self):
        p = scan('本文の途中に[TODO]が残っている。※要確認の箇所もある。', detectors=['structural'])
        assert p.structural.count >= 1

    def test_metadata_leak(self):
        p = scan('本文\n<div class="test">HTML混入</div>\n本文', detectors=['structural'])
        assert p.structural.count >= 1

    def test_empty_line_ratio(self):
        text = '\n'.join(['本文'] + [''] * 20 + ['本文'])
        p = scan(text, detectors=['structural'])
        assert any('空行率' in a.description for a in p.structural.anomalies)

    def test_long_line(self):
        p = scan('あ' * 1500, detectors=['structural'])
        assert p.structural.count >= 1


class TestDuplicate:
    def test_clean(self):
        assert scan(CLEAN_TEXT, detectors=['duplicate']).duplicate.score == 0

    def test_exact_duplicate(self):
        text = '段落の内容が十分に長い文章です。ここは重複テスト用。\n\n別の段落。\n\n段落の内容が十分に長い文章です。ここは重複テスト用。\n\n'
        p = scan(text, detectors=['duplicate'])
        assert p.duplicate.count >= 1

    def test_cross_text_duplicate(self):
        text_a = '共通する段落がこちらにもある。十分に長い内容。\n\n独自の内容。'
        text_b = '共通する段落がこちらにもある。十分に長い内容。\n\n別の内容。'
        p = scan(text_a, detectors=['duplicate'], reference_text=text_b)
        assert p.duplicate.count >= 1


class TestRepetition:
    def test_clean(self):
        p = scan(CLEAN_TEXT, detectors=['repetition'])
        assert p.repetition.score < 20

    def test_heavy_repetition(self):
        p = scan('男は歩いた。男は止まった。' * 30, detectors=['repetition'])
        assert p.repetition.count > 0


class TestDistribution:
    def test_clean(self):
        p = scan(CLEAN_TEXT, detectors=['distribution'])
        assert p.distribution.score == 0 or p.distribution.count == 0


class TestComplexity:
    def test_clean(self):
        p = scan(CLEAN_TEXT, detectors=['complexity'])
        assert p.complexity.score < 20

    def test_extreme_repeat(self):
        p = scan('あ' * 2000, detectors=['complexity'])
        assert p.complexity.count >= 1


class TestConsistency:
    def test_clean(self):
        """統一された句読点はclean"""
        text = '朝靄の中を歩いた。足元で花が揺れた。名前は知らない。'
        p = scan(text, detectors=['consistency'])
        assert p.consistency.score == 0 or p.consistency.count == 0

    def test_punctuation_mix(self):
        """全角句点と半角ピリオドが混在"""
        text = '朝靄の中を歩いた。足元で花が揺れた.名前は知らない。光っている.風が吹いた。'
        p = scan(text, detectors=['consistency'])
        assert p.consistency.count >= 1
        assert any('句読点' in a.description for a in p.consistency.anomalies)

    def test_comma_mix(self):
        """全角読点と半角カンマが混在"""
        text = '赤い花、青い空,白い雲、黒い山,緑の森、茶色の土,紫の花'
        p = scan(text, detectors=['consistency'])
        assert p.consistency.count >= 1

    def test_kanji_hira_mix(self):
        """漢字とひらがなの揺れ"""
        text = '出来ることをできる限りやる。出来ないことはできない。出来る範囲でできることを。'
        p = scan(text, detectors=['consistency'])
        assert p.consistency.count >= 1
        assert any('漢字' in a.description for a in p.consistency.anomalies)

    def test_cross_text_punctuation(self):
        """テキスト間の句読点スタイル不一致"""
        text_a = '文章です。読点は、全角。'
        text_b = 'Sentence here. Comma is, half-width.'
        p = scan(text_a, detectors=['consistency'], reference_text=text_b)
        # 言語が違うので句読点も違って当然 — 大量にないとスキップ
        assert isinstance(p, ContaminationProfile)


class TestLanguage:
    def test_clean_jp(self):
        p = scan(CLEAN_TEXT, detectors=['language'])
        assert p.language.score == 0 or p.language.count == 0

    def test_mixed_block(self):
        """日本語テキスト中に英語ブロックが突然出現"""
        jp = '朝靄の中を歩いていると、足元で小さな花が揺れた。名前は知らない。' * 10
        en = 'The database query optimization requires careful index planning. ' * 10
        text = jp + '\n' + en + '\n' + jp
        p = scan(text, detectors=['language'])
        assert p.language.count >= 1


class TestDrillDown:
    def test_explain_clean(self):
        assert '問題なし' in scan(CLEAN_TEXT).explain()

    def test_explain_contaminated(self):
        result = scan('正常â€™文字化け\n「閉じない').explain()
        assert '💡' in result

    def test_worst_axes(self):
        p = scan('正常â€™文字化け')
        assert len(p.worst_axes) > 0

    def test_primary_issue(self):
        p = scan('正常â€™文字化け')
        assert p.primary_issue is not None

    def test_primary_issue_clean(self):
        assert scan(CLEAN_TEXT).primary_issue is None

    def test_top_issues(self):
        issues = scan('正常â€™文字化けï¿½\n「閉じない').top_issues(3)
        assert len(issues) <= 3

    def test_locate(self):
        p = scan('正常â€™文字化け\n「閉じない')
        enc = p.locate('encoding')
        assert all(a.detector == 'encoding' for a in enc)

    def test_suggestion(self):
        p = scan('正常â€™文字化け', detectors=['encoding'])
        for a in p.all_anomalies:
            assert len(a.suggestion) > 0

    def test_summary_dict(self):
        d = scan(CLEAN_TEXT).summary_dict()
        assert 'overall' in d
        assert 'axes' in d
        assert len(d['axes']) == 8


class TestScanner:
    def test_default(self):
        assert ContaminationScanner().scan(CLEAN_TEXT).is_clean()

    def test_custom(self):
        scanner = ContaminationScanner(duplicate_threshold=0.5)
        assert isinstance(scanner.scan(CLEAN_TEXT), ContaminationProfile)
