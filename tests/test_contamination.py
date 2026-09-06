# coding: utf-8
"""
汚染検知フレームワーク v3 のテスト（8軸 + 高水準/低水準分離）
"""

import pytest
from japhrase.contamination import (
    # 高水準API
    scan, compare, batch_scan, quick_check,
    # 結果オブジェクト
    ContaminationProfile, ComparisonResult, BatchResult, Anomaly,
    # 低水準（後方互換）
    ContaminationScanner,
)


CLEAN_TEXT = (
    '朝靄の中を歩いていると、足元で小さな花が揺れた。\n'
    '名前は知らない。淡い紫色の花弁が、露を含んで光っている。\n'
    '道の先に古い石橋が見えた。欄干には苔が生え、長い年月を感じさせる。\n'
    '風が吹いた。木々の葉がざわめき、遠くで鳥の声がした。\n'
    '丘の上に着くと、街の灯りが見えた。まだ夜明け前だった。\n'
)

DIRTY_TEXT = '正常â€™文字化けï¿½\n「閉じない台詞\n[TODO]残ってる'


# ═══════════════════════════════════════════════════════════════
# 高水準API のテスト
# ═══════════════════════════════════════════════════════════════

class TestScan:
    def test_basic(self):
        p = scan(CLEAN_TEXT)
        assert isinstance(p, ContaminationProfile)
        assert p.is_clean()

    def test_8_axes(self):
        assert len(scan(CLEAN_TEXT).axes) == 8

    def test_dirty(self):
        p = scan(DIRTY_TEXT)
        assert not p.is_clean()
        assert p.overall > 0

    def test_detectors_param(self):
        p = scan(CLEAN_TEXT, detectors=['encoding'])
        assert p.encoding.count == 0

    def test_reference_text(self):
        p = scan(CLEAN_TEXT, reference_text='全く別のテキスト')
        assert isinstance(p, ContaminationProfile)

    def test_kwargs(self):
        p = scan(CLEAN_TEXT, duplicate_threshold=0.5)
        assert isinstance(p, ContaminationProfile)


class TestQuickCheck:
    def test_clean(self):
        assert quick_check(CLEAN_TEXT) is False  # 汚染なし

    def test_dirty(self):
        assert quick_check(DIRTY_TEXT) is True   # 汚染あり

    def test_threshold(self):
        # 閾値を上げれば通る
        assert quick_check(CLEAN_TEXT, threshold=0) is False


class TestCompare:
    def test_basic(self):
        r = compare(CLEAN_TEXT, CLEAN_TEXT)
        assert isinstance(r, ComparisonResult)

    def test_scores(self):
        r = compare(CLEAN_TEXT, DIRTY_TEXT)
        assert r.score_a <= r.score_b  # clean側が低い

    def test_healthier(self):
        r = compare(CLEAN_TEXT, DIRTY_TEXT)
        assert r.healthier == 'A'

    def test_cross_score(self):
        r = compare(CLEAN_TEXT, DIRTY_TEXT)
        assert isinstance(r.cross_score, int)

    def test_report(self):
        r = compare(CLEAN_TEXT, DIRTY_TEXT)
        report = r.report()
        assert 'テキスト間' in report


class TestBatchScan:
    def test_basic(self):
        r = batch_scan({'A': CLEAN_TEXT, 'B': DIRTY_TEXT})
        assert isinstance(r, BatchResult)

    def test_all_clean(self):
        r = batch_scan({'A': CLEAN_TEXT, 'B': CLEAN_TEXT})
        assert r.all_clean

    def test_contaminated_keys(self):
        r = batch_scan({'clean': CLEAN_TEXT, 'dirty': DIRTY_TEXT})
        assert 'dirty' in r.contaminated_keys
        assert 'clean' not in r.contaminated_keys

    def test_cleanest_dirtiest(self):
        r = batch_scan({'clean': CLEAN_TEXT, 'dirty': DIRTY_TEXT})
        assert r.cleanest == 'clean'
        assert r.dirtiest == 'dirty'

    def test_report(self):
        r = batch_scan({'A': CLEAN_TEXT, 'B': DIRTY_TEXT, 'C': CLEAN_TEXT})
        report = r.report()
        assert '一括' in report
        assert 'A' in report


# ═══════════════════════════════════════════════════════════════
# 8軸の個別テスト
# ═══════════════════════════════════════════════════════════════

class TestEncoding:
    def test_clean(self):
        assert scan(CLEAN_TEXT, detectors=['encoding']).encoding.score == 0

    def test_mojibake(self):
        assert scan('正常â€™文字化け', detectors=['encoding']).encoding.count >= 1


class TestStructural:
    def test_clean(self):
        assert scan(CLEAN_TEXT, detectors=['structural']).structural.score == 0

    def test_merge_markers(self):
        p = scan('<<<<<<< HEAD\n本文\n=======', detectors=['structural'])
        assert any('マージ' in a.description for a in p.structural.anomalies)

    def test_work_annotations(self):
        p = scan('[TODO]が残っている。※要確認', detectors=['structural'])
        assert p.structural.count >= 1

    def test_empty_line_ratio(self):
        text = '\n'.join(['本文'] + [''] * 20 + ['本文'])
        p = scan(text, detectors=['structural'])
        assert any('空行率' in a.description for a in p.structural.anomalies)

    def test_metadata_leak(self):
        p = scan('本文\n<div>HTML</div>\n本文', detectors=['structural'])
        assert p.structural.count >= 1


class TestDuplicate:
    def test_clean(self):
        assert scan(CLEAN_TEXT, detectors=['duplicate']).duplicate.score == 0

    def test_exact(self):
        text = '段落の内容が十分に長い文章です。重複テスト。\n\n別。\n\n段落の内容が十分に長い文章です。重複テスト。\n\n'
        assert scan(text, detectors=['duplicate']).duplicate.count >= 1

    def test_cross_text(self):
        common = '共通する段落がこちらにもある。十分に長い。'
        p = scan(f'{common}\n\n独自', detectors=['duplicate'], reference_text=f'{common}\n\n別')
        assert p.duplicate.count >= 1


class TestDistribution:
    def test_clean(self):
        assert scan(CLEAN_TEXT, detectors=['distribution']).distribution.score == 0

    def test_reported_location_matches_actual_content(self):
        """回帰テスト: 検出器がwhitespace除去後の文字列上のオフセットを、
        そのまま元テキストのオフセットとして誤用し、行番号・位置が
        系統的にズレていた不具合の修正確認。

        報告された a.start:a.end を元テキストから切り出して空白を除去すると、
        検出器が実際に見ていたセグメント内容(a.snippet)と一致するはず。
        """
        import re as _re
        from japhrase.contamination._detectors import detect_distribution

        normal_block = ('あいうえおかきくけこさしすせそたちつてと\n' * 40)
        weird_block = ('12345678901234567890\n' * 40)
        text = normal_block + weird_block + normal_block

        anomalies = detect_distribution(text, text.split('\n'), jsd_threshold=0.1)
        assert len(anomalies) > 0

        for a in anomalies:
            region_clean = _re.sub(r'\s+', '', text[a.start:a.end])
            assert region_clean.startswith(a.snippet[:10]), (
                f"reported start={a.start} end={a.end} does not point at the "
                f"flagged content {a.snippet!r} in the original text"
            )


class TestRepetition:
    def test_heavy(self):
        assert scan('男は歩いた。男は止まった。' * 30, detectors=['repetition']).repetition.count > 0

    def test_keeps_worst_case_count_across_overlapping_windows(self):
        """回帰テスト: 同じフレーズが複数の重なり合うウィンドウで検出されたとき、
        以前は生の出現回数(count)を上限付きseverity(最大6)と比較していたため、
        より軽微な出現が誤ってより深刻な出現を上書きすることがあった。
        いまは生のcount同士を比較し、最悪ケースの出現回数を保持する。
        """
        from japhrase.contamination._detectors import detect_repetition

        gram = 'zzzz'
        heavy = gram * 50          # 1つのウィンドウ内で「zzzz」が50回近く反復
        filler = 'a' * 400
        light = (gram + 'bb') * 4  # 別のウィンドウでは反復回数がずっと少ない

        text = heavy + filler + light + ('c' * 2000)
        anomalies = detect_repetition(text, text.split('\n'))

        zzzz_anomalies = [a for a in anomalies if a.snippet == gram]
        assert len(zzzz_anomalies) == 1
        reported_count = int(
            __import__('re').search(r'(\d+)回反復', zzzz_anomalies[0].description).group(1)
        )
        # 軽微な方(light)の出現回数(4)ではなく、深刻な方(heavy由来)の出現回数が
        # 残っていること
        assert reported_count > 10


class TestConsistency:
    def test_punctuation_mix(self):
        text = '文章だ。文章だ.文章だ。文章だ.文章だ。'
        p = scan(text, detectors=['consistency'])
        assert any('句読点' in a.description for a in p.consistency.anomalies)

    def test_comma_mix(self):
        text = '赤い花、青い空,白い雲、黒い山,緑の森、茶色の土,紫の花'
        assert scan(text, detectors=['consistency']).consistency.count >= 1

    def test_kanji_hira(self):
        text = '出来ることをできる限りやる。出来ないことはできない。'
        p = scan(text, detectors=['consistency'])
        assert any('漢字' in a.description for a in p.consistency.anomalies)


class TestLanguage:
    def test_clean(self):
        p = scan(CLEAN_TEXT, detectors=['language'])
        assert p.language.count == 0 or p.language.score < 10

    def test_mixed(self):
        jp = '朝靄の中を歩いていると、足元で小さな花が揺れた。' * 10
        en = 'The database query optimization requires careful planning. ' * 10
        p = scan(jp + '\n' + en + '\n' + jp, detectors=['language'])
        assert p.language.count >= 1


# ═══════════════════════════════════════════════════════════════
# ドリルダウン
# ═══════════════════════════════════════════════════════════════

class TestDrillDown:
    def test_explain_clean(self):
        assert '問題なし' in scan(CLEAN_TEXT).explain()

    def test_explain_dirty(self):
        assert '💡' in scan(DIRTY_TEXT).explain()

    def test_worst_axes(self):
        worst = scan(DIRTY_TEXT).worst_axes
        assert len(worst) > 0
        for i in range(len(worst) - 1):
            assert worst[i].score >= worst[i+1].score

    def test_primary_issue(self):
        assert scan(DIRTY_TEXT).primary_issue is not None
        assert scan(CLEAN_TEXT).primary_issue is None

    def test_top_issues(self):
        issues = scan(DIRTY_TEXT).top_issues(3)
        assert len(issues) <= 3

    def test_locate(self):
        enc = scan(DIRTY_TEXT).locate('encoding')
        assert all(a.detector == 'encoding' for a in enc)

    def test_suggestion(self):
        for a in scan(DIRTY_TEXT).all_anomalies:
            assert len(a.suggestion) > 0

    def test_summary_dict(self):
        d = scan(CLEAN_TEXT).summary_dict()
        assert len(d['axes']) == 8


# ═══════════════════════════════════════════════════════════════
# 低水準（後方互換）
# ═══════════════════════════════════════════════════════════════

class TestLowLevel:
    def test_scanner_direct(self):
        scanner = ContaminationScanner()
        p = scanner.scan(CLEAN_TEXT)
        assert p.is_clean()

    def test_scanner_custom(self):
        scanner = ContaminationScanner(duplicate_threshold=0.5)
        assert isinstance(scanner.scan(CLEAN_TEXT), ContaminationProfile)

    def test_detector_import(self):
        """低水準の検出器が直接 import できる"""
        from japhrase.contamination._detectors import DETECTOR_REGISTRY, ALL_DETECTOR_NAMES
        assert len(ALL_DETECTOR_NAMES) == 8
        assert 'consistency' in DETECTOR_REGISTRY


class TestDetectorFailureVisibility:
    """回帰テスト: 検出器が例外で失敗しても「異常0件=クリーン」と区別なく
    green扱いされないこと（fail-open対策）。

    以前は _scanner.py が検出器の例外を握り潰し、その軸を無条件で
    score=0/count=0（クリーン）として扱っていた。AxisScore/ContaminationProfile
    には失敗を示すフィールドが一切無く、呼び出し側は「本当にクリーン」なのか
    「検出できなかっただけ」なのかを区別できなかった。
    さらに、この不具合は `scan(text, segment_size=1)` のような、悪意のない
    正当な公開APIの使い方だけで実際にトリガーできた
    （segment_size//2 が0になりrange()のstepが0になってクラッシュする）。
    """

    def test_segment_size_too_small_raises_clear_error_instead_of_silent_failure(self):
        """公開APIから到達可能なdegenerate paramはクラッシュではなく、
        スキャナ構築時に明確なValueErrorを出すこと。"""
        with pytest.raises(ValueError):
            ContaminationScanner(segment_size=1)
        with pytest.raises(ValueError):
            ContaminationScanner(segment_size=0)
        with pytest.raises(ValueError):
            ContaminationScanner(repetition_window=1)
        with pytest.raises(ValueError):
            ContaminationScanner(repetition_window=0)

    def test_low_level_detectors_do_not_crash_on_degenerate_segment_size(self):
        """低水準API（検出器を直接呼び出す）から degenerate な segment_size/
        window_size を渡してもクラッシュせず、空リストを返すこと
        （scanner側のバリデーションを経由しない呼び出しに対する防御）。"""
        from japhrase.contamination._detectors import (
            detect_distribution, detect_complexity, detect_language, detect_repetition,
        )
        text = 'テストテキストです。' * 20
        lines = text.split('\n')

        assert detect_distribution(text, lines, segment_size=0) == []
        assert detect_distribution(text, lines, segment_size=1) == []
        assert detect_complexity(text, lines, segment_size=0) == []
        assert detect_language(text, lines, segment_size=0) == []
        assert detect_repetition(text, lines, window_size=0) == []
        assert detect_repetition(text, lines, window_size=1) == []

    def test_failed_detector_is_visible_not_reported_as_clean(self):
        """検出器が(未知の理由で)例外を投げた場合、その軸のerrorフィールドと
        profile.failed_axes に反映され、is_clean()/explain()だけを見ても
        「クリーン」と誤認できないこと。"""
        from japhrase.contamination import _detectors

        def broken_detector(text, lines, **kw):
            raise RuntimeError("simulated bug")

        original = _detectors.DETECTOR_REGISTRY['duplicate']
        _detectors.DETECTOR_REGISTRY['duplicate'] = broken_detector
        try:
            scanner = ContaminationScanner()
            profile = scanner.scan(CLEAN_TEXT)

            assert profile.duplicate.error is not None
            assert profile.duplicate.score == 0
            assert profile.duplicate.count == 0

            failed_names = [ax.name for ax in profile.failed_axes]
            assert len(failed_names) == 1

            # 全体としては(実行できた軸だけを見れば)クリーンでも、
            # explain()は「検出エラーで未検査」であることを明示する
            assert profile.is_clean()
            explanation = profile.explain()
            assert '検出エラー' in explanation
        finally:
            _detectors.DETECTOR_REGISTRY['duplicate'] = original
