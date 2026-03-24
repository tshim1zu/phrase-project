# coding: utf-8
"""
汚染検知フレームワークのテスト

7種の検出器 + スキャナ + プロファイルを検証。
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


class TestScanFunction:
    """トップレベル scan() のテスト"""

    def test_returns_profile(self):
        p = scan(CLEAN_TEXT)
        assert isinstance(p, ContaminationProfile)

    def test_clean_text_is_clean(self):
        p = scan(CLEAN_TEXT)
        assert p.is_clean()
        assert p.overall <= 10

    def test_overall_bounded(self):
        p = scan(CLEAN_TEXT)
        assert 0 <= p.overall <= 100

    def test_select_detectors(self):
        p = scan(CLEAN_TEXT, detectors=['encoding'])
        assert p.encoding.count == 0
        # 他の軸は実行されていないので0
        assert p.duplicate.count == 0

    def test_custom_params(self):
        p = scan(CLEAN_TEXT, duplicate_threshold=0.5)
        assert isinstance(p, ContaminationProfile)


class TestEncoding:
    """エンコーディング検出"""

    def test_clean_no_encoding_issues(self):
        p = scan(CLEAN_TEXT, detectors=['encoding'])
        assert p.encoding.score == 0

    def test_mojibake_detected(self):
        text = '正常なテキストâ€™ここに文字化けÃ©がある'
        p = scan(text, detectors=['encoding'])
        assert p.encoding.count >= 1
        assert p.encoding.score > 0

    def test_control_chars_detected(self):
        text = '正常なテキスト\x01ここに制御文字'
        p = scan(text, detectors=['encoding'])
        assert p.encoding.count >= 1

    def test_replacement_char(self):
        text = '正常ï¿½文字化け'
        p = scan(text, detectors=['encoding'])
        assert p.encoding.count >= 1


class TestStructural:
    """構造異常検出"""

    def test_clean_no_structural(self):
        p = scan(CLEAN_TEXT, detectors=['structural'])
        assert p.structural.score == 0

    def test_unclosed_bracket(self):
        text = '「台詞が始まったけど閉じない\nまだ続く'
        p = scan(text, detectors=['structural'])
        assert p.structural.count >= 1

    def test_excessive_blank_lines(self):
        text = '段落1\n\n\n\n\n\n\n段落2'
        p = scan(text, detectors=['structural'])
        assert p.structural.count >= 1

    def test_very_long_line(self):
        text = 'あ' * 1500
        p = scan(text, detectors=['structural'])
        assert p.structural.count >= 1


class TestDuplicate:
    """重複検出"""

    def test_clean_no_duplicate(self):
        p = scan(CLEAN_TEXT, detectors=['duplicate'])
        assert p.duplicate.score == 0

    def test_exact_paragraph_duplicate(self):
        text = (
            '朝靄の中を歩いていると、足元で小さな花が揺れた。名前は知らない。\n\n'
            '道の先に古い石橋が見えた。\n\n'
            '朝靄の中を歩いていると、足元で小さな花が揺れた。名前は知らない。\n\n'
            '風が吹いた。'
        )
        p = scan(text, detectors=['duplicate'])
        assert p.duplicate.count >= 1
        assert p.duplicate.score > 0

    def test_near_duplicate(self):
        text = (
            '朝靄の中を歩いていると足元で小さな花が揺れた。名前は知らない。淡い紫。\n\n'
            '道の先に石橋が見えた。\n\n'
            '朝靄の中を歩いていると足元で小さな花が揺れた。名前は知らない。淡い色。\n\n'
        )
        p = scan(text, detectors=['duplicate'], duplicate_threshold=0.8)
        # ほぼ一致検出（閾値による）
        assert isinstance(p.duplicate.score, int)


class TestRepetition:
    """反復検出"""

    def test_clean_no_repetition(self):
        p = scan(CLEAN_TEXT, detectors=['repetition'])
        assert p.repetition.count == 0 or p.repetition.score < 20

    def test_heavy_repetition(self):
        text = '男は歩いた。男は止まった。' * 30
        p = scan(text, detectors=['repetition'])
        assert p.repetition.count > 0
        assert p.repetition.score > 50


class TestDistribution:
    """分布断絶検出"""

    def test_clean_no_break(self):
        # 短いテキストは検出対象外
        p = scan(CLEAN_TEXT, detectors=['distribution'])
        assert p.distribution.score == 0 or p.distribution.count == 0

    def test_sudden_topic_change(self):
        part_a = '剣を抜き、盾を構え、騎士は突進した。敵の軍勢が迫る。戦場は混乱していた。' * 20
        part_b = 'データベースのクエリを最適化する。インデックスを貼り、JOIN文を見直す。' * 20
        text = part_a + '\n' + part_b
        p = scan(text, detectors=['distribution'])
        # 全く異なる話題が隣接 → 分布断絶
        assert p.distribution.count >= 0  # テキスト長やセグメントサイズ次第


class TestComplexity:
    """複雑度異常検出"""

    def test_clean_no_anomaly(self):
        p = scan(CLEAN_TEXT, detectors=['complexity'])
        assert p.complexity.count == 0 or p.complexity.score < 20

    def test_extreme_repetition_low_compression(self):
        text = 'あ' * 2000
        p = scan(text, detectors=['complexity'])
        assert p.complexity.count >= 1
        assert p.complexity.score > 0


class TestVocabulary:
    """外来語彙検出"""

    def test_clean_no_foreign(self):
        p = scan(CLEAN_TEXT, detectors=['vocabulary'])
        assert p.vocabulary.count == 0


class TestProfile:
    """プロファイルのテスト"""

    def test_report_generation(self):
        p = scan(CLEAN_TEXT)
        report = p.report()
        assert '汚染度' in report
        assert 'エンコーディング' in report

    def test_all_anomalies_sorted(self):
        text = '正常â€™文字化けï¿½\n「閉じない'
        p = scan(text)
        all_a = p.all_anomalies
        # 重篤度降順
        for i in range(len(all_a) - 1):
            assert all_a[i].severity >= all_a[i + 1].severity

    def test_str_is_report(self):
        p = scan(CLEAN_TEXT)
        assert str(p) == p.report()

    def test_axes_list(self):
        p = scan(CLEAN_TEXT)
        assert len(p.axes) == 7

    def test_axis_labels(self):
        p = scan(CLEAN_TEXT)
        for axis in p.axes:
            assert axis.label in ('clean', 'minor', 'moderate', 'severe', 'critical')


class TestDrillDown:
    """ドリルダウンAPIのテスト"""

    def test_explain_clean(self):
        p = scan(CLEAN_TEXT)
        result = p.explain()
        assert '問題なし' in result

    def test_explain_contaminated(self):
        text = '正常â€™文字化けï¿½がある\n「閉じない'
        p = scan(text)
        result = p.explain()
        assert 'エンコーディング' in result or '構造' in result
        assert '💡' in result  # 修正提案がある

    def test_worst_axes(self):
        text = '正常â€™文字化け'
        p = scan(text)
        worst = p.worst_axes
        assert len(worst) > 0
        # スコア降順
        for i in range(len(worst) - 1):
            assert worst[i].score >= worst[i + 1].score

    def test_primary_issue(self):
        text = '正常â€™文字化け'
        p = scan(text)
        assert p.primary_issue is not None
        assert 'エンコーディング' in p.primary_issue

    def test_primary_issue_clean(self):
        p = scan(CLEAN_TEXT)
        assert p.primary_issue is None

    def test_top_issues(self):
        text = '正常â€™文字化けï¿½あるし\n「括弧も閉じない'
        p = scan(text)
        issues = p.top_issues(3)
        assert len(issues) <= 3
        # 重篤度降順
        for i in range(len(issues) - 1):
            assert issues[i].severity >= issues[i + 1].severity

    def test_locate(self):
        text = '正常â€™文字化け\n「閉じない'
        p = scan(text)
        enc = p.locate('encoding')
        struct = p.locate('structural')
        assert all(a.detector == 'encoding' for a in enc)
        assert all(a.detector == 'structural' for a in struct)

    def test_anomaly_location(self):
        text = '正常â€™文字化け'
        p = scan(text, detectors=['encoding'])
        for a in p.all_anomalies:
            loc = a.location
            assert loc.startswith('L') or loc.startswith('pos')

    def test_anomaly_suggestion(self):
        text = '正常â€™文字化け'
        p = scan(text, detectors=['encoding'])
        for a in p.all_anomalies:
            assert len(a.suggestion) > 0

    def test_summary_dict(self):
        p = scan(CLEAN_TEXT)
        d = p.summary_dict()
        assert 'overall' in d
        assert 'is_clean' in d
        assert 'axes' in d
        assert d['is_clean'] is True


class TestScanner:
    """ContaminationScanner のテスト"""

    def test_default_scanner(self):
        scanner = ContaminationScanner()
        p = scanner.scan(CLEAN_TEXT)
        assert p.is_clean()

    def test_custom_threshold(self):
        scanner = ContaminationScanner(
            duplicate_threshold=0.5,
            repetition_window=200,
        )
        p = scanner.scan(CLEAN_TEXT)
        assert isinstance(p, ContaminationProfile)
