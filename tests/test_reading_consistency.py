from japhrase.reading_consistency import (
    ReadingConsistencyAnalyzer,
    ReadingObservation,
    normalize_reading,
)


def test_normalize_reading_folds_katakana_to_hiragana():
    assert normalize_reading("タイケイ") == normalize_reading("たいけい")


def test_same_surface_same_reading_is_not_conflict():
    rows = [
        ReadingObservation("体系", "タイケイ", context="体系を学ぶ"),
        ReadingObservation("体系", "たいけい", context="体系だった説明"),
    ]
    assert ReadingConsistencyAnalyzer().conflicts(rows) == []


def test_same_surface_multiple_readings_is_conflict():
    rows = [
        ReadingObservation("一人", "ヒトリ", context="一人で考える", location="a:1"),
        ReadingObservation("一人", "イチニン", context="一人を選ぶ", location="b:2"),
    ]
    found = ReadingConsistencyAnalyzer().conflicts(rows)
    assert len(found) == 1
    assert found[0].surface == "一人"
    assert set(found[0].readings) == {"ひとり", "いちにん"}


def test_ignored_surface_suppresses_intentional_heteronym():
    rows = [
        ReadingObservation("上手", "ジョウズ"),
        ReadingObservation("上手", "ウワテ"),
    ]
    assert ReadingConsistencyAnalyzer().conflicts(rows, ignored_surfaces={"上手"}) == []
