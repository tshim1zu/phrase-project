# coding: utf-8
"""corpus_terms（塊ごとに数えて統合する語抽出）のテスト"""

import json
import random
from collections import Counter, defaultdict

import pandas as pd
import pytest

from japhrase.corpus_terms import (
    ChunkedPhraseExtractor,
    merge_counts,
    find_term_variants,
    normalize_key,
    chunks_from_files,
)

CHUNKS = {
    "w01": ["認知バイアスは誰にでもあります", "認知バイアスの例を見ます", "記憶の仕組み"],
    "w02": ["認知バイアスと記憶", "記憶の仕組みは複雑です", "ストレスと健康"],
    "w03": ["認知バイアスを減らす", "ストレスの反応", "ストレスと記憶"],
    "w04": ["全く別の話題について説明します"],
}


def brute(chunks, min_length, max_length, min_count, min_chunks):
    """素朴な全n-gram列挙（正解）。"""
    import re
    tot, per = Counter(), defaultdict(Counter)
    for cid, texts in chunks.items():
        for t in texts:
            for seg in re.split(r"[\W_]+", t):
                for k in range(min_length, max_length + 1):
                    for i in range(len(seg) - k + 1):
                        tot[seg[i:i + k]] += 1
                        per[seg[i:i + k]][cid] += 1
    return {g: (n, len(per[g])) for g, n in tot.items() if n >= min_count and len(per[g]) >= min_chunks}


def raw_rows(ex, chunks):
    table = ex.count_table(chunks)
    return {g: (sum(v.values()), len(v)) for g, v in table.items()}


def test_apriori_matches_brute_force():
    ex = ChunkedPhraseExtractor(min_length=2, max_length=8, min_count=2, min_chunks=2)
    assert raw_rows(ex, CHUNKS) == brute(CHUNKS, 2, 8, 2, 2)


def test_apriori_matches_brute_force_random():
    rng = random.Random(1)
    alphabet = "あいうえ漢字語"
    chunks = {f"c{i}": ["".join(rng.choice(alphabet) for _ in range(rng.randint(3, 25)))
                        for _ in range(6)] for i in range(7)}
    for mc in (1, 3):
        ex = ChunkedPhraseExtractor(min_length=2, max_length=6, min_count=3, min_chunks=mc)
        assert raw_rows(ex, chunks) == brute(chunks, 2, 6, 3, mc)


def test_extract_reports_count_and_chunks():
    ex = ChunkedPhraseExtractor(min_length=2, max_length=10, min_count=2, min_chunks=3)
    df = ex.extract(CHUNKS)
    row = df[df["seqchar"] == "認知バイアス"].iloc[0]
    assert row["freq"] == 4 and row["chunks"] == 3
    assert {"seqchar", "freq", "chunks"} <= set(df.columns)
    # 3講義未満にしか出ない語は出ない
    assert "仕組み" not in set(df["seqchar"])


def test_closed_filter_drops_fully_contained_substrings():
    ex = ChunkedPhraseExtractor(min_length=2, max_length=10, min_count=2, min_chunks=2)
    terms = set(ex.extract(CHUNKS)["seqchar"])
    assert "認知バイアス" in terms
    # 「認知バイ」は常に「認知バイアス」の一部としてしか現れない
    assert "認知バイ" not in terms and "バイアス" not in terms


def test_edge_filter_drops_kana_edges():
    ex = ChunkedPhraseExtractor(min_length=2, max_length=10, min_count=2, min_chunks=2, edge_filter="kana")
    for term in ex.extract(CHUNKS)["seqchar"]:
        assert not ("ぁ" <= term[0] <= "ん") and not ("ぁ" <= term[-1] <= "ん"), term
    ex2 = ChunkedPhraseExtractor(min_length=2, max_length=10, min_count=2, min_chunks=2, edge_filter="none")
    assert any(("ぁ" <= t[-1] <= "ん") for t in ex2.extract(CHUNKS)["seqchar"])


def test_order_independent():
    ex = ChunkedPhraseExtractor(min_length=2, max_length=8, min_count=2, min_chunks=2)
    a = ex.extract(CHUNKS)
    items = list(CHUNKS.items())
    random.Random(3).shuffle(items)
    b = ChunkedPhraseExtractor(min_length=2, max_length=8, min_count=2, min_chunks=2).extract(dict(items))
    pd.testing.assert_frame_equal(a.reset_index(drop=True), b.reset_index(drop=True))


def test_cache_resume_skips_computed_chunks(tmp_path):
    kw = dict(min_length=2, max_length=8, min_count=2, min_chunks=2, cache_dir=tmp_path)
    ex1 = ChunkedPhraseExtractor(**kw)
    a = ex1.extract(CHUNKS)
    assert ex1.stats["computed"] > 0 and ex1.stats["cached"] == 0
    ex2 = ChunkedPhraseExtractor(**kw)
    b = ex2.extract(CHUNKS)
    assert ex2.stats["computed"] == 0 and ex2.stats["cached"] == ex1.stats["computed"]
    pd.testing.assert_frame_equal(a, b)


def test_cache_invalidated_when_chunk_text_changes(tmp_path):
    kw = dict(min_length=2, max_length=8, min_count=2, min_chunks=2, cache_dir=tmp_path)
    ChunkedPhraseExtractor(**kw).extract(CHUNKS)
    changed = dict(CHUNKS)
    changed["w02"] = ["認知バイアスと記憶は別物", "ストレスと健康"]
    ex = ChunkedPhraseExtractor(**kw)
    got = ex.extract(changed)
    assert ex.stats["computed"] > 0
    fresh = ChunkedPhraseExtractor(min_length=2, max_length=8, min_count=2, min_chunks=2).extract(changed)
    pd.testing.assert_frame_equal(got, fresh)


def test_corrupt_cache_file_is_recomputed(tmp_path):
    kw = dict(min_length=2, max_length=8, min_count=2, min_chunks=2, cache_dir=tmp_path)
    a = ChunkedPhraseExtractor(**kw).extract(CHUNKS)
    for f in tmp_path.glob("*.json"):
        f.write_text("{broken", encoding="utf-8")
    b = ChunkedPhraseExtractor(**kw).extract(CHUNKS)
    pd.testing.assert_frame_equal(a, b)


def test_merge_counts_is_union_by_chunk_and_idempotent():
    a = {"認知": {"w01": 2}, "記憶": {"w01": 1}}
    b = {"認知": {"w02": 3}, "感情": {"w02": 1}}
    m = merge_counts(a, b)
    assert m["認知"] == {"w01": 2, "w02": 3}
    assert merge_counts(a, b) == merge_counts(b, a)
    assert merge_counts(m, a) == m


def test_normalize_key_unifies_script_and_width():
    assert normalize_key("ストレス") == normalize_key("すとれす")
    assert normalize_key("ｽﾄﾚｽ") == normalize_key("ストレス")
    assert normalize_key("コンピューター") == normalize_key("コンピュータ")
    assert normalize_key("ABC") == normalize_key("ａｂｃ")


def test_find_term_variants_pairs_and_chunk_cooccurrence():
    table = {
        "ストレス": {"w01": 3, "w02": 2, "w03": 1},
        "すとれす": {"w04": 2, "w05": 1},
        "記憶": {"w01": 5, "w02": 5},
        "記録": {"w01": 5, "w02": 5},
        "コンピューター": {"w01": 2, "w02": 1},
        "コンピュータ": {"w03": 2, "w04": 1},
    }
    rows = find_term_variants(table)
    pairs = {frozenset((r["a"], r["b"])): r for r in rows}
    assert frozenset(("ストレス", "すとれす")) in pairs
    assert frozenset(("コンピューター", "コンピュータ")) in pairs
    # 同じ講義で並んで出る別語（記憶/記録）は「別の語」の可能性が高いので順位を下げる
    assert pairs[frozenset(("ストレス", "すとれす"))]["cooccur_chunks"] == 0
    if frozenset(("記憶", "記録")) in pairs:
        assert pairs[frozenset(("記憶", "記録"))]["cooccur_chunks"] == 2
        assert rows.index(pairs[frozenset(("記憶", "記録"))]) > rows.index(pairs[frozenset(("ストレス", "すとれす"))])


def test_find_term_variants_rejects_different_words_that_share_a_key():
    table = {
        "データ": {"w01": 3, "w02": 3, "w03": 3},
        "でた": {"w01": 1, "w04": 1},
        "うん": {"w01": 3},
        "うーん": {"w02": 3},
        "モノ": {"w01": 2},
        "もの": {"w02": 5},
    }
    pairs = {frozenset((r["a"], r["b"])) for r in find_term_variants(table)}
    assert frozenset(("データ", "でた")) not in pairs
    assert frozenset(("うん", "うーん")) in pairs
    assert frozenset(("モノ", "もの")) in pairs


def test_find_term_variants_similar_is_opt_in_and_skips_numbers():
    table = {
        "今日のテーマ": {"w01": 3, "w02": 3},
        "今日の内容": {"w01": 3, "w03": 2},
        "1995": {"w01": 3},
        "1996": {"w02": 3},
    }
    assert find_term_variants(table) == []
    got = find_term_variants(table, include_similar=True, similarity_threshold=0.6)
    names = {frozenset((r["a"], r["b"])) for r in got}
    assert frozenset(("1995", "1996")) not in names


def test_find_term_variants_scales():
    import time
    rng = random.Random(0)
    chars = [chr(0x4E00 + i) for i in range(400)]
    table = {"".join(rng.choice(chars) for _ in range(rng.randint(2, 6))): {"c1": 3, "c2": 3} for _ in range(6000)}
    t = time.time()
    find_term_variants(table)
    assert time.time() - t < 20


def test_chunks_from_files_by_file_and_by_lines(tmp_path):
    p = tmp_path / "a.md"
    p.write_text("\n".join(f"行{i}" for i in range(5)), encoding="utf-8")
    q = tmp_path / "b.md"
    q.write_text("別", encoding="utf-8")
    by_file = chunks_from_files([str(p), str(q)], chunk_by="file")
    assert list(by_file) == [str(p), str(q)] or len(by_file) == 2
    by_lines = chunks_from_files([str(p), str(q)], chunk_by="lines", lines_per_chunk=2)
    assert len(by_lines) == 4  # a:3塊(2,2,1) + b:1塊


def test_detector_top_n_none_compares_all():
    from japhrase.text_variant_detector import TextVariantDetector
    phrases = [f"語{chr(0x3042 + i % 80)}{i}" for i in range(700)]
    df = pd.DataFrame({"seqchar": phrases, "freq": list(range(700, 0, -1))})
    det = TextVariantDetector(similarity_threshold=0.99)
    assert det.detect_variants(df, top_n=None) is not None  # 上限なしで走り切る


def test_cli_corpus_terms(tmp_path):
    from click.testing import CliRunner
    from japhrase.cli import cli
    tails = ["の話をします", "を減らす", "と記憶"]
    for i in range(3):
        (tmp_path / f"b{i}.md").write_text(f"話者：「認知バイアス{tails[i]}」\n話者：「ストレス{tails[i]}」\n", encoding="utf-8")
    out = tmp_path / "out.json"
    res = CliRunner().invoke(cli, ["corpus-terms", "--glob", str(tmp_path / "*.md"), "--chunk-by", "file",
                                   "--min-chunks", "3", "--min-count", "3", "--output", str(out),
                                   "--line-regex", "「(?P<text>.+)」"])
    assert res.exit_code == 0, res.output
    data = json.loads(out.read_text(encoding="utf-8"))
    assert any(r["seqchar"] == "認知バイアス" and r["chunks"] == 3 for r in data["terms"])
    assert "variants" in data
