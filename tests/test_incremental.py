# coding:utf-8
"""
incremental.py (IncrementalPhraseState) のテスト
"""

import os
import tempfile

import pytest

from japhrase import PhraseExtracter
from japhrase.incremental import IncrementalPhraseState


class TestSaveLoadRoundtrip:
    """save/load の基本的な往復テスト"""

    def test_roundtrip_preserves_counts(self):
        state = IncrementalPhraseState(min_length=1)
        state.counts = {"foo": 5, "bar": 2}
        state.total_texts = 7

        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, "state.json")
            state.save(path)
            restored = IncrementalPhraseState.load(path)

        assert restored.counts == state.counts
        assert restored.total_texts == state.total_texts
        assert restored.min_length == state.min_length

    def test_save_is_atomic_no_temp_file_left_behind(self):
        """save()完了後、一時ファイルが残らないこと"""
        state = IncrementalPhraseState(min_length=1)
        state.counts = {"foo": 1}

        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, "state.json")
            state.save(path)
            assert os.listdir(d) == ["state.json"]

    def test_update_accumulates_across_calls(self):
        extractor = PhraseExtracter(min_count=1, verbose=0)
        state = IncrementalPhraseState.from_extractor(extractor)

        state.update(extractor, ["これはテストです。", "これはテストです。"])
        state.update(extractor, ["これはテストです。"])

        assert state.total_texts == 3


class TestConcurrentUpdateLostUpdate:
    """既知の制限: 同一state_pathへの並行更新はlost updateになる

    IncrementalPhraseState.save()自体はアトミック（クラッシュで壊れたファイルは
    残らない）だが、load -> update -> save という一連の流れ全体には排他制御が
    無い。2つの「プロセス」が同じ基底状態を読み込み、それぞれ別の差分を加えて
    保存すると、後勝ちで片方の更新が消える。

    これは「直すべきバグ」ではなく「現状の既知の制限」を固定するnegative
    controlであり、将来ロック機構を追加してこのテストが失敗するようになったら
    意図的に更新すべきテストである。
    """

    def test_concurrent_saves_lose_one_side_of_the_update(self):
        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, "shared_state.json")

            base = IncrementalPhraseState(min_length=1)
            base.counts = {"foo": 5}
            base.total_texts = 10
            base.save(path)

            # 2つの「並行プロセス」が同じベース状態から出発する
            run_a = IncrementalPhraseState.load(path)
            run_b = IncrementalPhraseState.load(path)

            run_a.counts["foo"] += 3
            run_a.total_texts += 1

            run_b.counts["bar"] = 7
            run_b.total_texts += 1

            # Aが先に保存
            run_a.save(path)
            # Bが後に保存し、Aの更新を（エラーなく）踏み潰す
            run_b.save(path)

            final = IncrementalPhraseState.load(path)

        # 既知の制限: run_aの更新（foo: 5->8）は消え、run_bの更新だけが残る
        assert final.counts == {"foo": 5, "bar": 7}
        assert final.total_texts == 11
        # 両方の更新が保持されるのが理想だが、現状はそうならないことを明示する
        assert final.counts.get("foo") != 8
