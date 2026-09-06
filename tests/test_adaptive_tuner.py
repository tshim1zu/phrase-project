# coding:utf-8
"""
adaptive_tuner.py (AdaptiveTuner) のテスト
"""

import os
import tempfile

from japhrase.adaptive_tuner import AdaptiveTuner


class TestSaveLoadRoundtrip:
    """save/load の基本的な往復テスト"""

    def test_roundtrip_preserves_params(self):
        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, "tuner_state.json")
            tuner = AdaptiveTuner(preset="default")
            tuner._state.params["min_count"] = 42
            tuner.save(path)

            restored = AdaptiveTuner.load(path)

        assert restored.params["min_count"] == 42

    def test_save_is_atomic_no_temp_file_left_behind(self):
        """save()完了後、ディレクトリに一時ファイルが残らないこと"""
        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, "tuner_state.json")
            tuner = AdaptiveTuner(preset="default")
            tuner.save(path)
            assert os.listdir(d) == ["tuner_state.json"]


class TestConcurrentUpdateLostUpdate:
    """既知の制限: 同一storage_pathへの並行tune()/save()はlost updateになる

    AdaptiveTuner.save()自体はアトミック（クラッシュで壊れたファイルは残らない）
    だが、load -> チューニング -> save という一連の流れ全体には排他制御が無い。
    2つの「プロセス」が同じ基底状態を読み込み、それぞれ別のパラメータ更新を
    加えて保存すると、後勝ちで片方の更新が消える。

    これは「直すべきバグ」ではなく「現状の既知の制限」を固定するnegative
    controlであり、将来ロック機構を追加してこのテストが失敗するようになったら
    意図的に更新すべきテストである。
    """

    def test_concurrent_saves_lose_one_side_of_the_update(self):
        with tempfile.TemporaryDirectory() as d:
            path = os.path.join(d, "shared_tuner_state.json")

            base = AdaptiveTuner(preset="default")
            original_min_count = base.params["min_count"]
            base.save(path)

            # 2つの「並行プロセス」が同じベース状態から出発する
            run_a = AdaptiveTuner.load(path)
            run_b = AdaptiveTuner.load(path)

            run_a._state.params["min_count"] = original_min_count + 100
            run_b._state.params["max_length"] = 999

            # Aが先に保存
            run_a.save(path)
            # Bが後に保存し、Aの更新を（エラーなく）踏み潰す
            run_b.save(path)

            final = AdaptiveTuner.load(path)

        # 既知の制限: run_aのmin_count更新は消え、run_bのmax_length更新だけが残る
        assert final.params["min_count"] == original_min_count
        assert final.params["max_length"] == 999
