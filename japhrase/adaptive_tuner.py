"""
AdaptiveTuner — テキストを蓄積しながらパラメータを動的に最適化する。

使い方:
    >>> from japhrase import AdaptiveTuner
    >>> tuner = AdaptiveTuner()                    # デフォルトプリセットで開始
    >>> tuner.feed(["文章1。", "文章2。", ...])      # テキストを蓄積
    >>> df = tuner.extract("新しいテキスト")          # 現在の最適パラメータで抽出
    >>> tuner.show_params()                         # 現在のパラメータを表示
    >>> tuner.tune(n_trials=50)                     # 手動チューニング
    >>> tuner.save("my_params.json")                # パラメータを保存
    >>> tuner = AdaptiveTuner.load("my_params.json") # 復元

設計思想:
    - 初見ユーザーはプリセットで即使える（テキスト蓄積ゼロ）
    - テキストが蓄積されるほど、そのユーザー固有のコーパスに最適化される
    - チューニング結果はJSONに保存・復元できる
    - Optunaがなくてもヒューリスティック推定で動く（Optunaがあればベイズ最適化）
"""

import json
import logging
from dataclasses import dataclass, field, asdict
from pathlib import Path
from typing import Any, Dict, List, Optional, Union

import pandas as pd

from .extracter import PhraseExtracter, PRESETS

logger = logging.getLogger(__name__)


# チューニングを自動実行するテキスト蓄積閾値
_AUTO_TUNE_THRESHOLDS = [50, 200, 1000]


@dataclass
class TunerState:
    """チューナーの永続化可能な状態。"""
    params: Dict[str, Any] = field(default_factory=lambda: dict(PRESETS['default']))
    preset_origin: str = 'default'
    texts_fed: int = 0
    tune_count: int = 0
    best_score: float = 0.0
    history: List[Dict[str, Any]] = field(default_factory=list)

    def to_dict(self) -> dict:
        d = asdict(self)
        d['params'] = {k: v for k, v in d['params'].items() if k != 'description'}
        return d

    @classmethod
    def from_dict(cls, d: dict) -> 'TunerState':
        return cls(**d)


class AdaptiveTuner:
    """テキストを蓄積しながらフレーズ抽出パラメータを動的に最適化する。

    使い方:
        >>> tuner = AdaptiveTuner()
        >>> tuner.feed(texts)           # テキストを蓄積
        >>> df = tuner.extract(text)    # 最適パラメータで抽出
        >>> tuner.show_params()         # 現在のパラメータを確認
        >>> tuner.tune()                # 手動でOptunaチューニング
    """

    def __init__(self, preset: str = 'default', storage_path: Optional[str] = None):
        """
        Args:
            preset: 初期プリセット名 ('sns', 'news', 'novel', 'report', 'default')
            storage_path: パラメータ保存先JSONパス (None=メモリのみ)
        """
        if preset not in PRESETS:
            raise ValueError(f"不明なプリセット: {preset}。使用可能: {list(PRESETS.keys())}")

        self._state = TunerState(
            params={k: v for k, v in PRESETS[preset].items() if k != 'description'},
            preset_origin=preset,
        )
        self._corpus: List[str] = []
        self._storage_path = Path(storage_path) if storage_path else None

        # 保存ファイルがあれば復元
        if self._storage_path and self._storage_path.exists():
            self._load_state()

    # ------------------------------------------------------------------ #
    #  公開API
    # ------------------------------------------------------------------ #

    def feed(self, texts: Union[str, List[str]]) -> 'AdaptiveTuner':
        """テキストを蓄積する。閾値に達すると自動チューニングが走る。

        Args:
            texts: テキスト文字列 or リスト

        Returns:
            self (チェーン呼び出し用)
        """
        if isinstance(texts, str):
            texts = [texts]
        self._corpus.extend(texts)
        self._state.texts_fed += len(texts)

        # 自動チューニング判定
        for threshold in _AUTO_TUNE_THRESHOLDS:
            prev = self._state.texts_fed - len(texts)
            if prev < threshold <= self._state.texts_fed:
                print(f"📊 テキスト {self._state.texts_fed} 件蓄積 — 自動チューニング開始...")
                self.tune(n_trials=10, verbose=0)
                break

        return self

    def extract(self, texts: Union[str, List[str]], **override) -> pd.DataFrame:
        """現在の最適パラメータでフレーズを抽出する。

        Args:
            texts: テキスト文字列 or リスト
            **override: パラメータの一時的な上書き

        Returns:
            DataFrame (seqchar, freq, ...)
        """
        params = {**self._state.params, **override}
        params.pop('description', None)
        params.setdefault('verbose', 0)
        extractor = PhraseExtracter(**params)
        return extractor.extract(texts)

    def tune(self, n_trials: int = 30, verbose: int = 1) -> Dict[str, Any]:
        """手動でチューニングを実行する。

        Optunaがインストールされていればベイズ最適化、
        なければヒューリスティック推定にフォールバックする。

        Args:
            n_trials: Optuna試行回数
            verbose: 進捗表示 (0=非表示, 1=表示)

        Returns:
            最適化されたパラメータ dict
        """
        if not self._corpus:
            print("⚠️ テキストが蓄積されていません。先に feed() でテキストを渡してください。")
            return self._state.params

        try:
            best_params = self._tune_optuna(n_trials, verbose)
        except ImportError:
            best_params = self._tune_heuristic(verbose)

        # 状態を更新
        old_params = dict(self._state.params)
        self._state.params.update(best_params)
        self._state.tune_count += 1
        self._state.history.append({
            'tune_id': self._state.tune_count,
            'texts_fed': self._state.texts_fed,
            'corpus_size': len(self._corpus),
            'params_before': old_params,
            'params_after': dict(self._state.params),
            'best_score': self._state.best_score,
        })

        if self._storage_path:
            self._save_state()

        if verbose >= 1:
            self.show_params()

        return dict(self._state.params)

    def show_params(self):
        """現在のパラメータと最適化状態を表示する。"""
        s = self._state
        print("=" * 50)
        print("  AdaptiveTuner — 現在のパラメータ")
        print("=" * 50)
        print(f"  初期プリセット:  {s.preset_origin}")
        print(f"  蓄積テキスト数:  {s.texts_fed}")
        print(f"  チューニング回数: {s.tune_count}")
        if s.best_score > 0:
            print(f"  ベストスコア:    {s.best_score:.4f}")
        print()
        print("  パラメータ:")
        for k, v in s.params.items():
            if k == 'description':
                continue
            if isinstance(v, float):
                print(f"    {k:30s} = {v:.4f}")
            else:
                print(f"    {k:30s} = {v}")
        print()
        print("  コピペ用:")
        def _fmt(k, v):
            if isinstance(v, float):
                return f"{k}={v:.4f}"
            return f"{k}={v!r}"
        params_str = ", ".join(
            _fmt(k, v) for k, v in s.params.items() if k != 'description'
        )
        print(f"    PhraseExtractor({params_str})")
        print("=" * 50)

    @property
    def params(self) -> Dict[str, Any]:
        """現在のパラメータ dict。"""
        return {k: v for k, v in self._state.params.items() if k != 'description'}

    @property
    def history(self) -> List[Dict[str, Any]]:
        """チューニング履歴。"""
        return list(self._state.history)

    def save(self, path: Optional[str] = None):
        """パラメータをJSONファイルに保存する。

        Args:
            path: 保存先 (None=コンストラクタで指定したパス)
        """
        p = Path(path) if path else self._storage_path
        if not p:
            raise ValueError("保存先が指定されていません。path引数かコンストラクタのstorage_pathを指定。")
        p.parent.mkdir(parents=True, exist_ok=True)
        with open(p, 'w', encoding='utf-8') as f:
            json.dump(self._state.to_dict(), f, indent=2, ensure_ascii=False)
        print(f"💾 パラメータを保存: {p}")

    @classmethod
    def load(cls, path: str) -> 'AdaptiveTuner':
        """JSONファイルからパラメータを復元する。

        Args:
            path: JSONファイルパス

        Returns:
            復元された AdaptiveTuner
        """
        p = Path(path)
        if not p.exists():
            raise FileNotFoundError(f"ファイルが見つかりません: {p}")
        with open(p, 'r', encoding='utf-8') as f:
            data = json.load(f)
        tuner = cls.__new__(cls)
        tuner._state = TunerState.from_dict(data)
        tuner._corpus = []
        tuner._storage_path = p
        print(f"📂 パラメータを復元: {p} (チューニング{tuner._state.tune_count}回済み)")
        return tuner

    @classmethod
    def demo(cls):
        """AdaptiveTuner のデモを実行する。"""
        _demo_adaptive_tuner()

    # ------------------------------------------------------------------ #
    #  内部実装
    # ------------------------------------------------------------------ #

    def _tune_optuna(self, n_trials: int, verbose: int) -> Dict[str, Any]:
        """Optunaによるベイズ最適化（全パラメータ対象）。"""
        import optuna

        if verbose == 0:
            optuna.logging.set_verbosity(optuna.logging.WARNING)

        texts = self._corpus
        n = len(texts)

        def objective(trial):
            # 重要度検証済み: weight_freq/weight_len/use_branching_entropy は <2% → 除外
            use_pmi = trial.suggest_categorical('use_pmi', [True, False])
            params = dict(
                min_count             = trial.suggest_int(  'min_count',             2, max(3, min(20, n // 10))),
                max_length            = trial.suggest_int(  'max_length',            5, 24),
                min_length            = trial.suggest_int(  'min_length',            2, 6),
                threshold_originality = trial.suggest_float('threshold_originality', 0.3, 0.9),
                use_pmi               = use_pmi,
                pmi_weight            = trial.suggest_float('pmi_weight', 0.1, 5.0, log=True) if use_pmi else 1.0,
                verbose=0,
            )
            try:
                df = PhraseExtracter(**params).get_dfphrase(texts)
                if df.empty or len(df) < 3:
                    return 0.0
                content = df[df['length'] >= 3]
                if len(content) < 2:
                    return 0.0
                quality = float((content['freq'] * content['length'] * content['originality']).mean())
                return quality * min(len(content), 20) / 20.0
            except Exception:
                return 0.0

        study = optuna.create_study(
            direction='maximize',
            sampler=optuna.samplers.TPESampler(seed=42),
        )
        study.optimize(objective, n_trials=n_trials, show_progress_bar=(verbose >= 1))

        self._state.best_score = study.best_value
        if verbose >= 1:
            print(f"✅ Optuna最適化完了 ({n_trials}試行, スコア={study.best_value:.4f})")

        best = dict(study.best_params)
        if not best.get('use_pmi', False):
            best['pmi_weight'] = 1.0
        return best

    def _tune_heuristic(self, verbose: int) -> Dict[str, Any]:
        """Optunaなしのヒューリスティック推定。"""
        from .parameter_optimizer import ParameterOptimizer

        optimizer = ParameterOptimizer()
        rec = optimizer.recommend_parameters(self._corpus)

        best = {
            'min_count': rec.min_count,
            'max_length': rec.max_length,
            'min_length': rec.min_length,
        }
        if verbose >= 1:
            print(f"✅ ヒューリスティック推定完了 (Optunaなし)")
            for reason_key, reason_val in rec.reasoning.items():
                print(f"  {reason_key}: {reason_val}")
        return best

    def _save_state(self):
        if self._storage_path:
            self.save(str(self._storage_path))

    def _load_state(self):
        if self._storage_path and self._storage_path.exists():
            with open(self._storage_path, 'r', encoding='utf-8') as f:
                data = json.load(f)
            self._state = TunerState.from_dict(data)

    def __repr__(self):
        s = self._state
        return (f"AdaptiveTuner(preset='{s.preset_origin}', "
                f"texts={s.texts_fed}, tuned={s.tune_count}x)")


def _demo_adaptive_tuner():
    """AdaptiveTuner のデモンストレーション。"""
    print("=" * 60)
    print("  AdaptiveTuner — 動的パラメータ最適化デモ")
    print("=" * 60)
    print()

    # 1. 初期状態（プリセット）
    print("  【ステップ1】プリセットで開始")
    tuner = AdaptiveTuner(preset='default')
    print(f"    {tuner}")
    print()

    # 2. テキストを蓄積
    sample_texts = [
        "ChatGPTの登場により生成AIが注目を集めている。",
        "生成AIはテキストだけでなく画像生成にも使われる。",
        "大規模言語モデルは生成AIの中核技術である。",
        "生成AIの倫理的課題について議論が活発化している。",
        "企業における生成AIの導入事例が増加している。",
    ] * 5

    print("  【ステップ2】テキストを蓄積 (25文)")
    tuner.feed(sample_texts)
    print(f"    {tuner}")
    print()

    # 3. 現在のパラメータで抽出
    print("  【ステップ3】現在のパラメータでフレーズ抽出")
    df = tuner.extract(sample_texts)
    if not df.empty:
        print(f"    検出フレーズ: {len(df)} 件")
        for _, row in df.head(3).iterrows():
            print(f"      「{row['seqchar']}」 — {int(row['freq'])}回")
    print()

    # 4. パラメータ表示
    print("  【ステップ4】パラメータを表示")
    tuner.show_params()
    print()

    print("  次のステップ:")
    print("    tuner.tune(n_trials=50)    # Optunaで本格チューニング")
    print("    tuner.save('params.json')  # パラメータを保存")
    print("    AdaptiveTuner.load('params.json')  # 次回復元")
    print("=" * 60)
