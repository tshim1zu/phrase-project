"""
japhrase フレーズ抽出パラメータの Optuna 最適化

どのパラメータが必要で、何が最適値かを自動探索します。
実行後に「パラメータ重要度」を表示するので、スコアへの影響が
小さいパラメータはデフォルトのまま固定できます。

使い方:
    python experiments/optimize.py                      # デフォルト（小説フィクスチャ）
    python experiments/optimize.py --text mytext.txt    # カスタムテキスト
    python experiments/optimize.py --trials 500         # 試行回数を増やす
    python experiments/optimize.py --resume             # 前回の続きから再開
"""
import sys
import argparse
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))

import optuna
optuna.logging.set_verbosity(optuna.logging.WARNING)

from japhrase import PhraseExtracter

# デフォルトデータ: テキストが多いほど最適化の精度が上がる
_ROOT = Path(__file__).parent.parent
DEFAULT_TEXTS = [
    _ROOT / 'tests/fixtures/novel_jp_ep01.txt',
    _ROOT / 'tests/fixtures/novel_jp_ep02.txt',
    _ROOT / 'tests/fixtures/novel_jp_ep03.txt',
]
DEFAULT_DB = Path(__file__).parent / 'results/optimize.db'


def load_texts(paths: list[Path]) -> list[str]:
    lines = []
    for p in paths:
        lines += [ln.strip() for ln in Path(p).read_text(encoding='utf-8').splitlines() if ln.strip()]
    if not lines:
        raise ValueError(f"テキストが空です: {paths}")
    return lines


def objective(trial: optuna.Trial, texts: list[str]) -> float:
    # 重要度検証済み: weight_freq/weight_len/use_branching_entropy は <2% → 除外
    use_pmi = trial.suggest_categorical('use_pmi', [True, False])

    params = dict(
        min_count             = trial.suggest_int(  'min_count',             2,   20),
        max_length            = trial.suggest_int(  'max_length',            5,   24),
        min_length            = trial.suggest_int(  'min_length',            2,    6),
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
        count_factor = min(len(content), 20) / 20.0
        return quality * count_factor
    except Exception:
        return 0.0


def run(text_paths: list[Path], n_trials: int, db_path: Path, resume: bool) -> optuna.Study:
    texts = load_texts(text_paths)
    print(f"データ: {len(texts)} 行 ({sum(len(t) for t in texts):,} 文字)")

    storage = f'sqlite:///{db_path}' if db_path else None
    if not resume and db_path and db_path.exists():
        db_path.unlink()

    db_path.parent.mkdir(parents=True, exist_ok=True)
    study = optuna.create_study(
        direction='maximize',
        storage=storage,
        study_name='japhrase_params',
        load_if_exists=resume,
    )

    study.optimize(
        lambda t: objective(t, texts),
        n_trials=n_trials,
        show_progress_bar=True,
    )

    _report(study)
    return study


def _report(study: optuna.Study) -> None:
    best = study.best_trial
    print(f"\n{'='*55}")
    print(f"最良スコア: {best.value:.4f}  (trial #{best.number})")
    print(f"{'='*55}")
    print("最良パラメータ:")
    for k, v in best.params.items():
        if isinstance(v, float):
            print(f"  {k:30s} = {v:.4f}")
        else:
            print(f"  {k:30s} = {v}")

    try:
        from optuna.importance import get_param_importances
        importance = get_param_importances(study)
        print(f"\nパラメータ重要度 ({len(study.trials)} trials):")
        for k, v in sorted(importance.items(), key=lambda x: -x[1]):
            bar = '█' * max(1, int(v * 40))
            mark = '  ← 要確認' if v < 0.05 else ''
            print(f"  {k:30s} {bar:<42} {v:.3f}{mark}")
        print("\n重要度 < 0.05 のパラメータはデフォルト固定で十分です。")
    except Exception as e:
        print(f"(重要度計算スキップ: {e})")

    # copy-paste 用コード出力
    print(f"\n# --- コピペ用 ---")
    print(f"extractor = PhraseExtracter(")
    for k, v in best.params.items():
        if k in ('use_pmi', 'use_entropy'):
            continue
        val = f"'{v}'" if isinstance(v, str) else (f"{v:.4f}" if isinstance(v, float) else v)
        print(f"    {k}={val},")
    if best.params.get('use_pmi'):
        print(f"    use_pmi=True,")
    if best.params.get('use_entropy'):
        print(f"    use_branching_entropy=True,")
    print(f")")


if __name__ == '__main__':
    p = argparse.ArgumentParser(description=__doc__, formatter_class=argparse.RawDescriptionHelpFormatter)
    p.add_argument('--text',   nargs='+', default=None,          help='テキストファイル（複数可）')
    p.add_argument('--trials', type=int,  default=300,           help='試行回数 (default: 300)')
    p.add_argument('--db',     default=str(DEFAULT_DB),          help='Optuna DB パス')
    p.add_argument('--resume', action='store_true',              help='前回の続きから再開')
    args = p.parse_args()

    paths = [Path(t) for t in args.text] if args.text else DEFAULT_TEXTS
    run(paths, args.trials, Path(args.db), args.resume)
