"""
小説・レポート向けパラメータ最適化実験
"""

import sys
sys.path.insert(0, '..')

from japhrase.optimization import OptunaOptimizer
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def get_sample_texts_novel():
    """小説風サンプルテキスト"""
    return [
        "彼は静かに部屋を出て行った。",
        "彼女は静かに頷いた。",
        "静かな夜が訪れた。",
        "彼は深く溜息をついた。",
        "彼女は深く考え込んだ。",
        "深い沈黙が流れた。",
        "窓の外を見つめていた。",
        "遠くを見つめていた。",
        "彼は何も言わなかった。",
        "彼女は何も答えなかった。",
        "そう言って彼は笑った。",
        "そう言って彼女は首を振った。",
        "時間だけが過ぎていった。",
        "長い時間が経過した。",
        "それは遠い昔のことだった。",
    ] * 10  # 150件にスケール


def get_sample_texts_report():
    """レポート採点向けサンプルテキスト"""
    return [
        "本研究の目的は、～を明らかにすることである。",
        "本論文の目的は、～を検討することである。",
        "研究の目的は、～を解明することである。",
        "先行研究によれば、～が指摘されている。",
        "先行研究では、～が報告されている。",
        "従来の研究において、～が示されている。",
        "本研究では、～という仮説を立てた。",
        "本研究では、～という方法を用いた。",
        "実験の結果、～が明らかになった。",
        "分析の結果、～が示された。",
        "結果から、～が示唆される。",
        "以上のことから、～が考えられる。",
        "以上の結果から、～と結論づけられる。",
        "今後の課題として、～が挙げられる。",
        "今後の研究において、～が必要である。",
    ] * 10  # 150件にスケール


def run_experiment_novel():
    """小説向け最適化実験"""
    logger.info("=" * 60)
    logger.info("小説向けパラメータ最適化実験")
    logger.info("=" * 60)

    texts = get_sample_texts_novel()
    logger.info(f"テキスト数: {len(texts)}")

    optimizer = OptunaOptimizer(
        n_trials=30,
        param_ranges={
            'min_count': (3, 15),
            'max_length': (8, 25),
            'min_length': (3, 8),
            'threshold_originality': (0.3, 0.8),
        },
        verbose=1
    )

    best_params, study = optimizer.optimize(
        texts,
        study_name='novel_optimization',
        storage='sqlite:///experiments/results/optuna_studies.db'
    )

    optimizer.save_results(study, output_dir='experiments/results')

    try:
        optimizer.visualize(study, output_dir='experiments/results')
    except Exception as e:
        logger.warning(f"可視化スキップ: {e}")

    logger.info("\n" + "=" * 60)
    logger.info("小説向け最適化完了")
    logger.info(f"最適パラメータ: {best_params}")
    logger.info(f"最適スコア: {study.best_value:.4f}")
    logger.info("=" * 60)

    return best_params, study


def run_experiment_report():
    """レポート採点向け最適化実験"""
    logger.info("\n" + "=" * 60)
    logger.info("レポート採点向けパラメータ最適化実験")
    logger.info("=" * 60)

    texts = get_sample_texts_report()
    logger.info(f"テキスト数: {len(texts)}")

    optimizer = OptunaOptimizer(
        n_trials=30,
        param_ranges={
            'min_count': (3, 15),
            'max_length': (10, 30),
            'min_length': (4, 10),
            'threshold_originality': (0.3, 0.8),
        },
        verbose=1
    )

    best_params, study = optimizer.optimize(
        texts,
        study_name='report_optimization',
        storage='sqlite:///experiments/results/optuna_studies.db'
    )

    optimizer.save_results(study, output_dir='experiments/results')

    try:
        optimizer.visualize(study, output_dir='experiments/results')
    except Exception as e:
        logger.warning(f"可視化スキップ: {e}")

    logger.info("\n" + "=" * 60)
    logger.info("レポート採点向け最適化完了")
    logger.info(f"最適パラメータ: {best_params}")
    logger.info(f"最適スコア: {study.best_value:.4f}")
    logger.info("=" * 60)

    return best_params, study


if __name__ == '__main__':
    logger.info("小説・レポート向け最適化実験を開始します\n")

    # 小説向け実験
    novel_params, novel_study = run_experiment_novel()

    # レポート採点向け実験
    report_params, report_study = run_experiment_report()

    # 結果サマリー
    logger.info("\n" + "=" * 60)
    logger.info("全実験完了 - 結果サマリー")
    logger.info("=" * 60)

    logger.info("\n【小説向け最適パラメータ】")
    for k, v in novel_params.items():
        logger.info(f"  {k}: {v}")
    logger.info(f"  スコア: {novel_study.best_value:.4f}")

    logger.info("\n【レポート採点向け最適パラメータ】")
    for k, v in report_params.items():
        logger.info(f"  {k}: {v}")
    logger.info(f"  スコア: {report_study.best_value:.4f}")

    logger.info("\n結果は experiments/results/ に保存されました")
    logger.info("=" * 60)
