"""
Optunaを使った最適化のデモ実験

このスクリプトは、サンプルデータでハイパーパラメータ最適化を行い、
エビデンスベースのプリセット設計のための実験を実施します。
"""

import sys
sys.path.insert(0, '..')

from japhrase.optimization import OptunaOptimizer
import logging

logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)


def get_sample_texts_sns():
    """SNS風サンプルテキスト"""
    return [
        "フォローありがとうございます！よろしくお願いします。",
        "フォローしてください！お願いします！",
        "プレゼントキャンペーン開催中です。応募してください。",
        "プレゼントキャンペーンに応募しました。",
        "よろしくお願いします。フォローありがとうございます。",
        "キャンペーン開催中です。ぜひ応募してください。",
        "応募してください。プレゼントがもらえます。",
        "ありがとうございます。よろしくお願いします。",
        "開催中です。プレゼントキャンペーンです。",
        "フォローお願いします。よろしくお願いします。",
        "いいねしてください。フォローもお願いします。",
        "RTお願いします。プレゼント企画開催中。",
        "当選しました！ありがとうございます！",
        "フォロバします。よろしくお願いします。",
        "リツイートありがとうございます。",
    ] * 10  # 150件にスケール


def get_sample_texts_news():
    """ニュース風サンプルテキスト"""
    return [
        "政府は本日、経済対策を発表しました。",
        "新しい経済対策が閣議決定されました。",
        "専門家は経済対策の効果について分析しています。",
        "経済対策の詳細が明らかになりました。",
        "財務省は予算案を提出しました。",
        "予算案の審議が始まりました。",
        "国会で予算案が可決されました。",
        "野党は予算案に反対しています。",
        "与党は予算案の成立を目指しています。",
        "政府は追加の経済対策を検討しています。",
        "専門家会議が開催されました。",
        "有識者による分析結果が公表されました。",
        "市場は政府の対応を注視しています。",
        "株価は上昇傾向を示しています。",
        "景気回復の兆しが見られます。",
    ] * 10  # 150件にスケール


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


def run_experiment_sns():
    """SNS向け最適化実験"""
    logger.info("=" * 60)
    logger.info("SNS向けパラメータ最適化実験")
    logger.info("=" * 60)

    texts = get_sample_texts_sns()
    logger.info(f"テキスト数: {len(texts)}")

    # Optunaオプティマイザー
    optimizer = OptunaOptimizer(
        n_trials=30,  # 実験では30回試行
        param_ranges={
            'min_count': (2, 15),
            'max_length': (8, 20),
            'min_length': (2, 6),
            'threshold_originality': (0.3, 0.8),
        },
        verbose=1
    )

    # 最適化実行
    best_params, study = optimizer.optimize(
        texts,
        study_name='sns_optimization',
        storage='sqlite:///experiments/results/optuna_studies.db'
    )

    # 結果保存
    optimizer.save_results(study, output_dir='experiments/results')

    # 可視化（plotlyがあれば）
    try:
        optimizer.visualize(study, output_dir='experiments/results')
    except Exception as e:
        logger.warning(f"可視化スキップ: {e}")

    logger.info("\n" + "=" * 60)
    logger.info("SNS向け最適化完了")
    logger.info(f"最適パラメータ: {best_params}")
    logger.info(f"最適スコア: {study.best_value:.4f}")
    logger.info("=" * 60)

    return best_params, study


def run_experiment_news():
    """ニュース向け最適化実験"""
    logger.info("\n" + "=" * 60)
    logger.info("ニュース向けパラメータ最適化実験")
    logger.info("=" * 60)

    texts = get_sample_texts_news()
    logger.info(f"テキスト数: {len(texts)}")

    # Optunaオプティマイザー
    optimizer = OptunaOptimizer(
        n_trials=30,
        param_ranges={
            'min_count': (3, 20),
            'max_length': (10, 30),
            'min_length': (3, 8),
            'threshold_originality': (0.3, 0.8),
        },
        verbose=1
    )

    # 最適化実行
    best_params, study = optimizer.optimize(
        texts,
        study_name='news_optimization',
        storage='sqlite:///experiments/results/optuna_studies.db'
    )

    # 結果保存
    optimizer.save_results(study, output_dir='experiments/results')

    # 可視化
    try:
        optimizer.visualize(study, output_dir='experiments/results')
    except Exception as e:
        logger.warning(f"可視化スキップ: {e}")

    logger.info("\n" + "=" * 60)
    logger.info("ニュース向け最適化完了")
    logger.info(f"最適パラメータ: {best_params}")
    logger.info(f"最適スコア: {study.best_value:.4f}")
    logger.info("=" * 60)

    return best_params, study


def run_experiment_novel():
    """小説向け最適化実験"""
    logger.info("\n" + "=" * 60)
    logger.info("小説向けパラメータ最適化実験")
    logger.info("=" * 60)

    texts = get_sample_texts_novel()
    logger.info(f"テキスト数: {len(texts)}")

    # Optunaオプティマイザー
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

    # 最適化実行
    best_params, study = optimizer.optimize(
        texts,
        study_name='novel_optimization',
        storage='sqlite:///experiments/results/optuna_studies.db'
    )

    # 結果保存
    optimizer.save_results(study, output_dir='experiments/results')

    # 可視化
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

    # Optunaオプティマイザー
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

    # 最適化実行
    best_params, study = optimizer.optimize(
        texts,
        study_name='report_optimization',
        storage='sqlite:///experiments/results/optuna_studies.db'
    )

    # 結果保存
    optimizer.save_results(study, output_dir='experiments/results')

    # 可視化
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
    logger.info("最適化実験を開始します\n")

    # SNS向け実験
    sns_params, sns_study = run_experiment_sns()

    # ニュース向け実験
    news_params, news_study = run_experiment_news()

    # 小説向け実験
    novel_params, novel_study = run_experiment_novel()

    # レポート採点向け実験
    report_params, report_study = run_experiment_report()

    # 結果サマリー
    logger.info("\n" + "=" * 60)
    logger.info("全実験完了 - 結果サマリー")
    logger.info("=" * 60)
    logger.info("\n【SNS向け最適パラメータ】")
    for k, v in sns_params.items():
        logger.info(f"  {k}: {v}")
    logger.info(f"  スコア: {sns_study.best_value:.4f}")

    logger.info("\n【ニュース向け最適パラメータ】")
    for k, v in news_params.items():
        logger.info(f"  {k}: {v}")
    logger.info(f"  スコア: {news_study.best_value:.4f}")

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
