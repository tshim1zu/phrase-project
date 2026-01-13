"""
PMI・分岐エントロピーのハイパーパラメータ最適化（高速版）

小規模データセットを使用して高速に最適化
"""
import sys
from pathlib import Path

sys.path.insert(0, str(Path(__file__).parent.parent))

from japhrase import PhraseExtracter
import optuna
from optuna.storages import RDBStorage
import logging

# ロギング設定
logging.basicConfig(level=logging.INFO)
logger = logging.getLogger(__name__)

# Optunaのログを抑制
optuna.logging.set_verbosity(optuna.logging.WARNING)


# 小規模な評価用データ
SAMPLE_TEXT = """
人工知能（じんこうちのう、英: artificial intelligence）、AI（エーアイ）は、「『計算（computation）』という概念と『コンピュータ（computer）』という道具を用いて『知能』を研究する計算機科学（computer science）の一分野」を指す語。
「言語の理解や推論、問題解決などの知的行動を人間に代わってコンピュータに行わせる技術」、または、「計算機（コンピュータ）による知的な情報処理システムの設計や実現に関する研究分野」ともされる。
AIの研究開発は「人工知能学」とも呼ばれる。
AIに関する大学での研究や教育は「電気工学・コンピュータ科学部 人工知能・意思決定論科」、情報工学科や情報理工学科コンピュータ科学専攻などで行われている。
人間の知的能力をコンピュータ上で実現する、様々な技術・ソフトウェア群・コンピュータシステム、アルゴリズムとも言われる（知的エージェントも参照）。
人工知能の例は、人間の日常的な言語を扱う自然言語処理（機械翻訳・かな漢字変換・構文解析・大規模言語モデル等）、専門家の推論や判断を模倣するエキスパートシステム、画像のパターンを検出や抽出する画像認識等がある。
機械学習や深層学習は人工知能の重要な技術である。
ニューラルネットワークは深層学習の基礎となる技術である。
Pythonは機械学習の開発によく使われるプログラミング言語である。
コンピュータシステムの設計においては、アルゴリズムの効率性が重要である。
データ処理においては、自然言語処理技術が活用されている。
画像認識技術は、ニューラルネットワークを用いて実装される。
"""


class FastHyperparamOptimizer:
    """高速ハイパーパラメータ最適化クラス"""

    def __init__(self):
        """初期化"""
        self.sentences = [line.strip() for line in SAMPLE_TEXT.split('\n') if line.strip()]

        # 評価基準
        self.technical_terms = [
            '人工知能', '機械学習', 'ニューラルネットワーク', '深層学習',
            'コンピュータ', 'アルゴリズム', 'データ', 'システム',
            '自然言語処理', '画像認識', 'Python',
            'エキスパートシステム', '情報処理', 'プログラミング'
        ]

        self.garbage_phrases = [
            'ている', 'ており', 'について', 'ことが', 'これを',
            'など', 'また', 'さらに', 'ため', 'として',
            'であり', 'である', 'では', 'には'
        ]

        logger.info(f"データ: {len(self.sentences)} センテンス")

    def calculate_score(self, extracted_phrases: list) -> float:
        """
        抽出されたフレーズの品質スコアを計算

        Parameters:
            extracted_phrases (list): 抽出されたフレーズのリスト

        Returns:
            float: 品質スコア
        """
        if not extracted_phrases:
            return -100.0

        score = 0.0
        top_n = min(15, len(extracted_phrases))

        for rank, phrase in enumerate(extracted_phrases[:top_n], 1):
            weight = (top_n - rank + 1)  # ランクが高いほど大きなweight

            # 専門用語ボーナス
            for term in self.technical_terms:
                if term in phrase:
                    score += weight * 3.0
                    break

            # ゴミフレーズペナルティ
            for garbage in self.garbage_phrases:
                if phrase == garbage or phrase.endswith(garbage):
                    score -= weight * 2.0
                    break

        # 専門用語の平均順位を計算（低いほど良い）
        tech_ranks = []
        for idx, phrase in enumerate(extracted_phrases[:top_n], 1):
            if any(term in phrase for term in self.technical_terms):
                tech_ranks.append(idx)

        if tech_ranks:
            avg_rank = sum(tech_ranks) / len(tech_ranks)
            score += (top_n - avg_rank) * 2.0  # 平均順位が高いほどボーナス

        return score

    def objective(self, trial: optuna.Trial) -> float:
        """Optunaの目的関数"""
        # ハイパーパラメータの探索
        pmi_weight = trial.suggest_float('pmi_weight', 0.1, 10.0, log=True)
        entropy_weight = trial.suggest_float('entropy_weight', 0.1, 10.0, log=True)
        min_count = trial.suggest_int('min_count', 2, 4)

        try:
            extractor = PhraseExtracter(
                min_count=min_count,
                max_length=16,
                use_pmi=True,
                use_branching_entropy=True,
                pmi_weight=pmi_weight,
                entropy_weight=entropy_weight,
                verbose=0
            )

            df = extractor.extract(self.sentences)

            if len(df) == 0:
                return -100.0

            extracted_phrases = df['seqchar'].tolist()
            score = self.calculate_score(extracted_phrases)

            return score

        except Exception as e:
            logger.error(f"Trial {trial.number} エラー: {e}")
            return -100.0

    def optimize(self, n_trials: int = 100):
        """最適化実行"""
        logger.info(f"\n{'='*60}")
        logger.info(f"ハイパーパラメータ最適化開始（試行回数: {n_trials}）")
        logger.info(f"{'='*60}\n")

        # Optunaストレージ
        storage_path = "experiments/results/optuna_hyperparam.db"
        storage = RDBStorage(f"sqlite:///{storage_path}")

        # スタディ作成
        study = optuna.create_study(
            study_name="pmi_entropy_fast_optimization",
            storage=storage,
            direction="maximize",
            load_if_exists=True
        )

        # 最適化実行
        study.optimize(self.objective, n_trials=n_trials, show_progress_bar=True)

        # 結果表示
        logger.info(f"\n{'='*60}")
        logger.info("最適化完了！")
        logger.info(f"{'='*60}")

        best_params = study.best_params
        best_value = study.best_value

        logger.info(f"\n最良スコア: {best_value:.2f}")
        logger.info(f"\n【最適パラメータ】")
        logger.info(f"  pmi_weight     : {best_params['pmi_weight']:.4f}")
        logger.info(f"  entropy_weight : {best_params['entropy_weight']:.4f}")
        logger.info(f"  min_count      : {best_params['min_count']}")

        # デフォルト値との比較
        logger.info(f"\n【デフォルト値】")
        logger.info(f"  pmi_weight     : 1.0000")
        logger.info(f"  entropy_weight : 1.0000")
        logger.info(f"  min_count      : 3")

        # 最適パラメータで抽出
        logger.info(f"\n{'='*60}")
        logger.info("最適パラメータでの抽出結果:")
        logger.info(f"{'='*60}\n")

        extractor_best = PhraseExtracter(
            min_count=best_params['min_count'],
            max_length=16,
            use_pmi=True,
            use_branching_entropy=True,
            pmi_weight=best_params['pmi_weight'],
            entropy_weight=best_params['entropy_weight'],
            verbose=0
        )

        df_best = extractor_best.extract(self.sentences)

        # デフォルトパラメータで抽出（比較用）
        extractor_default = PhraseExtracter(
            min_count=3,
            max_length=16,
            use_pmi=True,
            use_branching_entropy=True,
            pmi_weight=1.0,
            entropy_weight=1.0,
            verbose=0
        )

        df_default = extractor_default.extract(self.sentences)

        # 比較表示
        logger.info("【最適パラメータ】 vs 【デフォルト】\n")

        max_rows = max(len(df_best), len(df_default))
        for idx in range(min(15, max_rows)):
            # 最適パラメータの結果
            if idx < len(df_best):
                phrase_best = df_best.iloc[idx]['seqchar']
                score_best = df_best.iloc[idx]['sc_index']
                tag_best = ""
                if any(t in phrase_best for t in self.technical_terms):
                    tag_best = "✅"
                elif any(phrase_best == g or phrase_best.endswith(g) for g in self.garbage_phrases):
                    tag_best = "❌"
            else:
                phrase_best = "-"
                score_best = 0
                tag_best = ""

            # デフォルトの結果
            if idx < len(df_default):
                phrase_default = df_default.iloc[idx]['seqchar']
                score_default = df_default.iloc[idx]['sc_index']
                tag_default = ""
                if any(t in phrase_default for t in self.technical_terms):
                    tag_default = "✅"
                elif any(phrase_default == g or phrase_default.endswith(g) for g in self.garbage_phrases):
                    tag_default = "❌"
            else:
                phrase_default = "-"
                score_default = 0
                tag_default = ""

            logger.info(
                f"{idx+1:2d}. {phrase_best:20s} ({score_best:7.2f}) {tag_best:2s} | "
                f"{phrase_default:20s} ({score_default:7.2f}) {tag_default:2s}"
            )

        # スコア比較
        score_best = self.calculate_score(df_best['seqchar'].tolist())
        score_default = self.calculate_score(df_default['seqchar'].tolist())

        logger.info(f"\n{'='*60}")
        logger.info(f"品質スコア比較:")
        logger.info(f"  最適パラメータ: {score_best:.2f}")
        logger.info(f"  デフォルト    : {score_default:.2f}")
        logger.info(f"  改善率        : {((score_best/score_default - 1) * 100):.1f}%")
        logger.info(f"{'='*60}\n")

        return study


def main():
    """メイン処理"""
    import argparse

    parser = argparse.ArgumentParser(description='PMI・BE高速ハイパーパラメータ最適化')
    parser.add_argument('--trials', type=int, default=100, help='試行回数')

    args = parser.parse_args()

    optimizer = FastHyperparamOptimizer()
    study = optimizer.optimize(n_trials=args.trials)


if __name__ == '__main__':
    main()
