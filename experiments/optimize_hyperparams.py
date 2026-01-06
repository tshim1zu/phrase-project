"""
PMI・分岐エントロピーのハイパーパラメータ最適化

Optunaを使用して最適な pmi_weight と entropy_weight を探索
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


class HyperparamOptimizer:
    """ハイパーパラメータ最適化クラス"""

    def __init__(self, text_file: str = "experiments/data/wikipedia_tech.txt"):
        """初期化"""
        self.text_file = Path(text_file)
        self.sentences = None

        # 評価基準: 専門用語とゴミフレーズを定義
        self.technical_terms = [
            '人工知能', '機械学習', 'ニューラルネットワーク', '深層学習',
            'コンピュータ', 'アルゴリズム', 'データ', 'システム',
            '自然言語処理', '画像認識', 'Python', 'TensorFlow',
            'エキスパートシステム', '情報処理', '研究開発', 'プログラミング'
        ]

        self.garbage_phrases = [
            'ている', 'ており', 'について', 'ことが', 'これを', 'その他',
            'など', 'また', 'さらに', 'しかし', 'ため', 'として',
            'であり', 'である', 'では', 'には', 'から', 'まで'
        ]

    def load_data(self):
        """データを読み込む"""
        if not self.text_file.exists():
            raise FileNotFoundError(f"{self.text_file} が見つかりません")

        text = self.text_file.read_text(encoding='utf-8')
        self.sentences = [line.strip() for line in text.split('\n') if line.strip()]
        logger.info(f"データ読み込み完了: {len(self.sentences)} センテンス")

    def calculate_score(self, extracted_phrases: list) -> float:
        """
        抽出されたフレーズの品質スコアを計算

        スコア計算方法:
        - 専門用語が上位にあるほど高スコア
        - ゴミフレーズが上位にあるほど低スコア

        Parameters:
            extracted_phrases (list): 抽出されたフレーズのリスト（順位順）

        Returns:
            float: 品質スコア（高いほど良い）
        """
        if not extracted_phrases:
            return 0.0

        score = 0.0
        top_n = min(20, len(extracted_phrases))

        for rank, phrase in enumerate(extracted_phrases[:top_n], 1):
            # 専門用語のスコア: 上位にあるほど高得点
            for term in self.technical_terms:
                if term in phrase:
                    # ランクが高いほど高得点 (1位=20点, 2位=19点, ...)
                    score += (top_n - rank + 1) * 2.0
                    break

            # ゴミフレーズのペナルティ: 上位にあるほど大きなペナルティ
            for garbage in self.garbage_phrases:
                if phrase == garbage or phrase.endswith(garbage):
                    # ランクが高いほど大きなペナルティ
                    score -= (top_n - rank + 1) * 1.5
                    break

        return score

    def objective(self, trial: optuna.Trial) -> float:
        """
        Optunaの目的関数

        Parameters:
            trial: Optunaのトライアル

        Returns:
            float: 最大化したいスコア
        """
        # ハイパーパラメータの探索範囲
        pmi_weight = trial.suggest_float('pmi_weight', 0.1, 5.0)
        entropy_weight = trial.suggest_float('entropy_weight', 0.1, 5.0)
        min_count = trial.suggest_int('min_count', 2, 5)

        try:
            # PhraseExtracterで抽出
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
                return -100.0  # ペナルティ

            # 抽出されたフレーズのリスト
            extracted_phrases = df['seqchar'].tolist()

            # スコア計算
            score = self.calculate_score(extracted_phrases)

            # 上位5件をログ出力（デバッグ用）
            if trial.number % 10 == 0:
                logger.info(f"\nTrial {trial.number}:")
                logger.info(f"  pmi_weight={pmi_weight:.3f}, entropy_weight={entropy_weight:.3f}, min_count={min_count}")
                logger.info(f"  Score={score:.2f}")
                logger.info(f"  Top 5: {extracted_phrases[:5]}")

            return score

        except Exception as e:
            logger.error(f"Trial {trial.number} failed: {e}")
            return -100.0

    def optimize(self, n_trials: int = 50, storage_path: str = "experiments/results/optuna_studies.db"):
        """
        ハイパーパラメータ最適化を実行

        Parameters:
            n_trials (int): 試行回数
            storage_path (str): Optunaのストレージパス
        """
        logger.info(f"ハイパーパラメータ最適化を開始（試行回数: {n_trials}）")

        # データ読み込み
        self.load_data()

        # Optunaストレージ
        storage = RDBStorage(f"sqlite:///{storage_path}")

        # スタディ作成
        study = optuna.create_study(
            study_name="pmi_entropy_optimization",
            storage=storage,
            direction="maximize",
            load_if_exists=True
        )

        # 最適化実行
        study.optimize(self.objective, n_trials=n_trials, show_progress_bar=True)

        # 結果表示
        logger.info("\n" + "="*60)
        logger.info("最適化完了！")
        logger.info("="*60)

        best_params = study.best_params
        best_value = study.best_value

        logger.info(f"\n最良スコア: {best_value:.2f}")
        logger.info(f"\n最適パラメータ:")
        logger.info(f"  pmi_weight     : {best_params['pmi_weight']:.3f}")
        logger.info(f"  entropy_weight : {best_params['entropy_weight']:.3f}")
        logger.info(f"  min_count      : {best_params['min_count']}")

        # 最適パラメータで実際に抽出してみる
        logger.info("\n" + "="*60)
        logger.info("最適パラメータでの抽出結果:")
        logger.info("="*60)

        extractor = PhraseExtracter(
            min_count=best_params['min_count'],
            max_length=16,
            use_pmi=True,
            use_branching_entropy=True,
            pmi_weight=best_params['pmi_weight'],
            entropy_weight=best_params['entropy_weight'],
            verbose=0
        )

        df = extractor.extract(self.sentences)

        logger.info("\n上位20フレーズ:")
        for idx in range(min(20, len(df))):
            row = df.iloc[idx]
            phrase = row['seqchar']
            freq = int(row.get('freq', 0))
            score = float(row.get('sc_index', 0))

            # 専門用語かゴミフレーズかを判定
            tag = ""
            if any(term in phrase for term in self.technical_terms):
                tag = "✅"
            elif any(phrase == g or phrase.endswith(g) for g in self.garbage_phrases):
                tag = "❌"

            logger.info(f"  {idx+1:2d}. {phrase:20s} (頻度:{freq:3d}, スコア:{score:8.2f}) {tag}")

        return study


def main():
    """メイン処理"""
    import argparse

    parser = argparse.ArgumentParser(description='PMI・BEハイパーパラメータ最適化')
    parser.add_argument('--trials', type=int, default=50, help='試行回数')
    parser.add_argument('--data', default='experiments/data/wikipedia_tech.txt', help='データファイルパス')

    args = parser.parse_args()

    optimizer = HyperparamOptimizer(text_file=args.data)
    study = optimizer.optimize(n_trials=args.trials)


if __name__ == '__main__':
    main()
