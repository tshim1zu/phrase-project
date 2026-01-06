"""
ComfyUI プロンプト最適化用の差分分析モジュール

Good と Bad のプロンプトを比較して：
- Winning Factors (成功要因：Good にだけある)
- Failure Factors (失敗要因：Bad にだけある)
- Common Context (共通部：両方にある)
を抽出
"""

from typing import Dict, Set, List, Tuple
from pathlib import Path
import json
import logging
from scipy.stats import chi2_contingency
import numpy as np

from japhrase import PhraseExtracter


logger = logging.getLogger(__name__)


class ComparisonAnalyzer:
    """
    Good/Bad プロンプトの比較分析を行うクラス
    
    集合演算を用いて「成功要因」と「失敗要因」を特定する
    小規模データ（10-20枚）用に最適化
    """
    
    def __init__(self, min_count: int = 3, min_length: int = 5, use_pmi: bool = False):
        """
        Args:
            min_count: フレーズの最小出現数（小規模データ用：3推奨）
            min_length: フレーズの最小文字数
            use_pmi: PMIを使用するか（小規模データではFalse推奨）
        """
        self.extractor = PhraseExtracter(
            min_count=min_count,
            min_length=min_length,
            use_pmi=use_pmi
        )
        self.min_count = min_count
        self.min_length = min_length
        self.use_pmi = use_pmi
        
        logger.info(f"ComparisonAnalyzer initialized:")
        logger.info(f"  min_count={min_count}, min_length={min_length}, use_pmi={use_pmi}")
    
    def extract_phrases_with_counts(self, text: str) -> Dict[str, int]:
        """
        テキストからフレーズを抽出して出現回数をカウント
        
        Args:
            text: プロンプトテキスト（複数行可、コンマ区切り想定）
        
        Returns:
            {フレーズ: 出現回数}
        """
        lines = [line.strip() for line in text.split('\n') if line.strip()]
        
        phrase_counts = {}
        
        for line in lines:
            # コンマで分割してフレーズを抽出
            phrases = [p.strip() for p in line.split(',')]
            phrases = [p for p in phrases if p and len(p) >= self.min_length]
            
            for phrase in phrases:
                phrase_counts[phrase] = phrase_counts.get(phrase, 0) + 1
        
        logger.info(f"Counted {len(phrase_counts)} unique phrases from {len(lines)} prompts")
        return phrase_counts
    
    def compare_corpora(
        self,
        good_text: str,
        bad_text: str
    ) -> Dict:
        """
        Good と Bad のテキストを比較して差分を計算
        
        Args:
            good_text: Good プロンプトの連結テキスト
            bad_text: Bad プロンプトの連結テキスト
        
        Returns:
            {
                "winning_factors": [...],  # Good にだけある
                "failure_factors": [...],  # Bad にだけある
                "common_context": [...],   # 両方にある
                "winning_ranked": [...],   # スコア付き
                "failure_ranked": [...],   # スコア付き
                "analysis": {...}
            }
        """
        logger.info("Starting corpus comparison...")
        
        # フレーズを出現回数付きで抽出
        good_counts = self.extract_phrases_with_counts(good_text)
        bad_counts = self.extract_phrases_with_counts(bad_text)
        
        good_phrases = set(good_counts.keys())
        bad_phrases = set(bad_counts.keys())
        
        logger.info(f"Good phrases: {len(good_phrases)}")
        logger.info(f"Bad phrases: {len(bad_phrases)}")
        
        # 集合演算
        winning_factors = good_phrases - bad_phrases  # Good にだけある
        failure_factors = bad_phrases - good_phrases  # Bad にだけある
        common_context = good_phrases & bad_phrases   # 両方にある
        
        logger.info(f"Winning factors: {len(winning_factors)}")
        logger.info(f"Failure factors: {len(failure_factors)}")
        logger.info(f"Common context: {len(common_context)}")
        
        # スコアリング：Good での出現頻度 vs Bad での出現頻度
        winning_ranked = self._score_phrases(
            winning_factors, good_counts, bad_counts, is_winning=True
        )
        failure_ranked = self._score_phrases(
            failure_factors, bad_counts, good_counts, is_winning=False
        )
        
        # カイ二乗検定を実施
        chi2_winning = self._chi_square_test(good_counts, bad_counts, winning_factors)
        chi2_failure = self._chi_square_test(good_counts, bad_counts, failure_factors)
        
        result = {
            "winning_factors": sorted(list(winning_factors)),
            "failure_factors": sorted(list(failure_factors)),
            "common_context": sorted(list(common_context)),
            "winning_ranked": winning_ranked,  # スコア付き
            "failure_ranked": failure_ranked,  # スコア付き
            "chi_square_winning": chi2_winning,  # 統計検定結果
            "chi_square_failure": chi2_failure,  # 統計検定結果
            "analysis": {
                "good_phrase_count": len(good_phrases),
                "bad_phrase_count": len(bad_phrases),
                "winning_count": len(winning_factors),
                "failure_count": len(failure_factors),
                "common_count": len(common_context),
                "total_unique_phrases": len(good_phrases | bad_phrases),
                "good_total_occurrences": sum(good_counts.values()),
                "bad_total_occurrences": sum(bad_counts.values()),
                "statistical_test": "Chi-square test (α=0.05)"
            }
        }
        
        return result
    
    def _score_phrases(
        self,
        phrases: Set[str],
        primary_counts: Dict[str, int],
        secondary_counts: Dict[str, int],
        is_winning: bool = True
    ) -> List[Tuple[str, float]]:
        """
        フレーズにスコアを付ける
        
        Args:
            phrases: 対象フレーズセット
            primary_counts: メイン側の出現回数（Good or Bad）
            secondary_counts: サブ側の出現回数（Bad or Good）
            is_winning: winning の場合 True, failure の場合 False
        
        Returns:
            [(phrase, score), ...] ソート済みリスト
        """
        scored = []
        
        for phrase in phrases:
            primary_freq = primary_counts.get(phrase, 0)
            secondary_freq = secondary_counts.get(phrase, 0)
            
            # スコア計算：主側の頻度 / (主側 + 副側 + 1)
            # 値が大きいほど「主側に集中している」
            if primary_freq > 0:
                # TF-IDF 風スコア
                tf = primary_freq  # Term Frequency
                # Inverse Document Frequency 代わり
                idf = 1.0 / (1.0 + secondary_freq)  # 副側での出現が少ないほど高スコア
                score = tf * idf
            else:
                score = 0.0
            
            scored.append((phrase, score))
        
        # スコア降順でソート
        scored.sort(key=lambda x: x[1], reverse=True)
        
        return scored
    
    def _chi_square_test(
        self,
        good_counts: Dict[str, int],
        bad_counts: Dict[str, int],
        phrases: Set[str]
    ) -> Dict[str, Tuple[float, float]]:
        """
        各フレーズについてカイ二乗検定を実施
        
        帰無仮説: Good と Bad 間でフレーズの出現パターンに差がない
        
        Args:
            good_counts: Good 側の出現回数
            bad_counts: Bad 側の出現回数
            phrases: 検定対象フレーズ
        
        Returns:
            {phrase: (chi2_stat, p_value)}
        """
        results = {}
        
        for phrase in phrases:
            good_freq = good_counts.get(phrase, 0)
            bad_freq = bad_counts.get(phrase, 0)
            
            # 2x2 分割表を作成
            # [出現, 未出現] × [Good, Bad]
            good_not_occur = 10 - (1 if good_freq > 0 else 0)
            bad_not_occur = 10 - (1 if bad_freq > 0 else 0)
            
            # 分割表
            contingency_table = np.array([
                [1 if good_freq > 0 else 0, good_not_occur],
                [1 if bad_freq > 0 else 0, bad_not_occur]
            ])
            
            try:
                chi2, p_value, dof, expected = chi2_contingency(contingency_table)
                results[phrase] = (chi2, p_value)
            except:
                # ランク不足など計算できない場合
                results[phrase] = (0.0, 1.0)
        
        return results
    
    def compare_from_files(
        self,
        good_file: Path,
        bad_file: Path
    ) -> Dict:
        """
        ファイルから Good/Bad テキストを読み込んで比較
        
        Args:
            good_file: Good プロンプトファイルパス
            bad_file: Bad プロンプトファイルパス
        
        Returns:
            比較結果
        """
        logger.info(f"Loading from files: {good_file}, {bad_file}")
        
        with open(good_file, "r", encoding="utf-8") as f:
            good_text = f.read()
        
        with open(bad_file, "r", encoding="utf-8") as f:
            bad_text = f.read()
        
        return self.compare_corpora(good_text, bad_text)
    
    def generate_report(self, comparison_result: Dict) -> str:
        """
        比較結果からレポートを生成
        
        Args:
            comparison_result: compare_corpora() の戻り値
        
        Returns:
            レポートテキスト
        """
        report_lines = []
        report_lines.append("=" * 60)
        report_lines.append("ComfyUI プロンプト最適化分析レポート")
        report_lines.append("=" * 60)
        report_lines.append("")
        
        # 分析サマリー
        analysis = comparison_result["analysis"]
        report_lines.append("📊 分析サマリー")
        report_lines.append("-" * 60)
        report_lines.append(f"Good フレーズ数: {analysis['good_phrase_count']}")
        report_lines.append(f"Bad フレーズ数: {analysis['bad_phrase_count']}")
        report_lines.append(f"共通フレーズ数: {analysis['common_count']}")
        report_lines.append(f"勝利要因数: {analysis['winning_count']}")
        report_lines.append(f"失敗要因数: {analysis['failure_count']}")
        report_lines.append(f"全体ユニークフレーズ数: {analysis['total_unique_phrases']}")
        report_lines.append("")
        
        # 勝利の鍵（スコア付き）
        report_lines.append("🏆 勝利の鍵 (Winning Factors - スコア付き)")
        report_lines.append("Good に含まれており、Bad には含まれていないフレーズ")
        report_lines.append("-" * 60)
        
        winning_ranked = comparison_result.get("winning_ranked", [])
        if winning_ranked:
            for i, (phrase, score) in enumerate(winning_ranked[:20], 1):
                # スコアを 0-100 の範囲で表示
                score_pct = min(100, int(score * 10))
                bar = "█" * (score_pct // 10) + "░" * (10 - score_pct // 10)
                report_lines.append(f"  {i:2d}. {phrase:30s} [{bar}] {score:.2f}")
            if len(winning_ranked) > 20:
                report_lines.append(f"  ... +{len(winning_ranked) - 20} more")
        else:
            report_lines.append("  (該当なし)")
        report_lines.append("")
        
        # 敗北の呪い（スコア付き）
        report_lines.append("💀 敗北の呪い (Failure Factors - スコア付き)")
        report_lines.append("Bad に含まれており、Good には含まれていないフレーズ")
        report_lines.append("-" * 60)
        
        failure_ranked = comparison_result.get("failure_ranked", [])
        if failure_ranked:
            for i, (phrase, score) in enumerate(failure_ranked[:20], 1):
                score_pct = min(100, int(score * 10))
                bar = "█" * (score_pct // 10) + "░" * (10 - score_pct // 10)
                report_lines.append(f"  {i:2d}. {phrase:30s} [{bar}] {score:.2f}")
            if len(failure_ranked) > 20:
                report_lines.append(f"  ... +{len(failure_ranked) - 20} more")
        else:
            report_lines.append("  (該当なし)")
        report_lines.append("")
        
        # 共通コンテキスト
        report_lines.append("🔗 共通コンテキスト (Common Context)")
        report_lines.append("Good と Bad 両方に含まれるフレーズ（ベースライン）")
        report_lines.append("-" * 60)
        
        common = comparison_result["common_context"]
        if common:
            for phrase in common[:20]:  # TOP 20
                report_lines.append(f"  • {phrase}")
            if len(common) > 20:
                report_lines.append(f"  ... +{len(common) - 20} more")
        else:
            report_lines.append("  (該当なし)")
        report_lines.append("")
        
        # 推奨事項
        report_lines.append("💡 推奨事項")
        report_lines.append("-" * 60)
        
        if winning_ranked:
            top_winning = winning_ranked[0][0]
            report_lines.append(f"✅ '{top_winning}' などの勝利要因を積極的に使用してください")
        
        if failure_ranked:
            top_failure = failure_ranked[0][0]
            report_lines.append(f"❌ '{top_failure}' などの失敗要因は避けてください")
        
        report_lines.append("")
        
        # 統計検定結果
        report_lines.append("📈 統計的有意性（Chi-square test, α=0.05）")
        report_lines.append("-" * 60)
        
        chi2_winning = comparison_result.get("chi_square_winning", {})
        chi2_failure = comparison_result.get("chi_square_failure", {})
        
        significant_winning = [p for p, (chi2, pv) in chi2_winning.items() if pv < 0.05]
        significant_failure = [p for p, (chi2, pv) in chi2_failure.items() if pv < 0.05]
        
        report_lines.append(f"有意な勝利要因（p < 0.05）: {len(significant_winning)}個")
        report_lines.append(f"有意な失敗要因（p < 0.05）: {len(significant_failure)}個")
        report_lines.append("")
        
        report_lines.append("=" * 60)
        
        return "\n".join(report_lines)
    
    def save_results(
        self,
        comparison_result: Dict,
        output_file: Path,
        include_report: bool = True
    ):
        """
        結果をJSONファイルに保存
        
        Args:
            comparison_result: 比較結果
            output_file: 出力ファイルパス
            include_report: レポート生成も行うか
        """
        # JSON 保存
        with open(output_file, "w", encoding="utf-8") as f:
            json.dump(comparison_result, f, ensure_ascii=False, indent=2)
        
        logger.info(f"Results saved to {output_file}")
        
        # レポート生成
        if include_report:
            report_file = output_file.with_stem(output_file.stem + "_report").with_suffix(".txt")
            report = self.generate_report(comparison_result)
            
            with open(report_file, "w", encoding="utf-8") as f:
                f.write(report)
            
            logger.info(f"Report saved to {report_file}")
            
            return output_file, report_file
        
        return output_file, None


if __name__ == "__main__":
    import sys
    import io
    
    # ログ設定
    logging.basicConfig(
        level=logging.INFO,
        format="%(levelname)s: %(message)s"
    )
    
    # Windows のエンコーディング対応
    if sys.platform == "win32":
        sys.stdout.reconfigure(encoding='utf-8')
        sys.stderr.reconfigure(encoding='utf-8')
    
    # テスト用コード
    analyzer = ComparisonAnalyzer(min_count=3, min_length=5, use_pmi=False)
    
    # データセットディレクトリ
    dataset_dir = Path(__file__).parent.parent / "data" / "toy_dataset"
    
    if dataset_dir.exists():
        good_file = dataset_dir / "good_positive.txt"
        bad_file = dataset_dir / "bad_positive.txt"
        
        if good_file.exists() and bad_file.exists():
            print("🔍 Comparing Good vs Bad prompts...\n")
            result = analyzer.compare_from_files(good_file, bad_file)
            
            print(analyzer.generate_report(result))
            
            # 結果保存
            output_dir = dataset_dir
            output_file = output_dir / "comparison_results.json"
            
            json_file, report_file = analyzer.save_results(result, output_file)
            print(f"\n✅ Results saved to {json_file}")
            if report_file:
                print(f"✅ Report saved to {report_file}")
        else:
            print("⚠️  Dataset files not found. Run generate_comfy_toy_dataset.py first.")
    else:
        print("⚠️  Dataset directory not found. Run generate_comfy_toy_dataset.py first.")
