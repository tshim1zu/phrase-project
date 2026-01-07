"""
プロンプト最適化実行スクリプト

Good/Bad プロンプト群を比較して、成功テンプレート・失敗パターンを分析します。
"""

import argparse
import json
from pathlib import Path
import logging
import sys

# ローカルの japhrase モジュールをインポート
sys.path.insert(0, str(Path(__file__).parent.parent))
from japhrase.comparison_analyzer import ComparisonAnalyzer


logging.basicConfig(
    level=logging.INFO,
    format="%(levelname)s: %(message)s"
)
logger = logging.getLogger(__name__)


def analyze_prompt_quality(good_file: Path, bad_file: Path, output_dir: Path = None, top_n: int = 10):
    """
    プロンプト品質分析を実行
    
    Args:
        good_file: Good プロンプトファイル
        bad_file: Bad プロンプトファイル
        output_dir: 出力ディレクトリ（デフォルト：good_file と同じディレクトリ）
        top_n: 表示する TOP フレーズ数
    """
    if output_dir is None:
        output_dir = good_file.parent
    
    output_dir.mkdir(parents=True, exist_ok=True)
    
    logger.info("=" * 60)
    logger.info("プロンプト最適化分析")
    logger.info("=" * 60)
    logger.info(f"Good: {good_file}")
    logger.info(f"Bad: {bad_file}")
    logger.info(f"Output: {output_dir}")
    logger.info(f"Top-N: {top_n}")
    logger.info("")
    
    # 分析実行
    analyzer = ComparisonAnalyzer(min_count=3, min_length=5, use_pmi=False)
    result = analyzer.compare_from_files(good_file, bad_file)
    
    # レポート表示
    report = analyzer.generate_report(result)
    print(report)
    
    # 結果保存
    output_file = output_dir / "comfy_analysis_results.json"
    json_file, report_file = analyzer.save_results(result, output_file)
    
    logger.info(f"\n✅ Analysis complete!")
    logger.info(f"   JSON: {json_file}")
    logger.info(f"   Report: {report_file}")
    
    return result


def format_for_comfy_ui(result: dict, top_n: int = 10) -> dict:
    """
    分析結果を ComfyUI ユーザー向けフォーマットに変換
    
    Args:
        result: comparison_analyzer からの結果
        top_n: TOP フレーズ数
    
    Returns:
        ComfyUI 向けのフォーマット化された辞書
    """
    winning_ranked = result.get("winning_ranked", [])
    failure_ranked = result.get("failure_ranked", [])
    
    # TOP N を取得
    top_winning = [p for p, _ in winning_ranked[:top_n]]
    top_failure = [p for p, _ in failure_ranked[:top_n]]
    
    return {
        "type": "comfy_ui_prompt_optimization",
        "summary": {
            "total_good_phrases": len(winning_ranked) + len(result["common_context"]),
            "total_bad_phrases": len(failure_ranked) + len(result["common_context"]),
            "winning_count": len(winning_ranked),
            "failure_count": len(failure_ranked),
            "common_count": len(result["common_context"]),
            "top_n": top_n
        },
        "recommendations": {
            "must_use": top_winning,
            "must_avoid": top_failure,
            "base_tags": result["common_context"]
        },
        "usage": {
            "positive_prompt_template": ", ".join(top_winning + result["common_context"]),
            "negative_prompt_template": ", ".join(top_failure),
            "instructions": [
                f"Use all {len(top_winning)} items from 'must_use' for best quality",
                f"Avoid all {len(top_failure)} items from 'must_avoid' at all costs",
                f"Base tags (must include): {', '.join(result['common_context'][:3])}"
            ]
        }
    }


def main():
    parser = argparse.ArgumentParser(
        description="ComfyUI プロンプト最適化分析ツール",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
例：
  # トイデータセットで分析
  python run_comfy_analysis.py

  # カスタムファイルで分析
  python run_comfy_analysis.py --good my_good.txt --bad my_bad.txt --output result/

  # ComfyUI フォーマット出力
  python run_comfy_analysis.py --comfy-format
        """
    )
    
    parser.add_argument(
        "--good",
        type=Path,
        default=None,
        help="Good プロンプトファイル（デフォルト：data/toy_dataset/good_positive.txt）"
    )
    
    parser.add_argument(
        "--bad",
        type=Path,
        default=None,
        help="Bad プロンプトファイル（デフォルト：data/toy_dataset/bad_positive.txt）"
    )
    
    parser.add_argument(
        "--output",
        type=Path,
        default=None,
        help="出力ディレクトリ"
    )
    
    parser.add_argument(
        "--top-n",
        type=int,
        default=10,
        help="表示する TOP フレーズ数（デフォルト：10）"
    )
    
    parser.add_argument(
        "--comfy-format",
        action="store_true",
        help="ComfyUI フォーマットで出力"
    )
    
    args = parser.parse_args()
    
    # デフォルトパスを設定
    if args.good is None:
        project_root = Path(__file__).parent.parent
        args.good = project_root / "data" / "toy_dataset" / "good_positive.txt"
    
    if args.bad is None:
        project_root = Path(__file__).parent.parent
        args.bad = project_root / "data" / "toy_dataset" / "bad_positive.txt"
    
    # ファイルチェック
    if not args.good.exists():
        logger.error(f"❌ Good file not found: {args.good}")
        return 1
    
    if not args.bad.exists():
        logger.error(f"❌ Bad file not found: {args.bad}")
        return 1
    
    # 分析実行
    result = analyze_prompt_quality(args.good, args.bad, args.output, args.top_n)
    
    # ComfyUI フォーマット出力
    if args.comfy_format:
        comfy_result = format_for_comfy_ui(result, args.top_n)
        
        output_file = (args.output or args.good.parent) / "comfy_format.json"
        with open(output_file, "w", encoding="utf-8") as f:
            json.dump(comfy_result, f, ensure_ascii=False, indent=2)
        
        logger.info(f"✅ ComfyUI format saved to: {output_file}")
        
        # 画面表示
        print("\n" + "=" * 60)
        print("ComfyUI 最適化レコメンデーション")
        print("=" * 60)
        print(f"\n【必ず使う】Must Use (TOP {args.top_n}):")
        for i, phrase in enumerate(comfy_result["recommendations"]["must_use"], 1):
            print(f"  {i}. {phrase}")
        
        print(f"\n【避ける】Must Avoid (TOP {args.top_n}):")
        for i, phrase in enumerate(comfy_result["recommendations"]["must_avoid"], 1):
            print(f"  {i}. {phrase}")
        
        print("\n【ベースタグ】Base Tags:")
        print(f"  {', '.join(comfy_result['recommendations']['base_tags'])}")
        
        print("\n【推奨プロンプト】Recommended Positive:")
        print(f"  {comfy_result['usage']['positive_prompt_template']}")
        print("=" * 60)
    
    return 0


if __name__ == "__main__":
    exit(main())
