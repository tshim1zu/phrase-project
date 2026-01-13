"""
各ファイルを単独で分析して、ファイル間の個体差を可視化

各日付ファイルのフレーズプロフィールを抽出し、時系列で比較して、
ファイル固有のパターンを明らかにします。
"""

import sys
from pathlib import Path
import logging
from typing import Dict, List
import json
from datetime import datetime

# Windows環境でのUTF-8出力対応
if sys.platform == "win32":
    import io
    sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
    sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8')

from tqdm import tqdm
import pandas as pd

# ローカルの japhrase モジュールをインポート
sys.path.insert(0, str(Path(__file__).parent.parent))
from japhrase.extracter import PhraseExtracter


logging.basicConfig(
    level=logging.INFO,
    format="%(levelname)s: %(message)s"
)
logger = logging.getLogger(__name__)


class FileVariationAnalyzer:
    """各ファイルの固有パターンを分析"""

    def __init__(self, min_count: int = 2, min_length: int = 1):
        self.min_count = min_count
        self.min_length = min_length
        self.extractor = PhraseExtracter(
            min_count=min_count,
            min_length=min_length,
            verbose=0  # ファイル毎なので verbose は OFF
        )

    def extract_phrases_from_file(self, file_path: Path) -> pd.DataFrame:
        """1ファイルからフレーズを抽出"""
        try:
            with open(file_path, "r", encoding="utf-8") as f:
                texts = [line.strip() for line in f if line.strip()]

            if not texts:
                return None

            df_phrases = self.extractor.extract(texts)
            if df_phrases is None or len(df_phrases) == 0:
                return None

            return df_phrases

        except Exception as e:
            logger.warning(f"エラー {file_path.name}: {e}")
            return None

    def analyze_directory(self, directory: Path) -> Dict:
        """ディレクトリ内の各ファイルを単独で分析"""
        text_files = sorted(directory.glob("*.txt"))
        if not text_files:
            logger.error(f"テキストファイルが見つかりません: {directory}")
            return None

        logger.info(f"ディレクトリ: {directory}")
        logger.info(f"ファイル数: {len(text_files)}")
        logger.info("")

        # 各ファイルを分析
        results = {}
        for file_path in tqdm(text_files, desc="Analyzing files", unit="file"):
            df_phrases = self.extract_phrases_from_file(file_path)

            if df_phrases is not None:
                # ファイル名から日付を抽出（形式: 20250505.txt）
                file_stem = file_path.stem
                try:
                    # YYYYMMDD形式を想定
                    if len(file_stem) >= 8 and file_stem[:8].isdigit():
                        date_str = file_stem[:8]
                        date_obj = datetime.strptime(date_str, "%Y%m%d")
                    else:
                        date_str = file_stem
                        date_obj = None
                except:
                    date_str = file_stem
                    date_obj = None

                results[file_path.name] = {
                    "file_name": file_path.name,
                    "date_str": date_str,
                    "date_obj": date_obj,
                    "num_lines": len([l for l in open(file_path, encoding='utf-8').readlines() if l.strip()]),
                    "num_phrases": len(df_phrases),
                    "phrases": df_phrases.to_dict('records')
                }

        return {
            "directory": str(directory),
            "file_count": len(text_files),
            "analyzed_count": len(results),
            "results": results
        }

    def display_variation_report(self, analysis_result: Dict, top_n: int = 20):
        """個体差を可視化するレポートを表示"""
        results = analysis_result["results"]

        # 日付でソート
        sorted_results = sorted(
            results.items(),
            key=lambda x: (x[1]["date_obj"] is not None, x[1]["date_obj"], x[1]["date_str"]) if x[1]["date_obj"] else (False, float('inf'), x[1]["date_str"])
        )

        print("\n" + "=" * 100)
        print("ファイル個体差分析レポート")
        print("=" * 100)

        print(f"\n📊 対象ファイル数: {len(sorted_results)}")
        print("")

        # 1. ファイルサイズ（行数とフレーズ数）の時系列
        print("📈 時系列：ファイルサイズの変化")
        print("-" * 100)
        print(f"{'ファイル名':<30} {'行数':<8} {'フレーズ数':<12} {'平均フレーズ長':<15}")
        print("-" * 100)

        for file_name, data in sorted_results:
            num_lines = data["num_lines"]
            num_phrases = data["num_phrases"]
            avg_phrase_len = num_lines / num_phrases if num_phrases > 0 else 0
            print(f"{file_name:<30} {num_lines:<8} {num_phrases:<12} {avg_phrase_len:>14.2f}")

        # 2. 各ファイルの TOP フレーズを時系列で表示
        print(f"\n🔝 各ファイルの TOP {min(top_n, 10)} フレーズ")
        print("-" * 100)

        for file_name, data in sorted_results[:10]:  # 最初の10ファイルを表示
            phrases = sorted(data["phrases"], key=lambda x: x["freq"], reverse=True)[:min(top_n, 5)]
            print(f"\n{file_name}")
            for i, phrase in enumerate(phrases, 1):
                print(f"  {i}. [{phrase['freq']:5}] {phrase['phrase']}")

        # 3. 全体で出現フレーズの多いものと少ないもの
        print(f"\n🌍 全ファイル共通で出現するフレーズ")
        print("-" * 100)

        # 全ファイルで出現するフレーズを抽出
        all_phrases = {}
        for file_name, data in sorted_results:
            for phrase_info in data["phrases"]:
                phrase = phrase_info["phrase"]
                if phrase not in all_phrases:
                    all_phrases[phrase] = {
                        "total_freq": 0,
                        "file_count": 0,
                        "file_list": []
                    }
                all_phrases[phrase]["total_freq"] += phrase_info["freq"]
                all_phrases[phrase]["file_count"] += 1
                all_phrases[phrase]["file_list"].append(file_name)

        # 出現ファイル数でフィルタ
        common_phrases = {
            p: info for p, info in all_phrases.items()
            if info["file_count"] >= len(sorted_results) * 0.5  # 50%以上のファイルに出現
        }

        common_sorted = sorted(common_phrases.items(), key=lambda x: x[1]["total_freq"], reverse=True)

        print(f"{'フレーズ':<40} {'総出現数':<12} {'出現ファイル数':<18}")
        print("-" * 100)
        for phrase, info in common_sorted[:20]:
            print(f"{phrase:<40} {info['total_freq']:<12} {info['file_count']:<18}")

        # 4. ファイル固有のフレーズ（そのファイルにだけ出現）
        print(f"\n🎯 ファイル固有のフレーズ（そのファイルにのみ出現）")
        print("-" * 100)

        unique_phrases = {p: info for p, info in all_phrases.items() if info["file_count"] == 1}
        print(f"ユニークフレーズ総数: {len(unique_phrases)}")

        # ファイルごとに固有フレーズを表示
        for file_name, data in sorted_results:
            unique_in_file = [
                phrase_info for phrase_info in data["phrases"]
                if phrase_info["phrase"] in unique_phrases
                and unique_phrases[phrase_info["phrase"]]["file_list"][0] == file_name
            ]
            if unique_in_file:
                unique_in_file = sorted(unique_in_file, key=lambda x: x["freq"], reverse=True)
                print(f"\n{file_name}: {len(unique_in_file)}個の固有フレーズ")
                for phrase_info in unique_in_file[:5]:
                    print(f"  - [{phrase_info['freq']:5}] {phrase_info['phrase']}")
                if len(unique_in_file) > 5:
                    print(f"  ... 他 {len(unique_in_file) - 5}個")

        print("\n" + "=" * 100)

    def save_results(self, analysis_result: Dict, output_file: Path):
        """結果をJSON形式で保存"""
        # 日付オブジェクトを文字列に変換
        def convert_types(obj):
            if isinstance(obj, dict):
                return {k: convert_types(v) for k, v in obj.items()}
            elif isinstance(obj, (list, tuple)):
                return [convert_types(item) for item in obj]
            elif isinstance(obj, datetime):
                return obj.isoformat()
            elif hasattr(obj, 'item'):
                return obj.item()
            else:
                return obj

        result_serializable = convert_types(analysis_result)

        with open(output_file, "w", encoding="utf-8") as f:
            json.dump(result_serializable, f, ensure_ascii=False, indent=2)

        logger.info(f"結果を保存: {output_file}")


def main():
    import argparse

    parser = argparse.ArgumentParser(
        description="各ファイルを単独で分析して、ファイル間の個体差を可視化"
    )

    parser.add_argument("--input", type=Path, required=True, help="分析対象ディレクトリ")
    parser.add_argument("--top-n", type=int, default=20, help="表示する上位フレーズ数")
    parser.add_argument("--min-count", type=int, default=2, help="最小出現数")
    parser.add_argument("--min-length", type=int, default=1, help="最小文字数")
    parser.add_argument("--output", type=Path, default=None, help="結果をJSONで保存")

    args = parser.parse_args()

    if not args.input.exists():
        logger.error(f"ディレクトリが見つかりません: {args.input}")
        return 1

    # 分析実行
    analyzer = FileVariationAnalyzer(min_count=args.min_count, min_length=args.min_length)
    result = analyzer.analyze_directory(args.input)

    if result is None or result["analyzed_count"] == 0:
        logger.error("分析対象のファイルがありません")
        return 1

    # レポート表示
    analyzer.display_variation_report(result, args.top_n)

    # 結果を保存
    if args.output:
        analyzer.save_results(result, args.output)

    return 0


if __name__ == "__main__":
    exit(main())
