"""
2つのディレクトリから抽出したフレーズリスト同士を比較

各ディレクトリから単純にフレーズを抽出して、その結果のフレーズ間で比較します。
"""

import sys
from pathlib import Path
import logging
from typing import List, Tuple
import json

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


class PhraseListComparator:
    """抽出されたフレーズリスト同士を比較"""

    def __init__(self, min_count: int = 2, min_length: int = 1):
        self.min_count = min_count
        self.min_length = min_length
        self.extractor = PhraseExtracter(
            min_count=min_count,
            min_length=min_length,
            verbose=1
        )

    def extract_phrases_from_directory(self, directory: Path) -> pd.DataFrame:
        """ディレクトリから全フレーズを抽出"""
        text_files = sorted(directory.glob("*.txt"))
        if not text_files:
            logger.error(f"テキストファイルが見つかりません: {directory}")
            return None

        logger.info(f"ディレクトリ: {directory}")
        logger.info(f"ファイル数: {len(text_files)}")

        # 全ファイルを読み込む
        all_texts = []
        for txt_file in tqdm(text_files, desc="Loading files", unit="file", leave=False):
            try:
                with open(txt_file, "r", encoding="utf-8") as f:
                    lines = [line.strip() for line in f if line.strip()]
                    all_texts.extend(lines)
            except Exception as e:
                logger.warning(f"読み込みエラー {txt_file}: {e}")

        if not all_texts:
            logger.error("読み込んだテキストがありません")
            return None

        logger.info(f"読み込んだテキスト: {len(all_texts)}行")

        # フレーズ抽出
        logger.info("フレーズ抽出中...")
        df_phrases = self.extractor.extract(all_texts)

        if df_phrases is None or len(df_phrases) == 0:
            logger.warning("フレーズが抽出されませんでした")
            return None

        logger.info(f"抽出されたフレーズ: {len(df_phrases)}個")
        return df_phrases

    def compare_phrases(self, df_dir1: pd.DataFrame, df_dir2: pd.DataFrame) -> dict:
        """2つのフレーズリストを比較"""
        phrases_dir1 = set(df_dir1['phrase'].values)
        phrases_dir2 = set(df_dir2['phrase'].values)

        # スコア辞書を作成
        scores_dir1 = dict(zip(df_dir1['phrase'].values, df_dir1['freq'].values))
        scores_dir2 = dict(zip(df_dir2['phrase'].values, df_dir2['freq'].values))

        # 集合演算
        only_in_dir1 = phrases_dir1 - phrases_dir2
        only_in_dir2 = phrases_dir2 - phrases_dir1
        common = phrases_dir1 & phrases_dir2

        # スコアでソート
        dir1_only_sorted = sorted(
            [(phrase, scores_dir1[phrase]) for phrase in only_in_dir1],
            key=lambda x: x[1],
            reverse=True
        )
        dir2_only_sorted = sorted(
            [(phrase, scores_dir2[phrase]) for phrase in only_in_dir2],
            key=lambda x: x[1],
            reverse=True
        )
        common_sorted = sorted(
            [(phrase, scores_dir1.get(phrase, scores_dir2[phrase])) for phrase in common],
            key=lambda x: x[1],
            reverse=True
        )

        return {
            "only_in_dir1": dir1_only_sorted,
            "only_in_dir2": dir2_only_sorted,
            "common": common_sorted,
            "stats": {
                "dir1_unique_phrases": len(phrases_dir1),
                "dir2_unique_phrases": len(phrases_dir2),
                "only_in_dir1": len(only_in_dir1),
                "only_in_dir2": len(only_in_dir2),
                "common": len(common)
            }
        }

    def display_comparison(self, result: dict, dir1_name: str, dir2_name: str, top_n: int = 30):
        """比較結果を表示"""
        print("\n" + "=" * 90)
        print(f"フレーズリスト比較: {dir1_name} vs {dir2_name}")
        print("=" * 90)

        stats = result["stats"]
        print(f"\n📊 統計")
        print(f"  {dir1_name}: {stats['dir1_unique_phrases']:>5}個のユニークフレーズ")
        print(f"  {dir2_name}: {stats['dir2_unique_phrases']:>5}個のユニークフレーズ")
        print(f"  {dir1_name}のみ: {stats['only_in_dir1']:>5}個")
        print(f"  {dir2_name}のみ: {stats['only_in_dir2']:>5}個")
        print(f"  共通: {stats['common']:>5}個")

        # Dir1のみ
        print(f"\n✅ {dir1_name}のみに出現するフレーズ（上位 {min(top_n, len(result['only_in_dir1']))}）")
        print("-" * 90)
        for i, (phrase, freq) in enumerate(result['only_in_dir1'][:top_n], 1):
            print(f"  {i:2d}. [{freq:5d}] {phrase}")
        if len(result['only_in_dir1']) > top_n:
            print(f"  ... 他 {len(result['only_in_dir1']) - top_n}個")

        # Dir2のみ
        print(f"\n❌ {dir2_name}のみに出現するフレーズ（上位 {min(top_n, len(result['only_in_dir2']))}）")
        print("-" * 90)
        for i, (phrase, freq) in enumerate(result['only_in_dir2'][:top_n], 1):
            print(f"  {i:2d}. [{freq:5d}] {phrase}")
        if len(result['only_in_dir2']) > top_n:
            print(f"  ... 他 {len(result['only_in_dir2']) - top_n}個")

        # 共通
        print(f"\n🔗 両方に出現するフレーズ（上位 {min(top_n, len(result['common']))}）")
        print("-" * 90)
        for i, (phrase, freq) in enumerate(result['common'][:top_n], 1):
            print(f"  {i:2d}. [{freq:5d}] {phrase}")
        if len(result['common']) > top_n:
            print(f"  ... 他 {len(result['common']) - top_n}個")

        print("\n" + "=" * 90)


def main():
    import argparse

    parser = argparse.ArgumentParser(
        description="2つのディレクトリから抽出したフレーズリスト同士を比較"
    )

    parser.add_argument("--dir1", type=Path, required=True, help="ディレクトリ1")
    parser.add_argument("--dir2", type=Path, required=True, help="ディレクトリ2")
    parser.add_argument("--name1", type=str, default="Directory1", help="ディレクトリ1の表示名")
    parser.add_argument("--name2", type=str, default="Directory2", help="ディレクトリ2の表示名")
    parser.add_argument("--top-n", type=int, default=30, help="表示する上位フレーズ数")
    parser.add_argument("--min-count", type=int, default=2, help="最小出現数")
    parser.add_argument("--min-length", type=int, default=1, help="最小文字数")
    parser.add_argument("--output", type=Path, default=None, help="結果を保存するJSON")

    args = parser.parse_args()

    if not args.dir1.exists():
        logger.error(f"ディレクトリが見つかりません: {args.dir1}")
        return 1

    if not args.dir2.exists():
        logger.error(f"ディレクトリが見つかりません: {args.dir2}")
        return 1

    # 比較実行
    comparator = PhraseListComparator(min_count=args.min_count, min_length=args.min_length)

    logger.info("=" * 90)
    logger.info(f"フレーズ抽出開始: {args.name1}")
    logger.info("=" * 90)
    df_dir1 = comparator.extract_phrases_from_directory(args.dir1)

    if df_dir1 is None:
        return 1

    logger.info("")
    logger.info("=" * 90)
    logger.info(f"フレーズ抽出開始: {args.name2}")
    logger.info("=" * 90)
    df_dir2 = comparator.extract_phrases_from_directory(args.dir2)

    if df_dir2 is None:
        return 1

    # 比較
    logger.info("")
    logger.info("フレーズリスト同士を比較中...")
    result = comparator.compare_phrases(df_dir1, df_dir2)

    # 表示
    comparator.display_comparison(result, args.name1, args.name2, args.top_n)

    # 結果を保存
    if args.output:
        def convert_types(obj):
            if isinstance(obj, dict):
                return {k: convert_types(v) for k, v in obj.items()}
            elif isinstance(obj, (list, tuple)):
                return [convert_types(item) for item in obj]
            elif hasattr(obj, 'item'):
                return obj.item()
            else:
                return obj

        result_serializable = convert_types(result)
        with open(args.output, "w", encoding="utf-8") as f:
            json.dump(result_serializable, f, ensure_ascii=False, indent=2)
        logger.info(f"結果を保存: {args.output}")

    return 0


if __name__ == "__main__":
    exit(main())
