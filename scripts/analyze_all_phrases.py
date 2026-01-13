"""
ディレクトリ内の全ファイルからフレーズを抽出して頻度順に表示

単純に全テキストを読み込んで、頻出フレーズを統計的に抽出します。
Good/Bad比較などは行いません。
"""

import argparse
import sys
from pathlib import Path
import logging
from typing import List

# Windows環境でのUTF-8出力対応
if sys.platform == "win32":
    import io
    sys.stdout = io.TextIOWrapper(sys.stdout.buffer, encoding='utf-8')
    sys.stderr = io.TextIOWrapper(sys.stderr.buffer, encoding='utf-8')

from tqdm import tqdm

# ローカルの japhrase モジュールをインポート
sys.path.insert(0, str(Path(__file__).parent.parent))
from japhrase.extracter import PhraseExtracter


logging.basicConfig(
    level=logging.INFO,
    format="%(levelname)s: %(message)s"
)
logger = logging.getLogger(__name__)


def analyze_directory(directory: Path, min_count: int = 2, min_length: int = 1, top_n: int = 50):
    """
    ディレクトリ内の全テキストファイルからフレーズを抽出

    Args:
        directory: 分析対象ディレクトリ
        min_count: フレーズの最小出現数
        min_length: フレーズの最小文字数
        top_n: 表示する上位フレーズ数
    """
    # ファイルを取得
    text_files = sorted(directory.glob("*.txt"))
    if not text_files:
        logger.error(f"テキストファイルが見つかりません: {directory}")
        return

    logger.info(f"ディレクトリ: {directory}")
    logger.info(f"見つかったファイル: {len(text_files)}個")
    logger.info("")

    # 全ファイルを読み込む
    logger.info("ファイル読み込み中...")
    all_texts = []
    for txt_file in tqdm(text_files, desc="Loading files", unit="file"):
        try:
            with open(txt_file, "r", encoding="utf-8") as f:
                lines = [line.strip() for line in f if line.strip()]
                all_texts.extend(lines)
        except Exception as e:
            logger.warning(f"読み込みエラー {txt_file}: {e}")

    if not all_texts:
        logger.error("読み込んだテキストがありません")
        return

    logger.info(f"読み込んだテキスト総数: {len(all_texts)}行")
    logger.info("")

    # フレーズ抽出
    logger.info("フレーズ抽出中...")
    extractor = PhraseExtracter(
        min_count=min_count,
        min_length=min_length,
        verbose=1
    )

    df_phrases = extractor.extract(all_texts)

    if df_phrases is None or len(df_phrases) == 0:
        logger.warning("フレーズが抽出されませんでした")
        return

    logger.info(f"抽出されたフレーズ: {len(df_phrases)}個")
    logger.info("")

    # 結果を表示
    print("=" * 80)
    print(f"頻出フレーズ（上位 {min(top_n, len(df_phrases))}）")
    print("=" * 80)
    print("")
    print(f"{'順位':<5} {'出現数':<8} {'フレーズ':<60}")
    print("-" * 80)

    for i, row in df_phrases.iterrows():
        if i >= top_n:
            break
        phrase = row['phrase']
        freq = int(row['freq']) if 'freq' in row else int(row['score'])
        print(f"{i+1:<5} {freq:<8} {phrase}")

    print("-" * 80)
    print(f"総フレーズ数: {len(df_phrases)}")
    print("=" * 80)

    return df_phrases


def main():
    parser = argparse.ArgumentParser(
        description="ディレクトリ内の全ファイルからフレーズを抽出",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
例：
  # positiveディレクトリを分析
  python analyze_all_phrases.py --input C:\\Comfy\\prompts\\positive

  # 上位100を表示
  python analyze_all_phrases.py --input C:\\Comfy\\prompts\\positive --top-n 100

  # min_countを調整
  python analyze_all_phrases.py --input C:\\Comfy\\prompts\\positive --min-count 3 --min-length 2
        """
    )

    parser.add_argument(
        "--input",
        type=Path,
        required=True,
        help="分析対象ディレクトリ"
    )

    parser.add_argument(
        "--min-count",
        type=int,
        default=2,
        help="フレーズの最小出現数（デフォルト: 2）"
    )

    parser.add_argument(
        "--min-length",
        type=int,
        default=1,
        help="フレーズの最小文字数（デフォルト: 1）"
    )

    parser.add_argument(
        "--top-n",
        type=int,
        default=50,
        help="表示する上位フレーズ数（デフォルト: 50）"
    )

    args = parser.parse_args()

    if not args.input.exists():
        logger.error(f"ディレクトリが見つかりません: {args.input}")
        return 1

    # 分析実行
    analyze_directory(
        args.input,
        min_count=args.min_count,
        min_length=args.min_length,
        top_n=args.top_n
    )

    return 0


if __name__ == "__main__":
    exit(main())
