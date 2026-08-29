# coding: utf-8
"""
表記ゆれ検出・統一のCLI インターフェース

ユーザーが対話的に表記ゆれ候補を確認・統一できる
"""

import sys
import logging
from typing import Optional, List, Tuple
import pandas as pd
from pathlib import Path

from .text_variant_detector import TextVariantDetector
from .phrase_unifier import PhraseUnifier, InteractiveUnifier

logger = logging.getLogger(__name__)


class VariantCLI:
    """表記ゆれ検出・統一の CLI"""

    def __init__(self):
        """表記ゆれ検出器・統一器・対話操作器を初期化する。"""
        self.detector = TextVariantDetector(similarity_threshold=0.70)
        self.unifier = PhraseUnifier()
        self.interactive_unifier = InteractiveUnifier(self.detector, self.unifier)

    def run_interactive(
        self,
        df_phrases: pd.DataFrame,
        texts: Optional[List[str]] = None,
        top_k: int = 20
    ) -> Tuple[pd.DataFrame, Optional[List[str]]]:
        """対話的に表記ゆれを検出・統一"""
        print("\n" + "=" * 100)
        print("【表記ゆれ検出・統一ツール】")
        print("=" * 100)

        # 候補を検出
        candidates = self.detector.detect_variants(df_phrases, texts)

        if not candidates:
            print("\n✗ 表記ゆれ候補が見つかりませんでした")
            return df_phrases, texts

        print(f"\n表記ゆれ候補 {len(candidates[:top_k])} 件が見つかりました\n")

        # 対話的に統一
        df_result, texts_result = self.interactive_unifier.interactive_unify(
            df_phrases, texts, top_k
        )

        # 統計を表示
        self._display_summary(df_result)

        return df_result, texts_result

    def run_auto(
        self,
        df_phrases: pd.DataFrame,
        texts: Optional[List[str]] = None,
        confidence_threshold: float = 0.85
    ) -> Tuple[pd.DataFrame, Optional[List[str]]]:
        """確信度が高い候補を自動統一"""
        print("\n" + "=" * 100)
        print("【自動統一モード】")
        print("=" * 100)

        # 候補を検出
        candidates = self.detector.detect_variants(df_phrases, texts)

        if not candidates:
            print("\n✗ 表記ゆれ候補が見つかりませんでした")
            return df_phrases, texts

        # 確信度の高い候補を自動統一
        high_confidence_candidates = [c for c in candidates if c.confidence >= confidence_threshold]

        if not high_confidence_candidates:
            print(
                f"\n✗ 確信度 {confidence_threshold:.0%} 以上の候補がありませんでした。"
                f"対話的モードを使用してください。"
            )
            return df_phrases, texts

        print(f"\n確信度 {confidence_threshold:.0%} 以上の候補 {len(high_confidence_candidates)} 件を自動統一します\n")

        # 自動統一を実行
        df_result = df_phrases.copy()
        texts_result = texts.copy() if texts else None

        for i, candidate in enumerate(high_confidence_candidates, 1):
            print(f"[{i}/{len(high_confidence_candidates)}] {candidate.primary} "
                  f"(確信度: {candidate.confidence:.0%})")
            print(f"  → {', '.join(candidate.variants)} を統一中...")

            df_result, texts_result = self.unifier.unify_phrases(
                df_result,
                primary=candidate.primary,
                variants=candidate.variants,
                apply_to_texts=(texts_result is not None),
                texts=texts_result
            )

        # 統計を表示
        self._display_summary(df_result)

        return df_result, texts_result

    def export_results(
        self,
        df_result: pd.DataFrame,
        output_path: str,
        export_stats: bool = True,
        export_unification_map: bool = True
    ) -> None:
        """結果を出力"""
        output_dir = Path(output_path).parent
        output_dir.mkdir(parents=True, exist_ok=True)

        # フレーズを CSV に出力
        df_result.to_csv(output_path, index=False, encoding='utf-8-sig')
        print(f"\n✓ 統一後のフレーズを出力: {output_path}")

        if export_stats:
            stats_path = str(output_path).replace('.csv', '_stats.csv')
            self.unifier.export_stats(stats_path)
            print(f"✓ 統一統計を出力: {stats_path}")

        if export_unification_map:
            map_path = str(output_path).replace('.csv', '_map.txt')
            with open(map_path, 'w', encoding='utf-8') as f:
                f.write("【統一マッピング】\n\n")
                for variant, primary in self.unifier.unification_map.items():
                    f.write(f"{variant} → {primary}\n")
            print(f"✓ 統一マップを出力: {map_path}")

    def _display_summary(self, df_result: pd.DataFrame) -> None:
        """結果サマリーを表示"""
        print(f"\n{'=' * 100}")
        print(f"【統一完了】")
        print(f"{'=' * 100}\n")

        # 統計情報
        stats = self.unifier.get_unification_stats()
        if stats:
            print(f"統一処理数: {stats['unification_count']}")
            print(f"統一されたバリアント総数: {stats['total_variants']}")
            print(f"出現回数の増加: +{stats['total_freq_gain']}\n")

        print(f"フレーズ数: {len(df_result)}")
        if 'freq' in df_result.columns:
            total_freq = df_result['freq'].sum()
            print(f"総出現回数: {total_freq}\n")

        # トップフレーズ
        if 'seqchar' in df_result.columns:
            phrase_col = 'seqchar'
        elif 'phrase' in df_result.columns:
            phrase_col = 'phrase'
        else:
            return

        print(f"【出現頻度 TOP 10】")
        top_phrases = df_result.nlargest(10, 'freq')[[phrase_col, 'freq']]
        for idx, (_, row) in enumerate(top_phrases.iterrows(), 1):
            print(f"  {idx}. {row[phrase_col]}: {row['freq']} 回")


def main_interactive():
    """CLIメインエントリポイント（対話的）"""
    import argparse

    parser = argparse.ArgumentParser(description="表記ゆれ検出・統一ツール")
    parser.add_argument('input_file', help="入力フレーズファイル (CSV)")
    parser.add_argument('--texts', help="元テキストファイル (テキスト)")
    parser.add_argument('--top-k', type=int, default=20, help="提示する候補の上限")
    parser.add_argument('--output', help="出力先 (CSV)")
    parser.add_argument('--auto', action='store_true', help="確信度が高い候補を自動統一")
    parser.add_argument('--confidence', type=float, default=0.85, help="自動統一の確信度閾値")
    parser.add_argument('--export-map', action='store_true', help="統一マップを出力")

    args = parser.parse_args()

    # 入力を読み込み
    try:
        df_phrases = pd.read_csv(args.input_file, encoding='utf-8-sig')
        print(f"✓ フレーズを読み込み: {args.input_file} ({len(df_phrases)} 行)")
    except Exception as e:
        print(f"✗ ファイル読み込みエラー: {e}")
        return 1

    # テキストを読み込み（オプション）
    texts = None
    if args.texts:
        try:
            with open(args.texts, 'r', encoding='utf-8') as f:
                texts = [line.strip() for line in f if line.strip()]
            print(f"✓ テキストを読み込み: {args.texts} ({len(texts)} 行)")
        except Exception as e:
            print(f"✗ テキスト読み込みエラー: {e}")

    # CLI を実行
    cli = VariantCLI()

    if args.auto:
        df_result, texts_result = cli.run_auto(df_phrases, texts, args.confidence)
    else:
        df_result, texts_result = cli.run_interactive(df_phrases, texts, args.top_k)

    # 結果を出力
    if args.output:
        cli.export_results(
            df_result,
            args.output,
            export_stats=True,
            export_unification_map=args.export_map
        )
    else:
        print(f"\n出力先を指定してください（--output）")

    return 0


if __name__ == '__main__':
    sys.exit(main_interactive())
