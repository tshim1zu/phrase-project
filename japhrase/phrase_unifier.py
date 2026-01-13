# coding: utf-8
"""
フレーズ統一エンジン

検出された表記ゆれを実際に統一する機能
"""

import logging
from typing import List, Dict, Tuple, Optional
import pandas as pd
import re

logger = logging.getLogger(__name__)


class PhraseUnifier:
    """フレーズの統一を実行"""

    def __init__(self):
        self.unification_map = {}  # variant → primary のマッピング
        self.unification_history = []

    def unify_phrases(
        self,
        df_phrases: pd.DataFrame,
        primary: str,
        variants: List[str],
        apply_to_texts: bool = False,
        texts: Optional[List[str]] = None
    ) -> Tuple[pd.DataFrame, Optional[List[str]]]:
        """
        表記ゆれを統一

        Parameters:
            df_phrases: フレーズDataFrame
            primary: 基準フレーズ
            variants: 統一対象の候補リスト
            apply_to_texts: 元テキストにも適用するか
            texts: 元テキスト（apply_to_texts=True の場合）

        Returns:
            (統一後のDataFrame, 統一後のテキスト)
        """
        if 'seqchar' not in df_phrases.columns and 'phrase' not in df_phrases.columns:
            logger.error("フレーズ列が見つかりません")
            return df_phrases, texts

        phrase_col = 'seqchar' if 'seqchar' in df_phrases.columns else 'phrase'

        # 統一前の統計
        before_count = len(df_phrases[df_phrases[phrase_col] == primary])
        before_freq = df_phrases[df_phrases[phrase_col] == primary]['freq'].sum() if 'freq' in df_phrases.columns else before_count

        # variant フレーズの出現回数を集計
        variant_freq = df_phrases[df_phrases[phrase_col].isin(variants)]['freq'].sum() \
                      if 'freq' in df_phrases.columns else len(df_phrases[df_phrases[phrase_col].isin(variants)])

        # マッピングテーブルを更新
        for variant in variants:
            self.unification_map[variant] = primary

        # DataFrameを複製して更新
        df_unified = df_phrases.copy()

        # variant をすべて primary に置換
        for variant in variants:
            mask = df_unified[phrase_col] == variant
            df_unified.loc[mask, phrase_col] = primary

        # 同じフレーズが複数行ある場合は集約
        if 'freq' in df_unified.columns:
            df_unified = df_unified.groupby(phrase_col, as_index=False).agg({
                'freq': 'sum',
                **{col: 'first' for col in df_unified.columns if col not in [phrase_col, 'freq']}
            })

        # 統一後の統計
        after_count = len(df_unified[df_unified[phrase_col] == primary])
        after_freq = df_unified[df_unified[phrase_col] == primary]['freq'].sum() if 'freq' in df_unified.columns else after_count

        # 統一履歴に記録
        self.unification_history.append({
            'primary': primary,
            'variants': variants,
            'before_freq': before_freq,
            'after_freq': after_freq,
            'freq_gain': after_freq - before_freq,
            'variant_count': len(variants),
        })

        # テキストも統一
        texts_unified = None
        if apply_to_texts and texts:
            texts_unified = self._unify_texts(texts, primary, variants)

        logger.info(
            f"✓ 統一完了: '{primary}' ({before_freq} → {after_freq} 回, +{after_freq - before_freq})"
        )

        return df_unified, texts_unified

    def _unify_texts(self, texts: List[str], primary: str, variants: List[str]) -> List[str]:
        """元テキストのフレーズを統一"""
        texts_unified = []

        for text in texts:
            unified_text = text
            for variant in variants:
                # 単語境界を考慮して置換（オプション）
                unified_text = unified_text.replace(variant, primary)

            texts_unified.append(unified_text)

        logger.info(f"テキスト内のフレーズを統一しました ({len(texts)} 行)")
        return texts_unified

    def batch_unify(
        self,
        df_phrases: pd.DataFrame,
        unification_rules: List[Dict],
        texts: Optional[List[str]] = None
    ) -> Tuple[pd.DataFrame, Optional[List[str]]]:
        """
        複数の統一ルールを一括適用

        Parameters:
            df_phrases: フレーズDataFrame
            unification_rules: 統一ルールのリスト
                [
                    {'primary': 'テスト', 'variants': ['テスツ', 'てすと']},
                    {'primary': '実施', 'variants': ['実行']},
                ]
            texts: 元テキスト

        Returns:
            (統一後のDataFrame, 統一後のテキスト)
        """
        df_result = df_phrases.copy()
        texts_result = texts.copy() if texts else None

        for rule in unification_rules:
            primary = rule['primary']
            variants = rule['variants']

            df_result, texts_result = self.unify_phrases(
                df_result,
                primary=primary,
                variants=variants,
                apply_to_texts=(texts_result is not None),
                texts=texts_result
            )

        logger.info(f"✓ {len(unification_rules)} 件の統一ルールを適用しました")
        return df_result, texts_result

    def get_unification_stats(self) -> Dict:
        """統一処理の統計を取得"""
        if not self.unification_history:
            return {}

        total_freq_gain = sum(h['freq_gain'] for h in self.unification_history)
        total_variants = sum(h['variant_count'] for h in self.unification_history)

        return {
            'unification_count': len(self.unification_history),
            'total_variants': total_variants,
            'total_freq_gain': total_freq_gain,
            'history': self.unification_history,
        }

    def export_stats(self, filepath: str) -> None:
        """統一処理の統計を CSV に出力"""
        df_stats = pd.DataFrame(self.unification_history)
        df_stats.to_csv(filepath, index=False, encoding='utf-8-sig')
        logger.info(f"統一統計を {filepath} に出力しました")

    def generate_report(self) -> str:
        """統一処理のレポートを生成"""
        if not self.unification_history:
            return "統一処理の履歴がありません"

        report = "【フレーズ統一レポート】\n\n"

        total_freq_gain = sum(h['freq_gain'] for h in self.unification_history)
        total_variants = sum(h['variant_count'] for h in self.unification_history)

        report += f"統一処理数: {len(self.unification_history)}\n"
        report += f"統一されたバリアント総数: {total_variants}\n"
        report += f"出現回数の増加: +{total_freq_gain}\n\n"

        report += "【詳細】\n"
        for i, h in enumerate(self.unification_history, 1):
            report += (
                f"{i}. '{h['primary']}' "
                f"(バリアント: {h['variant_count']}個, "
                f"出現回数: {h['before_freq']} → {h['after_freq']} +{h['freq_gain']})\n"
            )
            report += f"   統一内容: {', '.join(self.unification_map.get(h['primary'], []))}\n"

        return report


class InteractiveUnifier:
    """対話的に表記ゆれを統一"""

    def __init__(self, detector, unifier):
        self.detector = detector
        self.unifier = unifier

    def interactive_unify(
        self,
        df_phrases: pd.DataFrame,
        texts: Optional[List[str]] = None,
        top_k: int = 20
    ) -> Tuple[pd.DataFrame, Optional[List[str]]]:
        """
        対話的にユーザーに候補を提示して統一

        Parameters:
            df_phrases: フレーズDataFrame
            texts: 元テキスト（コンテキスト分析用）
            top_k: 提示する候補の上限数

        Returns:
            (統一後のDataFrame, 統一後のテキスト)
        """
        # 表記ゆれ候補を検出
        candidates = self.detector.detect_variants(df_phrases, texts)

        if not candidates:
            print("表記ゆれ候補が見つかりませんでした")
            return df_phrases, texts

        df_result = df_phrases.copy()
        texts_result = texts.copy() if texts else None

        print(f"\n表記ゆれ候補 {len(candidates[:top_k])} 件が見つかりました\n")

        accepted_count = 0
        rejected_count = 0

        for i, candidate in enumerate(candidates[:top_k], 1):
            print(f"\n{'=' * 80}")
            print(f"【候補 {i}/{min(top_k, len(candidates))}】\n")

            self._display_candidate(candidate, i)

            # ユーザー入力
            while True:
                choice = input(
                    f"\nこの統一を実行? (y=yes, n=skip, q=quit): "
                ).strip().lower()

                if choice == 'y':
                    # 統一を実行
                    df_result, texts_result = self.unifier.unify_phrases(
                        df_result,
                        primary=candidate.primary,
                        variants=candidate.variants,
                        apply_to_texts=(texts_result is not None),
                        texts=texts_result
                    )
                    accepted_count += 1
                    break

                elif choice == 'n':
                    print("  → スキップしました")
                    rejected_count += 1
                    break

                elif choice == 'q':
                    print("\n対話を終了します")
                    return df_result, texts_result

                else:
                    print("  入力が無効です。y/n/q で入力してください")

        print(f"\n{'=' * 80}")
        print(f"\n【統一完了】")
        print(f"  受け入れ: {accepted_count} 件")
        print(f"  スキップ: {rejected_count} 件")
        print(f"  フレーズ数の変化: {len(df_phrases)} → {len(df_result)}")

        return df_result, texts_result

    def _display_candidate(self, candidate, index: int) -> None:
        """候補を見やすく表示"""
        print(f"基準フレーズ: '{candidate.primary}' (出現: {candidate.primary_freq}回)")
        print(f"表記ゆれ候補:")
        for variant, freq in zip(candidate.variants, candidate.variant_freqs):
            print(f"  • {variant} (出現: {freq}回)")

        print(f"\n【統計的根拠】")
        print(f"  編集距離スコア: {candidate.edit_distance_score:.2f}")
        print(f"    → 文字レベルの距離が近い")

        print(f"  文字種変化: {candidate.char_type_shift_score:.2f}")
        if candidate.char_type_changes:
            print(f"    → {', '.join(candidate.char_type_changes)}")

        print(f"  長さの比率: {candidate.length_ratio_score:.2f}")
        print(f"  共通接頭辞: {candidate.common_prefix_score:.2f}")

        print(f"  コンテキスト類似度: {candidate.context_similarity_score:.2f}")
        print(f"    → 前後の単語が {candidate.context_similarity_score:.0%} 重複")

        print(f"  同時出現率: {candidate.co_occurrence_score:.2f}")
        if candidate.co_occur_count == 0:
            print(f"    → 同じテキストでの同時出現: なし")
        else:
            print(f"    → 同じテキストでの同時出現: {candidate.co_occur_count}回")

        print(f"\n【評価】")
        print(f"  確信度: {candidate.confidence:.1%}")
        print(f"  推奨: {candidate.recommendation}")
        print(f"  理由: {candidate.reasoning}")

        freq_gain = sum(candidate.variant_freqs)
        print(f"\n  効果: '{candidate.primary}' の頻出度が +{freq_gain}回 向上")
