# coding: utf-8
"""
表記ゆれ検出システム

フレーズの表記ゆれを統計的に検出し、
ユーザーに統一候補を提示する
"""

import logging
from dataclasses import dataclass, field, asdict
from typing import List, Dict, Tuple, Set, Optional
from collections import defaultdict, Counter
import pandas as pd
from difflib import SequenceMatcher
import re

logger = logging.getLogger(__name__)


@dataclass
class VariantCandidate:
    """表記ゆれ候補"""
    primary: str                          # 基準フレーズ（最頻出）
    primary_freq: int                     # 基準の出現回数
    variants: List[str]                   # 表記ゆれ候補
    variant_freqs: List[int]              # 各候補の出現回数

    # スコア分解
    edit_distance_score: float            # 編集距離ベース
    char_type_shift_score: float          # 文字種変化
    length_ratio_score: float             # 長さの比率
    common_prefix_score: float            # 共通接頭辞
    context_similarity_score: float       # コンテキスト類似度
    co_occurrence_score: float            # 同時出現率

    # 複合スコア
    composite_score: float                # 統合スコア（0-1）
    confidence: float                     # 確信度（0-1）

    # 補助情報
    char_type_changes: List[str] = field(default_factory=list)
    context_overlap_ratio: float = 0.0
    co_occur_count: int = 0
    recommendation: str = ""
    reasoning: str = ""

    def to_dict(self) -> Dict:
        """辞書に変換（JSON化用）"""
        return asdict(self)


class TextVariantDetector:
    """表記ゆれを統計的に検出"""

    def __init__(self, similarity_threshold: float = 0.7):
        """
        Parameters:
            similarity_threshold: スコアがこの値以上のペアを候補とする
        """
        self.similarity_threshold = similarity_threshold
        self.context_freq = {}  # フレーズ周辺単語の統計

    def detect_variants(
        self,
        df_phrases: pd.DataFrame,
        texts: Optional[List[str]] = None,
        context_window: int = 2
    ) -> List[VariantCandidate]:
        """
        表記ゆれ候補を検出

        Parameters:
            df_phrases: フレーズDataFrame （seqchar, freq 列必須）
            texts: 元テキスト（コンテキスト分析用）
            context_window: 前後何単語を見るか

        Returns:
            確信度の高い順にソートされた候補リスト
        """
        if df_phrases.empty:
            return []

        # フレーズの出現回数を取得
        phrases = df_phrases['seqchar'].tolist() if 'seqchar' in df_phrases.columns else \
                  df_phrases['phrase'].tolist() if 'phrase' in df_phrases.columns else []
        freqs = df_phrases['freq'].tolist() if 'freq' in df_phrases.columns else [1] * len(phrases)

        phrase_dict = dict(zip(phrases, freqs))

        # コンテキスト分析（テキストがあれば）
        if texts:
            self.context_freq = self._analyze_context(phrases, texts, context_window)

        # ペアごとに類似度を計算
        candidates_raw = []
        compared = set()

        sorted_phrases = sorted(phrase_dict.items(), key=lambda x: x[1], reverse=True)

        for i, (phrase1, freq1) in enumerate(sorted_phrases):
            for phrase2, freq2 in sorted_phrases[i + 1:]:
                pair_key = tuple(sorted([phrase1, phrase2]))
                if pair_key in compared:
                    continue
                compared.add(pair_key)

                scores = self._similarity_scores(phrase1, phrase2)
                composite = self._composite_score(scores)

                # 閾値を超えたペアを記録
                if composite >= self.similarity_threshold:
                    # 基準は高頻出の方
                    if freq1 >= freq2:
                        primary, variant = phrase1, phrase2
                        p_freq, v_freq = freq1, freq2
                    else:
                        primary, variant = phrase2, phrase1
                        p_freq, v_freq = freq2, freq1

                    confidence = self._calculate_confidence(
                        scores, phrase1, phrase2, p_freq, v_freq, texts
                    )

                    candidates_raw.append({
                        'primary': primary,
                        'primary_freq': p_freq,
                        'variants': [variant],
                        'variant_freqs': [v_freq],
                        'scores': scores,
                        'composite': composite,
                        'confidence': confidence,
                    })

        # 候補をマージ（同じ基準フレーズのグループ化）
        candidates_merged = self._merge_candidates(candidates_raw)

        # 候補を確信度でソート
        candidates_merged.sort(key=lambda x: x.confidence, reverse=True)

        # 推奨文を生成
        for cand in candidates_merged:
            cand.recommendation = self._generate_recommendation(cand)
            cand.reasoning = self._generate_reasoning(cand)

        logger.info(f"表記ゆれ候補を {len(candidates_merged)} 件検出しました")
        return candidates_merged

    def _similarity_scores(self, phrase1: str, phrase2: str) -> Dict[str, float]:
        """複数の統計指標を計算"""
        return {
            'edit_distance_score': self._edit_distance_score(phrase1, phrase2),
            'char_type_shift_score': self._char_type_shift_score(phrase1, phrase2),
            'length_ratio_score': self._length_ratio_score(phrase1, phrase2),
            'common_prefix_score': self._common_prefix_score(phrase1, phrase2),
            'context_similarity_score': self._context_similarity_score(phrase1, phrase2),
            'co_occurrence_score': self._co_occurrence_score(phrase1, phrase2),
        }

    def _edit_distance_score(self, s1: str, s2: str) -> float:
        """編集距離ベースのスコア"""
        if s1 == s2:
            return 1.0

        ratio = SequenceMatcher(None, s1, s2).ratio()

        # 長さの大きく異なるペアはペナルティ
        if len(s1) == 0 or len(s2) == 0:
            return 0.0

        length_penalty = 1.0 - abs(len(s1) - len(s2)) / max(len(s1), len(s2))

        return ratio * 0.7 + length_penalty * 0.3

    def _char_type_shift_score(self, s1: str, s2: str) -> float:
        """文字種の変化を検出"""
        if s1 == s2:
            return 0.0  # 同じなら文字種変化なし

        changes = self._detect_char_type_changes(s1, s2)
        if not changes:
            return 0.0

        # 変化の種類に応じてスコア
        scores_map = {
            'full_width_half_width': 0.90,
            'katakana_hiragana': 0.85,
            'upper_lower': 0.95,
            'symbol_removed': 0.88,
            'space_variation': 0.92,
            'punct_variation': 0.85,
        }

        max_score = max(scores_map.get(t, 0.5) for t in changes)
        return max_score

    def _length_ratio_score(self, s1: str, s2: str) -> float:
        """長さの比が近いかどうか"""
        if len(s1) == 0 or len(s2) == 0:
            return 0.0

        ratio = min(len(s1), len(s2)) / max(len(s1), len(s2))

        if ratio >= 0.95:  # ほぼ同じ長さ
            return 0.95
        elif ratio >= 0.85:  # 15%以内の差
            return 0.85
        elif ratio >= 0.75:  # 25%以内
            return 0.70
        else:
            return 0.5

    def _common_prefix_score(self, s1: str, s2: str) -> float:
        """共通の接頭辞"""
        import os
        common = os.path.commonprefix([s1, s2])

        if not common:
            return 0.0

        common_ratio = len(common) / min(len(s1), len(s2))

        if common_ratio >= 0.8:
            return 0.95
        elif common_ratio >= 0.6:
            return 0.85
        elif common_ratio >= 0.4:
            return 0.70
        else:
            return 0.5

    def _context_similarity_score(self, phrase1: str, phrase2: str) -> float:
        """前後の単語の類似度"""
        if not self.context_freq:
            return 0.5  # データなし

        ctx1_pre = set(self.context_freq.get(phrase1, {}).get('preceding', []))
        ctx1_post = set(self.context_freq.get(phrase1, {}).get('following', []))
        ctx2_pre = set(self.context_freq.get(phrase2, {}).get('preceding', []))
        ctx2_post = set(self.context_freq.get(phrase2, {}).get('following', []))

        if not (ctx1_pre or ctx1_post or ctx2_pre or ctx2_post):
            return 0.5

        # Jaccard 類似度
        def jaccard(s1: Set, s2: Set) -> float:
            if not (s1 or s2):
                return 0.0
            return len(s1 & s2) / len(s1 | s2)

        pre_sim = jaccard(ctx1_pre, ctx2_pre)
        post_sim = jaccard(ctx1_post, ctx2_post)

        return (pre_sim + post_sim) / 2.0

    def _co_occurrence_score(self, phrase1: str, phrase2: str) -> float:
        """同じテキストで同時出現するか"""
        if not self.context_freq:
            return 0.5

        co_occur = self.context_freq.get((phrase1, phrase2), {}).get('co_occur_count', 0)
        total_texts = self.context_freq.get('total_texts', 1)

        co_occur_ratio = co_occur / total_texts

        if co_occur_ratio == 0:
            return 0.85  # 同時出現なし = 同じ意味の可能性大
        elif co_occur_ratio < 0.05:
            return 0.70
        elif co_occur_ratio < 0.15:
            return 0.50
        else:
            return 0.20  # よく一緒に出現 = 別の意味

    def _composite_score(self, scores: Dict[str, float]) -> float:
        """複数スコアを統合"""
        weights = {
            'edit_distance_score': 0.30,
            'char_type_shift_score': 0.20,
            'length_ratio_score': 0.10,
            'common_prefix_score': 0.15,
            'context_similarity_score': 0.15,
            'co_occurrence_score': 0.10,
        }

        total = sum(scores.get(k, 0) * weights[k] for k in weights)
        return total

    def _calculate_confidence(
        self,
        scores: Dict[str, float],
        phrase1: str,
        phrase2: str,
        freq1: int,
        freq2: int,
        texts: Optional[List[str]] = None
    ) -> float:
        """確信度を計算（0-1）"""
        base_score = self._composite_score(scores)

        # 頻度差が小さいほど確信度が高い
        freq_ratio = min(freq1, freq2) / max(freq1, freq2)
        freq_factor = freq_ratio * 0.2  # 0-0.2

        # 複数のスコアがすべて高いと確信度が上がる
        high_score_count = sum(1 for v in scores.values() if v > 0.8)
        consistency_factor = high_score_count / len(scores) * 0.1  # 0-0.1

        confidence = min(base_score + freq_factor + consistency_factor, 1.0)
        return confidence

    def _detect_char_type_changes(self, s1: str, s2: str) -> List[str]:
        """文字種の変化を検出"""
        changes = []

        # 全角・半角
        if self._has_full_width(s1) != self._has_full_width(s2):
            changes.append('full_width_half_width')

        # カタカナ・ひらがな
        if self._has_katakana(s1) != self._has_katakana(s2):
            changes.append('katakana_hiragana')

        # 大文字・小文字
        if s1.isupper() != s2.isupper():
            changes.append('upper_lower')

        # 記号の有無
        s1_symbols = len(re.findall(r'[!！?？,、。\s]', s1))
        s2_symbols = len(re.findall(r'[!！?？,、。\s]', s2))
        if (s1_symbols == 0) != (s2_symbols == 0):
            changes.append('symbol_removed')

        # 空白の有無
        if (' ' in s1) != (' ' in s2):
            changes.append('space_variation')

        # 句読点の有無
        s1_punct = len(re.findall(r'[。、！？]', s1))
        s2_punct = len(re.findall(r'[。、！？]', s2))
        if (s1_punct == 0) != (s2_punct == 0):
            changes.append('punct_variation')

        return changes

    @staticmethod
    def _has_full_width(text: str) -> bool:
        """全角文字を含むか"""
        # 全角英数（FF01-FF5E）、カタカナ（30A0-30FF）、ひらがな（3040-309F）など
        return any(ord(c) >= 0x3000 and ord(c) <= 0xFFFF for c in text)

    @staticmethod
    def _has_katakana(text: str) -> bool:
        """カタカナを含むか"""
        return any('\u30a0' <= c <= '\u30ff' for c in text)

    def _analyze_context(self, phrases: List[str], texts: List[str],
                        context_window: int = 2) -> Dict:
        """フレーズの周辺単語を分析"""
        context_freq = {
            'total_texts': len(texts),
        }

        for phrase in phrases:
            preceding = []
            following = []

            for text in texts:
                # フレーズが含まれているか
                if phrase in text:
                    # 簡易的に空白で分割
                    words = text.split()
                    try:
                        idx = words.index(phrase)
                        # 前の単語
                        if idx > 0:
                            preceding.extend(words[max(0, idx - context_window):idx])
                        # 後の単語
                        if idx < len(words) - 1:
                            following.extend(words[idx + 1:min(len(words), idx + context_window + 1)])
                    except ValueError:
                        pass

            context_freq[phrase] = {
                'preceding': preceding,
                'following': following,
            }

        # 同時出現をカウント
        for i, phrase1 in enumerate(phrases):
            for phrase2 in phrases[i + 1:]:
                co_occur = sum(1 for text in texts if phrase1 in text and phrase2 in text)
                context_freq[(phrase1, phrase2)] = {'co_occur_count': co_occur}

        return context_freq

    def _merge_candidates(self, candidates_raw: List[Dict]) -> List[VariantCandidate]:
        """同じ基準フレーズの候補をマージ"""
        merged = {}

        for cand in candidates_raw:
            primary = cand['primary']

            if primary not in merged:
                merged[primary] = {
                    'primary': primary,
                    'primary_freq': cand['primary_freq'],
                    'variants': cand['variants'],
                    'variant_freqs': cand['variant_freqs'],
                    'scores_list': [cand['scores']],
                    'confidences': [cand['confidence']],
                }
            else:
                merged[primary]['variants'].extend(cand['variants'])
                merged[primary]['variant_freqs'].extend(cand['variant_freqs'])
                merged[primary]['scores_list'].append(cand['scores'])
                merged[primary]['confidences'].append(cand['confidence'])

        # VariantCandidate オブジェクトに変換
        result = []
        for primary, data in merged.items():
            # スコアの平均を計算
            avg_scores = {}
            for key in data['scores_list'][0].keys():
                avg_scores[key] = sum(s[key] for s in data['scores_list']) / len(data['scores_list'])

            avg_confidence = sum(data['confidences']) / len(data['confidences'])

            candidate = VariantCandidate(
                primary=primary,
                primary_freq=data['primary_freq'],
                variants=data['variants'],
                variant_freqs=data['variant_freqs'],
                edit_distance_score=avg_scores['edit_distance_score'],
                char_type_shift_score=avg_scores['char_type_shift_score'],
                length_ratio_score=avg_scores['length_ratio_score'],
                common_prefix_score=avg_scores['common_prefix_score'],
                context_similarity_score=avg_scores['context_similarity_score'],
                co_occurrence_score=avg_scores['co_occurrence_score'],
                composite_score=self._composite_score(avg_scores),
                confidence=avg_confidence,
            )

            result.append(candidate)

        return result

    def _generate_recommendation(self, candidate: VariantCandidate) -> str:
        """推奨を生成"""
        if candidate.confidence >= 0.85:
            return "✓✓ 統一を強く推奨"
        elif candidate.confidence >= 0.70:
            return "✓ 統一を推奨"
        elif candidate.confidence >= 0.55:
            return "△ 確認の上、統一検討"
        else:
            return "✗ 統一推奨しない"

    def _generate_reasoning(self, candidate: VariantCandidate) -> str:
        """根拠を生成"""
        reasons = []

        if candidate.edit_distance_score >= 0.9:
            reasons.append(f"編集距離が小さい（{candidate.edit_distance_score:.2f}）")

        if candidate.char_type_shift_score >= 0.85:
            changes = ", ".join(candidate.char_type_changes) if candidate.char_type_changes else "変化あり"
            reasons.append(f"文字種の変化のみ（{changes}）")

        if candidate.length_ratio_score >= 0.85:
            reasons.append(f"長さが似ている（{candidate.length_ratio_score:.2f}）")

        if candidate.context_similarity_score >= 0.70:
            reasons.append(f"前後の単語が重複（{candidate.context_similarity_score:.0%}）")

        if candidate.co_occurrence_score >= 0.70:
            if candidate.co_occur_count == 0:
                reasons.append("同じテキストで同時出現しない（= 別フレーズとして区別されている）")
            else:
                reasons.append(f"同時出現は少数（{candidate.co_occur_count}回）")

        total_freq = candidate.primary_freq + sum(candidate.variant_freqs)
        unified_freq_gain = sum(candidate.variant_freqs)
        reasons.append(f"統一時、フレーズの信頼度が +{unified_freq_gain/candidate.primary_freq*100:.0f}%向上")

        return "。".join(reasons) + "。"

    def export_candidates(self, candidates: List[VariantCandidate],
                         filepath: str) -> None:
        """候補を CSV に出力"""
        data = []
        for cand in candidates:
            data.append({
                '基準フレーズ': cand.primary,
                '出現回数': cand.primary_freq,
                '候補': ", ".join(cand.variants),
                '候補出現回数': ", ".join(map(str, cand.variant_freqs)),
                '編集距離': f"{cand.edit_distance_score:.2f}",
                '文字種変化': f"{cand.char_type_shift_score:.2f}",
                '長さ': f"{cand.length_ratio_score:.2f}",
                '接頭辞': f"{cand.common_prefix_score:.2f}",
                'コンテキスト': f"{cand.context_similarity_score:.2f}",
                '同時出現': f"{cand.co_occurrence_score:.2f}",
                '確信度': f"{cand.confidence:.1%}",
                '推奨': cand.recommendation,
            })

        df_export = pd.DataFrame(data)
        df_export.to_csv(filepath, index=False, encoding='utf-8-sig')
        logger.info(f"候補を {filepath} に出力しました")
