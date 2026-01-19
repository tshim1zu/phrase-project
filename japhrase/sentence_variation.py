"""
同文反復検出の自動バリエーション生成モジュール

同じ文が複数回繰り返される場合を検出し、自動的にリライト候補を生成します。

使用例:
    >>> from japhrase import SentenceVariationGenerator
    >>> generator = SentenceVariationGenerator()
    >>> repetitions = generator.detect_repetitions("彼は走った。彼は走った。")
    >>> for rep in repetitions:
    ...     print(rep['variations'])
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2026"

import re
import json
import logging
from typing import List, Dict, Tuple
from collections import defaultdict
import hashlib

from .utils_robustness import ScoreValidator, ConfigValidator, TextProcessor, ErrorHandler
from .utils_advanced import CachingAnalyzer, MetricsCollector, StreamingAnalyzer

logger = logging.getLogger(__name__)


class SentenceVariationGenerator:
    """同一文の反復を検出し、自動的にバリエーションを生成するクラス"""

    def __init__(self, similarity_threshold: float = 0.95, min_repetitions: int = 2):
        """
        Args:
            similarity_threshold: 文の類似度閾値（0.0-1.0）
            min_repetitions: 検出する最小繰り返し回数
        """
        self.similarity_threshold = similarity_threshold
        self.min_repetitions = min_repetitions

    def detect_repetitions(self, text: str) -> List[Dict[str, any]]:
        """
        テキストから同一または類似した文の繰り返しを検出

        Args:
            text: 検査対象のテキスト

        Returns:
            繰り返しのリスト。各要素は以下の辞書:
                - sentence: 繰り返されている文
                - count: 出現回数
                - positions: 出現位置のリスト
                - variations: 自動生成されたバリエーション候補
        """
        sentences = self._split_sentences(text)
        sentence_map = defaultdict(list)

        # 文を正規化してグループ化
        position = 0
        for i, sentence in enumerate(sentences):
            if not sentence.strip():
                position += len(sentence)
                continue

            normalized = self._normalize_sentence(sentence)
            if normalized:
                sentence_map[normalized].append({
                    'original': sentence,
                    'index': i,
                    'position': position
                })

            position += len(sentence)

        # 繰り返しを検出
        repetitions = []
        for normalized, instances in sentence_map.items():
            if len(instances) >= self.min_repetitions:
                # バリエーションを生成
                variations = self._generate_variations(instances[0]['original'])

                repetitions.append({
                    'sentence': instances[0]['original'],
                    'normalized': normalized,
                    'count': len(instances),
                    'positions': [inst['position'] for inst in instances],
                    'indices': [inst['index'] for inst in instances],
                    'variations': variations
                })

        return sorted(repetitions, key=lambda x: x['count'], reverse=True)

    def _split_sentences(self, text: str) -> List[str]:
        """テキストを文単位に分割"""
        # 句点、感嘆符、疑問符、改行で分割
        sentences = re.split(r'([。！？\n])', text)

        # 区切り文字を前の文に結合
        result = []
        for i in range(0, len(sentences) - 1, 2):
            if i + 1 < len(sentences):
                result.append(sentences[i] + sentences[i + 1])
            else:
                result.append(sentences[i])

        # 最後の要素が区切り文字でない場合
        if len(sentences) % 2 == 1:
            result.append(sentences[-1])

        return result

    def _normalize_sentence(self, sentence: str) -> str:
        """文を正規化（空白除去、句読点統一など）"""
        # 空白を除去
        normalized = re.sub(r'\s+', '', sentence)
        # 句読点を統一
        normalized = normalized.strip('。！？\n')
        return normalized

    def _generate_variations(self, sentence: str) -> List[Dict[str, str]]:
        """
        文のバリエーションを自動生成

        Args:
            sentence: 元の文

        Returns:
            バリエーションのリスト
        """
        variations = []

        # 1. 文末表現の変更
        variations.extend(self._vary_sentence_ending(sentence))

        # 2. 語順の変更（限定的）
        variations.extend(self._vary_word_order(sentence))

        # 3. 省略形の提案
        variations.extend(self._suggest_omissions(sentence))

        # 4. 接続詞の追加
        variations.extend(self._add_conjunctions(sentence))

        return variations

    def _vary_sentence_ending(self, sentence: str) -> List[Dict[str, str]]:
        """文末表現のバリエーションを生成"""
        variations = []

        # 文末パターンのマッピング
        ending_patterns = [
            # だ/である調
            (r'(.+)だ([。！？])$', [r'\1である\2', r'\1です\2']),
            (r'(.+)である([。！？])$', [r'\1だ\2', r'\1です\2']),
            (r'(.+)です([。！？])$', [r'\1だ\2', r'\1である\2']),

            # た/ていた調
            (r'(.+)た([。！？])$', [r'\1ていた\2', r'\1ました\2']),
            (r'(.+)ていた([。！？])$', [r'\1た\2', r'\1ました\2']),
            (r'(.+)ました([。！？])$', [r'\1た\2', r'\1ていた\2']),

            # る/している調
            (r'(.+)る([。！？])$', [r'\1している\2', r'\1ます\2']),
            (r'(.+)している([。！？])$', [r'\1る\2', r'\1ます\2']),
            (r'(.+)ます([。！？])$', [r'\1る\2', r'\1している\2']),
        ]

        for pattern, replacements in ending_patterns:
            match = re.match(pattern, sentence)
            if match:
                for replacement in replacements:
                    varied = re.sub(pattern, replacement, sentence)
                    if varied != sentence:
                        variations.append({
                            'type': 'ending_variation',
                            'text': varied,
                            'description': '文末表現を変更'
                        })
                break  # 最初にマッチしたパターンのみ適用

        return variations

    def _vary_word_order(self, sentence: str) -> List[Dict[str, str]]:
        """語順のバリエーションを生成（限定的）"""
        variations = []

        # 時間表現を文末に移動
        time_pattern = r'^(昨日|今日|明日|先週|来週|去年|今年|来年)([、,]?\s*)(.+)([。！？])$'
        match = re.match(time_pattern, sentence)
        if match:
            time_word = match.group(1)
            rest = match.group(3)
            ending = match.group(4)
            varied = f"{rest}{time_word}{ending}"
            variations.append({
                'type': 'word_order',
                'text': varied,
                'description': '時間表現を文末に移動'
            })

        # 場所表現を文末に移動
        place_pattern = r'^(.+)(で|に|へ)([^。！？]+)([。！？])$'
        match = re.match(place_pattern, sentence)
        if match and len(match.group(1)) < 10:  # 短い場所表現のみ
            place = match.group(1) + match.group(2)
            action = match.group(3)
            ending = match.group(4)
            varied = f"{action}{place}{ending}"
            variations.append({
                'type': 'word_order',
                'text': varied,
                'description': '場所表現を文末に移動'
            })

        return variations

    def _suggest_omissions(self, sentence: str) -> List[Dict[str, str]]:
        """省略可能な部分を提案"""
        variations = []

        # 冗長な表現の省略
        redundant_patterns = [
            (r'(.+)という(こと|事)([。！？])$', r'\1\3', '「という」を省略'),
            (r'(.+)のである([。！？])$', r'\1である\2', '「の」を省略'),
            (r'(.+)のだ([。！？])$', r'\1だ\2', '「の」を省略'),
            (r'(.+)ということだ([。！？])$', r'\1だ\2', '「ということ」を省略'),
        ]

        for pattern, replacement, desc in redundant_patterns:
            match = re.match(pattern, sentence)
            if match:
                varied = re.sub(pattern, replacement, sentence)
                if varied != sentence:
                    variations.append({
                        'type': 'omission',
                        'text': varied,
                        'description': desc
                    })

        return variations

    def _add_conjunctions(self, sentence: str) -> List[Dict[str, str]]:
        """接続詞を追加したバリエーションを生成"""
        variations = []

        # 文頭に接続詞を追加
        conjunctions = [
            ('しかし、', '逆接の接続詞を追加'),
            ('また、', '並列の接続詞を追加'),
            ('そして、', '順接の接続詞を追加'),
            ('ただし、', '条件の接続詞を追加'),
        ]

        for conj, desc in conjunctions:
            # すでに接続詞で始まっていない場合のみ
            if not re.match(r'^(しかし|また|そして|ただし|なお|さらに)', sentence):
                varied = conj + sentence
                variations.append({
                    'type': 'conjunction',
                    'text': varied,
                    'description': desc
                })

        return variations

    def format_report(self, repetitions: List[Dict[str, any]], text: str = None) -> str:
        """
        繰り返し検出結果をフォーマット

        Args:
            repetitions: detect_repetitions() の戻り値
            text: 元のテキスト（コンテキスト表示用）

        Returns:
            フォーマットされた文字列
        """
        if not repetitions:
            return "✓ 同一文の繰り返しは見つかりませんでした。"

        lines = [f"\n{len(repetitions)}件の繰り返しが見つかりました:\n"]

        for i, rep in enumerate(repetitions, 1):
            lines.append(f"{i}. 【{rep['count']}回繰り返し】")
            lines.append(f"   元の文: {rep['sentence']}")

            if rep['variations']:
                lines.append(f"   \n   💡 バリエーション候補:")
                for j, var in enumerate(rep['variations'][:5], 1):  # 最大5個まで表示
                    lines.append(f"      {j}. {var['text']}")
                    lines.append(f"         ({var['description']})")

            lines.append("")

        return "\n".join(lines)

    def detect_and_report(self, text: str) -> str:
        """検出とレポート生成を一度に実行する便利メソッド"""
        repetitions = self.detect_repetitions(text)
        return self.format_report(repetitions, text)

    def export_repetitions_json(self, repetitions: List[Dict[str, any]], 
                               filepath: str) -> None:
        """
        検出された繰り返しをJSON形式で出力

        Args:
            repetitions: detect_repetitions() の戻り値
            filepath: 出力ファイルパス
        """
        output = {
            'metadata': {
                'total_repetitions': len(repetitions),
                'high_priority': sum(1 for r in repetitions if r['count'] >= 3),
            },
            'repetitions': repetitions
        }

        with open(filepath, 'w', encoding='utf-8') as f:
            json.dump(output, f, ensure_ascii=False, indent=2)

    def generate_correction_text(self, text: str, 
                               repetitions: List[Dict[str, any]] = None,
                               apply_all: bool = False,
                               select_indices: Dict[str, int] = None) -> str:
        """
        繰り返しを修正したテキストを生成

        Args:
            text: 元のテキスト
            repetitions: detect_repetitions() の戻り値（Noneの場合は自動検出）
            apply_all: Trueの場合、最初の候補を自動適用
            select_indices: {normalized_sentence: variation_index} で候補を指定

        Returns:
            修正されたテキスト
        """
        if repetitions is None:
            repetitions = self.detect_repetitions(text)

        if not repetitions:
            return text

        select_indices = select_indices or {}
        corrected = text

        # 後ろから置換（位置ズレを防ぐ）
        sorted_reps = sorted(repetitions, 
                            key=lambda x: x['positions'][0] if x['positions'] else 0,
                            reverse=True)

        for rep in sorted_reps:
            normalized = rep['normalized']
            positions = rep['positions']
            original = rep['sentence']

            # バリエーション候補を選択
            if normalized in select_indices:
                var_index = select_indices[normalized]
                if var_index < len(rep['variations']):
                    selected_var = rep['variations'][var_index]
                    replacement_text = selected_var['text']
                else:
                    continue
            elif apply_all and rep['variations']:
                replacement_text = rep['variations'][0]['text']
            else:
                continue

            # 2番目以降の出現をバリエーションで置換
            for i, pos in enumerate(positions[1:], 1):
                # テキスト上の位置から置換対象を特定
                end_pos = pos + len(original)
                if corrected[pos:end_pos] == original:
                    corrected = corrected[:pos] + replacement_text + corrected[end_pos:]

        return corrected

    def suggest_corrections(self, text: str, repetitions: List[Dict[str, any]] = None,
                          priority: str = 'high') -> List[Dict[str, any]]:
        """
        修正提案を生成

        Args:
            text: 元のテキスト
            repetitions: detect_repetitions() の戻り値
            priority: 'high' (3回以上), 'medium' (2-3回), 'all'

        Returns:
            修正提案のリスト
        """
        if repetitions is None:
            repetitions = self.detect_repetitions(text)

        suggestions = []

        priority_map = {'high': 3, 'medium': 2, 'all': 1}
        min_count = priority_map.get(priority, 3)

        for rep in repetitions:
            if rep['count'] < min_count:
                continue

            for var_idx, variation in enumerate(rep['variations']):
                # 最初の出現位置
                first_pos = rep['positions'][0]
                
                # 2番目以降の位置で置換を提案
                for pos_idx in range(1, len(rep['positions'])):
                    pos = rep['positions'][pos_idx]
                    
                    suggestions.append({
                        'position': pos,
                        'length': len(rep['sentence']),
                        'original': rep['sentence'],
                        'original_first_occurrence': first_pos,
                        'current_occurrence': pos_idx + 1,
                        'total_occurrences': rep['count'],
                        'suggested_replacement': variation['text'],
                        'variation_type': variation['type'],
                        'variation_description': variation['description'],
                        'reasoning': f"この文は{rep['count']}回繰り返されています（最初は {first_pos} 行目）"
                    })

        return suggestions

    def apply_suggestions(self, text: str, 
                        suggestions: List[Dict[str, any]],
                        confirm_func=None) -> str:
        """
        提案を対話的にテキストに適用

        Args:
            text: 元のテキスト
            suggestions: suggest_corrections() の戻り値
            confirm_func: 各提案について確認を取る関数（True/False を返す）

        Returns:
            修正されたテキスト
        """
        if not suggestions:
            return text

        # 後ろから置換（位置ズレを防ぐ）
        sorted_suggestions = sorted(suggestions, 
                                   key=lambda x: x['position'],
                                   reverse=True)

        result = text
        applied_count = 0

        for sugg in sorted_suggestions:
            if confirm_func and not confirm_func(sugg):
                continue

            pos = sugg['position']
            length = sugg['length']
            replacement = sugg['suggested_replacement']

            # テキスト上の位置から置換対象を特定
            if result[pos:pos+length] == sugg['original']:
                result = result[:pos] + replacement + result[pos+length:]
                applied_count += 1

        return result, applied_count
