# coding: utf-8
"""
キャラクター文体指紋（Character Stylometry）

キャラごとの台詞・地の文を分離し、語彙傾向が一貫しているか、
キャラ間で混ざっていないかを統計的に検出する。

使用例:
    >>> cs = CharacterStylometry()
    >>> fingerprints = cs.build_fingerprints(
    ...     texts={"EP101": text1, "EP102": text2},
    ...     characters=["エリス", "レティシア", "ソフィア"],
    ... )
    >>> print(cs.compare_characters(fingerprints, "エリス", "レティシア"))
"""

__author__ = "Takeshi SHIMIZU"
__copyright__ = "Copyright 2023-2026"

import re
import logging
from dataclasses import dataclass, field
from typing import Dict, List, Optional, Tuple, Set
from collections import Counter

import numpy as np
import pandas as pd

from ..stylometry import StylometryAnalyzer
from ..distribution_comparator import DistributionComparator
from ..complexity_metrics import ComplexityAnalyzer

logger = logging.getLogger(__name__)


@dataclass
class CharacterFingerprint:
    """キャラクターの文体指紋"""
    name: str
    # コーパスサイズ
    total_chars: int
    speech_chars: int           # 台詞部分の文字数
    narration_chars: int        # 地の文（そのキャラ周辺）の文字数
    ep_count: int               # 登場EP数

    # 台詞の文体指標
    speech_mattr: float
    speech_hapax_ratio: float
    speech_avg_sentence_len: float
    speech_kanji_ratio: float

    # 語彙頻度（上位語）
    top_speech_terms: List[Tuple[str, int]]     # [(語, 頻度), ...]
    top_narration_terms: List[Tuple[str, int]]

    # キーワード（この人だけに多い語）
    keyness_terms: List[Tuple[str, float]]      # [(語, keyness_score), ...]


@dataclass
class CharacterComparison:
    """2キャラの比較結果"""
    char_a: str
    char_b: str
    speech_jsd: float           # 台詞の JSD
    narration_jsd: float        # 地の文の JSD
    separation_score: float     # 分離度（0=完全混合, 1=完全分離）
    shared_top_terms: List[str] # 共通する頻出語
    distinguishing_a: List[str] # A だけに多い語
    distinguishing_b: List[str] # B だけに多い語


class CharacterStylometry:
    """
    キャラクター文体分析エンジン

    使用例:
        >>> cs = CharacterStylometry()
        >>> fps = cs.build_fingerprints(ep_texts, characters)
        >>> report = cs.full_report(fps)
    """

    # 台詞抽出パターン
    # JP: 「〜」 / 『〜』
    _SPEECH_JP = re.compile(r'[「『](.*?)[」』]', re.DOTALL)
    # EN: "〜" / 'single quotes for thought'
    _SPEECH_EN = re.compile(r'"(.*?)"', re.DOTALL)

    def __init__(
        self,
        context_window: int = 200,
        min_speech_chars: int = 50,
        lang: str = 'jp',
    ):
        """
        Parameters:
            context_window: キャラ名の前後何文字を地の文コーパスとするか
            min_speech_chars: 最低台詞文字数（これ未満はスキップ）
            lang: 'jp' or 'en'
        """
        self.context_window = context_window
        self.min_speech_chars = min_speech_chars
        self.lang = lang
        self.stylometry = StylometryAnalyzer()
        self.comparator = DistributionComparator()

    def build_fingerprints(
        self,
        ep_texts: Dict[str, str],
        characters: List[str],
    ) -> Dict[str, CharacterFingerprint]:
        """
        キャラクター別の文体指紋を構築

        Parameters:
            ep_texts: {ラベル: テキスト}
            characters: キャラクター名のリスト

        Returns:
            {キャラ名: CharacterFingerprint}
        """
        # 全テキスト結合
        full_text = '\n'.join(ep_texts.values())

        # キャラ別コーパス構築
        char_speech = {c: [] for c in characters}
        char_narration = {c: [] for c in characters}
        char_eps = {c: set() for c in characters}

        for label, text in ep_texts.items():
            for char_name in characters:
                # 台詞の抽出: キャラ名の直後の「〜」を拾う
                speech_segments = self._extract_speech(text, char_name)
                if speech_segments:
                    char_speech[char_name].extend(speech_segments)
                    char_eps[char_name].add(label)

                # 地の文の抽出: キャラ名周辺のコンテキスト
                narration_segments = self._extract_narration(text, char_name)
                if narration_segments:
                    char_narration[char_name].extend(narration_segments)
                    char_eps[char_name].add(label)

        # 全キャラの台詞を結合（キーネス計算の reference 用）
        all_speech = ' '.join(
            seg for segs in char_speech.values() for seg in segs
        )
        all_speech_freq = self._text_to_freq(all_speech)

        # 指紋生成
        fingerprints = {}
        for char_name in characters:
            speech_text = ' '.join(char_speech[char_name])
            narration_text = ' '.join(char_narration[char_name])

            if len(speech_text.replace(' ', '')) < self.min_speech_chars:
                logger.debug(f"{char_name}: 台詞が少なすぎてスキップ")
                continue

            # 文体分析
            adv = self.stylometry.analyze_advanced_diversity(speech_text)
            mattr_r = self.stylometry.analyze_mattr(speech_text, window_size=30)
            char_type = self.stylometry.analyze_char_type_ratio(speech_text)
            sent = self.stylometry.analyze_sentence_length(speech_text)

            # 頻度
            speech_freq = self._text_to_freq(speech_text)
            narration_freq = self._text_to_freq(narration_text)

            top_speech = speech_freq.most_common(20)
            top_narration = narration_freq.most_common(20)

            # キーネス（このキャラ固有の語）
            if speech_freq and all_speech_freq:
                comp = self.comparator.compare(speech_freq, all_speech_freq)
                keyness = [
                    (kr.term, kr.keyness_score)
                    for kr in comp.keyness_results[:15]
                    if kr.direction == 'overuse' and kr.significance_level != 'ns'
                ]
            else:
                keyness = []

            fingerprints[char_name] = CharacterFingerprint(
                name=char_name,
                total_chars=len(speech_text) + len(narration_text),
                speech_chars=len(speech_text.replace(' ', '')),
                narration_chars=len(narration_text.replace(' ', '')),
                ep_count=len(char_eps[char_name]),
                speech_mattr=mattr_r['mattr'],
                speech_hapax_ratio=adv['hapax_ratio'],
                speech_avg_sentence_len=sent.get('avg_length', 0),
                speech_kanji_ratio=char_type.get('kanji_ratio', 0),
                top_speech_terms=top_speech,
                top_narration_terms=top_narration,
                keyness_terms=keyness,
            )

        return fingerprints

    def compare_characters(
        self,
        fingerprints: Dict[str, CharacterFingerprint],
        char_a: str,
        char_b: str,
        ep_texts: Optional[Dict[str, str]] = None,
    ) -> CharacterComparison:
        """
        2キャラの文体を比較

        Parameters:
            fingerprints: build_fingerprints の結果
            char_a: キャラA名
            char_b: キャラB名
            ep_texts: 元テキスト（台詞再抽出に使用）
        """
        fp_a = fingerprints.get(char_a)
        fp_b = fingerprints.get(char_b)

        if not fp_a or not fp_b:
            return CharacterComparison(
                char_a=char_a, char_b=char_b,
                speech_jsd=1.0, narration_jsd=1.0,
                separation_score=0.0,
                shared_top_terms=[], distinguishing_a=[], distinguishing_b=[],
            )

        # 台詞の語彙分布を比較
        freq_a = Counter(dict(fp_a.top_speech_terms))
        freq_b = Counter(dict(fp_b.top_speech_terms))

        speech_comp = self.comparator.compare(freq_a, freq_b)

        # 地の文
        nfreq_a = Counter(dict(fp_a.top_narration_terms))
        nfreq_b = Counter(dict(fp_b.top_narration_terms))
        narr_comp = self.comparator.compare(nfreq_a, nfreq_b)

        # 共通語 / 固有語
        terms_a = {t for t, _ in fp_a.top_speech_terms[:15]}
        terms_b = {t for t, _ in fp_b.top_speech_terms[:15]}
        shared = terms_a & terms_b
        only_a = terms_a - terms_b
        only_b = terms_b - terms_a

        # 分離度 = JSD の平均（高いほど分離されている）
        separation = (speech_comp.jsd + narr_comp.jsd) / 2

        return CharacterComparison(
            char_a=char_a,
            char_b=char_b,
            speech_jsd=speech_comp.jsd,
            narration_jsd=narr_comp.jsd,
            separation_score=round(separation, 6),
            shared_top_terms=sorted(shared),
            distinguishing_a=sorted(only_a),
            distinguishing_b=sorted(only_b),
        )

    def separation_matrix(
        self,
        fingerprints: Dict[str, CharacterFingerprint],
    ) -> pd.DataFrame:
        """全キャラペアの分離度マトリクス"""
        chars = sorted(fingerprints.keys())
        n = len(chars)
        matrix = np.zeros((n, n))

        for i in range(n):
            for j in range(i + 1, n):
                comp = self.compare_characters(fingerprints, chars[i], chars[j])
                matrix[i, j] = comp.separation_score
                matrix[j, i] = comp.separation_score

        return pd.DataFrame(matrix, index=chars, columns=chars)

    def full_report(
        self,
        fingerprints: Dict[str, CharacterFingerprint],
    ) -> str:
        """全キャラの文体指紋レポート"""
        lines = [
            "=" * 72,
            "【キャラクター文体指紋レポート】",
            "=" * 72,
            "",
        ]

        # 個別指紋
        for name, fp in sorted(fingerprints.items()):
            lines.append(f"【{name}】")
            lines.append(f"  台詞: {fp.speech_chars}字 / 地の文: {fp.narration_chars}字 / 登場EP: {fp.ep_count}")
            lines.append(f"  MATTR: {fp.speech_mattr:.4f} / Hapax: {fp.speech_hapax_ratio:.4f}")
            lines.append(f"  平均文長: {fp.speech_avg_sentence_len:.1f}字 / 漢字率: {fp.speech_kanji_ratio:.1%}")

            if fp.keyness_terms:
                keywords = ', '.join(f"{t}({s:.3f})" for t, s in fp.keyness_terms[:5])
                lines.append(f"  固有語: {keywords}")
            lines.append("")

        # 分離度マトリクス
        if len(fingerprints) >= 2:
            matrix = self.separation_matrix(fingerprints)
            lines.append("-" * 72)
            lines.append("【キャラ間分離度マトリクス（JSD）】")
            lines.append("  高い = よく分離されている / 低い = 語彙が混ざっている")
            lines.append("")

            chars = list(matrix.columns)
            # ヘッダ
            header = f"  {'':>8s} " + ' '.join(f"{c[:4]:>6s}" for c in chars)
            lines.append(header)

            for i, char in enumerate(chars):
                row_vals = ' '.join(
                    f"{matrix.iloc[i, j]:6.4f}" if i != j else f"{'---':>6s}"
                    for j in range(len(chars))
                )
                lines.append(f"  {char[:8]:>8s} {row_vals}")

            # 最も混ざりやすいペア
            min_sep = float('inf')
            min_pair = ('', '')
            for i in range(len(chars)):
                for j in range(i + 1, len(chars)):
                    if matrix.iloc[i, j] < min_sep:
                        min_sep = matrix.iloc[i, j]
                        min_pair = (chars[i], chars[j])

            if min_sep < float('inf'):
                lines.append(f"\n  ⚠ 最も混ざりやすい: {min_pair[0]} ↔ {min_pair[1]} (JSD={min_sep:.6f})")

        lines.append("")
        lines.append("=" * 72)
        return "\n".join(lines)

    # ─── Private ────────────────────────────────────────────

    def _extract_speech(self, text: str, char_name: str) -> List[str]:
        """キャラの台詞を抽出

        キャラ名が短い区間内に複数回登場すると（地の文中の言及の繰り返し等）、
        同じ台詞が複数のname出現それぞれから見つかってしまう。台詞の絶対位置
        (開始/終了オフセット)で重複排除し、同一台詞を二重に数えないようにする。
        """
        segments = []
        seen_spans = set()
        pat = self._SPEECH_JP if self.lang == 'jp' else self._SPEECH_EN

        # キャラ名の近くにある台詞を抽出
        # パターン: {char_name}...「台詞」 or 台詞の直前にキャラ名がある
        for m in re.finditer(re.escape(char_name), text):
            # キャラ名の後方200字以内の台詞を探す
            after = text[m.end():m.end() + self.context_window]
            for speech_m in pat.finditer(after):
                span = (m.end() + speech_m.start(), m.end() + speech_m.end())
                if span in seen_spans:
                    break  # 別のname出現から既に拾い済みの同一台詞
                speech_text = speech_m.group(1)
                if len(speech_text) >= 2:
                    segments.append(speech_text)
                    seen_spans.add(span)
                break  # 最初の台詞のみ

        return segments

    def _extract_narration(self, text: str, char_name: str) -> List[str]:
        """キャラ周辺の地の文を抽出

        キャラ名が短い区間内に複数回登場すると、各出現の前後コンテキスト
        ウィンドウが重なり合い、同じ地の文が何度も数えられてしまう。
        先に全出現のウィンドウ範囲を統合(マージ)してから、重ならない
        区間ごとに1回だけ地の文を抽出する。
        """
        segments = []
        pat = self._SPEECH_JP if self.lang == 'jp' else self._SPEECH_EN

        ranges = []
        for m in re.finditer(re.escape(char_name), text):
            start = max(0, m.start() - self.context_window)
            end = min(len(text), m.end() + self.context_window)
            ranges.append((start, end))

        for start, end in self._merge_ranges(ranges):
            context = text[start:end]

            # 台詞を除去して地の文だけ残す
            narration = pat.sub('', context)
            narration = re.sub(r'\s+', ' ', narration).strip()
            if len(narration) >= 10:
                segments.append(narration)

        return segments

    @staticmethod
    def _merge_ranges(ranges: List[Tuple[int, int]]) -> List[Tuple[int, int]]:
        """重なり合う/隣接する (start, end) 区間を統合する。"""
        if not ranges:
            return []
        merged = [sorted(ranges)[0]]
        for start, end in sorted(ranges)[1:]:
            last_start, last_end = merged[-1]
            if start <= last_end:
                merged[-1] = (last_start, max(last_end, end))
            else:
                merged.append((start, end))
        return merged

    def _text_to_freq(self, text: str) -> Counter:
        """空白を除いた文字2-gramの頻度分布を返す。"""
        clean = re.sub(r'\s+', '', text)
        return Counter(clean[i:i + 2] for i in range(len(clean) - 1))
