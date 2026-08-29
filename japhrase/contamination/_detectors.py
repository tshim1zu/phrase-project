# coding: utf-8
"""
8種の汚染検出器（v2: 8軸再設計版）

各検出器は (text, lines, **kwargs) → List[Anomaly] を返す純粋関数。

8軸:
  1. encoding     — 文字化け・不正文字
  2. structural   — 括弧不整合 + マージ痕 + メタデータ + 作業注釈 + 空文字率 + 改行過多
  3. duplicate    — 段落/文の重複
  4. repetition   — フレーズ反復
  5. distribution — 分布断絶 + 外来語彙（旧vocabulary統合）
  6. complexity   — 圧縮率異常
  7. consistency  — 表記ゆれ + 句読点揺れ
  8. language     — 言語混在

全検出器は reference_text (テキスト間比較用) をオプション引数で受け取る。
"""

import re
import zlib
import math
from collections import Counter
from typing import List, Optional

import numpy as np

from .profile import Anomaly

# ═══════════════════════════════════════════════════════════════
# 1. ENCODING — 文字化け・不正Unicode・mojibake残骸
# ═══════════════════════════════════════════════════════════════

_MOJIBAKE = re.compile(
    r'[\xc0-\xff][\x80-\xbf]'
    r'|â€[™""\x9c\x9d\x98\x99]'
    r'|Ã[©¨»¼½¡±]'
    r'|ï¿½'
    r'|\x00'
    r'|[\ufffe\uffff]'
    r'|[\udc80-\udcff]'
)
_CONTROL = re.compile(r'[\x01-\x08\x0b\x0c\x0e-\x1f\x7f]')


def detect_encoding(text: str, lines: List[str], **kw) -> List[Anomaly]:
    """Detect encoding errors (mojibake and invalid control characters).
    
    Scans each line for invalid UTF-8 sequences, corrupted character patterns,
    and forbidden control characters.
    
    Args:
        text: Full text content (unused in this detector).
        lines: Text split into lines.
        **kw: Additional keyword arguments (unused).
    
    Returns:
        List of Anomaly objects for detected encoding issues.
    """
    anomalies = []
    for i, line in enumerate(lines):
        for m in _MOJIBAKE.finditer(line):
            anomalies.append(Anomaly(
                detector='encoding', severity=8,
                start=m.start(), end=m.end(), line_no=i,
                description=f'mojibakeパターン: {repr(m.group())}',
                snippet=line[max(0, m.start()-10):m.end()+10][:50],
            ))
        for m in _CONTROL.finditer(line):
            anomalies.append(Anomaly(
                detector='encoding', severity=6,
                start=m.start(), end=m.end(), line_no=i,
                description=f'不正制御文字: U+{ord(m.group()):04X}',
                snippet=line[max(0, m.start()-10):m.end()+10][:50],
            ))
    return anomalies


# ═══════════════════════════════════════════════════════════════
# 2. STRUCTURAL — 括弧 + マージ痕 + メタ混入 + 作業注釈 + 空文字率 + 改行過多
# ═══════════════════════════════════════════════════════════════

_OPEN_BRACKETS = {'「': '」', '『': '』', '（': '）', '(': ')', '[': ']', '【': '】'}
_CLOSE_BRACKETS = {v: k for k, v in _OPEN_BRACKETS.items()}

_MERGE_MARKERS = re.compile(r'^(<{7}|={7}|>{7})\s')
_METADATA_LEAK = re.compile(
    r'^---\s*$'             # YAML separator in body
    r'|</?[a-zA-Z][^>]*>'  # HTML tags
    r'|<!--.*?-->'          # HTML comments
    r'|\[\^.+?\]'           # Footnote markers without footnote
)
_WORK_ANNOTATIONS = re.compile(
    r'\[TODO\]|\[FIXME\]|\[TBD\]|\[WIP\]'
    r'|※要確認|※仮|（仮）|\(仮\)'
    r'|PLACEHOLDER|DUMMY|Lorem ipsum'
    r'|テスト文章|ダミーテキスト'
    r'|XXXX+|○○○+|△△△+',
    re.IGNORECASE,
)


def detect_structural(
    text: str, lines: List[str],
    empty_line_ratio_warn: float = 0.4,
    empty_line_ratio_error: float = 0.6,
    **kw,
) -> List[Anomaly]:
    """Detect structural anomalies (brackets, merge markers, metadata, blank lines).
    
    Checks for unmatched brackets, version control merge markers, metadata leaks,
    work annotations, excessive blank lines, and abnormally long lines.
    
    Args:
        text: Full text content.
        lines: Text split into lines.
        empty_line_ratio_warn: Threshold ratio for warning on empty lines (default 0.4).
        empty_line_ratio_error: Threshold ratio for error on empty lines (default 0.6).
        **kw: Additional keyword arguments (unused).
    
    Returns:
        List of Anomaly objects for detected structural issues.
    """
    anomalies = []
    n_lines = len(lines)

    # 括弧不整合
    for i, line in enumerate(lines):
        stack = []
        for ch in line:
            if ch in _OPEN_BRACKETS:
                stack.append(ch)
            elif ch in _CLOSE_BRACKETS:
                if stack and stack[-1] == _CLOSE_BRACKETS[ch]:
                    stack.pop()
                else:
                    anomalies.append(Anomaly(
                        detector='structural', severity=5,
                        start=0, end=len(line), line_no=i,
                        description=f'閉じ括弧 {ch} に対応する開き括弧がない',
                        snippet=line[:50],
                    ))
        for ob in stack:
            anomalies.append(Anomaly(
                detector='structural', severity=5,
                start=0, end=len(line), line_no=i,
                description=f'開き括弧 {ob} が閉じていない',
                snippet=line[:50],
            ))

    # マージ衝突痕
    for i, line in enumerate(lines):
        if _MERGE_MARKERS.match(line):
            anomalies.append(Anomaly(
                detector='structural', severity=9,
                start=0, end=len(line), line_no=i,
                description='バージョン管理のマージ衝突痕',
                snippet=line[:50],
            ))

    # メタデータ混入
    for i, line in enumerate(lines):
        if i == 0 and line.strip() == '---':
            continue  # ファイル先頭のYAML frontmatterは正常
        for m in _METADATA_LEAK.finditer(line):
            anomalies.append(Anomaly(
                detector='structural', severity=4,
                start=m.start(), end=m.end(), line_no=i,
                description=f'メタデータ/マークアップ混入: {m.group()[:30]}',
                snippet=line[:50],
            ))

    # 作業注釈・プレースホルダー残存
    for i, line in enumerate(lines):
        for m in _WORK_ANNOTATIONS.finditer(line):
            anomalies.append(Anomaly(
                detector='structural', severity=6,
                start=m.start(), end=m.end(), line_no=i,
                description=f'作業注釈/プレースホルダー残存: {m.group()[:30]}',
                snippet=line[:50],
            ))

    # 空文字率（空行の割合）
    if n_lines > 5:
        empty_count = sum(1 for line in lines if line.strip() == '')
        empty_ratio = empty_count / n_lines

        if empty_ratio >= empty_line_ratio_error:
            anomalies.append(Anomaly(
                detector='structural', severity=7,
                start=0, end=len(text), line_no=-1,
                description=f'空行率 {empty_ratio:.0%} — 全体の{empty_ratio:.0%}が空行',
                snippet=f'{empty_count}/{n_lines}行が空行',
            ))
        elif empty_ratio >= empty_line_ratio_warn:
            anomalies.append(Anomaly(
                detector='structural', severity=4,
                start=0, end=len(text), line_no=-1,
                description=f'空行率 {empty_ratio:.0%} — やや多い',
                snippet=f'{empty_count}/{n_lines}行が空行',
            ))

    # 連続空行（4行以上）
    consecutive = 0
    for i, line in enumerate(lines):
        if line.strip() == '':
            consecutive += 1
            if consecutive == 4:
                anomalies.append(Anomaly(
                    detector='structural', severity=3,
                    start=0, end=0, line_no=i,
                    description=f'連続空行が4行以上',
                    snippet='(空行)',
                ))
        else:
            consecutive = 0

    # 異常に長い行（1000字超）
    for i, line in enumerate(lines):
        if len(line) > 1000:
            anomalies.append(Anomaly(
                detector='structural', severity=4,
                start=0, end=len(line), line_no=i,
                description=f'異常に長い行 ({len(line)}字)',
                snippet=line[:30] + '...' + line[-20:],
            ))

    return anomalies


# ═══════════════════════════════════════════════════════════════
# 3. DUPLICATE — 段落/文の重複
# ═══════════════════════════════════════════════════════════════

def detect_duplicate(
    text: str, lines: List[str],
    min_length: int = 20,
    similarity_threshold: float = 0.9,
    reference_text: Optional[str] = None,
    **kw,
) -> List[Anomaly]:
    """Detect exact and near-duplicate paragraphs.
    
    Identifies duplicate and near-duplicate paragraphs within the text,
    and optionally compares against a reference text.
    
    Args:
        text: Full text content.
        lines: Text split into lines.
        min_length: Minimum paragraph length to consider for duplication detection (default 20).
        similarity_threshold: Jaccard similarity threshold for near-duplicate detection (default 0.9).
        reference_text: Optional reference text to compare against for cross-text duplication.
        **kw: Additional keyword arguments (unused).
    
    Returns:
        List of Anomaly objects for detected duplicate paragraphs.
    """
    anomalies = []

    # 段落分割
    paragraphs = _split_paragraphs(lines, min_length)

    # テキスト内重複
    seen = {}
    for line_no, para in paragraphs:
        normalized = re.sub(r'\s+', '', para)
        if normalized in seen:
            anomalies.append(Anomaly(
                detector='duplicate', severity=9,
                start=0, end=len(para), line_no=line_no,
                description=f'段落が完全重複 (初出: L{seen[normalized]+1})',
                snippet=para[:50],
            ))
        else:
            seen[normalized] = line_no

    # ほぼ一致
    if similarity_threshold < 1.0 and len(paragraphs) > 1:
        _detect_near_duplicates(paragraphs, anomalies, similarity_threshold)

    # テキスト間重複（reference_text が渡された場合）
    if reference_text:
        ref_paragraphs = _split_paragraphs(reference_text.split('\n'), min_length)
        ref_set = {re.sub(r'\s+', '', p) for _, p in ref_paragraphs}
        for line_no, para in paragraphs:
            normalized = re.sub(r'\s+', '', para)
            if normalized in ref_set:
                anomalies.append(Anomaly(
                    detector='duplicate', severity=7,
                    start=0, end=len(para), line_no=line_no,
                    description='別テキストとの段落重複',
                    snippet=para[:50],
                ))

    return anomalies


def _split_paragraphs(lines, min_length=20):
    """Split lines into paragraphs by blank lines.
    
    Args:
        lines: Text split into lines.
        min_length: Minimum paragraph length to include (default 20).
    
    Returns:
        List of tuples (start_line_no, paragraph_text) for paragraphs meeting min_length.
    """
    paragraphs = []
    current, start = [], 0
    for i, line in enumerate(lines):
        if line.strip() == '':
            if current:
                p = '\n'.join(current)
                if len(p.strip()) >= min_length:
                    paragraphs.append((start, p.strip()))
                current = []
            start = i + 1
        else:
            if not current:
                start = i
            current.append(line)
    if current:
        p = '\n'.join(current)
        if len(p.strip()) >= min_length:
            paragraphs.append((start, p.strip()))
    return paragraphs


def _detect_near_duplicates(paragraphs, anomalies, threshold):
    """Detect near-duplicate paragraphs using Jaccard similarity on 3-grams.
    
    Args:
        paragraphs: List of (line_no, paragraph_text) tuples.
        anomalies: List to append detected Anomaly objects to.
        threshold: Jaccard similarity threshold for near-duplicate detection.
    
    Returns:
        None (modifies anomalies list in place).
    """
    ng = 3

    def _ngrams(s):
        """Extract n-grams (character substrings of length ng) from input string.
        
        Args:
            s: Input string to extract n-grams from.
        
        Returns:
            Set of n-grams (character substrings of length 3 after whitespace removal).
        """
        s = re.sub(r'\s+', '', s)
        return set(s[i:i+ng] for i in range(len(s)-ng+1))

    items = [(ln, p, _ngrams(p)) for ln, p in paragraphs]
    for i in range(len(items)):
        for j in range(i+1, len(items)):
            ln_i, p_i, ng_i = items[i]
            ln_j, p_j, ng_j = items[j]
            if not ng_i or not ng_j:
                continue
            if re.sub(r'\s+', '', p_i) == re.sub(r'\s+', '', p_j):
                continue
            jaccard = len(ng_i & ng_j) / len(ng_i | ng_j)
            if jaccard >= threshold:
                anomalies.append(Anomaly(
                    detector='duplicate', severity=7,
                    start=0, end=len(p_j), line_no=ln_j,
                    description=f'段落がほぼ重複 (類似度{jaccard:.0%}, 参照: L{ln_i+1})',
                    snippet=p_j[:50],
                ))


# ═══════════════════════════════════════════════════════════════
# 4. REPETITION — 短区間内の異常なフレーズ反復
# ═══════════════════════════════════════════════════════════════

def detect_repetition(
    text: str, lines: List[str],
    window_size: int = 500,
    min_phrase_len: int = 4,
    max_repeat: int = 3,
    **kw,
) -> List[Anomaly]:
    """Detect abnormal phrase repetition within short windows.
    
    Identifies phrases that repeat excessively within a short text window,
    indicating potential stylistic issues or copy-paste errors.
    
    Args:
        text: Full text content.
        lines: Text split into lines (unused in this detector).
        window_size: Character window size for repetition detection (default 500).
        min_phrase_len: Minimum phrase length to check for repetition (default 4).
        max_repeat: Maximum acceptable repetition count per window (default 3).
        **kw: Additional keyword arguments (unused).
    
    Returns:
        List of Anomaly objects for detected phrase repetition.
    """
    anomalies = []
    clean = re.sub(r'\s+', '', text)
    if len(clean) < window_size:
        windows = [(0, clean)]
    else:
        step = window_size // 2
        windows = [(i, clean[i:i+window_size]) for i in range(0, len(clean)-window_size+1, step)]

    seen_phrases = {}
    for win_start, window in windows:
        for n in range(min_phrase_len, min_phrase_len+3):
            freq = Counter(window[i:i+n] for i in range(len(window)-n+1))
            for gram, count in freq.items():
                if count > max_repeat:
                    if gram not in seen_phrases or count > seen_phrases[gram].severity:
                        pos = text.find(gram)
                        line_no = text[:pos].count('\n') if pos >= 0 else -1
                        seen_phrases[gram] = Anomaly(
                            detector='repetition',
                            severity=min(6, count - max_repeat + 3),
                            start=win_start, end=win_start+window_size,
                            line_no=line_no,
                            description=f'「{gram}」が{window_size}字内に{count}回反復',
                            snippet=gram,
                        )
    return list(seen_phrases.values())


# ═══════════════════════════════════════════════════════════════
# 5. DISTRIBUTION — 分布断絶 + 外来語彙（旧vocabulary統合）
# ═══════════════════════════════════════════════════════════════

def detect_distribution(
    text: str, lines: List[str],
    segment_size: int = 300,
    jsd_threshold: float = 0.35,
    foreign_threshold: float = 0.3,
    reference_text: Optional[str] = None,
    **kw,
) -> List[Anomaly]:
    """Detect character distribution anomalies and foreign vocabulary concentration.
    
    Identifies segments with significantly different character distributions (using JSD)
    or unusual concentration of vocabulary patterns, suggesting copy-paste or encoding issues.
    
    Args:
        text: Full text content.
        lines: Text split into lines (unused in this detector).
        segment_size: Character window size for distribution analysis (default 300).
        jsd_threshold: Jensen-Shannon divergence threshold for distribution discontinuity (default 0.35).
        foreign_threshold: Ratio threshold for foreign vocabulary concentration (default 0.3).
        reference_text: Optional reference text to compare overall distribution against.
        **kw: Additional keyword arguments (unused).
    
    Returns:
        List of Anomaly objects for detected distribution anomalies.
    """
    anomalies = []
    clean = re.sub(r'\s+', '', text)
    if len(clean) < segment_size * 2:
        return anomalies

    # セグメント分割 + 2-gram 頻度
    segments = []
    for i in range(0, len(clean)-segment_size+1, segment_size//2):
        seg = clean[i:i+segment_size]
        freq = Counter(seg[j:j+2] for j in range(len(seg)-1))
        segments.append((i, seg, freq))

    # --- 隣接JSD（分布断絶）---
    for i in range(1, len(segments)):
        pos_p, _, fp = segments[i-1]
        pos_c, seg_c, fc = segments[i]
        jsd = _jsd_from_counters(fp, fc)
        if jsd > jsd_threshold:
            line_no = text[:pos_c].count('\n') if pos_c < len(text) else -1
            anomalies.append(Anomaly(
                detector='distribution', severity=min(7, int(jsd*10)),
                start=pos_c, end=pos_c+segment_size, line_no=line_no,
                description=f'分布断絶 JSD={jsd:.4f}',
                snippet=seg_c[:30],
            ))

    # --- 外来語彙（旧vocabulary）---
    global_freq = Counter(clean[i:i+2] for i in range(len(clean)-1))
    total_bg = sum(global_freq.values())
    for pos, seg, local_freq in segments:
        local_total = sum(local_freq.values())
        if local_total == 0:
            continue
        foreign_count = 0
        for bg, lc in local_freq.items():
            gr = global_freq.get(bg, 0) / total_bg
            lr = lc / local_total
            if gr > 0 and lr > gr * 10:
                foreign_count += lc
        ratio = foreign_count / local_total
        if ratio > foreign_threshold:
            line_no = text[:pos].count('\n') if pos < len(text) else -1
            anomalies.append(Anomaly(
                detector='distribution', severity=min(6, int(ratio*10)),
                start=pos, end=pos+segment_size, line_no=line_no,
                description=f'外来語彙集中 {ratio:.0%}',
                snippet=seg[:30],
            ))

    # --- テキスト間比較（reference_text）---
    if reference_text:
        ref_clean = re.sub(r'\s+', '', reference_text)
        ref_freq = Counter(ref_clean[i:i+2] for i in range(len(ref_clean)-1))
        whole_freq = Counter(clean[i:i+2] for i in range(len(clean)-1))
        jsd = _jsd_from_counters(whole_freq, ref_freq)
        if jsd > jsd_threshold:
            anomalies.append(Anomaly(
                detector='distribution', severity=min(7, int(jsd*10)),
                start=0, end=len(text), line_no=-1,
                description=f'テキスト間の分布距離 JSD={jsd:.4f}',
                snippet='(全体)',
            ))

    return anomalies


def _jsd_from_counters(a: Counter, b: Counter) -> float:
    """Calculate Jensen-Shannon divergence between two character frequency distributions.
    
    Args:
        a: Counter object of character frequencies (first distribution).
        b: Counter object of character frequencies (second distribution).
    
    Returns:
        Jensen-Shannon divergence as a float in [0.0, 1.0].
    """
    keys = sorted(set(a.keys()) | set(b.keys()))
    p = np.array([a.get(k, 0) for k in keys], dtype=float)
    q = np.array([b.get(k, 0) for k in keys], dtype=float)
    ps, qs = p.sum(), q.sum()
    if ps == 0 or qs == 0:
        return 1.0
    p /= ps
    q /= qs
    m = 0.5 * (p + q)
    jsd = 0.0
    for arr in [p, q]:
        mask = arr > 0
        jsd += 0.5 * float(np.sum(arr[mask] * np.log2(arr[mask] / m[mask])))
    return max(0.0, min(1.0, jsd))


# ═══════════════════════════════════════════════════════════════
# 6. COMPLEXITY — 局所圧縮率異常
# ═══════════════════════════════════════════════════════════════

def detect_complexity(
    text: str, lines: List[str],
    segment_size: int = 300,
    compression_low: float = 0.15,
    compression_high: float = 0.85,
    **kw,
) -> List[Anomaly]:
    """Detect complexity anomalies via zlib compression rate analysis.
    
    Identifies segments with unusually low compression (repetitive patterns) or
    high compression (random/high-entropy data), which may indicate data issues.
    
    Args:
        text: Full text content.
        lines: Text split into lines (unused in this detector).
        segment_size: Character window size for compression analysis (default 300).
        compression_low: Compression ratio threshold for low complexity (default 0.15).
        compression_high: Compression ratio threshold for high complexity (default 0.85).
        **kw: Additional keyword arguments (unused).
    
    Returns:
        List of Anomaly objects for detected compression anomalies.
    """
    anomalies = []
    clean = re.sub(r'\n{3,}', '\n\n', text)
    if len(clean) < segment_size:
        return anomalies

    segments = []
    for i in range(0, len(clean)-segment_size+1, segment_size):
        seg = clean[i:i+segment_size]
        if len(seg.strip()) < 50:
            continue
        segments.append((i, seg))

    if not segments:
        return anomalies

    ratios = []
    for pos, seg in segments:
        enc = seg.encode('utf-8')
        ratio = len(zlib.compress(enc, 9)) / len(enc) if len(enc) > 0 else 0
        ratios.append((pos, seg, ratio))

    all_r = [r for _, _, r in ratios]
    mean_r, std_r = np.mean(all_r), np.std(all_r)

    for pos, seg, ratio in ratios:
        line_no = text[:pos].count('\n') if pos < len(text) else -1
        if ratio < compression_low:
            anomalies.append(Anomaly(
                detector='complexity', severity=6,
                start=pos, end=pos+segment_size, line_no=line_no,
                description=f'圧縮率極低 ({ratio:.3f}) — 同一パターン繰り返し',
                snippet=seg[:40],
            ))
        elif ratio > compression_high:
            anomalies.append(Anomaly(
                detector='complexity', severity=4,
                start=pos, end=pos+segment_size, line_no=line_no,
                description=f'圧縮率極高 ({ratio:.3f}) — ランダムデータの可能性',
                snippet=seg[:40],
            ))
        elif std_r > 0 and abs(ratio - mean_r) > 2.5 * std_r:
            anomalies.append(Anomaly(
                detector='complexity', severity=5,
                start=pos, end=pos+segment_size, line_no=line_no,
                description=f'圧縮率2.5σ逸脱 ({ratio:.3f}, μ={mean_r:.3f})',
                snippet=seg[:40],
            ))

    return anomalies


# ═══════════════════════════════════════════════════════════════
# 7. CONSISTENCY — 表記ゆれ + 句読点揺れ
# ═══════════════════════════════════════════════════════════════

# 句読点ペア: (正, 揺れ) — 同一文書内で混在したら揺れ
_PUNCTUATION_PAIRS = [
    ('、', ','),        # 読点
    ('。', '.'),        # 句点
    ('！', '!'),        # 感嘆符
    ('？', '?'),        # 疑問符
    ('「', '"'),        # 鉤括弧 vs ダブルクォート
    ('…', '...'),       # 三点リーダー
    ('—', 'ー'),        # ダッシュ vs 長音（文脈による）
]

# カタカナ長音ゆれ: サーバー/サーバ 等
_KATAKANA_LONGVOWEL = re.compile(r'[ァ-ヶ]{2,}ー')
_KATAKANA_NO_LONG = re.compile(r'[ァ-ヶ]{3,}(?!ー)')


def detect_consistency(
    text: str, lines: List[str],
    reference_text: Optional[str] = None,
    **kw,
) -> List[Anomaly]:
    """Detect orthographic inconsistencies (punctuation and character representation).
    
    Identifies mixing of full-width and half-width punctuation, kanji/hiragana variations,
    and katakana long-vowel inconsistencies indicating poor editorial review.
    
    Args:
        text: Full text content.
        lines: Text split into lines (unused in this detector).
        reference_text: Optional reference text to compare punctuation style against.
        **kw: Additional keyword arguments (unused).
    
    Returns:
        List of Anomaly objects for detected inconsistencies.
    """
    anomalies = []

    # --- 句読点の揺れ ---
    for full, half in _PUNCTUATION_PAIRS:
        count_full = text.count(full)
        count_half = text.count(half)
        if count_full > 0 and count_half > 0:
            total = count_full + count_half
            minority = min(count_full, count_half)
            majority_char = full if count_full >= count_half else half
            minority_char = half if majority_char == full else full
            ratio = minority / total

            if ratio > 0.05:  # 5%以上混在で検出
                severity = 3 if ratio < 0.2 else 5 if ratio < 0.4 else 7
                anomalies.append(Anomaly(
                    detector='consistency', severity=severity,
                    start=0, end=len(text), line_no=-1,
                    description=(
                        f'句読点揺れ: {majority_char}({count_full}回) vs '
                        f'{minority_char}({count_half}回) — {ratio:.0%}混在'
                    ),
                    snippet=f'{majority_char}→{minority_char}',
                ))

    # --- 表記ゆれ: 同じ語の異表記 ---
    # カタカナ長音の有無（サーバー vs サーバ）
    with_long = set()
    without_long = set()
    for m in _KATAKANA_LONGVOWEL.finditer(text):
        base = m.group().rstrip('ー')
        if len(base) >= 2:
            with_long.add(base)
    for m in _KATAKANA_NO_LONG.finditer(text):
        word = m.group()
        base = word.rstrip('ー')
        if base in with_long:
            without_long.add(base)

    for base in without_long:
        pos = text.find(base)
        line_no = text[:pos].count('\n') if pos >= 0 else -1
        anomalies.append(Anomaly(
            detector='consistency', severity=4,
            start=pos, end=pos+len(base), line_no=line_no,
            description=f'カタカナ長音揺れ: {base}ー / {base}',
            snippet=f'{base}ー vs {base}',
        ))

    # --- 漢字/ひらがな揺れ（よくあるペア） ---
    _KANJI_HIRA_PAIRS = [
        ('出来る', 'できる'), ('事', 'こと'), ('物', 'もの'),
        ('所', 'ところ'), ('時', 'とき'), ('為', 'ため'),
        ('様', 'よう'), ('筈', 'はず'), ('訳', 'わけ'),
        ('迄', 'まで'), ('位', 'くらい'), ('程', 'ほど'),
    ]
    for kanji, hira in _KANJI_HIRA_PAIRS:
        ck = text.count(kanji)
        ch = text.count(hira)
        if ck > 0 and ch > 0:
            total = ck + ch
            minority = min(ck, ch)
            if minority / total > 0.1:
                anomalies.append(Anomaly(
                    detector='consistency', severity=3,
                    start=0, end=len(text), line_no=-1,
                    description=f'漢字/ひらがな揺れ: {kanji}({ck}回) vs {hira}({ch}回)',
                    snippet=f'{kanji} vs {hira}',
                ))

    # --- テキスト間: 句読点スタイルの不一致 ---
    if reference_text:
        for full, half in _PUNCTUATION_PAIRS:
            # このテキストの多数派
            my_full, my_half = text.count(full), text.count(half)
            ref_full, ref_half = reference_text.count(full), reference_text.count(half)
            my_pref = full if my_full >= my_half else half
            ref_pref = full if ref_full >= ref_half else half
            if my_pref != ref_pref and (my_full + my_half > 3) and (ref_full + ref_half > 3):
                anomalies.append(Anomaly(
                    detector='consistency', severity=4,
                    start=0, end=len(text), line_no=-1,
                    description=f'テキスト間の句読点不一致: 本文={my_pref}, 参照={ref_pref}',
                    snippet=f'{my_pref} vs {ref_pref}',
                ))

    return anomalies


# ═══════════════════════════════════════════════════════════════
# 8. LANGUAGE — 言語混在
# ═══════════════════════════════════════════════════════════════

_JP_CHARS = re.compile(r'[\u3040-\u30ff\u4e00-\u9fff]')  # ひらがな+カタカナ+漢字
_EN_CHARS = re.compile(r'[a-zA-Z]')


def detect_language(
    text: str, lines: List[str],
    segment_size: int = 200,
    mix_threshold: float = 0.3,
    **kw,
) -> List[Anomaly]:
    """
    言語混在検出

    日本語テキスト中に英語ブロックが混入（またはその逆）を検出。
    文字種比率のセグメント間変動で判定。
    """
    anomalies = []
    clean = text.replace('\n', ' ')
    if len(clean) < segment_size:
        return anomalies

    # 全体の言語比率を基準にする
    total_jp = len(_JP_CHARS.findall(clean))
    total_en = len(_EN_CHARS.findall(clean))
    total_chars = total_jp + total_en
    if total_chars == 0:
        return anomalies
    global_jp_ratio = total_jp / total_chars

    # セグメントごとの言語比率
    for i in range(0, len(clean)-segment_size+1, segment_size):
        seg = clean[i:i+segment_size]
        seg_jp = len(_JP_CHARS.findall(seg))
        seg_en = len(_EN_CHARS.findall(seg))
        seg_total = seg_jp + seg_en
        if seg_total == 0:
            continue
        seg_jp_ratio = seg_jp / seg_total

        # 全体と大きく乖離しているセグメント
        deviation = abs(seg_jp_ratio - global_jp_ratio)
        if deviation > mix_threshold:
            line_no = text[:i].count('\n') if i < len(text) else -1
            if seg_jp_ratio < global_jp_ratio:
                desc = f'英語ブロック混入 (JP率{seg_jp_ratio:.0%}, 全体{global_jp_ratio:.0%})'
            else:
                desc = f'日本語ブロック混入 (JP率{seg_jp_ratio:.0%}, 全体{global_jp_ratio:.0%})'

            anomalies.append(Anomaly(
                detector='language', severity=min(6, int(deviation * 10)),
                start=i, end=i+segment_size, line_no=line_no,
                description=desc,
                snippet=seg[:40].strip(),
            ))

    return anomalies


# ═══════════════════════════════════════════════════════════════
# Registry
# ═══════════════════════════════════════════════════════════════

DETECTOR_REGISTRY = {
    'encoding': detect_encoding,
    'structural': detect_structural,
    'duplicate': detect_duplicate,
    'repetition': detect_repetition,
    'distribution': detect_distribution,
    'complexity': detect_complexity,
    'consistency': detect_consistency,
    'language': detect_language,
}

ALL_DETECTOR_NAMES = list(DETECTOR_REGISTRY.keys())
