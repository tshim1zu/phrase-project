# coding: utf-8
"""
7種の汚染検出器

各検出器は (text, lines) → List[Anomaly] を返す純粋関数。
状態を持たない。閾値はパラメータで渡す。
"""

import re
import zlib
import math
from collections import Counter
from typing import List, Tuple, Dict, Optional

import numpy as np

from .profile import Anomaly

# ═══════════════════════════════════════════════════════════════
# 1. ENCODING — 文字化け・不正Unicode・mojibake残骸
# ═══════════════════════════════════════════════════════════════

# 既知のmojibakeパターン（UTF-8をLatin-1等で誤読した残骸）
_MOJIBAKE_PATTERNS = re.compile(
    r'[\xc0-\xff][\x80-\xbf]'           # 生UTF-8バイト列がLatin-1で表示
    r'|â€[™""\x9c\x9d\x98\x99]'         # UTF-8 smart quotes → Latin-1
    r'|Ã[©¨»¼½¡±]'                      # UTF-8 accented chars → Latin-1
    r'|ï¿½'                              # U+FFFD replacement character
    r'|\x00'                             # null byte
    r'|[\ufffe\uffff]'                   # BOM/noncharacter
    r'|[\udc80-\udcff]'                  # surrogate halves (lone)
)

# 制御文字（タブ・改行・CR以外）
_CONTROL_CHARS = re.compile(r'[\x01-\x08\x0b\x0c\x0e-\x1f\x7f]')


def detect_encoding(text: str, lines: List[str], **kwargs) -> List[Anomaly]:
    """文字化け・不正Unicode検出"""
    anomalies = []

    for i, line in enumerate(lines):
        # mojibake
        for m in _MOJIBAKE_PATTERNS.finditer(line):
            anomalies.append(Anomaly(
                detector='encoding', severity=8,
                start=m.start(), end=m.end(), line_no=i,
                description=f'mojibakeパターン検出: {repr(m.group())}',
                snippet=line[max(0, m.start() - 10):m.end() + 10][:50],
            ))
        # 制御文字
        for m in _CONTROL_CHARS.finditer(line):
            anomalies.append(Anomaly(
                detector='encoding', severity=6,
                start=m.start(), end=m.end(), line_no=i,
                description=f'不正制御文字: U+{ord(m.group()):04X}',
                snippet=line[max(0, m.start() - 10):m.end() + 10][:50],
            ))

    return anomalies


# ═══════════════════════════════════════════════════════════════
# 2. STRUCTURAL — 文の切断・括弧欠損・空白異常
# ═══════════════════════════════════════════════════════════════

_OPEN_BRACKETS = {'「': '」', '『': '』', '（': '）', '(': ')', '[': ']', '【': '】'}
_CLOSE_BRACKETS = {v: k for k, v in _OPEN_BRACKETS.items()}


def detect_structural(text: str, lines: List[str], **kwargs) -> List[Anomaly]:
    """構造的異常検出"""
    anomalies = []

    # 括弧の不整合（行単位）
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
        for open_b in stack:
            anomalies.append(Anomaly(
                detector='structural', severity=5,
                start=0, end=len(line), line_no=i,
                description=f'開き括弧 {open_b} が閉じていない',
                snippet=line[:50],
            ))

    # 異常な連続空行（4行以上の空行）
    consecutive_empty = 0
    for i, line in enumerate(lines):
        if line.strip() == '':
            consecutive_empty += 1
            if consecutive_empty == 4:
                anomalies.append(Anomaly(
                    detector='structural', severity=3,
                    start=0, end=0, line_no=i,
                    description=f'連続空行が{consecutive_empty}行以上',
                    snippet='(空行)',
                ))
        else:
            if consecutive_empty > 4:
                anomalies[-1].description = f'連続空行が{consecutive_empty}行'
            consecutive_empty = 0

    # 異常に長い行（1000字以上 — 改行の欠損を示唆）
    for i, line in enumerate(lines):
        if len(line) > 1000:
            anomalies.append(Anomaly(
                detector='structural', severity=4,
                start=0, end=len(line), line_no=i,
                description=f'異常に長い行 ({len(line)}字) — 改行が欠損している可能性',
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
    **kwargs,
) -> List[Anomaly]:
    """段落・文の重複検出"""
    anomalies = []

    # 段落分割（空行区切り）
    paragraphs = []
    current = []
    start_line = 0
    for i, line in enumerate(lines):
        if line.strip() == '':
            if current:
                para_text = '\n'.join(current)
                if len(para_text.strip()) >= min_length:
                    paragraphs.append((start_line, para_text.strip()))
                current = []
            start_line = i + 1
        else:
            if not current:
                start_line = i
            current.append(line)
    if current:
        para_text = '\n'.join(current)
        if len(para_text.strip()) >= min_length:
            paragraphs.append((start_line, para_text.strip()))

    # 完全一致の重複検出
    seen = {}
    for line_no, para in paragraphs:
        normalized = re.sub(r'\s+', '', para)
        if normalized in seen:
            orig_line = seen[normalized]
            anomalies.append(Anomaly(
                detector='duplicate', severity=9,
                start=0, end=len(para), line_no=line_no,
                description=f'段落が完全重複 (初出: L{orig_line + 1})',
                snippet=para[:50],
            ))
        else:
            seen[normalized] = line_no

    # ほぼ一致の重複検出（N-gram Jaccard）
    if similarity_threshold < 1.0 and len(paragraphs) > 1:
        ngram_size = 3

        def _ngrams(s):
            s = re.sub(r'\s+', '', s)
            return set(s[i:i + ngram_size] for i in range(len(s) - ngram_size + 1))

        para_ngrams = [(ln, p, _ngrams(p)) for ln, p in paragraphs]

        for i in range(len(para_ngrams)):
            for j in range(i + 1, len(para_ngrams)):
                ln_i, p_i, ng_i = para_ngrams[i]
                ln_j, p_j, ng_j = para_ngrams[j]

                if not ng_i or not ng_j:
                    continue

                # 完全一致は上で既に検出済み
                normalized_i = re.sub(r'\s+', '', p_i)
                normalized_j = re.sub(r'\s+', '', p_j)
                if normalized_i == normalized_j:
                    continue

                jaccard = len(ng_i & ng_j) / len(ng_i | ng_j)
                if jaccard >= similarity_threshold:
                    anomalies.append(Anomaly(
                        detector='duplicate', severity=7,
                        start=0, end=len(p_j), line_no=ln_j,
                        description=f'段落がほぼ重複 (類似度{jaccard:.0%}, 参照: L{ln_i + 1})',
                        snippet=p_j[:50],
                    ))

    return anomalies


# ═══════════════════════════════════════════════════════════════
# 4. REPETITION — 短区間内の異常なフレーズ反復
# ═══════════════════════════════════════════════════════════════

def detect_repetition(
    text: str, lines: List[str],
    window_size: int = 500,
    min_phrase_len: int = 4,
    max_repeat: int = 3,
    **kwargs,
) -> List[Anomaly]:
    """短区間内でのフレーズ異常反復"""
    anomalies = []
    clean = re.sub(r'\s+', '', text)

    if len(clean) < window_size:
        windows = [(0, clean)]
    else:
        step = window_size // 2
        windows = [
            (i, clean[i:i + window_size])
            for i in range(0, len(clean) - window_size + 1, step)
        ]

    for win_start, window in windows:
        # N-gram 頻度
        for n in range(min_phrase_len, min_phrase_len + 3):
            freq = Counter()
            for i in range(len(window) - n + 1):
                gram = window[i:i + n]
                freq[gram] += 1

            for gram, count in freq.items():
                if count > max_repeat:
                    # テキスト内の行番号を推定
                    pos = text.find(gram)
                    line_no = text[:pos].count('\n') if pos >= 0 else -1

                    anomalies.append(Anomaly(
                        detector='repetition', severity=min(6, count - max_repeat + 3),
                        start=win_start, end=win_start + window_size,
                        line_no=line_no,
                        description=f'「{gram}」が{window_size}字以内に{count}回反復',
                        snippet=gram,
                    ))

    # 重複排除（同じフレーズは最も重篤な1件だけ残す）
    seen_phrases = {}
    for a in anomalies:
        key = a.snippet
        if key not in seen_phrases or a.severity > seen_phrases[key].severity:
            seen_phrases[key] = a
    return list(seen_phrases.values())


# ═══════════════════════════════════════════════════════════════
# 5. DISTRIBUTION — 文脈から浮いた「異物」区間
# ═══════════════════════════════════════════════════════════════

def detect_distribution(
    text: str, lines: List[str],
    segment_size: int = 300,
    jsd_threshold: float = 0.35,
    **kwargs,
) -> List[Anomaly]:
    """スライディングウィンドウJSDで分布断絶を検出"""
    anomalies = []
    clean = re.sub(r'\s+', '', text)

    if len(clean) < segment_size * 2:
        return anomalies

    # セグメント分割
    segments = []
    for i in range(0, len(clean) - segment_size + 1, segment_size // 2):
        seg = clean[i:i + segment_size]
        segments.append((i, seg))

    if len(segments) < 3:
        return anomalies

    # 各セグメントの 2-gram 頻度ベクトル
    def _freq_vec(s):
        return Counter(s[i:i + 2] for i in range(len(s) - 1))

    seg_freqs = [(pos, _freq_vec(seg)) for pos, seg in segments]

    # 隣接セグメント間の JSD を計算
    for i in range(1, len(seg_freqs)):
        pos_prev, freq_prev = seg_freqs[i - 1]
        pos_curr, freq_curr = seg_freqs[i]

        all_keys = sorted(set(freq_prev.keys()) | set(freq_curr.keys()))
        p = np.array([freq_prev.get(k, 0) for k in all_keys], dtype=float)
        q = np.array([freq_curr.get(k, 0) for k in all_keys], dtype=float)

        p_sum, q_sum = p.sum(), q.sum()
        if p_sum == 0 or q_sum == 0:
            continue
        p /= p_sum
        q /= q_sum
        m = 0.5 * (p + q)

        jsd = 0.0
        for arr in [p, q]:
            mask = arr > 0
            jsd += 0.5 * float(np.sum(arr[mask] * np.log2(arr[mask] / m[mask])))

        if jsd > jsd_threshold:
            # テキスト位置→行番号
            line_no = text[:pos_curr].count('\n') if pos_curr < len(text) else -1
            anomalies.append(Anomaly(
                detector='distribution', severity=min(7, int(jsd * 10)),
                start=pos_curr, end=pos_curr + segment_size,
                line_no=line_no,
                description=f'分布断絶 JSD={jsd:.4f} (閾値{jsd_threshold})',
                snippet=clean[pos_curr:pos_curr + 30],
            ))

    return anomalies


# ═══════════════════════════════════════════════════════════════
# 6. COMPLEXITY — 局所パープレキシティ/圧縮率のスパイク
# ═══════════════════════════════════════════════════════════════

def detect_complexity(
    text: str, lines: List[str],
    segment_size: int = 300,
    compression_low: float = 0.15,
    compression_high: float = 0.85,
    **kwargs,
) -> List[Anomaly]:
    """局所的な複雑度異常を検出"""
    anomalies = []
    clean = re.sub(r'\n{3,}', '\n\n', text)

    if len(clean) < segment_size:
        return anomalies

    # セグメント分割
    segments = []
    for i in range(0, len(clean) - segment_size + 1, segment_size):
        seg = clean[i:i + segment_size]
        if len(seg.strip()) < 50:
            continue
        segments.append((i, seg))

    if not segments:
        return anomalies

    # 各セグメントの圧縮率
    ratios = []
    for pos, seg in segments:
        encoded = seg.encode('utf-8')
        compressed = zlib.compress(encoded, 9)
        ratio = len(compressed) / len(encoded) if len(encoded) > 0 else 0
        ratios.append((pos, seg, ratio))

    # 全体の統計
    all_ratios = [r for _, _, r in ratios]
    if not all_ratios:
        return anomalies
    mean_r = np.mean(all_ratios)
    std_r = np.std(all_ratios)

    for pos, seg, ratio in ratios:
        line_no = text[:pos].count('\n') if pos < len(text) else -1

        if ratio < compression_low:
            anomalies.append(Anomaly(
                detector='complexity', severity=6,
                start=pos, end=pos + segment_size, line_no=line_no,
                description=f'圧縮率が極端に低い ({ratio:.3f}) — 同一パターンの繰り返し',
                snippet=seg[:40],
            ))
        elif ratio > compression_high:
            anomalies.append(Anomaly(
                detector='complexity', severity=4,
                start=pos, end=pos + segment_size, line_no=line_no,
                description=f'圧縮率が極端に高い ({ratio:.3f}) — ランダムデータの可能性',
                snippet=seg[:40],
            ))
        elif std_r > 0 and abs(ratio - mean_r) > 2.5 * std_r:
            anomalies.append(Anomaly(
                detector='complexity', severity=5,
                start=pos, end=pos + segment_size, line_no=line_no,
                description=f'圧縮率が全体から2.5σ以上逸脱 ({ratio:.3f}, 平均{mean_r:.3f})',
                snippet=seg[:40],
            ))

    return anomalies


# ═══════════════════════════════════════════════════════════════
# 7. VOCABULARY — 文書全体に属さない「外来語彙」の混入
# ═══════════════════════════════════════════════════════════════

def detect_vocabulary(
    text: str, lines: List[str],
    segment_size: int = 300,
    foreign_threshold: float = 0.3,
    **kwargs,
) -> List[Anomaly]:
    """外来語彙（文書全体の語彙に属さない語の割合）を検出"""
    anomalies = []
    clean = re.sub(r'\s+', '', text)

    if len(clean) < segment_size * 2:
        return anomalies

    # 文書全体の 2-gram 語彙
    global_vocab = set()
    for i in range(len(clean) - 1):
        global_vocab.add(clean[i:i + 2])

    # セグメントごとに「文書全体に存在しない2-gramの割合」を計算
    # ……これだと全部0になる（全体に含まれるから）
    # → 代わりに「全体で低頻度なのに局所で高頻度」な語を検出する

    # 全体頻度
    global_freq = Counter()
    for i in range(len(clean) - 1):
        global_freq[clean[i:i + 2]] += 1
    total_bigrams = sum(global_freq.values())

    # セグメント分割
    for seg_start in range(0, len(clean) - segment_size + 1, segment_size):
        seg = clean[seg_start:seg_start + segment_size]
        local_freq = Counter(seg[i:i + 2] for i in range(len(seg) - 1))
        local_total = sum(local_freq.values())

        if local_total == 0:
            continue

        # 「全体では稀なのにこのセグメントでは多い」語の割合
        foreign_count = 0
        for bigram, local_count in local_freq.items():
            global_rate = global_freq.get(bigram, 0) / total_bigrams
            local_rate = local_count / local_total

            # 全体での出現率の10倍以上 = 局所的に異常に多い
            if global_rate > 0 and local_rate > global_rate * 10:
                foreign_count += local_count

        foreign_ratio = foreign_count / local_total

        if foreign_ratio > foreign_threshold:
            line_no = text[:seg_start].count('\n') if seg_start < len(text) else -1
            anomalies.append(Anomaly(
                detector='vocabulary', severity=min(7, int(foreign_ratio * 10)),
                start=seg_start, end=seg_start + segment_size,
                line_no=line_no,
                description=f'外来語彙比率 {foreign_ratio:.1%} (閾値{foreign_threshold:.0%})',
                snippet=seg[:40],
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
    'vocabulary': detect_vocabulary,
}

ALL_DETECTOR_NAMES = list(DETECTOR_REGISTRY.keys())
