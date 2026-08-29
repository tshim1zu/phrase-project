# coding: utf-8
"""
Phrase evidence generation using statistical signals and contexts.
"""

from __future__ import annotations

from typing import Dict, Iterable, List

from .extracter import PhraseExtracter


def _iter_phrase_occurrences(
    texts: List[str],
    phrase: str,
    context_chars: int,
    max_samples: int,
):
    """フレーズの出現位置と前後文脈を上限件数まで収集する。"""
    samples = []
    for line_idx, line in enumerate(texts):
        if phrase not in line:
            continue
        start = 0
        while True:
            hit = line.find(phrase, start)
            if hit == -1:
                break
            left = max(0, hit - context_chars)
            right = min(len(line), hit + len(phrase) + context_chars)
            context = (
                line[left:hit]
                + "[[" + line[hit:hit + len(phrase)] + "]]"
                + line[hit + len(phrase):right]
            )
            samples.append(
                {
                    "line_num": line_idx + 1,
                    "char_pos": hit,
                    "context": context,
                }
            )
            if len(samples) >= max_samples:
                return samples
            start = hit + len(phrase)
    return samples


def build_phrase_evidence(
    texts: List[str],
    phrases: Iterable[str],
    phrase_freqs: Dict[str, int],
    context_chars: int = 20,
    max_samples: int = 3,
) -> List[Dict]:
    """Build evidence for phrases with PMI, entropy, and context samples."""
    texts = [str(t) for t in texts]
    phrases = list(phrases)
    all_text = "".join(texts)

    extractor = PhraseExtracter(min_count=1, min_length=1, verbose=0)
    pmi_scores = extractor.calculate_pmi(phrases, all_text)
    entropy_scores = extractor.calculate_branching_entropy(texts, phrases)

    results = []
    for phrase in phrases:
        left_entropy, right_entropy, boundary_score = entropy_scores.get(
            phrase, (0.0, 0.0, 0.0)
        )
        results.append(
            {
                "phrase": phrase,
                "frequency": int(phrase_freqs.get(phrase, 0)),
                "pmi": float(pmi_scores.get(phrase, 0.0)),
                "left_entropy": float(left_entropy),
                "right_entropy": float(right_entropy),
                "boundary_score": float(boundary_score),
                "samples": _iter_phrase_occurrences(
                    texts, phrase, context_chars, max_samples
                ),
            }
        )

    return results
