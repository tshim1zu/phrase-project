"""Reading consistency analysis for repeated surface forms.

This module deliberately does not perform morphological analysis or TTS calls.
Callers provide observed (surface, reading, context) rows from their own
language stack. Japhrase owns normalization/grouping and reports only genuine
multi-reading conflicts for the same written surface.
"""
from __future__ import annotations

from dataclasses import dataclass
import re
import unicodedata
from typing import Iterable


_KATAKANA_START = ord("ァ")
_KATAKANA_END = ord("ヶ")
_HIRAGANA_OFFSET = ord("ぁ") - ord("ァ")


def normalize_reading(reading: str) -> str:
    """Normalize kana readings for stable equality checks.

    - Unicode NFKC
    - katakana -> hiragana
    - remove whitespace and punctuation separators
    - preserve long vowel mark because it can distinguish lexical readings
    """
    text = unicodedata.normalize("NFKC", str(reading or "")).strip()
    out = []
    for ch in text:
        code = ord(ch)
        if _KATAKANA_START <= code <= _KATAKANA_END:
            ch = chr(code + _HIRAGANA_OFFSET)
        if re.match(r"[\s、。・,.;:!?！？「」『』（）()\[\]{}]", ch):
            continue
        out.append(ch)
    return "".join(out)


@dataclass(frozen=True)
class ReadingObservation:
    surface: str
    reading: str
    context: str = ""
    location: str = ""
    speaker: str = ""
    source: str = ""


@dataclass(frozen=True)
class ReadingConflict:
    surface: str
    readings: tuple[str, ...]
    observations: tuple[ReadingObservation, ...]


class ReadingConsistencyAnalyzer:
    """Group repeated surface forms and report incompatible readings."""

    def __init__(self, *, min_occurrences: int = 2):
        if min_occurrences < 2:
            raise ValueError("min_occurrences must be >= 2")
        self.min_occurrences = min_occurrences

    def conflicts(
        self,
        observations: Iterable[ReadingObservation],
        *,
        ignored_surfaces: Iterable[str] = (),
    ) -> list[ReadingConflict]:
        ignored = set(ignored_surfaces)
        grouped: dict[str, list[ReadingObservation]] = {}
        for obs in observations:
            surface = str(obs.surface or "").strip()
            normalized = normalize_reading(obs.reading)
            if not surface or not normalized or surface in ignored:
                continue
            grouped.setdefault(surface, []).append(obs)

        out: list[ReadingConflict] = []
        for surface, rows in grouped.items():
            if len(rows) < self.min_occurrences:
                continue
            readings = sorted({normalize_reading(row.reading) for row in rows})
            if len(readings) <= 1:
                continue
            out.append(
                ReadingConflict(
                    surface=surface,
                    readings=tuple(readings),
                    observations=tuple(rows),
                )
            )
        return sorted(out, key=lambda c: (-len(c.observations), c.surface))
