# coding: utf-8
"""
Incremental phrase extraction state.

This keeps cumulative counts and allows resuming from disk without
reprocessing prior inputs.
"""

from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Dict, Iterable
import json
from pathlib import Path


STATE_VERSION = 1
PARSER_VERSION = 1


@dataclass
class IncrementalPhraseState:
    """Serializable state for incremental phrase extraction."""

    counts: Dict[str, int] = field(default_factory=dict)
    total_texts: int = 0
    min_length: int = 1
    parser_version: int = PARSER_VERSION
    state_version: int = STATE_VERSION

    @classmethod
    def from_extractor(cls, extractor: Any) -> "IncrementalPhraseState":
        return cls(min_length=int(getattr(extractor, "min_length", 1)))

    def update(self, extractor: Any, texts: Iterable[str]) -> None:
        self._validate(extractor)
        texts_list = list(texts)
        phrase_counts = extractor.count_phrases(texts_list)
        for phrase, count in phrase_counts.items():
            self.counts[phrase] = self.counts.get(phrase, 0) + int(count)
        self.total_texts += len(texts_list)

    def to_df(self, extractor: Any):
        self._validate(extractor)
        return extractor.df_from_counts(self.counts)

    def to_dict(self) -> Dict[str, Any]:
        return {
            "state_version": self.state_version,
            "parser_version": self.parser_version,
            "min_length": self.min_length,
            "total_texts": self.total_texts,
            "counts": self.counts,
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "IncrementalPhraseState":
        return cls(
            counts=dict(data.get("counts", {})),
            total_texts=int(data.get("total_texts", 0)),
            min_length=int(data.get("min_length", 1)),
            parser_version=int(data.get("parser_version", PARSER_VERSION)),
            state_version=int(data.get("state_version", STATE_VERSION)),
        )

    def save(self, path: str) -> None:
        target = Path(path)
        target.parent.mkdir(parents=True, exist_ok=True)
        with target.open("w", encoding="utf-8") as f:
            json.dump(self.to_dict(), f, ensure_ascii=True)

    @classmethod
    def load(cls, path: str) -> "IncrementalPhraseState":
        with Path(path).open("r", encoding="utf-8") as f:
            data = json.load(f)
        return cls.from_dict(data)

    def _validate(self, extractor: Any) -> None:
        extractor_min_length = int(getattr(extractor, "min_length", 1))
        if self.min_length != extractor_min_length:
            raise ValueError(
                "Incremental state min_length mismatch: "
                f"{self.min_length} != {extractor_min_length}"
            )
