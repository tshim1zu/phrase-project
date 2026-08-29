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
        """Create state from an extractor instance.
        
        Args:
            extractor: Phrase extractor instance with min_length attribute.
        
        Returns:
            New IncrementalPhraseState initialized with extractor's min_length.
        """
        return cls(min_length=int(getattr(extractor, "min_length", 1)))

    def update(self, extractor: Any, texts: Iterable[str]) -> None:
        """Update state by processing new batch of texts.
        
        Accumulates phrase counts from the new texts and increments total text count.
        Validates extractor configuration matches state.
        
        Args:
            extractor: Phrase extractor instance to use for counting.
            texts: Iterable of text strings to process.
        
        Returns:
            None (modifies state in place).
        """
        self._validate(extractor)
        texts_list = list(texts)
        phrase_counts = extractor.count_phrases(texts_list)
        for phrase, count in phrase_counts.items():
            self.counts[phrase] = self.counts.get(phrase, 0) + int(count)
        self.total_texts += len(texts_list)

    def to_df(self, extractor: Any):
        """Convert accumulated phrase counts to DataFrame format.
        
        Uses extractor's method to convert counts dictionary to DataFrame.
        Validates extractor configuration matches state.
        
        Args:
            extractor: Phrase extractor instance with df_from_counts method.
        
        Returns:
            DataFrame representation of accumulated phrase counts.
        """
        self._validate(extractor)
        return extractor.df_from_counts(self.counts)

    def to_dict(self) -> Dict[str, Any]:
        """Convert state to dictionary for serialization.
        
        Returns:
            Dictionary with keys: state_version, parser_version, min_length,
            total_texts, counts.
        """
        return {
            "state_version": self.state_version,
            "parser_version": self.parser_version,
            "min_length": self.min_length,
            "total_texts": self.total_texts,
            "counts": self.counts,
        }

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "IncrementalPhraseState":
        """Create state from serialized dictionary.
        
        Args:
            data: Dictionary with state data (from to_dict() or JSON).
        
        Returns:
            IncrementalPhraseState instance reconstructed from dictionary.
        """
        return cls(
            counts=dict(data.get("counts", {})),
            total_texts=int(data.get("total_texts", 0)),
            min_length=int(data.get("min_length", 1)),
            parser_version=int(data.get("parser_version", PARSER_VERSION)),
            state_version=int(data.get("state_version", STATE_VERSION)),
        )

    def save(self, path: str) -> None:
        """Save state to JSON file.
        
        Creates parent directories if needed. Overwrites existing file.
        Uses UTF-8 encoding.
        
        Args:
            path: File path to save state to.
        
        Returns:
            None.
        """
        target = Path(path)
        target.parent.mkdir(parents=True, exist_ok=True)
        with target.open("w", encoding="utf-8") as f:
            json.dump(self.to_dict(), f, ensure_ascii=True)

    @classmethod
    def load(cls, path: str) -> "IncrementalPhraseState":
        """Load state from JSON file.
        
        Reads and parses state from JSON file, then reconstructs IncrementalPhraseState.
        
        Args:
            path: File path to load state from.
        
        Returns:
            IncrementalPhraseState instance loaded from file.
        
        Raises:
            FileNotFoundError: If file does not exist.
            json.JSONDecodeError: If file is not valid JSON.
        """
        with Path(path).open("r", encoding="utf-8") as f:
            data = json.load(f)
        return cls.from_dict(data)

    def _validate(self, extractor: Any) -> None:
        """Validate that extractor configuration matches state.
        
        Checks that extractor's min_length matches state's min_length.
        Raises error if mismatch, preventing state corruption from incompatible extractor.
        
        Args:
            extractor: Extractor instance to validate.
        
        Returns:
            None.
        
        Raises:
            ValueError: If extractor min_length differs from state min_length.
        """
        extractor_min_length = int(getattr(extractor, "min_length", 1))
        if self.min_length != extractor_min_length:
            raise ValueError(
                "Incremental state min_length mismatch: "
                f"{self.min_length} != {extractor_min_length}"
            )
