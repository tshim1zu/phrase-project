# coding: utf-8
"""
Stats helpers for phrase analysis.
"""

from __future__ import annotations

from typing import Dict, List, Optional
from datetime import datetime

import numpy as np
import pandas as pd


def resolve_phrase_column(df: pd.DataFrame) -> str:
    """Resolve the name of the phrase column in a DataFrame.
    
    Checks for 'seqchar' or 'phrase' columns and returns the first found.
    
    Args:
        df: DataFrame to search for phrase column.
    
    Returns:
        Name of the phrase column ('seqchar' or 'phrase').
    
    Raises:
        ValueError: If neither 'seqchar' nor 'phrase' column exists.
    """
    if "seqchar" in df.columns:
        return "seqchar"
    if "phrase" in df.columns:
        return "phrase"
    raise ValueError("Phrase column not found in DataFrame.")


def resolve_frequency_column(df: pd.DataFrame) -> str:
    """Resolve the name of the frequency column in a DataFrame.
    
    Checks for 'freq' or 'frequency' columns and returns the first found.
    
    Args:
        df: DataFrame to search for frequency column.
    
    Returns:
        Name of the frequency column ('freq' or 'frequency').
    
    Raises:
        ValueError: If neither 'freq' nor 'frequency' column exists.
    """
    if "freq" in df.columns:
        return "freq"
    if "frequency" in df.columns:
        return "frequency"
    raise ValueError("Frequency column not found in DataFrame.")


def ensure_length_column(df: pd.DataFrame) -> pd.DataFrame:
    """Ensure DataFrame has a 'length' column with phrase lengths.
    
    Adds a 'length' column if missing, computed as character length of phrases.
    
    Args:
        df: DataFrame to process (assumed to have phrase column).
    
    Returns:
        DataFrame with 'length' column added (or unchanged if already present).
    """
    if "length" in df.columns:
        return df
    phrase_col = resolve_phrase_column(df)
    df = df.copy()
    df["length"] = df[phrase_col].astype(str).map(len)
    return df


def compute_stats_data(
    phrases_df: pd.DataFrame,
    texts: List[str],
    parameters: Dict[str, object],
    top_n: int = 20,
    total_texts_override: Optional[int] = None,
) -> Dict:
    """Compute comprehensive statistics on extracted phrases.
    
    Analyzes frequency, length, originality, and diversity of phrases,
    optionally filtered to top-N by frequency.
    
    Args:
        phrases_df: DataFrame with phrase data (or None/empty for empty result).
        texts: List of source texts analyzed.
        parameters: Dictionary of parameters used in phrase extraction.
        top_n: Maximum number of phrases to include in top-N ranking (default 20).
        total_texts_override: Override for total text count (uses len(texts) if None).
    
    Returns:
        Dictionary with keys: status, timestamp, parameters, summary, frequency,
        length, originality, diversity, top_phrases.
    """
    if phrases_df is None or phrases_df.empty:
        return {
            "status": "empty",
            "timestamp": datetime.now().isoformat(),
            "parameters": parameters,
            "summary": {
                "total_phrases": 0,
                "unique_phrases": 0,
                "text_lines": int(total_texts_override or len(texts)),
                "total_phrase_occurrences": 0,
            },
            "frequency": {},
            "length": {},
            "originality": {},
            "diversity": {},
            "top_phrases": [],
        }

    phrases_df = ensure_length_column(phrases_df)
    phrase_col = resolve_phrase_column(phrases_df)
    freq_col = resolve_frequency_column(phrases_df)

    freq_col_values = phrases_df[freq_col].values.astype(float)
    length_col_values = phrases_df["length"].values.astype(float)
    originality_col = (
        phrases_df["originality"].values.astype(float)
        if "originality" in phrases_df.columns
        else np.ones_like(freq_col_values)
    )

    total_texts = int(total_texts_override or len(texts))
    total_occurrences = int(freq_col_values.sum()) if len(freq_col_values) else 0
    freq_dist = freq_col_values / total_occurrences if total_occurrences else freq_col_values

    stats_data = {
        "status": "success",
        "timestamp": datetime.now().isoformat(),
        "parameters": parameters,
        "summary": {
            "total_phrases": int(len(phrases_df)),
            "unique_phrases": int(len(phrases_df)),
            "text_lines": total_texts,
            "total_phrase_occurrences": total_occurrences,
        },
        "frequency": {
            "mean": float(np.mean(freq_col_values)),
            "median": float(np.median(freq_col_values)),
            "std_dev": float(np.std(freq_col_values)),
            "min": int(np.min(freq_col_values)),
            "max": int(np.max(freq_col_values)),
        },
        "length": {
            "mean": float(np.mean(length_col_values)),
            "median": float(np.median(length_col_values)),
            "std_dev": float(np.std(length_col_values)),
            "min": int(np.min(length_col_values)),
            "max": int(np.max(length_col_values)),
        },
        "originality": {
            "mean": float(np.mean(originality_col)),
            "median": float(np.median(originality_col)),
            "std_dev": float(np.std(originality_col)),
            "min": float(np.min(originality_col)),
            "max": float(np.max(originality_col)),
        },
        "diversity": {
            "entropy": float(-np.sum(freq_dist * np.log2(freq_dist + 1e-10))) if total_occurrences else 0.0,
            "gini_coefficient": float(
                2 * np.sum(np.arange(1, len(freq_col_values) + 1) * np.sort(freq_col_values))
                / (len(freq_col_values) * np.sum(freq_col_values))
                - (len(freq_col_values) + 1) / len(freq_col_values)
            )
            if total_occurrences
            else 0.0,
        },
        "top_phrases": [],
    }

    top_phrases_df = phrases_df.nlargest(min(top_n, len(phrases_df)), freq_col)
    for _, row in top_phrases_df.iterrows():
        stats_data["top_phrases"].append(
            {
                "phrase": str(row[phrase_col]),
                "frequency": int(row[freq_col]),
                "length": int(row["length"]),
                "originality": float(row["originality"]) if "originality" in row else 0.0,
            }
        )

    return stats_data


def flatten_stats_for_csv(stats_data: Dict) -> pd.DataFrame:
    """Flatten hierarchical statistics dictionary to DataFrame rows for CSV export.
    
    Extracts key metrics from the stats_data dictionary and creates a flat
    DataFrame suitable for CSV output.
    
    Args:
        stats_data: Statistics dictionary from compute_stats_data().
    
    Returns:
        DataFrame with columns 'metric' and 'value' containing summary statistics.
    """
    rows = [
        {"metric": "total_phrases", "value": stats_data["summary"]["total_phrases"]},
        {"metric": "frequency_mean", "value": stats_data["frequency"].get("mean", 0)},
        {"metric": "frequency_median", "value": stats_data["frequency"].get("median", 0)},
        {"metric": "length_mean", "value": stats_data["length"].get("mean", 0)},
        {"metric": "entropy", "value": stats_data["diversity"].get("entropy", 0)},
    ]
    return pd.DataFrame(rows)
