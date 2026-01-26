# coding: utf-8
from pathlib import Path
import json

from japhrase import PhraseExtracter
from japhrase.incremental import IncrementalPhraseState
from japhrase.evidence import build_phrase_evidence
from japhrase.stats_utils import compute_stats_data
from japhrase.stats_report import render_stats_html


def main():
    texts_a = [
        "alpha, beta, gamma",
        "alpha, beta",
        "beta, gamma",
    ]
    texts_b = [
        "alpha, delta",
        "gamma, delta",
    ]

    extractor = PhraseExtracter(min_count=2, min_length=3, verbose=0)
    state = IncrementalPhraseState.from_extractor(extractor)
    state.update(extractor, texts_a)
    state.update(extractor, texts_b)

    df = state.to_df(extractor)
    print(df.head())

    preset = PhraseExtracter.infer_preset_name(texts_a + texts_b)
    print("auto preset:", preset)

    phrase_freqs = {row["phrase"]: int(row["freq"]) for _, row in df.iterrows()}
    evidence = build_phrase_evidence(
        texts_a + texts_b,
        df["phrase"].head(3).tolist(),
        phrase_freqs,
        context_chars=10,
        max_samples=2,
    )
    print(json.dumps(evidence, ensure_ascii=False, indent=2))

    stats_data = compute_stats_data(
        df,
        texts_a + texts_b,
        {
            "input_file": "demo",
            "min_count": 2,
            "max_length": 16,
            "preset": preset,
        },
        top_n=5,
    )
    html = render_stats_html(stats_data, title="Incremental Demo Report")
    Path("results/demo_stats.html").parent.mkdir(parents=True, exist_ok=True)
    Path("results/demo_stats.html").write_text(html, encoding="utf-8")


if __name__ == "__main__":
    main()
