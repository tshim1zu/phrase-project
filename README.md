# japhrase

**Pure-math text intelligence engine for Japanese & English**

Statistical NLP toolkit that measures writing quality, tracks vocabulary evolution, detects stylistic drift, and compares documents — all without external AI services or API keys.

[![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)](https://opensource.org/licenses/MIT)
[![Python 3.8+](https://img.shields.io/badge/python-3.8+-blue.svg)](https://www.python.org/downloads/)
[![Tests](https://img.shields.io/badge/tests-290%2B%20passing-brightgreen)](https://github.com/tshim1zu/japhrase)

---

## Why japhrase?

Most text analysis tools are either bag-of-words counters or black-box LLM wrappers. japhrase sits in between: **real statistical measures** (PMI, chi-squared, entropy, JSD, Heaps' law, compression theory) applied to practical writing problems.

- **Zero external dependencies** — runs on numpy + scipy alone. No API keys, no internet, no GPU.
- **Deterministic** — same input always produces the same output. No hallucinations.
- **Japanese + English** — character-level N-gram engine works natively with both languages.
- **92 tests pass in 4 seconds** on a single core.

---

## What can it do?

### Statistical Engines (Core)

| Engine | What it measures | Key metrics |
|--------|-----------------|-------------|
| **DistributionComparator** | How different two texts are | Log-Likelihood (G²), Jensen-Shannon Divergence, Effect Size, Keyness |
| **CollocationScorer** | How strongly words bind together | PMI, MI³, t-score, z-score, Log-Dice, Delta-P |
| **StylometryAnalyzer** | Vocabulary richness and diversity | Hapax ratio, Brunet's W, Honoré's R, Simpson's D, MATTR, Heaps' Law |
| **ComplexityAnalyzer** | Text difficulty and information density | N-gram Perplexity, compression ratio, lexical density, information rate |
| **TemporalAnalyzer** | How writing evolves over a series | Burstiness detection, vocabulary saturation, JSD distance matrix |
| **StatisticalScorer** | Phrase significance | Chi-squared, mutual information, Zipf anomaly, Wilson CI |
| **PhraseExtracter** | Frequent phrase extraction | N-gram + PMI + entropy filtering |

### Applied Features (Writing Workflow)

Built on top of the statistical engines, these modules solve real editorial problems:

| Module | Problem it solves | Example output |
|--------|-------------------|----------------|
| **PreflightChecker** | "Is this chapter ready to publish?" | GO / WARN / NOGO verdict with quality score (0-100) |
| **EPDashboard** | "How is my vocabulary changing across chapters?" | MATTR trend, entropy trend, vocabulary saturation, burst detection |
| **HabitDriftDetector** | "Am I developing bad writing habits?" | Worsening/improving habit tracking with sparkline visualization |
| **JPENDivergenceChecker** | "Did the translation lose quality?" | Per-chapter translation loss rate, degradation alerts |
| **CharacterStylometry** | "Do my characters sound distinct?" | Per-character vocabulary fingerprint, JSD separation matrix |
| **PartHealthReport** | "What's the overall quality of this arc?" | A-E grade with 6-section breakdown and improvement priorities |

---

## Quick Start

```python
from japhrase import DistributionComparator, StylometryAnalyzer, ComplexityAnalyzer
from collections import Counter

# Compare two texts
comp = DistributionComparator()
freq_a = Counter({"sword": 10, "knight": 8, "castle": 5})
freq_b = Counter({"data": 12, "experiment": 9, "monitor": 7})
result = comp.compare(freq_a, freq_b)
print(f"JSD: {result.jsd:.4f}")   # How different are they?
print(comp.generate_report(freq_a, freq_b))

# Measure vocabulary richness
stylo = StylometryAnalyzer()
print(stylo.analyze_advanced_diversity(text))
# → hapax_ratio, brunets_w, honores_r, simpsons_d, ...

# Measure text complexity
cx = ComplexityAnalyzer()
print(cx.analyze(text))
# → perplexity, compression_ratio, lexical_density, information_rate
```

### Publish-ready quality check

```python
from japhrase.applied import PreflightChecker

checker = PreflightChecker()
result = checker.check(chapter_text, lang='jp', platform='royalroad')
print(result.verdict)        # 'GO', 'WARN', or 'NOGO'
print(result.quality_score)  # 0-100
print(result.report())       # Full breakdown
```

### Track quality across chapters

```python
from japhrase.applied import EPDashboard

dashboard = EPDashboard()
result = dashboard.analyze({
    "Ch.1": ch1_text, "Ch.2": ch2_text, "Ch.3": ch3_text,
})
print(f"Vocabulary saturation: {result.vocab_saturation:.2f}")
print(f"MATTR trend: {result.mattr_trend:+.6f}")
print(result.report())
```

### Detect writing habit drift

```python
from japhrase.applied import HabitDriftDetector

detector = HabitDriftDetector()
result = detector.analyze({"Ch.1": t1, "Ch.2": t2, "Ch.3": t3})
print(f"Worsening habits: {result.worsening_count}")
print(result.report())  # Includes sparkline visualization
```

### Character voice separation

```python
from japhrase.applied import CharacterStylometry

cs = CharacterStylometry()
fps = cs.build_fingerprints(chapter_texts, ["Eris", "Leticia", "Sofia"])
print(cs.full_report(fps))
# → Per-character MATTR, keyness terms, JSD separation matrix
```

### Full arc health check

```python
from japhrase.applied import PartHealthReport

report = PartHealthReport()
grade = report.diagnose(
    chapter_texts,
    characters=["Eris", "Leticia", "Sofia"],
    part_label="Arc 1",
)
print(grade.report())
# → Overall: B (78.3/100)
# → Vocabulary: A (95.2) | Tempo: C (62.1) | Habits: A (90.0) | ...
```

---

## Installation

```bash
pip install japhrase

# Or from source
git clone https://github.com/tshim1zu/japhrase.git
cd japhrase
pip install -e .
```

**Requirements**: Python 3.8+, numpy, pandas, scipy (all standard scientific Python).

## Testing

```bash
pytest                    # Run all 290+ tests
pytest tests/test_applied.py -v   # Applied modules only
pytest --cov=japhrase     # With coverage
```

## Architecture

```
japhrase/
├── extracter.py              # N-gram phrase extraction (core engine)
├── statistical_scorer.py     # Chi², MI, Zipf, CI, p-value
├── distribution_comparator.py # G², JSD, Log Ratio, Dice, Keyness
├── collocation_scorer.py     # PMI, MI³, t-score, z-score, Log-Dice, Delta-P
├── stylometry.py             # TTR, Yule's K, Hapax, Brunet, Honoré, MATTR, Heaps
├── complexity_metrics.py     # Perplexity, compression, lexical density
├── temporal_analyzer.py      # Burstiness, vocab saturation, trend tracking
├── writing_habit_detector.py # freq × PMI⁻¹ habit detection
├── entropy_pacing.py         # Shannon entropy pacing analysis
├── chekhov_gun_detector.py   # Narrative setup/payoff tracking
│
├── applied/                  # Writing workflow integration
│   ├── preflight_stats.py    # Pre-publish quality gate
│   ├── ep_dashboard.py       # Chapter-over-chapter dashboard
│   ├── habit_drift.py        # Habit trend tracking
│   ├── jpen_divergence.py    # JP↔EN translation quality
│   ├── character_stylometry.py # Character voice fingerprinting
│   └── part_health.py        # Arc-level health grading (A-E)
│
├── similarity.py             # Levenshtein / Jaccard / cosine
├── cooccurrence.py           # Co-occurrence analysis
├── document_vectorizer.py    # NMF-based document vectors
└── ...                       # 50+ modules total
```

## License

MIT License — Takeshi SHIMIZU

---

**japhrase**: Because good writing deserves better measurement than word count.
