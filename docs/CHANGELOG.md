# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Added
- English support (`lang='en'`) — word-level N-gram + Strategy pattern

## [0.3.13] - 2026-05-09

### Added
- `AdaptiveTuner` — dynamic parameter optimization as text accumulates
- `PhraseExtracter.auto_tune()` / `tune()` / `show_params()` / `save_params()` methods
- Auto-load of saved parameters on next `PhraseExtracter()` instantiation

### Changed
- README fully restructured for two persona paths (quick-start / deep-dive)
- All README code blocks verified as copy-paste runnable

## [0.3.11] - 2026-05-01

### Changed
- Repository cleanup: moved dev artifacts (experiments, examples, old docs) to gitignore
- README overhauled: self-contained blocks, verified output examples, no misleading code

## [0.3.8] - 2026-04-15

### Fixed
- First-time user error corrections (encoding, empty input messages)

## [0.3.7] - 2026-04-10

### Changed
- `python-Levenshtein` and `scikit-learn` promoted to core dependencies (removed from optional extras)

## [0.3.6] - 2026-04-05

### Added
- `.demo()` classmethod on major classes (`StylometryAnalyzer`, `ComplexityAnalyzer`, etc.)

## [0.3.0] - 2026-03-01

### Added
- `DistributionComparator` — keyness analysis between two corpora
- `CollocationScorer` — collocational strength scoring
- `ComplexityAnalyzer` — text complexity metrics (type-token ratio, Yule's K, sentence length)
- `TemporalAnalyzer` — time-series phrase trend analysis
- `StatisticalScorer` — chi-square / mutual information phrase significance evaluation
- `TextVariantDetector` — orthographic variant detection (6 statistical indicators)
- `EncodingDetector` / `BOMHandler` — auto encoding detection with garbling score
- `StreamingProcessor` — incremental / batch phrase extraction
- `ParameterOptimizer` — evidence-based hyperparameter optimization
- `InsightGenerator` — auto narrative generation from phrase statistics
- `WritingHabitDetector` — high-frequency / low-PMI habit detection
- `DocumentVectorizer` — NMF-based document vectorization
- Phase B: 107 passing tests for statistical significance evaluation

## [0.2.0] - 2026-01-27

### Added
- `DialogueAnalyzer` — dialogue vs. narrative ratio analysis
- `OrthographyVariantDetector` (`OrthographyChecker`) — variant form detection
- `StylometryAnalyzer` — vocabulary richness (TTR, Yule's K), character-type ratios
- `CharacterNetworkGenerator` — co-occurrence network with GEXF / CSV export
- `PromptOptimizer` — ComfyUI / Stable Diffusion prompt quality scoring
- `TEXT_MINING_GUIDE.md` — comprehensive guide for all 5 text mining modules

### Performance
- PMI calculation: O(n×m) → O(n+m), ~33× faster on 100k-character texts
- Branching entropy: O(n²) → O(n×w) ≈ O(n), ~120× faster on 100k-character texts

## [0.1.3] - 2026-01-06

### Fixed
- Fixed wheel distribution packaging configuration
- Corrected dependency version specifications (added upper bounds for numpy, pandas)
- Improved MANIFEST.in for comprehensive package inclusion
- Updated pyproject.toml to modern setuptools standards

### Changed
- Removed setuptools_scm dependency (not needed for static versioning)
- Updated Python minimum version to 3.8+ for better compatibility
- Enhanced package classifiers for better PyPI discoverability
- Reorganized dependency specifications for clarity

### Added
- Added CHANGELOG.md for proper release documentation
- Added build and distribution scripts
- Enhanced documentation for package development

## [0.1.2] - TBD

### Added
- Initial features placeholder

## [0.1.1] - TBD

### Added
- Initial features placeholder

## [0.1.0] - TBD

### Added
- Project initialization
