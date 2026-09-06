# Changelog

All notable changes to this project will be documented in this file.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

### Fixed
- `ContaminationScanner`/`japhrase.contamination` detectors: any detector
  exception was silently swallowed and treated as "0 anomalies" (clean) for
  that axis, with no field anywhere on `AxisScore`/`ContaminationProfile` to
  distinguish "genuinely clean" from "detector crashed and we never
  checked". This was reachable through the plain public API with entirely
  ordinary-looking arguments: `scan(text, segment_size=1)` (or `=0`) crashed
  `detect_distribution` with `ValueError: range() arg 3 must not be zero`
  (`segment_size // 2 == 0`); the equivalent applied to `complexity`,
  `language` (`segment_size`) and `repetition` (`repetition_window`).
  `ContaminationScanner.__init__` now validates `segment_size`/
  `repetition_window` (>= 2) and raises immediately instead of deferring to
  a swallowed crash; the four affected detectors also guard against
  degenerate window/segment sizes directly, since they're documented as a
  supported low-level API callable without going through the scanner.
  Independently of those specific triggers, `AxisScore` now carries an
  `error: Optional[str]` field and `ContaminationProfile.failed_axes`
  reports which axes failed, so a *future* detector bug can no longer look
  identical to "no contamination found" — `report()`/`explain()` now call
  out failed axes explicitly instead of only ever showing "✅ 問題なし".
- `HabitDriftDetector.analyze()`: `worsening_count`/`improving_count` were
  computed *after* truncating the candidate list to the top `top_n` (default
  30) entries sorted by worsening slope descending. Once a Part had more than
  `top_n` candidate habit phrases (routine for any real manuscript), this
  systematically discarded improving habits from the count — a synthetic
  worst-case reproduction went from a true 18 worsening / 21 improving split
  down to a reported 5 worsening / 0 improving. This fed directly into
  `PartHealthReport._diagnose_habits()`'s health score, making "書き癖負債"
  look far worse than reality. Aggregate counts are now computed over the
  full candidate set; `top_n` only bounds the displayed list. Added
  `DriftResult.total_candidates` (the correct, untruncated denominator) and
  `top_improving` (so `report()`'s "改善中の書き癖" section stops re-deriving
  from the already-truncated display list); `PartHealthReport._diagnose_habits`
  now divides by `total_candidates` instead of `len(result.habits)`.
- `CharacterStylometry._extract_speech` / `_extract_narration`: when a
  character's name appeared multiple times in a short span (e.g. repeated in
  narration before a single line of dialogue), the same quoted speech or the
  same overlapping narration context window was captured once per name
  occurrence, duplicating it in the character's corpus. This skewed every
  downstream style metric (MATTR, hapax ratio, sentence length, keyness) by
  how often a character's name happened to be mentioned nearby rather than by
  their actual dialogue/narration content. Speech is now deduplicated by
  absolute span; narration context windows are merged before extraction so
  overlapping regions are only counted once.
- `japhrase.contamination` detectors (`detect_distribution`, `detect_complexity`,
  `detect_repetition`): these strip whitespace/collapse newlines into an
  internal `clean` string before computing offsets, but were using those
  `clean`-string offsets directly as if they were offsets into the original
  `text` when building `Anomaly.start`/`.end`/`.line_no`. Since `clean` is
  always shorter than `text`, the reported location drifted further off the
  longer the document, so `profile.explain()`/`anomaly.location` could point
  at the wrong line entirely. Fixed via a proper clean-offset → text-offset
  mapping. Also fixed `detect_repetition` comparing a phrase's raw occurrence
  count against a previously-recorded *capped severity* (max 6) instead of
  the previous raw count when the same phrase repeated across overlapping
  windows, which could let a milder occurrence overwrite a far worse one in
  the reported `回反復` count (severity itself was usually unaffected since
  it saturates quickly, so this mainly affected the displayed count, not
  the axis score).
- `PhraseExtracter`: `max_length` off-by-one (and the resulting dead
  `max_length=-1` branch), `min_count` boundary inconsistency between
  `count_characters`/`df_from_counts`, `save_params`/`load_params` dropping
  tuned weights, branching-entropy sentence-boundary leakage, and
  `lang='en'` `length` being counted in characters instead of words
- `DocumentVectorizer`/`vectorization_utils`: `low_pmi`/`high_pmi` feature
  matrices were silently near-all-zero (analyzer/vocabulary mismatch);
  `hybrid` mode now actually combines the TF-IDF and low-PMI feature
  spaces instead of discarding the former; duplicate basenames from
  different directories no longer collide in comparison labels
- `CooccurrenceAnalyzer.extract_context`: infinite loop on an empty
  `target`; overlapping/adjacent context windows around closely-spaced
  target occurrences are now merged instead of double-counted
- `import japhrase.extracter` was returning the `PhraseExtracter` class
  instead of the `extracter` submodule (namespace collision with the
  package-level `extracter` class alias); `sys.modules['japhrase.extracter']`
  itself was always correct, but any code binding the imported name
  directly (`import japhrase.extracter as m`, or introspecting it as a
  module) got the class
- CLI fail-open audit: three commands could report success (exit 0) while
  effectively doing nothing —
  - `japhrase workflow`: a workflow YAML with zero tasks (`tasks: []`, or
    the `tasks` key omitted) passed `WorkflowDefinition.validate()` (an
    empty task graph is trivially acyclic) and was reported as
    "0/0 succeeded" / "all tasks completed successfully". `validate()` now
    rejects workflows with no tasks, and the CLI's success check now
    requires every task to have actually reached `COMPLETED` (previously
    it only checked `failed == 0`, which would also miss tasks stuck at
    `PENDING`/`RUNNING`/`SKIPPED`)
  - `japhrase extract --format csv` / `--format json` without `-o/--output`
    silently fell back to printing a table and reported success, never
    writing the requested file; it now errors and exits 1, matching the
    `stats` command's existing (and now consistent) behavior
  - `japhrase check`: a config file with no `[check]` section, or a
    `[check]` section with none of the recognized rule keys, ran zero
    checks and reported "✅ すべてのチェックに合格しました" (0 errors from
    0 checks run); this is now treated as a configuration error (exit 1)
    unless `[check] enabled = false` is set explicitly to opt out of
    checking on purpose
- `IncrementalPhraseState.save()`, `AdaptiveTuner.save()`,
  `PhraseExtracter.save_params()`, `VectorizationResult.save()`: writes
  went directly to the target path (`open(path, 'w'/'wb')`), so a process
  crash/kill mid-write could leave a truncated, unreadable state file.
  These now write through a temp file + `os.replace` and are atomic with
  respect to crashes. This does **not** fix concurrent-write data loss —
  see the known limitation below.

### Known limitations
- `IncrementalPhraseState` and `AdaptiveTuner`: the atomic-write fix above
  only protects against crash-time corruption. The higher-level
  load → update → save cycle used by `--state-path`/`--resume` and by
  `AdaptiveTuner`'s auto-tuning still has no locking: if two processes
  point at the same state/storage path concurrently, whichever one saves
  last silently overwrites the other's update with no error (a classic
  lost update). Do not run multiple concurrent jobs against the same
  state path. Documented and pinned by negative-control regression tests
  in `tests/test_incremental.py` and `tests/test_adaptive_tuner.py`.

### Changed
- **Breaking:** `from japhrase import extracter` (the package-level
  backward-compat alias for the `PhraseExtracter` class) has been removed;
  it collided with the `japhrase.extracter` submodule name and could not
  coexist with a correct `import japhrase.extracter`. Use
  `from japhrase.extracter import extracter` instead — the alias inside
  the submodule itself is unchanged.
- **Breaking:** `DocumentVectorizer.from_files()` / `.from_texts()` are no
  longer `@classmethod`s — call them on a configured instance
  (`DocumentVectorizer(...).from_files(...)`), not on the class directly.
  The classmethod form silently discarded every constructor argument
  except `n_topics`/`feature_mode` (including `pmi_threshold`, `min_count`,
  `ngram_range`), so any code relying on that discarding behavior — or
  calling `DocumentVectorizer.from_files(...)` on the class itself — will
  need to construct an instance first.
- `DocumentVectorizer` hybrid mode's feature names are now namespaced
  (`tfidf:...` / `low_pmi:...`) since the same phrase can legitimately
  appear in both feature spaces

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
