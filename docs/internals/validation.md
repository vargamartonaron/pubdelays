---
title: Validation
description: Correctness gates, smoke checks, and fixture test coverage.
icon: octicons/check-circle-16
---

# Validation

This page separates correctness checks from performance benchmarking. It is grounded in `tests/`, `pyproject.toml`, and the current CLI.

## Static checks

Run before committing source or docs changes:

```bash
python -m compileall -q src tests
ruff check src tests
pytest -q
git diff --check
```

With Nix:

```bash
nix flake check --show-trace
nix build --show-trace --print-build-logs
```

With uv:

```bash
uv sync --extra dev
uv run pytest -q
uv run python -m pubdelays.cli --help
```

## CLI smoke checks

These should work without full raw data, though `preflight` may report missing inputs:

```bash
pubdelays --help
pubdelays init-dirs
pubdelays preflight
pubdelays manifest --limit 10
pubdelays schema
```

## Parser validation

Fixture coverage should include normal `PubmedArticle`, missing history, missing `ArticleDate` with present `PubDate`, `DeleteCitation`, DOI extraction, publication type extraction, linking ISSN extraction, and MeSH terms.

```bash
pytest -q tests/test_medline_parser.py
```

## External metadata validation

External preprocessors should check required input columns and produce documented lookup columns.

```bash
pytest -q tests/test_external_preprocessors.py
```

Expected processed outputs include:

```text
data/processed_data/scimago.csv
data/processed_data/web_of_science.csv
data/processed_data/doaj.csv
data/processed_data/norwegian_list.csv
data/processed_data/retraction_watch.csv
```

## Transform validation

Behavioral tests should cover required received/accepted dates, article-date preference, pubdate fallback, negative delay drops, ceased-journal filtering, retraction DOI joins, year-specific metadata, and final schema order.

```bash
pytest -q tests/test_transform_articles.py
pytest -q tests/test_end_to_end_pipeline.py
```

## Manifest validation

After a smoke run:

```bash
pubdelays manifest --limit 20
pubdelays manifest summary
```

Check that failed rows include error text, skipped rows include skip metadata, success rows include output path and row count when applicable, worker identity is present, and elapsed seconds are nonnegative.

## Cross-run validation

When full raw data are available, compare a new run against the previous accepted processed dataset:

```bash
pubdelays compare-outputs \
  --baseline data/processed_data/processed.previous.csv \
  --candidate data/processed_data/processed.csv \
  --output data/processed_data/validation/differential.csv
```

Review total row count, unique DOI/PMID counts if available, column order, filter counts, join cardinalities, missingness in key fields, validation excluded rows, and distributions of `acceptance_delay` and `publication_delay`.

## Methods note

The final analysis dataset keeps records that pass the configured article-date and delay bounds. `validate-analysis` writes the kept dataset and the excluded rows separately, plus `excluded_by_reason.csv`, excluded-row breakdowns by quartile, discipline, NPI level, and open-access status, and missingness tables grouped by year, quartile, discipline, NPI level, open-access status, and pairwise field co-missingness. Use these outputs when describing data handling and missing-data mechanisms in the methods section.

For the full data-quality debrief, run `pubdelays quality-report`. Transform
sidecars measure missingness before filtering rather than only describing
survivors. The report provides source-specific join success by year, completeness
for every variable, typed variable distributions, and pairwise marginal
missingness. Each marginal uses its own available denominator; it does not first
restrict the dataset to complete cases on unrelated variables.
