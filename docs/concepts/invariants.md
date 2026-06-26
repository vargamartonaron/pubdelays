---
title: Invariants
description: Stable semantic decisions and corrections for parser, transform, and aggregation behavior.
icon: octicons/shield-check-16
---

# Invariants

This page records semantics that are intentionally stable across code, tests, and documentation.

## Date semantics

```text
publication_date = article_date if present else pubdate
```

Rows with missing `article_date` but usable `pubdate` are retained when all other date checks pass. The transform records the source in `publication_date_source`.

The fallback is covered by transform and end-to-end tests.

## Journal eligibility

Journal metadata eligibility uses the article publication year:

```text
keep if is_conference == 0
keep if received >= transform.min_received
keep if ceased year is missing or ceased year >= article publication year
```

Ceased journals are not filtered by the current year or by received year.

## Filter-count contract

`src/pubdelays/schema.py` defines the ordered filter stages:

```text
raw_records
non_deleted_records
has_required_parsed_fields
has_received_and_accepted_dates
journal_articles
has_linking_issn
coherent_dates
nonnegative_delays
after_external_joins
eligible_journal_metadata
distinct_titles
final_rows
```

Transform sidecars use these names to expose every row-dropping gate in order.

## Intermediate formats

- Parser output remains JSONL by default because it is line-oriented and streaming-friendly.
- Parser JSON output is retained for small fixtures and interoperability, not full-corpus defaults.
- Transform output remains Parquet by default because it is the canonical intermediate for aggregation.

## Optional peer-review metadata

Peer-review metadata is not bundled with the repository. Transform commands accept `--peer-review path/to/table.csv`; when omitted, peer-review columns remain present but empty.

## Legacy relationship

`LEGACY.md` records the legacy R/shell implementation and intentional corrections. If a semantic change diverges from legacy behavior, update tests and document the correction rather than silently changing output meaning.
