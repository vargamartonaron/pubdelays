---
title: Data model
description: Conceptual model for parsed records, lookup tables, shards, and final output.
icon: octicons/table-16
---

# Data model

The pipeline does not keep one in-memory dataframe for the entire PubMed baseline. It moves through explicit file-backed products.

## Entities

| Entity | File shape | Produced by | Consumed by |
| --- | --- | --- | --- |
| PubMed source file | `.xml` or `.xml.gz` | NCBI download or manual placement | `parse`, `parse-one` |
| Parsed record shard | `.jsonl` preferred; `.json` supported for small cases | `parse`, `parse-one` | `validate`, `transform`, `transform-shards` |
| External lookup table | CSV under `data/processed_data/` | `external-*` commands | `transform-one`, `transform`, `transform-shards` |
| Article shard | `articles-shard-00000-of-00064.parquet` by default | `transform-shard`, `transform-shards` | `validate-shards`, `aggregate`, `aggregate-all` |
| Analysis dataset | `processed.parquet`, `processed.csv` | `aggregate-all` | `schema`, `summaries`, downstream analysis |
| Manifest row | SQLite `runs` row | Manifested stage handlers | `manifest` subcommands, audit review |

## Delay fields

The central derived fields are:

```text
acceptance_delay  = accepted_date - received_date
publication_delay = publication_date - accepted_date
publication_date  = article_date if present else pubdate
```

Rows must have coherent dates: received before accepted, accepted before publication, and nonnegative delay values.

## External metadata joins

Transform joins lookup tables by normalized `issn_linking` for journal metadata and by normalized DOI for Retraction Watch. Optional peer-review metadata is joined by `doi`, `pmid`, or `title` when the table has those keys.

!!! note "Missing metadata"
Missing Scimago, WoS, DOAJ, NPI, publisher, Retraction Watch, or peer-review values do not by themselves remove rows. Canonical output columns remain present and are filled with empty strings or explicit boolean flags as implemented in `src/pubdelays/transform/articles.py`.

Retraction Watch's original-paper date is retained separately as
`retraction_original_date`. The PubMed-derived `article_date` remains the sole
publication date used for chronological filtering and delay calculation.

## Quality diagnostics

Transform records missingness immediately before and after every row-dropping
gate. External joins record missing keys, keyed matches, keyed non-matches, and
whether a source was supplied, overall and by publication year. The
`quality-report` command aggregates these sidecars and describes every final
variable without applying complete-case filtering.
