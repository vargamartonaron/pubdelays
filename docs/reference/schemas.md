---
title: Schemas
description: analysis_dataset_v1 columns, parsed requirements, filter counts, and shard naming.
icon: octicons/table-16
---

# Schemas

`analysis_dataset_v1` is the public schema for `data/processed_data/processed.parquet` and `data/processed_data/processed.csv`. The version constant, required parsed fields, filter stages, peer-review columns, and canonical output order live in `src/pubdelays/schema.py`.

All exported columns are stored as strings for CSV/Parquet interoperability. Boolean flags use string values `True` and `False`; missing text, dates, and unavailable numeric metadata use the empty string. Delay columns are integer day counts encoded as strings.

!!! warning "Schema changes"
    Changing `CANONICAL_ARTICLE_COLUMNS` requires tests that deliberately update the expected order. Use `pubdelays schema --input ...` to validate produced datasets.

## Required parsed fields

Transform requires these parsed fields before date and journal filters:

```text
history
journal
pubdate
publication_types
issn_linking
```

## Filter stages

Transform filter sidecars use the ordered stages from `FILTER_STAGES`:

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

## Article shard naming

`src/pubdelays/shards.py` defines the canonical transform shard pattern:

```text
articles-shard-<index>-of-<total>.<format>
```

Example for a 64-shard Parquet run:

```text
articles-shard-00000-of-00064.parquet
articles-shard-00063-of-00064.parquet
```

`validate-shards` checks expected shard IDs, duplicate outputs, wrong `of-N` totals, unsupported formats, unreadable files, and missing canonical columns.

## Final columns

| Column | Group | Meaning | Units / values |
| --- | --- | --- | --- |
| `is_covid` | flags | Article title/keyword COVID synonym flag. | `True`/`False` |
| `received` | dates | Manuscript received date from PubMed history. | ISO date |
| `article_date` | dates | Publication date used for publication delay. | ISO date |
| `article_date_raw` | dates | Raw ArticleDate-derived date before fallback. | ISO date or empty |
| `publication_date_source` | dates | Source for `article_date`. | `article_date`, `pubdate`, or empty |
| `acceptance_delay` | outcomes | Accepted minus received. | days |
| `is_mega` | flags | Linking ISSN is in the configured megajournal set. | `True`/`False` |
| `issn_linking` | identifiers | Normalized PubMed linking ISSN. | ISSN without punctuation |
| `h_index_year` | journal metadata | SCImago h-index for the article year. | integer string |
| `open_access` | external enrichment | Open-access flag from DOAJ, WoS, or NPI evidence. | `True`/`False` |
| `publication_delay` | outcomes | Publication date minus accepted date. | days |
| `publication_types` | article metadata | PubMed publication type labels. | semicolon/text |
| `title` | identifiers | Article title. | text |
| `journal` | identifiers | Journal title from PubMed. | text |
| `quartile_year` | journal metadata | SCImago quartile for the article year. | `Q1`-`Q4` or empty |
| `rank_year` | journal metadata | SCImago rank for the article year. | integer string |
| `discipline` | field classification | First WoS-derived umbrella discipline. | text |
| `asjc` | field classification | First WoS ASJC code. | code string |
| `discipline_all` | field classification | All WoS-derived umbrella disciplines. | pipe-delimited text |
| `asjc_all` | field classification | All WoS ASJC codes for the journal. | pipe-delimited code strings |
| `scimago_categories` | field classification | SCImago categories from current-year metadata. | pipe-delimited text |
| `publisher` | external enrichment | First non-empty publisher name for the ISSN. | text |
| `publisher_group` | external enrichment | First non-empty publisher group/parent. | text |
| `publisher_conflict` | external enrichment | Publisher rows disagree for the ISSN. | `True`/`False` or empty |
| `publisher_group_conflict` | external enrichment | Publisher-group rows disagree for the ISSN. | `True`/`False` or empty |
| `npi_discipline` | field classification | Norwegian Publication Indicator discipline. | text |
| `npi_field` | field classification | Norwegian Publication Indicator field. | text |
| `npi_year` | journal metadata | NPI level for the article year. | level string |
| `is_series` | journal metadata | NPI series flag. | source value |
| `established` | journal metadata | NPI established year. | year string |
| `country` | journal metadata | Journal country from external metadata. | text |
| `keywords` | article metadata | PubMed keyword text. | text |
| `apc` | external enrichment | DOAJ APC availability flag/value. | source value |
| `apc_amount` | external enrichment | DOAJ APC amount. | amount string |
| `doi` | identifiers | Normalized article DOI. | lowercase DOI |
| `retraction_nature` | retractions | Retraction Watch nature. | text |
| `reason` | retractions | Retraction Watch reason. | text |
| `retraction_date` | retractions | Retraction date. | ISO date or source date |
| `is_retracted` | retractions | Retraction evidence exists. | `True`/`False` |
| `n_review_round` | optional peer review | Review round count from a licensed/private peer-review metadata table. | integer string or empty |
| `n_reviews` | optional peer review | Review count from the optional peer-review table. | integer string or empty |
| `first_review_date` | optional peer review | First review date from the optional peer-review table. | ISO date or empty |
| `last_review_date` | optional peer review | Last review date from the optional peer-review table. | ISO date or empty |
| `n_reviewers` | optional peer review | Reviewer count from the optional peer-review table. | integer string or empty |
| `date_first_accepted` | optional peer review | First accepted date from the optional peer-review table. | ISO date or empty |
| `review_cycle_delay` | optional peer review | Review-cycle delay from the optional peer-review table. | days or empty |
| `review_finding_delay` | optional peer review | Review-finding delay from the optional peer-review table. | days or empty |
| `first_decision_delay` | optional peer review | First-decision delay from the optional peer-review table. | days or empty |
| `final_decision_delay` | optional peer review | Final-decision delay from the optional peer-review table. | days or empty |
| `first_review_delay` | optional peer review | First-review delay from the optional peer-review table. | days or empty |
| `peer_review_delay` | optional peer review | Total peer-review delay from the optional peer-review table. | days or empty |

Validate a produced dataset's column contract with:

```bash
pubdelays schema --input data/processed_data/processed.parquet
```

Run final-output quality validation with:

```bash
pubdelays validate-analysis --input data/processed_data/processed.parquet
```
