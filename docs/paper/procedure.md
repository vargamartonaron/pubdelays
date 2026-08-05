# Procedure

## Data sources and reproducibility

We constructed an article-level dataset from PubMed/MEDLINE XML and attached
journal- or article-level metadata from SCImago Journal Rank, the Scopus Source List, the
Directory of Open Access Journals (DOAJ), the Norwegian Publication Indicator (NPI),
study-curated publisher metadata, Retraction Watch, and, when lawfully supplied, an
optional private peer-review dataset. PubMed was the source of article titles,
publication types, journal identifiers, keywords, DOI, receipt and acceptance dates,
and publication dates. All processing was performed by the versioned Python pipeline
in this repository. Each stage wrote a machine-readable SQLite manifest containing the
stage name, input and output paths, record counts, status, timestamps, checksums, and
stage metadata. Raw downloads, generated datasets, manifests, and caches were not
treated as repository source files.

Public PubMed baseline files were downloaded with their MD5 sidecars and accepted
only after checksum verification. DOAJ and Retraction Watch were downloaded from
their current public CSV endpoints. Other journal sources were supplied as dated,
licensed or manually exported snapshots. The acquisition procedure and current
official endpoints are documented in [Data acquisition and snapshotting](../guides/data-acquisition.md).

## Parsing and initial curation

The XML parser operated in bounded memory and represented deletion notices explicitly.
Before every row-removing operation, the pipeline recorded the number and proportion
missing for every variable, both overall and marginally by publication year; it then
recorded the same measures after the operation. The ordered filtering checkpoints
were: all parsed records; non-deleted records; records containing the required parsed
fields, including a nonempty title; records with usable receipt and acceptance dates; records whose publication
types contained `Journal Article`; records with a linking ISSN; chronologically
coherent dates; nonnegative derived delays; rows after external left joins; journals
eligible under journal metadata; distinct titles; and final rows.

The receipt date came from PubMed history entries with status `received`, and the
acceptance date from entries with status `accepted`. Publication date was the PubMed
article date when available and otherwise the journal-issue publication date; the
selected origin was retained in `publication_date_source`, and the unmodified article
date remained in `article_date_raw`. We required receipt < acceptance < publication.
Acceptance delay was acceptance minus receipt and publication delay was publication
minus acceptance, both in days. These checks exclude equal or reversed dates. Receipt
dates before 1 January 2013 were excluded. The analysis validation window was 1
January 2016 through 31 December 2025, and delay analyses retained values from 1 to
1,095 days. Exact-title duplicates were reduced deterministically to the first row in
stable input order; records without a title were excluded before this operation.

## External joins and variable handling

ISSNs were normalized by removing punctuation and uppercasing a terminal `X`; DOIs
were lowercased and stripped of DOI URL and prefix forms. Journal databases were left
joined by linking ISSN so an absent match did not remove an otherwise eligible
article. Retraction Watch was left joined by normalized DOI. Peer-review metadata, if
supplied, used DOI, then PMID, then title according to the preprocessor's declared key
availability. Multi-ISSN source rows were expanded before joining, and each external
source was reduced to one deterministic row per normalized join key. Conflicting
publisher and publisher-group values were retained as explicit conflict flags.

For every join, diagnostics were computed on the incoming population before any
missingness-based restriction. They report source availability, availability of the
article join key, matched and unmatched counts and ratios, duplicate-key pressure,
and row-cardinality change, overall and by year. Thus join success for one database is
marginalized over missingness in all other databases. No article is filtered merely
because an optional external variable is missing. Missing optional values are emitted
as empty fields in the canonical public schema; categorical model inputs subsequently
use an explicit `__MISSING__` level and numeric inputs remain missing for CatBoost's
native handling.

Year-specific SCImago and NPI values were selected using publication year through
2025. Open access was `True` when any implemented DOAJ, Scopus Source List, or NPI
condition was positive. Megajournal status used the fixed list
of 21 linking ISSNs encoded in the schema. COVID-19 status was based on a
case-insensitive controlled synonym search across title and keywords. A record was
classified as retracted when a DOI-matched Retraction Watch record contained a
retraction nature or reason. `OriginalPaperDate` is retained only as
`retraction_original_date`; it never overwrites the PubMed publication date.

## Variable provenance

The executable registry `pubdelays provenance` contains one ordered record for every
canonical output variable, including level, variable kind, source fields, join key,
derivation, units, and missingness interpretation. The source overview is:

| Variables | Level | Source |
| --- | --- | --- |
| `received`, `article_date`, `article_date_raw`, `publication_date_source`, `acceptance_delay`, `publication_delay`, `publication_types`, `title`, `journal`, `issn_linking`, `keywords`, `doi`, `is_covid` | article/journal | PubMed/MEDLINE; delays and COVID flag derived |
| `is_mega` | journal | fixed study classification by linking ISSN |
| `h_index_year`, `quartile_year`, `rank_year`, `scimago_categories` | journal-year | SCImago Journal Rank |
| `discipline`, `asjc`, `discipline_all`, `asjc_all` | journal | Scopus Source List |
| `open_access` | journal | derived from DOAJ, Scopus Source List, and NPI |
| `publisher`, `publisher_group`, `publisher_conflict`, `publisher_group_conflict` | journal | study-curated publisher metadata |
| `npi_discipline`, `npi_field`, `npi_year`, `is_series`, `established`, `country` | journal/journal-year | NPI, with country fallback from available journal metadata |
| `apc`, `apc_amount` | journal | DOAJ |
| `retraction_nature`, `reason`, `retraction_date`, `retraction_original_date`, `is_retracted` | article | Retraction Watch joined by DOI |
| `n_review_round`, `n_reviews`, `first_review_date`, `last_review_date`, `n_reviewers`, `date_first_accepted`, `review_cycle_delay`, `review_finding_delay`, `first_decision_delay`, `final_decision_delay`, `first_review_delay`, `peer_review_delay` | article | optional private peer-review metadata, with date-derived intervals |

The exact table can be exported without manual transcription:

```bash
pubdelays provenance --format csv --output data/processed_data/variable_provenance.csv
```

## Missingness and final data-quality reporting

Shard-level filter and quality sidecars are aggregated only after all shards have
completed. `filter_counts.csv` gives the population remaining at each ordered
checkpoint. `stage_and_join_debrief.csv` contains the pre/post, overall/yearly
missingness and join diagnostics. `variable_quality_debrief.csv` gives, for every final
variable and year plus the overall population, row count, present and missing counts,
missing percentage, and number of distinct nonmissing values;
`variable_distributions.parquet` provides numeric, date, categorical, or text-length
distribution summaries as appropriate. The reports preserve zero-match and
source-not-supplied states rather than conflating them. Schema order, CSV/Parquet row
parity, join cardinality, manifest integrity, chronological ranges, and output paths
are checked by automated tests and by the isolated live smoke workflow.

## Retrospective boosting analysis

The optional analysis models acceptance and publication delays separately with
CatBoost regression. This is a retrospective predictive-association analysis:
post-event journal or review variables may be included, and neither feature
importance nor SHAP values are interpreted causally. Primary models include
retracted articles and `is_retracted`; a complete sensitivity run excludes retracted
records. The feature set includes journal, discipline, open-access and megajournal
flags, COVID-19 status, APC indicator, country, publication-date source, annual
SCImago and NPI measures, journal series and establishment fields, peer-review counts
when supplied, and calendar features derived from publication date.

Models train on 2016–2023, use 2024 for early stopping and iteration selection, refit
on 2016–2024, and evaluate once on 2025. Evaluation reports MAE, median absolute
error, RMSE, and R² against global and journal-specific historical-median baselines.
Journal-grouped cross-validation on the training period evaluates transfer across
journals. Test diagnostics are stratified by discipline, open-access status,
megajournal status, and whether the journal appeared in development data. Outputs
include predictions, residual plots, feature importance, sampled SHAP values and
mean absolute SHAP importance, feature missingness by split, serialized models, and a
JSON model manifest recording configuration, features, split sizes, and the
noncausal interpretation.

No individuals participated in the study; therefore, ethical approval was not sought.
