"""Canonical schema constants for the publication-delay analysis dataset."""

from __future__ import annotations

from pathlib import Path

import polars as pl

ANALYSIS_DATASET_VERSION = "analysis_dataset_v1"

REQUIRED_PARSED_FIELDS: tuple[str, ...] = (
    "history",
    "journal",
    "pubdate",
    "publication_types",
    "issn_linking",
)

FILTER_STAGES: tuple[str, ...] = (
    "raw_records",
    "non_deleted_records",
    "has_required_parsed_fields",
    "has_received_and_accepted_dates",
    "journal_articles",
    "has_linking_issn",
    "coherent_dates",
    "nonnegative_delays",
    "after_external_joins",
    "eligible_journal_metadata",
    "distinct_titles",
    "final_rows",
)

PEER_REVIEW_COLUMNS: tuple[str, ...] = (
    "n_review_round",
    "n_reviews",
    "first_review_date",
    "last_review_date",
    "n_reviewers",
    "date_first_accepted",
    "review_cycle_delay",
    "review_finding_delay",
    "first_decision_delay",
    "final_decision_delay",
    "first_review_delay",
    "peer_review_delay",
)

# Public analysis dataset column order. Optional metadata columns are emitted as
# empty strings when their source table is not supplied.
CANONICAL_ARTICLE_COLUMNS: tuple[str, ...] = (
    "is_covid",
    "received",
    "article_date",
    "article_date_raw",
    "publication_date_source",
    "acceptance_delay",
    "is_mega",
    "issn_linking",
    "h_index_year",
    "open_access",
    "publication_delay",
    "publication_types",
    "title",
    "journal",
    "quartile_year",
    "rank_year",
    "discipline",
    "asjc",
    "discipline_all",
    "asjc_all",
    "scimago_categories",
    "publisher",
    "publisher_group",
    "publisher_conflict",
    "publisher_group_conflict",
    "npi_discipline",
    "npi_field",
    "npi_year",
    "is_series",
    "established",
    "country",
    "keywords",
    "apc",
    "apc_amount",
    "doi",
    "retraction_nature",
    "reason",
    "retraction_date",
    "is_retracted",
    *PEER_REVIEW_COLUMNS,
)

COVID_SYNONYMS: tuple[str, ...] = (
    "covid",
    "covid-19",
    "coronavirus disease 19",
    "sars-cov-2",
    "2019-ncov",
    "2019ncov",
    "2019-n-cov",
    "2019n-cov",
    "ncov-2019",
    "n-cov-2019",
    "coronavirus-2019",
    "wuhan pneumonia",
    "wuhan virus",
    "wuhan coronavirus",
    "coronavirus 2",
)

REPLICATION_SYNONYMS: tuple[str, ...] = (
    "replication",
    "replicating",
    "replication of",
    "replication study",
)

MEGAJOURNAL_ISSNS: frozenset[str] = frozenset(
    {
        "24701343",
        "21583226",
        "20466390",
        "20446055",
        "23251026",
        "22115463",
        "21601836",
        "21693536",
        "20513305",
        "21678359",
        "19326203",
        "20545703",
        "21582440",
        "20452322",
        "20566700",
        "23915447",
        "22991093",
        "24058440",
        "21508925",
        "2050084X",
        "20461402",
    }
)


def read_analysis_columns(path: Path) -> list[str]:
    path = Path(path)
    suffix = path.suffix.lower()
    if suffix == ".parquet":
        return list(pl.scan_parquet(path).collect_schema().names())
    if suffix == ".tsv":
        return list(pl.read_csv(path, separator="\t", n_rows=0).columns)
    return list(pl.read_csv(path, n_rows=0).columns)


def validate_analysis_dataset_schema(path: Path) -> tuple[bool, list[str]]:
    columns = read_analysis_columns(path)
    expected = list(CANONICAL_ARTICLE_COLUMNS)
    errors: list[str] = []
    missing = [column for column in expected if column not in columns]
    extra = [column for column in columns if column not in expected]
    if missing:
        errors.append("missing columns: " + ", ".join(missing))
    if extra:
        errors.append("unexpected columns: " + ", ".join(extra))
    if not missing and not extra and columns != expected:
        errors.append("column order differs from analysis_dataset_v1")
    return not errors, errors
