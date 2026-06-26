"""Polars implementation of the article transformation stage.

The stage consumes parsed PubMed/MEDLINE JSONL shards, joins cleaned external
metadata, applies explicit filter stages, and writes Parquet/TSV/CSV through
atomic output files.  The hot path uses Polars DataFrames and native joins;
small Python helpers are retained only for unit-level semantic checks.
"""

from __future__ import annotations

import json
import re
from collections import Counter
from collections.abc import Mapping
from dataclasses import dataclass
from datetime import date, datetime
from pathlib import Path
from typing import Any

import polars as pl

from pubdelays.external.common import (
    doi_expr,
    issn_expr,
    normalize_doi_text,
    normalize_header,
    normalize_issn_text,
    scan_tabular,
    write_frame,
)
from pubdelays.schema import (
    CANONICAL_ARTICLE_COLUMNS,
    COVID_SYNONYMS,
    FILTER_STAGES,
    MEGAJOURNAL_ISSNS,
    PEER_REVIEW_COLUMNS,
    REQUIRED_PARSED_FIELDS,
)

JsonRecord = dict[str, Any]
Row = dict[str, Any]


@dataclass(frozen=True)
class ExternalInputs:
    """Optional processed metadata inputs joined during article transformation."""

    scimago: Path | None = None
    web_of_science: Path | None = None
    doaj: Path | None = None
    norwegian_list: Path | None = None
    retraction_watch: Path | None = None
    publisher: Path | None = None
    peer_review: Path | None = None


@dataclass(frozen=True)
class TransformResult:
    """Paths and filter counts emitted by one transform call."""

    output_path: Path
    filters_path: Path | None
    counts: Mapping[str, int]


def normalize_issn(value: Any) -> str:
    return normalize_issn_text(value)


def normalize_doi(value: Any) -> str:
    return normalize_doi_text(value)


def parse_date(value: Any) -> date | None:
    text = "" if value is None else str(value).strip()
    if not text:
        return None
    for fmt in ("%Y-%m-%d", "%Y-%m", "%Y"):
        try:
            parsed = datetime.strptime(text, fmt)
            return parsed.date()
        except ValueError:
            pass
    return None


def iso(value: date | None) -> str:
    return "" if value is None else value.isoformat()


def publication_type_labels(value: Any) -> str:
    if value is None:
        return ""
    labels: list[str] = []
    for part in re.split(r";\s*", str(value)):
        if not part:
            continue
        labels.append(part.split(":", 1)[1].strip() if ":" in part else part.strip())
    return ", ".join(label for label in labels if label)


def contains_any_term(text: str, terms: tuple[str, ...]) -> bool:
    for term in terms:
        if re.search(rf"\b{re.escape(term)}\b", text or "", flags=re.IGNORECASE):
            return True
    return False


def date_expr(expr: pl.Expr) -> pl.Expr:
    s = expr.cast(pl.Utf8, strict=False).str.strip_chars()
    exact = s.str.strptime(pl.Date, "%Y-%m-%d", strict=False)
    year_month = (s + pl.lit("-01")).str.strptime(pl.Date, "%Y-%m-%d", strict=False)
    year = (s + pl.lit("-01-01")).str.strptime(pl.Date, "%Y-%m-%d", strict=False)
    return exact.fill_null(year_month).fill_null(year)


def day_delta_expr(later: pl.Expr, earlier: pl.Expr) -> pl.Expr:
    return (later - earlier).dt.total_days()


def publication_types_expr(expr: pl.Expr) -> pl.Expr:
    return (
        expr.cast(pl.Utf8, strict=False)
        .fill_null("")
        .str.replace_all(r"[A-Za-z0-9]+:", "")
    )


def bool_text_expr(expr: pl.Expr) -> pl.Expr:
    return (
        pl.when(expr.fill_null(False)).then(pl.lit("True")).otherwise(pl.lit("False"))
    )


def first_stage_record(record: JsonRecord, counts: Counter[str]) -> Row | None:
    """Small semantic reference implementation used by unit tests.

    The production transform is vectorized below.  This function keeps date and
    ceased-year semantics easy to test without constructing a Polars frame.
    """

    counts["raw_records"] += 1
    if record.get("delete"):
        return None
    counts["non_deleted_records"] += 1
    if any(field not in record for field in REQUIRED_PARSED_FIELDS):
        return None
    counts["has_required_parsed_fields"] += 1

    row: Row = dict(record)
    history = record.get("history")
    if isinstance(history, dict):
        row.update(history)
    row["publication_types"] = publication_type_labels(row.get("publication_types"))
    row["keywords"] = str(row.get("keywords") or "").replace(";", ",")
    row["issn_linking"] = normalize_issn(row.get("issn_linking"))
    row["doi"] = normalize_doi(row.get("doi"))

    received = parse_date(row.get("received"))
    accepted = parse_date(row.get("accepted"))
    pubdate = parse_date(row.get("pubdate"))
    article_dt = parse_date(row.get("article_date"))
    if received is None or accepted is None:
        return None
    counts["has_received_and_accepted_dates"] += 1
    if "Journal Article" not in row["publication_types"]:
        return None
    counts["journal_articles"] += 1
    if not row["issn_linking"]:
        return None
    counts["has_linking_issn"] += 1

    publication_dt = article_dt or pubdate
    source = (
        "article_date"
        if article_dt is not None
        else "pubdate"
        if pubdate is not None
        else ""
    )
    if (
        publication_dt is None
        or received >= publication_dt
        or accepted >= publication_dt
        or accepted <= received
    ):
        return None
    counts["coherent_dates"] += 1
    acceptance_delay = (accepted - received).days
    publication_delay = (publication_dt - accepted).days
    if acceptance_delay < 0 or publication_delay < 0:
        return None
    counts["nonnegative_delays"] += 1
    row.update(
        {
            "received": iso(received),
            "accepted": iso(accepted),
            "pubdate": iso(pubdate),
            "article_date": iso(publication_dt),
            "article_date_raw": iso(article_dt),
            "publication_date_source": source,
            "acceptance_delay": acceptance_delay,
            "publication_delay": publication_delay,
            "is_covid": contains_any_term(
                f"{row.get('title', '')} {row.get('keywords', '')}", COVID_SYNONYMS
            ),
        }
    )
    return row


def journal_metadata_eligible(row: Mapping[str, Any], min_received: date) -> bool:
    is_conference = _coerce_int(row.get("is_conference"))
    received = parse_date(row.get("received"))
    publication_dt = parse_date(row.get("article_date")) or parse_date(
        row.get("pubdate")
    )
    ceased_year = _coerce_int(row.get("ceased"))
    if is_conference != 0 or received is None or received < min_received:
        return False
    return ceased_year is None or (
        publication_dt is not None and ceased_year >= publication_dt.year
    )


def _coerce_int(value: Any) -> int | None:
    try:
        text = str(value).strip()
        if not text:
            return None
        return int(float(text))
    except (TypeError, ValueError):
        return None


def _iter_input_paths(input_path: Path | list[Path] | tuple[Path, ...]) -> list[Path]:
    if isinstance(input_path, (list, tuple)):
        return [Path(path) for path in input_path]
    input_path = Path(input_path)
    if input_path.is_dir():
        return sorted(
            list(input_path.rglob("*.jsonl")) + list(input_path.rglob("*.json"))
        )
    return [input_path]


def _read_json_frames(paths: list[Path]) -> pl.DataFrame:
    """Read parsed JSON/JSONL files while preserving late non-null field types."""
    frames: list[pl.DataFrame] = []
    for path in paths:
        if path.suffix == ".jsonl":
            frames.append(pl.read_ndjson(path, infer_schema_length=None))
        else:
            # JSON arrays are accepted for small fixtures or interoperability,
            # but JSONL is the canonical fast/resumable format.
            with Path(path).open("r", encoding="utf-8") as handle:
                data = json.load(handle)
            if isinstance(data, dict):
                data = [data]
            frames.append(pl.DataFrame(data))
    if not frames:
        return pl.DataFrame()
    return pl.concat(frames, how="diagonal_relaxed")


def _ensure_columns(df: pl.DataFrame, columns: list[str]) -> pl.DataFrame:
    exprs = [pl.lit(None).alias(col) for col in columns if col not in df.columns]
    return df.with_columns(exprs) if exprs else df


def _history_field_expr(df: pl.DataFrame, field: str) -> pl.Expr:
    dtype = df.schema.get("history")
    fields = getattr(dtype, "fields", []) if dtype is not None else []
    names = {getattr(f, "name", "") for f in fields}
    if field in names:
        return (
            pl.col("history")
            .struct.field(field)
            .cast(pl.Utf8, strict=False)
            .alias(field)
        )
    return pl.lit(None).cast(pl.Utf8).alias(field)


def _load_external(path: Path | None) -> pl.DataFrame:
    if path is None or not Path(path).exists():
        return pl.DataFrame()
    df = scan_tabular(Path(path)).collect()
    df = df.rename({name: normalize_header(name) for name in df.columns})
    if "issn_linking" in df.columns:
        df = df.with_columns(issn_expr(pl.col("issn_linking")).alias("issn_linking"))
        df = df.filter(pl.col("issn_linking") != "").unique(
            subset=["issn_linking"], keep="first", maintain_order=True
        )
    return df


def _left_join_external(df: pl.DataFrame, path: Path | None) -> pl.DataFrame:
    right = _load_external(path)
    if right.is_empty() or "issn_linking" not in right.columns:
        return df
    return df.join(right, on="issn_linking", how="left", coalesce=True)


def _left_join_peer_review(df: pl.DataFrame, path: Path | None) -> pl.DataFrame:
    right = _load_external(path)
    if right.is_empty():
        return df
    if "review_delay" in right.columns and "review_cycle_delay" not in right.columns:
        right = right.rename({"review_delay": "review_cycle_delay"})
    for key in ("doi", "pmid", "title"):
        if key in df.columns and key in right.columns:
            if key == "doi":
                right = right.with_columns(doi_expr(pl.col("doi")).alias("doi"))
            return df.join(
                right.unique(subset=[key], keep="first", maintain_order=True),
                on=key,
                how="left",
                coalesce=True,
            )
    return df


def _load_retractions(path: Path | None) -> pl.DataFrame:
    if path is None or not Path(path).exists():
        return pl.DataFrame(
            {
                "doi": [],
                "retraction_doi": [],
                "retraction_nature": [],
                "reason": [],
                "retraction_date": [],
                "original_date": [],
            }
        )
    df = scan_tabular(Path(path)).collect()
    df = df.rename({name: normalize_header(name) for name in df.columns})
    for col in [
        "doi",
        "retraction_doi",
        "retraction_nature",
        "reason",
        "retraction_date",
        "original_date",
    ]:
        if col not in df.columns:
            df = df.with_columns(pl.lit(None).cast(pl.Utf8).alias(col))
    return (
        df.with_columns(
            doi_expr(pl.coalesce([pl.col("doi"), pl.col("retraction_doi")])).alias(
                "doi"
            )
        )
        .filter(pl.col("doi") != "")
        .unique(subset=["doi"], keep="first", maintain_order=True)
        .select(
            "doi",
            "retraction_doi",
            "retraction_nature",
            "reason",
            "retraction_date",
            "original_date",
        )
    )


def _year_value_expr(prefix: str, year_expr: pl.Expr) -> pl.Expr:
    expr: pl.Expr | None = None
    for year in range(2015, 2025):
        col = f"{prefix}_{year}"
        if prefix == "npi_level":
            col = f"npi_level_{str(year)[-2:]}"
        branch = pl.when(year_expr == year).then(
            pl.col(col) if col in _CURRENT_COLUMNS else pl.lit(None)
        )
        expr = (
            branch
            if expr is None
            else expr.when(year_expr == year).then(
                pl.col(col) if col in _CURRENT_COLUMNS else pl.lit(None)
            )
        )
    # This function is rewritten below once columns are known.
    return pl.lit(None)


_CURRENT_COLUMNS: set[str] = set()


def year_lookup_expr(df: pl.DataFrame, prefix: str, year_column: str) -> pl.Expr:
    year_expr = (
        pl.when(pl.col(year_column) >= 2025)
        .then(pl.lit(2024))
        .otherwise(pl.col(year_column))
    )
    result = pl.lit(None).cast(pl.Utf8)
    for year in range(2015, 2025):
        if prefix == "npi_level":
            candidates = [f"npi_level_{str(year)[-2:]}", f"npi_level_{year}"]
        else:
            candidates = [f"{prefix}_{year}"]
        value = next(
            (
                pl.col(c).cast(pl.Utf8, strict=False)
                for c in candidates
                if c in df.columns
            ),
            pl.lit(None).cast(pl.Utf8),
        )
        result = pl.when(year_expr == year).then(value).otherwise(result)
    return result


def _write_filter_counts(path: Path, counts: Mapping[str, int]) -> None:
    stages = list(FILTER_STAGES)
    kept = [int(counts.get(stage, 0)) for stage in stages]
    previous = [kept[index - 1] if index else kept[index] for index in range(len(kept))]
    dropped = [max(before - after, 0) for before, after in zip(previous, kept, strict=True)]
    kept_percent = [round(after / before * 100, 4) if before else 100.0 for before, after in zip(previous, kept, strict=True)]
    df = pl.DataFrame(
        {
            "stage": stages,
            "count": kept,
            "dropped": dropped,
            "drop_reason": stages,
            "kept_percent": kept_percent,
        }
    )
    write_frame(path, df)


def transform_files(
    input_path: Path | list[Path] | tuple[Path, ...],
    output_path: Path,
    *,
    filters_path: Path | None = None,
    external: ExternalInputs | None = None,
    min_received: date = date(2013, 1, 1),
) -> TransformResult:
    """Filter, enrich, and write parsed PubMed records as one article shard."""
    external = external or ExternalInputs()
    counts: Counter[str] = Counter({stage: 0 for stage in FILTER_STAGES})
    paths = _iter_input_paths(input_path)
    df = _read_json_frames(paths)

    counts["raw_records"] = df.height
    if df.is_empty():
        out = pl.DataFrame({col: [] for col in CANONICAL_ARTICLE_COLUMNS})
        write_frame(Path(output_path), out)
        if filters_path:
            _write_filter_counts(filters_path, counts)
        return TransformResult(Path(output_path), filters_path, dict(counts))

    df = _ensure_columns(
        df,
        [
            *REQUIRED_PARSED_FIELDS,
            "delete",
            "title",
            "keywords",
            "doi",
            "article_date",
            "pubdate",
        ],
    )
    df = df.filter(~pl.col("delete").fill_null(False).cast(pl.Boolean, strict=False))
    counts["non_deleted_records"] = df.height

    df = df.filter(
        pl.all_horizontal([pl.col(c).is_not_null() for c in REQUIRED_PARSED_FIELDS])
    )
    counts["has_required_parsed_fields"] = df.height

    df = df.with_columns(
        _history_field_expr(df, "received"),
        _history_field_expr(df, "accepted"),
        publication_types_expr(pl.col("publication_types")).alias("publication_types"),
        pl.col("keywords")
        .cast(pl.Utf8, strict=False)
        .fill_null("")
        .str.replace_all(";", ",")
        .alias("keywords"),
        issn_expr(pl.col("issn_linking")).alias("issn_linking"),
        doi_expr(pl.col("doi")).alias("doi"),
    ).with_columns(
        date_expr(pl.col("received")).alias("received_date"),
        date_expr(pl.col("accepted")).alias("accepted_date"),
        date_expr(pl.col("pubdate")).alias("pubdate_date"),
        date_expr(pl.col("article_date")).alias("article_date_parsed"),
    )

    df = df.filter(
        pl.col("received_date").is_not_null() & pl.col("accepted_date").is_not_null()
    )
    counts["has_received_and_accepted_dates"] = df.height

    df = df.filter(pl.col("publication_types").str.contains("Journal Article"))
    counts["journal_articles"] = df.height

    df = df.filter(pl.col("issn_linking") != "")
    counts["has_linking_issn"] = df.height

    df = df.with_columns(
        pl.coalesce([pl.col("article_date_parsed"), pl.col("pubdate_date")]).alias(
            "publication_date"
        ),
        pl.when(pl.col("article_date_parsed").is_not_null())
        .then(pl.lit("article_date"))
        .when(pl.col("pubdate_date").is_not_null())
        .then(pl.lit("pubdate"))
        .otherwise(pl.lit(""))
        .alias("publication_date_source"),
    )
    df = df.filter(
        pl.col("publication_date").is_not_null()
        & (pl.col("received_date") < pl.col("publication_date"))
        & (pl.col("accepted_date") < pl.col("publication_date"))
        & (pl.col("accepted_date") > pl.col("received_date"))
    )
    counts["coherent_dates"] = df.height

    df = df.with_columns(
        (pl.col("accepted_date") - pl.col("received_date"))
        .dt.total_days()
        .alias("acceptance_delay"),
        (pl.col("publication_date") - pl.col("accepted_date"))
        .dt.total_days()
        .alias("publication_delay"),
    ).filter((pl.col("acceptance_delay") >= 0) & (pl.col("publication_delay") >= 0))
    counts["nonnegative_delays"] = df.height

    covid_regex = "(?i)" + "|".join(
        rf"\b{re.escape(term)}\b" for term in COVID_SYNONYMS
    )
    df = df.with_columns(
        pl.col("received_date").dt.strftime("%Y-%m-%d").alias("received"),
        pl.col("accepted_date").dt.strftime("%Y-%m-%d").alias("accepted"),
        pl.col("publication_date").dt.strftime("%Y-%m-%d").alias("article_date"),
        pl.col("article_date_parsed")
        .dt.strftime("%Y-%m-%d")
        .fill_null("")
        .alias("article_date_raw"),
        pl.col("pubdate_date").dt.strftime("%Y-%m-%d").fill_null("").alias("pubdate"),
        (
            pl.col("title").cast(pl.Utf8, strict=False).fill_null("")
            + pl.lit(" ")
            + pl.col("keywords").fill_null("")
        )
        .str.contains(covid_regex)
        .fill_null(False)
        .alias("is_covid_bool"),
    )

    for path in [
        external.scimago,
        external.web_of_science,
        external.doaj,
        external.norwegian_list,
        external.publisher,
    ]:
        df = _left_join_external(df, path)
    df = _left_join_peer_review(df, external.peer_review)
    counts["after_external_joins"] = df.height

    # If NPI metadata is absent, keep local smoke tests usable.  Real full runs
    # should provide NPI and are checked by `preflight`.
    for col, default in [
        ("is_conference", "0"),
        ("ceased", None),
        ("is_series", ""),
        ("established", ""),
    ]:
        if col not in df.columns:
            df = df.with_columns(pl.lit(default).alias(col))

    df = df.with_columns(
        pl.col("is_conference").cast(pl.Int64, strict=False).alias("is_conference_int"),
        pl.col("ceased").cast(pl.Int64, strict=False).alias("ceased_year"),
        pl.col("publication_date").dt.year().alias("article_year"),
    ).filter(
        (pl.col("is_conference_int") == 0)
        & (pl.col("received_date") >= pl.lit(min_received))
        & (
            pl.col("ceased_year").is_null()
            | (pl.col("ceased_year") >= pl.col("article_year"))
        )
    )
    counts["eligible_journal_metadata"] = df.height

    df = df.unique(subset=["title"], keep="first", maintain_order=True)
    counts["distinct_titles"] = df.height

    for col in [
        "asjc",
        "discipline",
        "asjc_all",
        "discipline_all",
        "scimago_categories",
        "publisher",
        "publisher_group",
        "publisher_conflict",
        "publisher_group_conflict",
        "npi_discipline",
        "npi_field",
        "apc",
        "apc_amount",
        "country",
        "country_of_publication",
        "open_access_status",
        "npi_open_access",
        "does_the_journal_comply_to_doaj_s_definition_of_open_access",
        *PEER_REVIEW_COLUMNS,
    ]:
        if col not in df.columns:
            df = df.with_columns(pl.lit(None).cast(pl.Utf8).alias(col))

    df = df.with_columns(
        date_expr(pl.col("first_review_date")).alias("_first_review_date"),
        date_expr(pl.col("last_review_date")).alias("_last_review_date"),
        date_expr(pl.col("date_first_accepted")).alias("_date_first_accepted"),
    ).with_columns(
        day_delta_expr(pl.col("_date_first_accepted"), pl.col("received_date"))
        .fill_null(pl.col("review_finding_delay").cast(pl.Int64, strict=False))
        .alias("review_finding_delay"),
        day_delta_expr(pl.col("_first_review_date"), pl.col("_date_first_accepted"))
        .fill_null(pl.col("first_decision_delay").cast(pl.Int64, strict=False))
        .alias("first_decision_delay"),
        day_delta_expr(pl.col("accepted_date"), pl.col("_last_review_date"))
        .fill_null(pl.col("final_decision_delay").cast(pl.Int64, strict=False))
        .alias("final_decision_delay"),
        day_delta_expr(pl.col("_first_review_date"), pl.col("received_date"))
        .fill_null(pl.col("first_review_delay").cast(pl.Int64, strict=False))
        .alias("first_review_delay"),
        day_delta_expr(pl.col("accepted_date"), pl.col("_first_review_date"))
        .fill_null(pl.col("peer_review_delay").cast(pl.Int64, strict=False))
        .alias("peer_review_delay"),
        pl.col("_first_review_date")
        .dt.strftime("%Y-%m-%d")
        .fill_null(pl.col("first_review_date"))
        .alias("first_review_date"),
        pl.col("_last_review_date")
        .dt.strftime("%Y-%m-%d")
        .fill_null(pl.col("last_review_date"))
        .alias("last_review_date"),
        pl.col("_date_first_accepted")
        .dt.strftime("%Y-%m-%d")
        .fill_null(pl.col("date_first_accepted"))
        .alias("date_first_accepted"),
    )

    df = df.with_columns(
        year_lookup_expr(df, "quartile", "article_year").alias("quartile_year"),
        year_lookup_expr(df, "rank", "article_year").alias("rank_year"),
        year_lookup_expr(df, "h_index", "article_year").alias("h_index_year"),
        year_lookup_expr(df, "npi_level", "article_year").alias("npi_year"),
        pl.col("issn_linking").is_in(list(MEGAJOURNAL_ISSNS)).alias("is_mega_bool"),
        (
            (
                pl.col("does_the_journal_comply_to_doaj_s_definition_of_open_access")
                == "Yes"
            )
            | (pl.col("open_access_status") == "Unpaywall Open Acess")
            | (pl.col("npi_open_access") == "DOAJ")
        )
        .fill_null(False)
        .alias("open_access_bool"),
    )

    retractions = _load_retractions(external.retraction_watch)
    if not retractions.is_empty():
        df = df.join(
            retractions, on="doi", how="left", suffix="_retraction", coalesce=True
        )
    for col in ["retraction_nature", "reason", "retraction_date", "original_date"]:
        if col not in df.columns:
            df = df.with_columns(pl.lit(None).cast(pl.Utf8).alias(col))

    df = df.with_columns(
        (
            (pl.col("reason").is_not_null() & (pl.col("reason") != ""))
            | (
                pl.col("retraction_nature").is_not_null()
                & (pl.col("retraction_nature") != "")
            )
        ).alias("is_retracted_bool"),
        date_expr(pl.col("original_date")).alias("original_date_parsed"),
    ).with_columns(
        pl.when(pl.col("original_date_parsed").is_not_null())
        .then(pl.col("original_date_parsed").dt.strftime("%Y-%m-%d"))
        .otherwise(pl.col("article_date"))
        .alias("article_date"),
        bool_text_expr(pl.col("is_covid_bool")).alias("is_covid"),
        bool_text_expr(pl.col("is_mega_bool")).alias("is_mega"),
        bool_text_expr(pl.col("open_access_bool")).alias("open_access"),
        bool_text_expr(pl.col("is_retracted_bool")).alias("is_retracted"),
        pl.coalesce([pl.col("country"), pl.col("country_of_publication")]).alias(
            "country"
        ),
    )

    for col in CANONICAL_ARTICLE_COLUMNS:
        if col not in df.columns:
            df = df.with_columns(pl.lit("").alias(col))

    out = df.select(
        [
            pl.col(col).cast(pl.Utf8, strict=False).fill_null("").alias(col)
            for col in CANONICAL_ARTICLE_COLUMNS
        ]
    )
    counts["final_rows"] = out.height
    write_frame(Path(output_path), out, format=None)

    if filters_path is not None:
        _write_filter_counts(Path(filters_path), counts)

    return TransformResult(
        output_path=Path(output_path), filters_path=filters_path, counts=dict(counts)
    )
