"""Validation helpers for processed PubMed publication-delay outputs."""

from __future__ import annotations

import hashlib
from dataclasses import dataclass
from datetime import date
from pathlib import Path

import polars as pl

from pubdelays.external.common import write_frame

KEY_COLUMNS = ("doi", "pmid", "title")
HASH_COLUMNS = (
    "doi",
    "title",
    "journal",
    "issn_linking",
    "received",
    "accepted",
    "article_date",
    "acceptance_delay",
    "publication_delay",
)
VALIDATION_TABLES: tuple[str, ...] = (
    "validation_checks",
    "outlier_delays",
    "journal_n",
    "journal_articles_n",
    "journal_articles_10",
    "articles_per_year",
    "articles_per_month",
    "covid_articles",
    "wos_discipline",
    "npi_discipline",
    "agreement_between_disciplines",
)


@dataclass(frozen=True)
class DifferentialValidationResult:
    """Summary of a baseline-vs-candidate dataset comparison report."""

    report_path: Path
    rows: int
    categories: dict[str, int]


@dataclass(frozen=True)
class AnalysisValidationResult:
    """Summary of final analysis dataset validation outputs."""

    output_dir: Path
    tables: dict[str, Path]
    filtered_output: Path | None
    excluded_output: Path | None
    rows_in: int
    rows_kept: int
    failed_checks: int


def _read_table(path: Path) -> pl.DataFrame:
    path = Path(path)
    if path.suffix == ".parquet":
        return pl.read_parquet(path)
    if path.suffix == ".tsv":
        return pl.read_csv(path, separator="\t", infer_schema=False)
    return pl.read_csv(path, infer_schema=False)


def _ensure_text(df: pl.DataFrame) -> pl.DataFrame:
    return df.with_columns(
        [pl.col(col).cast(pl.Utf8, strict=False).fill_null("").alias(col) for col in df.columns]
    )


def _ensure_columns(df: pl.DataFrame, columns: tuple[str, ...]) -> pl.DataFrame:
    for column in columns:
        if column not in df.columns:
            df = df.with_columns(pl.lit("").alias(column))
    return df


def _row_hash(row: dict[str, object]) -> str:
    payload = "\x1f".join(str(row.get(col, "") or "") for col in HASH_COLUMNS)
    return hashlib.sha256(payload.encode("utf-8")).hexdigest()


def _key(row: dict[str, object]) -> str:
    for col in KEY_COLUMNS:
        value = str(row.get(col, "") or "").strip().lower()
        if value:
            return f"{col}:{value}"
    return "title:"


def _pubdate_correction(row: dict[str, object]) -> bool:
    return (
        str(row.get("publication_date_source", "")) == "pubdate"
        and str(row.get("publication_delay", "")) not in {"", "None"}
    )


def _ceased_correction(row: dict[str, object]) -> bool:
    return str(row.get("ceased_before_publication", "")).lower() in {"1", "true", "yes"}


def compare_outputs(baseline_path: Path, candidate_path: Path, output_path: Path) -> DifferentialValidationResult:
    """Write a row-level comparison report between two processed outputs."""
    baseline = _ensure_text(_read_table(baseline_path))
    candidate = _ensure_text(_read_table(candidate_path))
    baseline_rows = {_key(row): row for row in baseline.to_dicts()}
    candidate_rows = {_key(row): row for row in candidate.to_dicts()}
    records: list[dict[str, str]] = []

    categories = {
        "expected_correction": 0,
        "format_or_type_difference": 0,
        "potential_migration_bug": 0,
    }

    if baseline.columns != candidate.columns:
        records.append(
            {
                "category": "format_or_type_difference",
                "key": "columns",
                "detail": f"baseline={baseline.columns}; candidate={candidate.columns}",
            }
        )
        categories["format_or_type_difference"] += 1

    for key in sorted(set(baseline_rows) | set(candidate_rows)):
        old = baseline_rows.get(key)
        new_row = candidate_rows.get(key)
        if old is None and new_row is not None:
            category = "expected_correction" if _pubdate_correction(new_row) else "potential_migration_bug"
            detail = "candidate-only row"
        elif new_row is None and old is not None:
            category = "expected_correction" if _ceased_correction(old) else "potential_migration_bug"
            detail = "baseline-only row"
        elif old is not None and new_row is not None and _row_hash(old) != _row_hash(new_row):
            category = "format_or_type_difference"
            detail = "matched row hash differs"
        else:
            continue
        categories[category] += 1
        records.append({"category": category, "key": key, "detail": detail})

    report = pl.DataFrame(records or [{"category": "ok", "key": "", "detail": "no differences"}])
    write_frame(output_path, report)
    return DifferentialValidationResult(Path(output_path), len(records), categories)


def _analysis_frame(path: Path) -> pl.DataFrame:
    columns = (
        "title",
        "journal",
        "issn_linking",
        "received",
        "article_date",
        "publication_types",
        "acceptance_delay",
        "publication_delay",
        "is_covid",
        "open_access",
        "apc",
        "apc_amount",
        "is_retracted",
        "reason",
        "retraction_nature",
        "discipline",
        "npi_discipline",
        "quartile_year",
        "rank_year",
        "h_index_year",
        "asjc",
        "npi_year",
        "established",
        "first_review_date",
    )
    df = _ensure_columns(_ensure_text(_read_table(path)), columns)
    return df.with_columns(
        pl.col("received").str.strptime(pl.Date, "%Y-%m-%d", strict=False).alias("received_date"),
        pl.col("article_date").str.strptime(pl.Date, "%Y-%m-%d", strict=False).alias("article_date_parsed"),
        pl.col("article_date").str.slice(0, 4).alias("article_year"),
        pl.col("article_date").str.slice(0, 7).alias("article_month"),
        pl.col("acceptance_delay").cast(pl.Int64, strict=False).alias("acceptance_delay_days"),
        pl.col("publication_delay").cast(pl.Int64, strict=False).alias("publication_delay_days"),
        pl.col("established").cast(pl.Int64, strict=False).alias("established_year"),
        pl.col("rank_year").cast(pl.Int64, strict=False).alias("rank_year_num"),
        pl.col("h_index_year").cast(pl.Int64, strict=False).alias("h_index_year_num"),
        pl.col("asjc").cast(pl.Int64, strict=False).alias("asjc_num"),
        pl.col("npi_year").cast(pl.Int64, strict=False).alias("npi_year_num"),
    )


def _check_record(name: str, passed: bool, checked: int, failed: int, detail: str = "") -> dict[str, str]:
    return {
        "check": name,
        "status": "pass" if passed else "fail",
        "checked": str(checked),
        "failed": str(failed),
        "detail": detail,
    }


def _validation_checks(
    df: pl.DataFrame,
    *,
    min_article_date: date,
    max_article_date: date,
    min_delay_days: int,
    max_delay_days: int,
) -> pl.DataFrame:
    checks: list[dict[str, str]] = []
    n = df.height
    required = ("title", "journal", "issn_linking", "received", "article_date", "acceptance_delay", "publication_delay")
    for column in required:
        failed = df.filter((pl.col(column) == "") | pl.col(column).is_null()).height
        checks.append(_check_record(f"missing:{column}", failed == 0, n, failed))

    conditional = df.filter(pl.col("open_access") == "True")
    if conditional.height:
        failed = conditional.filter(
            (pl.col("apc") == "")
            | ((pl.col("apc") == "Yes") & (pl.col("apc_amount") == ""))
        ).height
        checks.append(_check_record("missing:open_access_apc", failed == 0, conditional.height, failed))

    retracted = df.filter(pl.col("is_retracted") == "True")
    if retracted.height:
        failed = retracted.filter((pl.col("reason") == "") & (pl.col("retraction_nature") == "")).height
        checks.append(_check_record("missing:retraction_reason", failed == 0, retracted.height, failed))

    range_checks = {
        "range:article_date": pl.col("article_date_parsed").is_between(min_article_date, max_article_date),
        "range:received_date": pl.col("received_date") <= pl.col("article_date_parsed"),
        "range:acceptance_delay": pl.col("acceptance_delay_days").is_between(min_delay_days, max_delay_days),
        "range:publication_delay": pl.col("publication_delay_days").is_between(min_delay_days, max_delay_days),
        "range:established": pl.col("established_year").is_null() | pl.col("established_year").is_between(1500, 2026),
        "range:h_index": pl.col("h_index_year_num").is_null() | (pl.col("h_index_year_num") >= 0),
        "range:rank": pl.col("rank_year_num").is_null() | (pl.col("rank_year_num") >= 0),
        "range:asjc": pl.col("asjc_num").is_null() | pl.col("asjc_num").is_between(1000, 3699),
        "range:npi_year": pl.col("npi_year_num").is_null() | pl.col("npi_year_num").is_between(0, 2),
    }
    for name, expr in range_checks.items():
        failed = df.filter(~expr.fill_null(False)).height
        checks.append(_check_record(name, failed == 0, n, failed))

    journal_article_failed = df.filter(~pl.col("publication_types").str.contains("Journal Article").fill_null(False)).height
    checks.append(_check_record("content:journal_article", journal_article_failed == 0, n, journal_article_failed))
    title_duplicates = n - df.select("title").unique().height
    checks.append(_check_record("unique:title", title_duplicates == 0, n, title_duplicates))
    return pl.DataFrame(checks)


def _count_by(df: pl.DataFrame, keys: list[str]) -> pl.DataFrame:
    return df.group_by(keys, maintain_order=True).agg(pl.len().alias("articles")).sort(keys)


def _missingness_by(df: pl.DataFrame, key: str) -> pl.DataFrame:
    fields = ["received", "article_date", "quartile_year", "discipline", "npi_year", "open_access", "first_review_date"]
    exprs = [pl.len().alias("total")]
    exprs.extend(
        ((pl.col(field) == "") | pl.col(field).is_null()).sum().alias(f"missing_{field}")
        for field in fields
    )
    return df.group_by(key, maintain_order=True).agg(exprs).sort(key)


def _pairwise_missingness(df: pl.DataFrame) -> pl.DataFrame:
    fields = ["received", "article_date", "quartile_year", "discipline", "npi_year", "open_access", "first_review_date"]
    rows = []
    total = max(df.height, 1)
    for left in fields:
        left_missing = (pl.col(left) == "") | pl.col(left).is_null()
        for right in fields:
            if left == right:
                continue
            right_missing = (pl.col(right) == "") | pl.col(right).is_null()
            rows.append(
                {
                    "v1": left,
                    "v2": right,
                    "both_missing_percent": str(round(df.filter(left_missing & right_missing).height / total * 100, 2)),
                }
            )
    return pl.DataFrame(rows)


def validate_analysis_output(
    input_path: Path,
    output_dir: Path,
    *,
    filtered_output: Path | None = None,
    excluded_output: Path | None = None,
    min_article_date: date = date(2016, 1, 1),
    max_article_date: date = date(2025, 6, 1),
    min_delay_days: int = 1,
    max_delay_days: int = 1095,
) -> AnalysisValidationResult:
    """Write analysis validation tables for a final processed dataset."""
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    df = _analysis_frame(input_path)
    in_window = (
        pl.col("article_date_parsed").is_between(min_article_date, max_article_date)
        & pl.col("acceptance_delay_days").is_between(min_delay_days, max_delay_days)
        & pl.col("publication_delay_days").is_between(min_delay_days, max_delay_days)
    )
    keep_mask = in_window.fill_null(False)
    filtered = df.filter(keep_mask)
    excluded = df.filter(~keep_mask)

    tables: dict[str, pl.DataFrame] = {
        "validation_checks": _validation_checks(
            df,
            min_article_date=min_article_date,
            max_article_date=max_article_date,
            min_delay_days=min_delay_days,
            max_delay_days=max_delay_days,
        ),
        "outlier_delays": pl.DataFrame(
            [
                {
                    "metric": "acceptance_delay_below_min",
                    "count": df.filter(pl.col("acceptance_delay_days") < min_delay_days).height,
                },
                {
                    "metric": "acceptance_delay_above_max",
                    "count": df.filter(pl.col("acceptance_delay_days") > max_delay_days).height,
                },
                {
                    "metric": "publication_delay_below_min",
                    "count": df.filter(pl.col("publication_delay_days") < min_delay_days).height,
                },
                {
                    "metric": "publication_delay_above_max",
                    "count": df.filter(pl.col("publication_delay_days") > max_delay_days).height,
                },
            ]
        ),
        "journal_n": pl.DataFrame([{"journals": df.select("journal").unique().height}]),
        "journal_articles_n": _count_by(df, ["journal", "issn_linking"]),
        "journal_articles_10": pl.DataFrame(
            [
                {
                    "journals": df.select("journal").unique().height,
                    "journals_with_less_than_10_articles": _count_by(df, ["journal"]).filter(pl.col("articles") < 10).height,
                }
            ]
        ),
        "articles_per_year": _count_by(df, ["article_year"]),
        "articles_per_month": _count_by(df, ["article_month"]),
        "covid_articles": _count_by(df.filter(pl.col("article_year") >= "2020"), ["is_covid"]),
        "wos_discipline": _count_by(df, ["discipline"]),
        "npi_discipline": _count_by(df, ["npi_discipline"]),
        "agreement_between_disciplines": _count_by(df, ["discipline", "npi_discipline"]),
        "excluded_by_reason": pl.DataFrame(
            [
                {
                    "reason": "article_date_outside_window",
                    "count": excluded.filter(~pl.col("article_date_parsed").is_between(min_article_date, max_article_date).fill_null(False)).height,
                },
                {
                    "reason": "acceptance_delay_outside_window",
                    "count": excluded.filter(~pl.col("acceptance_delay_days").is_between(min_delay_days, max_delay_days).fill_null(False)).height,
                },
                {
                    "reason": "publication_delay_outside_window",
                    "count": excluded.filter(~pl.col("publication_delay_days").is_between(min_delay_days, max_delay_days).fill_null(False)).height,
                },
            ]
        ),
        "excluded_by_quartile": _count_by(excluded, ["quartile_year"]),
        "excluded_by_discipline": _count_by(excluded, ["discipline"]),
        "excluded_by_npi": _count_by(excluded, ["npi_year"]),
        "excluded_by_open_access": _count_by(excluded, ["open_access"]),
        "missingness_by_year": _missingness_by(df, "article_year"),
        "missingness_by_quartile": _missingness_by(df, "quartile_year"),
        "missingness_by_discipline": _missingness_by(df, "discipline"),
        "missingness_by_npi": _missingness_by(df, "npi_year"),
        "missingness_by_open_access": _missingness_by(df, "open_access"),
        "missingness_pairwise": _pairwise_missingness(df),
    }

    outputs: dict[str, Path] = {}
    for name, table in tables.items():
        path = output_dir / f"{name}.csv"
        write_frame(path, table.with_columns(pl.all().cast(pl.Utf8, strict=False).fill_null("")))
        outputs[name] = path

    input_columns = _read_table(input_path).columns
    filtered_path = Path(filtered_output) if filtered_output else None
    if filtered_path is not None:
        write_frame(filtered_path, filtered.select(input_columns), format=None)
    excluded_path = Path(excluded_output) if excluded_output else None
    if excluded_path is not None:
        write_frame(excluded_path, excluded.select(input_columns), format=None)

    failed_checks = tables["validation_checks"].filter(pl.col("status") == "fail").height
    return AnalysisValidationResult(
        output_dir, outputs, filtered_path, excluded_path, df.height, filtered.height, failed_checks
    )
