"""Aggregate transformation diagnostics and describe final data quality."""

from __future__ import annotations

from dataclasses import dataclass
from pathlib import Path

import polars as pl

from pubdelays.external.common import write_frame
from pubdelays.schema import PEER_REVIEW_COLUMNS

DATE_VARIABLES = {
    "received",
    "article_date",
    "article_date_raw",
    "retraction_date",
    "retraction_original_date",
    "first_review_date",
    "last_review_date",
    "date_first_accepted",
}
NUMERIC_VARIABLES = {
    "acceptance_delay",
    "publication_delay",
    "h_index_year",
    "rank_year",
    "npi_year",
    "established",
    "apc_eur_proxy",
    "apc_eur_proxy_min",
    "apc_eur_proxy_max",
    "apc_quote_count",
    "n_review_round",
    "n_reviews",
    "n_reviewers",
    *(name for name in PEER_REVIEW_COLUMNS if name.endswith("_delay")),
}


@dataclass(frozen=True)
class QualityReportResult:
    output_dir: Path
    tables: dict[str, Path]
    rows: int


def _read_table(path: Path) -> pl.DataFrame:
    if path.suffix == ".parquet":
        return pl.read_parquet(path)
    return pl.read_csv(path, separator="\t" if path.suffix == ".tsv" else ",", infer_schema=False)


def _missing_expr(df: pl.DataFrame, column: str) -> pl.Expr:
    expression = pl.col(column).is_null()
    if df.schema[column] == pl.Utf8:
        expression = expression | (pl.col(column).str.strip_chars() == "")
    return expression.fill_null(True)


def _year_frame(df: pl.DataFrame) -> pl.DataFrame:
    if "article_date" not in df.columns:
        return df.with_columns(pl.lit("__missing__").alias("__year"))
    return df.with_columns(
        pl.col("article_date")
        .cast(pl.Utf8, strict=False)
        .str.slice(0, 4)
        .replace("", "__missing__")
        .fill_null("__missing__")
        .alias("__year")
    )


def _invalid_count(frame: pl.DataFrame, variable: str) -> int:
    present = frame.filter(~_missing_expr(frame, variable))
    if variable in DATE_VARIABLES:
        parsed = pl.col(variable).cast(pl.Utf8, strict=False).str.strptime(
            pl.Date, "%Y-%m-%d", strict=False
        )
    elif variable in NUMERIC_VARIABLES:
        parsed = pl.col(variable).cast(pl.Float64, strict=False)
    else:
        return 0
    return int(present.select(parsed.is_null().sum()).item() or 0)


def _text(value: object) -> str:
    return "" if value is None else str(value)


def _groups(df: pl.DataFrame) -> list[tuple[str, pl.DataFrame]]:
    frame = _year_frame(df)
    years = frame["__year"].unique().sort().to_list()
    return [("__all__", frame), *[(str(year), frame.filter(pl.col("__year") == year)) for year in years]]


def variable_quality(df: pl.DataFrame) -> pl.DataFrame:
    rows: list[dict[str, object]] = []
    for year, frame in _groups(df):
        for variable in df.columns:
            missing = int(frame.select(_missing_expr(frame, variable).sum()).item() or 0)
            present = frame.height - missing
            distinct = int(
                frame.filter(~_missing_expr(frame, variable)).select(pl.col(variable).n_unique()).item()
                or 0
            )
            rows.append(
                {
                    "year": year,
                    "variable": variable,
                    "total": frame.height,
                    "present": present,
                    "missing": missing,
                    "missing_percent": round(missing / frame.height * 100, 4) if frame.height else 0.0,
                    "invalid": _invalid_count(frame, variable),
                    "distinct_nonmissing": distinct,
                }
            )
    return pl.DataFrame(rows)


def variable_distributions(df: pl.DataFrame) -> pl.DataFrame:
    rows: list[dict[str, object]] = []
    date_variables = DATE_VARIABLES & set(df.columns)
    for year, frame in _groups(df):
        for variable in df.columns:
            present = frame.filter(~_missing_expr(frame, variable))
            text = pl.col(variable).cast(pl.Utf8, strict=False)
            numeric = present.select(text.cast(pl.Float64, strict=False).alias("value"))["value"].drop_nulls()
            nonmissing = present.height
            if nonmissing and numeric.len() / nonmissing >= 0.9:
                stats = numeric.to_frame().select(
                    pl.col("value").mean().alias("mean"),
                    pl.col("value").std().alias("std"),
                    pl.col("value").min().alias("min"),
                    pl.col("value").quantile(0.01).alias("p01"),
                    pl.col("value").quantile(0.05).alias("p05"),
                    pl.col("value").quantile(0.25).alias("p25"),
                    pl.col("value").median().alias("median"),
                    pl.col("value").quantile(0.75).alias("p75"),
                    pl.col("value").quantile(0.95).alias("p95"),
                    pl.col("value").quantile(0.99).alias("p99"),
                    pl.col("value").max().alias("max"),
                ).row(0, named=True)
                for metric, value in stats.items():
                    rows.append({"year": year, "variable": variable, "kind": "numeric", "metric": metric, "stratum": "", "value": _text(value), "count": numeric.len()})
                continue
            if variable in date_variables:
                parsed = present.select(text.str.strptime(pl.Date, "%Y-%m-%d", strict=False).alias("value"))["value"]
                for metric, value in (("valid", parsed.drop_nulls().len()), ("invalid", nonmissing - parsed.drop_nulls().len()), ("min", parsed.min()), ("max", parsed.max())):
                    rows.append({"year": year, "variable": variable, "kind": "date", "metric": metric, "stratum": "", "value": _text(value), "count": nonmissing})
                continue
            distinct = present.select(pl.col(variable).n_unique()).item() if nonmissing else 0
            if distinct <= 20:
                frequencies = present.group_by(variable).len().sort("len", descending=True)
                for item in frequencies.iter_rows(named=True):
                    count = int(item["len"])
                    rows.append({"year": year, "variable": variable, "kind": "categorical", "metric": "frequency", "stratum": str(item[variable]), "value": str(round(count / nonmissing * 100, 4) if nonmissing else 0.0), "count": count})
            else:
                lengths = present.select(text.str.len_chars().alias("length"))["length"]
                for metric, value in (("distinct", distinct), ("length_min", lengths.min()), ("length_median", lengths.median()), ("length_max", lengths.max())):
                    rows.append({"year": year, "variable": variable, "kind": "text", "metric": metric, "stratum": "", "value": _text(value), "count": nonmissing})
    return pl.DataFrame(rows)


def pairwise_missingness(df: pl.DataFrame) -> pl.DataFrame:
    rows: list[dict[str, object]] = []
    for year, frame in _groups(df):
        for other in df.columns:
            other_missing = _missing_expr(frame, other)
            text = pl.col(other).cast(pl.Utf8, strict=False)
            present = frame.filter(~other_missing)
            distinct = int(present.select(pl.col(other).n_unique()).item() or 0)
            is_date = other.endswith("date") or other in {
                "received",
                "article_date",
                "article_date_raw",
            }
            numeric = present.select(
                text.cast(pl.Float64, strict=False).alias("value")
            )["value"].drop_nulls()
            if is_date:
                stratum = (
                    pl.when(other_missing)
                    .then(pl.lit("__missing__"))
                    .otherwise(text.str.slice(0, 4))
                )
            elif present.height and numeric.len() / present.height >= 0.9:
                values = [numeric.quantile(q) for q in (0.1, 0.25, 0.5, 0.75, 0.9)]
                stratum = pl.when(other_missing).then(pl.lit("__missing__"))
                labels = ("le_p10", "p10_p25", "p25_p50", "p50_p75", "p75_p90")
                for threshold, label in zip(values, labels, strict=True):
                    if threshold is not None:
                        stratum = stratum.when(
                            text.cast(pl.Float64, strict=False) <= threshold
                        ).then(pl.lit(label))
                stratum = stratum.otherwise(pl.lit("gt_p90"))
            elif distinct <= 20:
                stratum = (
                    pl.when(other_missing)
                    .then(pl.lit("__missing__"))
                    .otherwise(text)
                )
            else:
                stratum = (
                    pl.when(other_missing)
                    .then(pl.lit("__missing__"))
                    .otherwise(pl.lit("__present__"))
                )
            targets = [target for target in df.columns if target != other]
            grouped = (
                frame.with_columns(stratum.alias("__stratum"))
                .group_by("__stratum", maintain_order=True)
                .agg(
                    pl.len().alias("total"),
                    *[
                        _missing_expr(frame, target).sum().alias(target)
                        for target in targets
                    ],
                )
            )
            for item in grouped.iter_rows(named=True):
                total = int(item["total"])
                for target in targets:
                    count = int(item[target] or 0)
                    rows.append(
                        {
                            "year": year,
                            "target_variable": target,
                            "stratum_variable": other,
                            "stratum": str(item["__stratum"]),
                            "total": total,
                            "target_missing": count,
                            "target_present": total - count,
                            "target_missing_percent": round(count / total * 100, 4) if total else 0.0,
                        }
                    )
    return pl.DataFrame(rows)


def _collect_sidecars(input_dir: Path) -> pl.DataFrame:
    paths = sorted(Path(input_dir).glob("*.quality.parquet"))
    if not paths:
        return pl.DataFrame()
    return pl.concat([pl.read_parquet(path).with_columns(pl.lit(path.name).alias("shard")) for path in paths], how="diagonal_relaxed")


def _write_table(output_dir: Path, name: str, frame: pl.DataFrame, *, csv: bool = False) -> Path:
    path = output_dir / f"{name}.{'csv' if csv else 'parquet'}"
    write_frame(path, frame)
    return path


def build_quality_report(input_dir: Path, dataset: Path, output_dir: Path) -> QualityReportResult:
    """Write mergeable diagnostics and final all-variable quality tables."""
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    df = _read_table(Path(dataset))
    sidecars = _collect_sidecars(Path(input_dir))
    tables: dict[str, Path] = {}
    if not sidecars.is_empty():
        tables["stage_and_join_quality"] = _write_table(output_dir, "stage_and_join_quality", sidecars)
        debrief = (
            sidecars.group_by("record_type", "checkpoint", "source", "year", "variable", "metric", maintain_order=True)
            .agg(
                pl.when(
                    pl.col("metric").first().is_in(
                        ["source_rows", "source_unique_keys", "source_duplicate_key_rows"]
                    )
                )
                .then(pl.col("numerator").cast(pl.Int64, strict=False).max())
                .otherwise(pl.col("numerator").cast(pl.Int64, strict=False).sum())
                .alias("numerator"),
                pl.when(
                    pl.col("metric").first().is_in(
                        ["source_rows", "source_unique_keys", "source_duplicate_key_rows"]
                    )
                )
                .then(pl.col("denominator").cast(pl.Int64, strict=False).max())
                .otherwise(pl.col("denominator").cast(pl.Int64, strict=False).sum())
                .alias("denominator"),
            )
            .with_columns(
                pl.when(pl.col("denominator") > 0)
                .then((pl.col("numerator") / pl.col("denominator") * 100).round(4))
                .otherwise(0.0)
                .alias("percent")
            )
        )
        tables["stage_and_join_debrief"] = _write_table(output_dir, "stage_and_join_debrief", debrief, csv=True)
    quality = variable_quality(df)
    distributions = variable_distributions(df)
    pairwise = pairwise_missingness(df)
    tables["variable_quality"] = _write_table(output_dir, "variable_quality", quality)
    tables["variable_quality_debrief"] = _write_table(output_dir, "variable_quality_debrief", quality, csv=True)
    tables["variable_distributions"] = _write_table(output_dir, "variable_distributions", distributions)
    tables["pairwise_missingness"] = _write_table(output_dir, "pairwise_missingness", pairwise)
    return QualityReportResult(output_dir=output_dir, tables=tables, rows=df.height)
