"""Derived, pipeline-generic summary tables."""

from __future__ import annotations

from pathlib import Path

import polars as pl

from pubdelays.external.common import write_frame

SUMMARY_TABLES: tuple[str, ...] = (
    "journal_year",
    "field_year",
    "publisher_year",
    "delay_distribution",
)


def _scan_processed(path: Path) -> pl.LazyFrame:
    path = Path(path)
    if path.suffix == ".parquet":
        return pl.scan_parquet(path)
    if path.suffix == ".tsv":
        return pl.scan_csv(path, separator="\t", infer_schema=False)
    return pl.scan_csv(path, infer_schema=False)


def _base_frame(path: Path) -> pl.DataFrame:
    return (
        _scan_processed(path)
        .select(
            "journal",
            "issn_linking",
            "discipline",
            "publisher",
            "publisher_group",
            "article_date",
            "acceptance_delay",
            "publication_delay",
        )
        .with_columns(
            pl.col("article_date").str.slice(0, 4).alias("article_year"),
            pl.col("acceptance_delay").cast(pl.Int64, strict=False).alias("acceptance_delay_days"),
            pl.col("publication_delay").cast(pl.Int64, strict=False).alias("publication_delay_days"),
        )
        .filter(pl.col("article_year").str.contains(r"^\d{4}$"))
        .collect()
    )


def _text(df: pl.DataFrame) -> pl.DataFrame:
    return df.with_columns(pl.all().cast(pl.Utf8, strict=False).fill_null(""))


def _summary(df: pl.DataFrame, keys: list[str]) -> pl.DataFrame:
    return _text(
        df.group_by(keys, maintain_order=True)
        .agg(
            pl.len().alias("articles"),
            pl.col("acceptance_delay_days").mean().round(2).alias("acceptance_delay_mean_days"),
            pl.col("publication_delay_days").mean().round(2).alias("publication_delay_mean_days"),
        )
        .sort(keys)
    )


def _delay_distribution(df: pl.DataFrame) -> pl.DataFrame:
    return _text(
        df.group_by("article_year", maintain_order=True)
        .agg(
            pl.len().alias("articles"),
            pl.col("acceptance_delay_days").median().alias("acceptance_delay_median_days"),
            pl.col("publication_delay_days").median().alias("publication_delay_median_days"),
            pl.col("acceptance_delay_days").quantile(0.25).alias("acceptance_delay_p25_days"),
            pl.col("acceptance_delay_days").quantile(0.75).alias("acceptance_delay_p75_days"),
            pl.col("publication_delay_days").quantile(0.25).alias("publication_delay_p25_days"),
            pl.col("publication_delay_days").quantile(0.75).alias("publication_delay_p75_days"),
        )
        .sort("article_year")
    )


def derive_summary_tables(processed_path: Path, output_dir: Path) -> dict[str, Path]:
    """Create lightweight CSV summary tables from the final processed dataset."""
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    df = _base_frame(processed_path)

    tables = {
        "journal_year": _summary(df, ["journal", "issn_linking", "article_year"]),
        "field_year": _summary(df, ["discipline", "article_year"]),
        "publisher_year": _summary(
            df.filter((pl.col("publisher") != "") | (pl.col("publisher_group") != "")),
            ["publisher_group", "publisher", "article_year"],
        ),
        "delay_distribution": _delay_distribution(df),
    }

    outputs: dict[str, Path] = {}
    for name, table in tables.items():
        path = output_dir / f"{name}.csv"
        write_frame(path, table)
        outputs[name] = path
    return outputs
