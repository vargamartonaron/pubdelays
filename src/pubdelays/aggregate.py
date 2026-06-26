"""Aggregate transformed article shards with Polars."""

from __future__ import annotations

from pathlib import Path

import polars as pl

from pubdelays.external.common import write_frame
from pubdelays.schema import CANONICAL_ARTICLE_COLUMNS
from pubdelays.shards import iter_article_paths

FILTER_COUNT_COLUMNS = ("stage", "count", "dropped", "drop_reason", "kept_percent")


def _scan_article(path: Path) -> pl.LazyFrame:
    path = Path(path)
    if path.suffix == ".parquet":
        return pl.scan_parquet(path)
    if path.suffix == ".tsv":
        return pl.scan_csv(path, separator="\t", infer_schema_length=10000)
    return pl.scan_csv(path, infer_schema_length=10000)


def collect_articles(input_path: Path) -> pl.DataFrame:
    """Collect article shards and apply the final title-level deduplication."""

    paths = iter_article_paths(Path(input_path))
    if not paths:
        return pl.DataFrame({col: [] for col in CANONICAL_ARTICLE_COLUMNS})

    lf = pl.concat([_scan_article(path) for path in paths], how="diagonal_relaxed")
    df = lf.collect()
    for col in CANONICAL_ARTICLE_COLUMNS:
        if col not in df.columns:
            df = df.with_columns(pl.lit("").alias(col))
    return df.select(
        [
            pl.col(col).cast(pl.Utf8, strict=False).fill_null("").alias(col)
            for col in CANONICAL_ARTICLE_COLUMNS
        ]
    ).unique(subset=["title"], keep="first", maintain_order=True)


def aggregate_articles(input_path: Path, output_path: Path) -> int:
    """Aggregate article shards and write one output.

    The aggregate keeps the first row per title, then writes Parquet/CSV/TSV
    based on the output suffix.
    """

    df = collect_articles(Path(input_path))
    return write_frame(Path(output_path), df)


def aggregate_outputs(input_path: Path, output_paths: list[Path]) -> int:
    """Aggregate once and write several output formats without rereading shards."""

    df = collect_articles(Path(input_path))
    for output_path in output_paths:
        write_frame(Path(output_path), df)
    return df.height


def collect_filter_counts(input_path: Path, output_path: Path) -> int:
    """Aggregate per-shard filter sidecars into one audit table."""
    paths = sorted(Path(input_path).glob("*.filters.csv"))
    if not paths:
        out = pl.DataFrame({column: [] for column in FILTER_COUNT_COLUMNS})
        return write_frame(Path(output_path), out)

    frames = [pl.read_csv(path, infer_schema_length=10000).with_columns(pl.lit(path.name).alias("source")) for path in paths]
    df = pl.concat(frames, how="diagonal_relaxed")
    for column in ("stage", "count"):
        if column not in df.columns:
            df = df.with_columns(pl.lit(0 if column == "count" else "").alias(column))
    totals = (
        df.with_columns(pl.col("count").cast(pl.Int64, strict=False).fill_null(0))
        .group_by("stage", maintain_order=True)
        .agg(pl.col("count").sum().alias("count"))
        .with_columns(
            (pl.col("count").shift(1) - pl.col("count")).fill_null(0).clip(0).alias("dropped"),
            pl.when(pl.col("count").shift(1) > 0)
            .then((pl.col("count") / pl.col("count").shift(1) * 100).round(4))
            .otherwise(pl.lit(100.0))
            .alias("kept_percent"),
        )
        .with_columns(pl.col("stage").alias("drop_reason"))
        .select(FILTER_COUNT_COLUMNS)
    )
    return write_frame(Path(output_path), totals)


# Alias retained for tests and direct callers that still use the previous name.
def aggregate_tsvs(input_path: Path, output_csv: Path) -> int:
    """Wrapper for TSV-to-CSV aggregation callers."""
    return aggregate_articles(input_path, output_csv)
