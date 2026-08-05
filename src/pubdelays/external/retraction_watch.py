"""Retraction Watch preprocessing, implemented with Polars."""

from __future__ import annotations

from datetime import date, datetime
from pathlib import Path

import polars as pl

from .common import doi_expr, normalize_columns, read_csv_polars, write_frame

RETRACTION_FIELDS = [
    "title",
    "doi",
    "retraction_doi",
    "retraction_nature",
    "reason",
    "retraction_date",
    "original_date",
]


def parse_retraction_datetime(value: str) -> date | None:
    text = str(value or "").strip()
    if not text:
        return None
    for fmt in ("%m/%d/%Y %H:%M", "%m/%d/%Y"):
        try:
            return datetime.strptime(text, fmt).date()
        except ValueError:
            pass
    return None


def preprocess_retraction_watch(input_csv: Path, output: Path) -> int:
    df = normalize_columns(read_csv_polars(Path(input_csv)))
    needed = [
        "retractiondate",
        "originalpaperdate",
        "title",
        "originalpaperdoi",
        "retractiondoi",
        "retractionnature",
        "reason",
    ]
    # Some CSV exports are already normalized with underscores.
    alt = {
        "retraction_date": "retractiondate",
        "original_paper_date": "originalpaperdate",
        "original_paper_doi": "originalpaperdoi",
        "retraction_doi": "retractiondoi",
        "retraction_nature": "retractionnature",
    }
    df = df.rename(
        {k: v for k, v in alt.items() if k in df.columns and v not in df.columns}
    )
    for col in needed:
        if col not in df.columns:
            df = df.with_columns(pl.lit(None).cast(pl.Utf8).alias(col))

    df = (
        df.with_columns(
            pl.col("retractiondate")
            .cast(pl.Utf8)
            .str.strptime(pl.Date, "%m/%d/%Y %H:%M", strict=False)
            .fill_null(
                pl.col("retractiondate")
                .cast(pl.Utf8)
                .str.strptime(pl.Date, "%m/%d/%Y", strict=False)
            )
            .alias("retraction_date"),
            pl.col("originalpaperdate")
            .cast(pl.Utf8)
            .str.strptime(pl.Date, "%m/%d/%Y %H:%M", strict=False)
            .fill_null(
                pl.col("originalpaperdate")
                .cast(pl.Utf8)
                .str.strptime(pl.Date, "%m/%d/%Y", strict=False)
            )
            .alias("original_date"),
            doi_expr(pl.col("originalpaperdoi")).alias("doi"),
            doi_expr(pl.col("retractiondoi")).alias("retraction_doi"),
            pl.col("retractionnature").alias("retraction_nature"),
        )
        .select(RETRACTION_FIELDS)
    )
    return write_frame(output, df)
