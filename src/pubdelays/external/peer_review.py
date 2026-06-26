"""Private peer-review metadata preprocessing, implemented with Polars."""

from __future__ import annotations

from pathlib import Path

import polars as pl

from .common import doi_expr, normalize_columns, read_csv_polars, write_frame

PEER_REVIEW_FIELDS = [
    "doi",
    "n_review_round",
    "n_reviews",
    "first_review_date",
    "last_review_date",
    "n_reviewers",
    "date_first_accepted",
    "review_cycle_delay",
]


# The proprietary February 2026 export contains early historical rows and a
# partial 2026 tail; the pasted R workflow works with the 2013-2025 study window.
MIN_REVIEW_DATE = pl.date(2013, 1, 1)
MAX_REVIEW_DATE = pl.date(2025, 12, 31)


def preprocess_peer_review(input_csv: Path, output: Path) -> int:
    """Clean raw proprietary peer-review events into a DOI-keyed lookup table."""

    df = normalize_columns(read_csv_polars(Path(input_csv)))
    for col in ["date_accepted", "doi", "date_reviewed", "review_round"]:
        if col not in df.columns:
            df = df.with_columns(pl.lit(None).cast(pl.Utf8).alias(col))

    df = (
        df.with_columns(
            doi_expr(pl.col("doi")).alias("doi"),
            pl.col("date_reviewed")
            .cast(pl.Utf8)
            .str.strptime(pl.Date, "%Y-%m-%d", strict=False)
            .alias("date_reviewed"),
            pl.col("date_accepted")
            .cast(pl.Utf8)
            .str.strptime(pl.Date, "%Y-%m-%d", strict=False)
            .alias("date_accepted"),
            pl.col("review_round").cast(pl.Int64, strict=False).alias("review_round"),
        )
        .filter(
            (pl.col("doi") != "")
            & pl.col("date_reviewed").is_not_null()
            & (pl.col("date_reviewed") >= MIN_REVIEW_DATE)
            & (pl.col("date_reviewed") <= MAX_REVIEW_DATE)
        )
        .group_by("doi", maintain_order=True)
        .agg(
            pl.col("review_round").max().alias("n_review_round"),
            pl.len().alias("n_reviews"),
            pl.col("date_reviewed").min().alias("first_review_date"),
            pl.col("date_reviewed").max().alias("last_review_date"),
            pl.len().alias("n_reviewers"),
            pl.col("date_accepted").min().alias("date_first_accepted"),
        )
        .with_columns(
            (pl.col("last_review_date") - pl.col("first_review_date"))
            .dt.total_days()
            .alias("review_cycle_delay")
        )
        .select(PEER_REVIEW_FIELDS)
    )
    return write_frame(output, df)
