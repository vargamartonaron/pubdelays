"""Scopus Source List preprocessing, under legacy ``wos`` identifiers."""

from __future__ import annotations

from pathlib import Path
from typing import Any

import polars as pl

from .common import (
    issn_expr,
    normalize_columns,
    read_csv_polars,
    write_frame,
)

WOS_FIELDS = [
    "source_title",
    "issn_linking",
    "open_access_status",
    "source_type",
    "asjc",
    "discipline",
    "asjc_all",
    "discipline_all",
]


def discipline_for_asjc(value: Any) -> str:
    try:
        asjc = int(float(str(value).strip()))
    except (TypeError, ValueError):
        return "NA"
    if asjc == 1000:
        return "multidisciplinary"
    if asjc <= 1111:
        return "life_sciences"
    if asjc <= 1213:
        return "social_sciences_and_humanities"
    if asjc <= 1315:
        return "life_sciences"
    if asjc <= 1410:
        return "social_sciences_and_humanities"
    if asjc <= 1712:
        return "physical_sciences"
    if asjc <= 1804:
        return "social_sciences_and_humanities"
    if asjc <= 1913:
        return "physical_sciences"
    if asjc <= 2003:
        return "social_sciences_and_humanities"
    if asjc <= 2312:
        return "physical_sciences"
    if asjc <= 2406:
        return "life_sciences"
    if asjc <= 2614:
        return "physical_sciences"
    if asjc <= 2748:
        return "health_sciences"
    if asjc <= 2809:
        return "life_sciences"
    if asjc <= 2923:
        return "health_sciences"
    if asjc <= 3005:
        return "life_sciences"
    if asjc <= 3109:
        return "physical_sciences"
    if asjc <= 3322:
        return "social_sciences_and_humanities"
    if asjc <= 3616:
        return "health_sciences"
    return "NA"


def discipline_expr(expr: pl.Expr) -> pl.Expr:
    asjc = expr.cast(pl.Int64, strict=False)
    return (
        pl.when(asjc == 1000)
        .then(pl.lit("multidisciplinary"))
        .when(asjc <= 1111)
        .then(pl.lit("life_sciences"))
        .when(asjc <= 1213)
        .then(pl.lit("social_sciences_and_humanities"))
        .when(asjc <= 1315)
        .then(pl.lit("life_sciences"))
        .when(asjc <= 1410)
        .then(pl.lit("social_sciences_and_humanities"))
        .when(asjc <= 1712)
        .then(pl.lit("physical_sciences"))
        .when(asjc <= 1804)
        .then(pl.lit("social_sciences_and_humanities"))
        .when(asjc <= 1913)
        .then(pl.lit("physical_sciences"))
        .when(asjc <= 2003)
        .then(pl.lit("social_sciences_and_humanities"))
        .when(asjc <= 2312)
        .then(pl.lit("physical_sciences"))
        .when(asjc <= 2406)
        .then(pl.lit("life_sciences"))
        .when(asjc <= 2614)
        .then(pl.lit("physical_sciences"))
        .when(asjc <= 2748)
        .then(pl.lit("health_sciences"))
        .when(asjc <= 2809)
        .then(pl.lit("life_sciences"))
        .when(asjc <= 2923)
        .then(pl.lit("health_sciences"))
        .when(asjc <= 3005)
        .then(pl.lit("life_sciences"))
        .when(asjc <= 3109)
        .then(pl.lit("physical_sciences"))
        .when(asjc <= 3322)
        .then(pl.lit("social_sciences_and_humanities"))
        .when(asjc <= 3616)
        .then(pl.lit("health_sciences"))
        .otherwise(pl.lit("NA"))
    )


def preprocess_wos(input_csv: Path, output: Path) -> int:
    df = normalize_columns(read_csv_polars(Path(input_csv)))
    if "issn" in df.columns and "print_issn" not in df.columns:
        df = df.rename({"issn": "print_issn"})
    if "eissn" in df.columns and "e_issn" not in df.columns:
        df = df.rename({"eissn": "e_issn"})
    needed = [
        "source_title",
        "print_issn",
        "e_issn",
        "source_type",
        "all_science_journal_classification_codes_asjc",
        "open_access_status",
    ]
    for col in needed:
        if col not in df.columns:
            df = df.with_columns(pl.lit(None).cast(pl.Utf8).alias(col))

    exploded = (
        df.filter(pl.col("source_type") == "Journal")
        .select(needed)
        .with_columns(
            pl.col("all_science_journal_classification_codes_asjc")
            .cast(pl.Utf8)
            .str.split(";")
            .alias("asjc_parts"),
            pl.concat_list(
                [pl.col("print_issn").cast(pl.Utf8), pl.col("e_issn").cast(pl.Utf8)]
            ).alias("issn_parts"),
        )
        .explode("asjc_parts")
        .explode("issn_parts")
        .with_columns(
            pl.col("asjc_parts").cast(pl.Utf8).str.strip_chars().alias("asjc"),
            issn_expr(pl.col("issn_parts")).alias("issn_linking"),
        )
        .filter((pl.col("asjc") != "") & (pl.col("issn_linking") != ""))
        .with_columns(discipline_expr(pl.col("asjc")).alias("discipline"))
    )
    df = exploded.group_by("issn_linking", maintain_order=True).agg(
        pl.col("source_title").first(),
        pl.col("open_access_status").first(),
        pl.col("source_type").first(),
        pl.col("asjc").first().alias("asjc"),
        pl.col("discipline").first().alias("discipline"),
        pl.col("asjc").unique(maintain_order=True).str.join("|").alias("asjc_all"),
        pl.col("discipline")
        .unique(maintain_order=True)
        .str.join("|")
        .alias("discipline_all"),
    )
    return write_frame(output, df.select(WOS_FIELDS))
