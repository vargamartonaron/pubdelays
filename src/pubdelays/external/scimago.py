"""Scimago Journal Rank preprocessing, implemented with Polars."""

from __future__ import annotations

from io import StringIO
from pathlib import Path

import polars as pl

from .common import (
    first_by_key,
    issn_expr,
    normalize_columns,
    read_csv_polars,
    write_frame,
)

SCIMAGO_FIELDS = [
    "issn_linking",
    "quartile_2025",
    "h_index_2025",
    "rank_2025",
    "sjr_2025",
    "quartile_2024",
    "h_index_2024",
    "journal_title",
    "scimago_categories",
    "rank_2024",
    "sjr_2024",
    "quartile_2015",
    "h_index_2015",
    "rank_2015",
    "sjr_2015",
    "quartile_2016",
    "h_index_2016",
    "rank_2016",
    "sjr_2016",
    "quartile_2017",
    "h_index_2017",
    "rank_2017",
    "sjr_2017",
    "quartile_2018",
    "h_index_2018",
    "rank_2018",
    "sjr_2018",
    "quartile_2019",
    "h_index_2019",
    "rank_2019",
    "sjr_2019",
    "quartile_2020",
    "h_index_2020",
    "rank_2020",
    "sjr_2020",
    "quartile_2021",
    "h_index_2021",
    "rank_2021",
    "sjr_2021",
    "quartile_2022",
    "h_index_2022",
    "rank_2022",
    "sjr_2022",
    "quartile_2023",
    "h_index_2023",
    "rank_2023",
    "sjr_2023",
]


def _escape_unescaped_scimago_quotes(text: str, *, separator: str = ";") -> str:
    """Repair SCImago rows that contain bare quotes inside quoted fields."""

    repaired: list[str] = []
    in_quotes = False
    at_field_start = True
    i = 0
    while i < len(text):
        char = text[i]
        if in_quotes and char == '"':
            if i + 1 < len(text) and text[i + 1] == '"':
                repaired.append('""')
                i += 2
                at_field_start = False
                continue
            next_index = i + 1
            while next_index < len(text) and text[next_index] in " \t":
                next_index += 1
            if next_index == len(text) or text[next_index] in f"{separator}\r\n":
                in_quotes = False
                repaired.append(char)
            else:
                repaired.append('""')
            at_field_start = False
            i += 1
            continue

        if char == '"' and at_field_start:
            in_quotes = True
            at_field_start = False
        elif not in_quotes and char == separator:
            at_field_start = True
        elif not in_quotes and char in "\r\n":
            at_field_start = True
        elif char not in " \t":
            at_field_start = False
        repaired.append(char)
        i += 1
    return "".join(repaired)


def _read_scimago_csv(path: Path) -> pl.DataFrame:
    try:
        return read_csv_polars(path, separator=";")
    except pl.exceptions.ComputeError:
        repaired = _escape_unescaped_scimago_quotes(
            Path(path).read_text(encoding="utf-8", errors="replace")
        )
        return pl.read_csv(
            StringIO(repaired),
            separator=";",
            infer_schema=False,
            ignore_errors=False,
            try_parse_dates=False,
        )


def _read_scimago_file(path: Path, year: int) -> pl.DataFrame:
    df = normalize_columns(_read_scimago_csv(path))
    required = ["title", "issn", "sjr_best_quartile", "h_index", "rank", "sjr", "categories"]
    for col in required:
        if col not in df.columns:
            df = df.with_columns(pl.lit(None).cast(pl.Utf8).alias(col))

    df = (
        df.select(required)
        .with_columns(
            pl.col("issn").cast(pl.Utf8).str.split(",").alias("issn_parts"),
            pl.col("categories").cast(pl.Utf8).str.split(";").alias("category_parts"),
        )
        .explode("issn_parts")
        .explode("category_parts")
        .with_columns(
            issn_expr(pl.col("issn_parts")).alias("issn_linking"),
            pl.col("category_parts")
            .cast(pl.Utf8)
            .str.strip_chars()
            .alias("scimago_category"),
        )
        .filter(pl.col("issn_linking") != "")
    )

    if year >= 2024:
        return df.group_by("issn_linking", maintain_order=True).agg(
            pl.col("sjr_best_quartile").first().alias(f"quartile_{year}"),
            pl.col("h_index").first().alias(f"h_index_{year}"),
            pl.col("title").first().alias("journal_title"),
            pl.col("scimago_category")
            .filter(pl.col("scimago_category") != "")
            .unique(maintain_order=True)
            .str.join("|")
            .alias("scimago_categories"),
            pl.col("rank").first().alias(f"rank_{year}"),
            pl.col("sjr").first().alias(f"sjr_{year}"),
        )
    return df.group_by("issn_linking", maintain_order=True).agg(
        pl.col("sjr_best_quartile").first().alias(f"quartile_{year}"),
        pl.col("h_index").first().alias(f"h_index_{year}"),
        pl.col("rank").first().alias(f"rank_{year}"),
        pl.col("sjr").first().alias(f"sjr_{year}"),
    )


def preprocess_scimago(
    input_dir: Path, output: Path, *, start_year: int = 2015, end_year: int = 2025
) -> int:
    input_dir = Path(input_dir)
    scimago = _read_scimago_file(input_dir / f"scimagojr {end_year}.csv", end_year)
    scimago = first_by_key(
        scimago.filter(
            pl.col(f"quartile_{end_year}").is_not_null()
            & (pl.col(f"quartile_{end_year}") != "")
        ),
        "issn_linking",
    )

    for year in range(start_year, end_year):
        year_df = first_by_key(
            _read_scimago_file(input_dir / f"scimagojr {year}.csv", year),
            "issn_linking",
        )
        scimago = scimago.join(year_df, on="issn_linking", how="left")

    for col in SCIMAGO_FIELDS:
        if col not in scimago.columns:
            scimago = scimago.with_columns(pl.lit(None).cast(pl.Utf8).alias(col))
    scimago = scimago.select(SCIMAGO_FIELDS).with_columns(
        [
            pl.col(col)
            .cast(pl.Utf8, strict=False)
            .fill_null("")
            .replace({"-": "", "NA": "", "_": ""})
            for col in SCIMAGO_FIELDS
        ]
    )
    return write_frame(output, scimago)
