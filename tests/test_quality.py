from __future__ import annotations

from pathlib import Path

import polars as pl

from pubdelays.quality import build_quality_report
from pubdelays.schema import CANONICAL_ARTICLE_COLUMNS


def canonical_row(**updates: str) -> dict[str, str]:
    row = {column: "" for column in CANONICAL_ARTICLE_COLUMNS}
    row.update(
        {
            "title": "Article",
            "journal": "Journal",
            "issn_linking": "12345678",
            "received": "2020-01-01",
            "article_date": "2020-03-01",
            "acceptance_delay": "31",
            "publication_delay": "29",
            "publication_types": "Journal Article",
        }
    )
    row.update(updates)
    return row


def test_quality_report_covers_every_variable_and_year(tmp_path: Path) -> None:
    shard_dir = tmp_path / "shards"
    shard_dir.mkdir()
    pl.DataFrame(
        {
            "record_type": ["join", "missingness"],
            "checkpoint": ["external_join", "received:before"],
            "source": ["doaj", ""],
            "year": ["2020", "2020"],
            "variable": ["", "received"],
            "metric": ["matched", "missing"],
            "numerator": [1, 1],
            "denominator": [2, 2],
            "value": ["", ""],
        }
    ).write_parquet(shard_dir / "articles-shard-00000-of-00001.quality.parquet")
    dataset = tmp_path / "processed.parquet"
    pl.DataFrame(
        [
            canonical_row(acceptance_delay="0"),
            canonical_row(
                title="Article 2",
                article_date="2021-03-01",
                article_date_raw="not-a-date",
                quartile_year="Q1",
            ),
        ]
    ).write_parquet(dataset)

    result = build_quality_report(shard_dir, dataset, tmp_path / "quality")

    assert result.rows == 2
    quality = pl.read_parquet(result.tables["variable_quality"])
    overall = quality.filter(pl.col("year") == "__all__")
    assert set(overall["variable"]) == set(CANONICAL_ARTICLE_COLUMNS)
    assert set(quality["year"]) >= {"__all__", "2020", "2021"}
    assert quality.filter(
        (pl.col("year") == "__all__") & (pl.col("variable") == "article_date_raw")
    )["invalid"].item() == 1
    joins = pl.read_csv(result.tables["stage_and_join_debrief"], infer_schema=False)
    assert joins.filter(pl.col("source") == "doaj")["percent"].item() == "50.0"
    pairwise = pl.read_parquet(result.tables["pairwise_missingness"])
    assert pairwise.filter(
        (pl.col("target_variable") == "quartile_year")
        & (pl.col("stratum_variable") == "article_date")
    ).height >= 3
    distributions = pl.read_parquet(result.tables["variable_distributions"])
    assert distributions.filter(
        (pl.col("year") == "2020")
        & (pl.col("variable") == "acceptance_delay")
        & (pl.col("metric") == "min")
    )["value"].item() == "0.0"
