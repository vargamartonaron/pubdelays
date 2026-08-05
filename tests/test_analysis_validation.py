from __future__ import annotations

from pathlib import Path

import polars as pl

from pubdelays.aggregate import collect_filter_counts
from pubdelays.schema import CANONICAL_ARTICLE_COLUMNS
from pubdelays.validation import validate_analysis_output


def canonical_row(**updates: str) -> dict[str, str]:
    row = {column: "" for column in CANONICAL_ARTICLE_COLUMNS}
    row.update(
        {
            "title": "A",
            "journal": "Example Journal",
            "issn_linking": "12345678",
            "received": "2020-01-01",
            "article_date": "2020-02-01",
            "publication_types": "Journal Article",
            "acceptance_delay": "19",
            "publication_delay": "12",
            "is_covid": "False",
            "open_access": "False",
            "is_retracted": "False",
            "discipline": "health_sciences",
            "npi_discipline": "Medicine",
            "quartile_year": "Q1",
        }
    )
    row.update(updates)
    return row


def test_validate_analysis_output_writes_validation_tables(tmp_path: Path) -> None:
    processed = tmp_path / "processed.parquet"
    pl.DataFrame(
        [
            canonical_row(title="A"),
            canonical_row(
                title="B",
                acceptance_delay="0",
                publication_delay="1200",
                article_date="2026-01-01",
                is_covid="True",
            ),
        ]
    ).write_parquet(processed)

    result = validate_analysis_output(
        processed,
        tmp_path / "validation",
        filtered_output=tmp_path / "validated.parquet",
        excluded_output=tmp_path / "excluded.parquet",
    )

    assert result.rows_in == 2
    assert result.rows_kept == 1
    assert result.filtered_output == tmp_path / "validated.parquet"
    assert result.excluded_output == tmp_path / "excluded.parquet"
    assert set(result.tables) >= {
        "validation_checks",
        "outlier_delays",
        "articles_per_year",
        "journal_articles_n",
        "excluded_by_reason",
        "excluded_by_quartile",
        "missingness_by_year",
        "missingness_pairwise",
    }
    checks = pl.read_csv(result.tables["validation_checks"], infer_schema=False)
    assert "range:publication_delay" in checks["check"].to_list()
    outliers = pl.read_csv(result.tables["outlier_delays"], infer_schema=False)
    assert outliers.filter(pl.col("metric") == "publication_delay_above_max")["count"].item() == "1"
    assert pl.read_parquet(tmp_path / "validated.parquet").height == 1
    assert pl.read_parquet(tmp_path / "excluded.parquet").height == 1
    excluded_by_quartile = pl.read_csv(result.tables["excluded_by_quartile"], infer_schema=False)
    assert excluded_by_quartile.filter(pl.col("quartile_year") == "Q1")["articles"].item() == "1"
    missingness = pl.read_csv(result.tables["missingness_by_year"], infer_schema=False)
    assert "missing_quartile_year" in missingness.columns


def test_collect_filter_counts_aggregates_sidecars_with_drop_metadata(tmp_path: Path) -> None:
    shard_dir = tmp_path / "shards"
    shard_dir.mkdir()
    pl.DataFrame(
        {
            "stage": ["raw_records", "journal_articles", "final_rows"],
            "count": [10, 8, 5],
            "dropped": [0, 2, 3],
            "drop_reason": ["raw_records", "journal_articles", "final_rows"],
            "kept_percent": [100.0, 80.0, 62.5],
        }
    ).write_csv(shard_dir / "a.filters.csv")
    pl.DataFrame(
        {
            "stage": ["raw_records", "journal_articles", "final_rows"],
            "count": [4, 3, 3],
            "dropped": [0, 1, 0],
            "drop_reason": ["raw_records", "journal_articles", "final_rows"],
            "kept_percent": [100.0, 75.0, 100.0],
        }
    ).write_csv(shard_dir / "b.filters.csv")

    output = tmp_path / "filter_counts.csv"
    assert collect_filter_counts(shard_dir, output) == 3

    data = pl.read_csv(output, infer_schema=False)
    assert data.filter(pl.col("stage") == "final_rows")["count"].item() == "8"
    assert "dropped" in data.columns


def test_open_access_without_apc_does_not_require_an_amount(tmp_path: Path) -> None:
    processed = tmp_path / "processed.parquet"
    pl.DataFrame(
        [
            canonical_row(open_access="True", apc="No", apc_amount=""),
            canonical_row(title="B", open_access="True", apc="Yes", apc_amount="1200"),
        ]
    ).write_parquet(processed)

    result = validate_analysis_output(processed, tmp_path / "validation")
    checks = pl.read_csv(result.tables["validation_checks"], infer_schema=False)
    apc_check = checks.filter(pl.col("check") == "missing:open_access_apc")

    assert apc_check["status"].item() == "pass"
    assert apc_check["failed"].item() == "0"
