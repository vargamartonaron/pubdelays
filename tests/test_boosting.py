from __future__ import annotations

from pathlib import Path

import polars as pl
import pytest
from pubdelays_analysis.boosting import (
    CATEGORICAL_FEATURES,
    FEATURES,
    BoostingConfig,
    fit_outcome,
    prepare_model_frame,
    temporal_partitions,
)


def write_model_input(path: Path) -> None:
    rows = []
    for year, delay in ((2015, 10), (2016, 20), (2023, 30), (2024, 40), (2025, 50), (2026, 60)):
        rows.append(
            {
                "title": f"article-{year}",
                "article_date": f"{year}-06-15",
                "acceptance_delay": delay,
                "publication_delay": delay + 1,
                "is_mega": "False",
                "open_access": "True",
                "is_covid": "False",
                "journal": "Journal A" if year < 2025 else "Journal B",
                "discipline": "Medicine",
                "apc": "False",
                "country": "HU",
                "publication_date_source": "electronic",
                "quartile_year": "Q1",
                "asjc": "2700",
                "is_series": "False",
                "is_retracted": "False",
                "h_index_year": "100",
                "rank_year": "1",
                "established": "1990",
                "n_review_round": "",
                "n_reviewers": "2",
                "npi_year": "2",
            }
        )
    pl.DataFrame(rows).write_csv(path)


def test_prepare_model_frame_derives_features_and_temporal_partitions(tmp_path: Path) -> None:
    path = tmp_path / "processed.csv"
    write_model_input(path)

    frame = prepare_model_frame(path, "acceptance_delay")
    parts = temporal_partitions(frame)

    assert frame["article_year"].to_list() == [2016, 2023, 2024, 2025]
    assert frame.columns == ["title", "article_date", "target", *FEATURES]
    assert frame.filter(pl.col("article_year") == 2016)["n_review_round"].item() is None
    assert all(frame[column].dtype == pl.String for column in CATEGORICAL_FEATURES)
    assert {name: value.height for name, value in parts.items()} == {
        "train": 2,
        "validation": 1,
        "test": 1,
    }


def test_prepare_model_frame_rejects_invalid_requests(tmp_path: Path) -> None:
    path = tmp_path / "processed.csv"
    write_model_input(path)

    with pytest.raises(ValueError, match="unsupported outcome"):
        prepare_model_frame(path, "other")
    with pytest.raises(ValueError, match="sample_fraction"):
        prepare_model_frame(path, "publication_delay", sample_fraction=0)


def test_fit_outcome_writes_complete_small_artifact_set(tmp_path: Path) -> None:
    pytest.importorskip("catboost")
    path = tmp_path / "processed.csv"
    base = tmp_path / "base.csv"
    write_model_input(base)
    templates = pl.read_csv(base, infer_schema=False).filter(
        pl.col("article_date").str.slice(0, 4).cast(pl.Int64).is_between(2016, 2025)
    )
    rows = []
    for replicate in range(4):
        rows.append(
            templates.with_columns(
                (pl.col("title") + f"-{replicate}").alias("title"),
                (pl.col("acceptance_delay").cast(pl.Int64) + replicate).alias(
                    "acceptance_delay"
                ),
                pl.lit(f"Journal {replicate}").alias("journal"),
            )
        )
    pl.concat(rows).write_csv(path)

    outputs = fit_outcome(
        path,
        tmp_path / "model",
        "acceptance_delay",
        config=BoostingConfig(iterations=5, early_stopping_rounds=2, shap_max_rows=4),
    )

    assert set(outputs) == {
        "metrics",
        "predictions",
        "importance",
        "shap_values",
        "shap_importance",
        "model",
        "manifest",
        "missingness",
    }
    assert all(output.is_file() and output.stat().st_size > 0 for output in outputs.values())
    metrics = pl.read_csv(outputs["metrics"])
    assert {"catboost", "global_historical_median", "journal_historical_median"} <= set(
        metrics["model"]
    )
