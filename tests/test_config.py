from __future__ import annotations

import copy
from pathlib import Path
from typing import Any

import pytest

from pubdelays.cli import main
from pubdelays.config import ConfigError, load_config

VALID_CONFIG: dict[str, Any] = {
    "pipeline": {
        "manifest": "data/manifests/pipeline.sqlite",
        "parse_inputs": "data/manifests/parse_inputs.txt",
        "transform_inputs": "data/manifests/transform_inputs.txt",
    },
    "pubmed": {
        "xml_dir": "data/raw_data/pubmed/xmls",
        "jsonl_dir": "data/temp_data/pubmed/jsonl",
    },
    "external": {
        "raw": {
            "scimago_dir": "data/raw_data/scimago",
            "web_of_science_csv": "data/raw_data/web_of_science/wos.csv",
            "doaj_csv": "data/raw_data/directory_of_open_access_journals/doaj.csv",
            "norwegian_list_csv": "data/raw_data/norwegian_publication_indicator/npi.csv",
            "retraction_watch_csv": "data/raw_data/retraction_watch/retraction_watch.csv",
            "publisher_csv": "data/raw_data/publisher_metadata/publishers.csv",
            "peer_review_csv": "data/raw_data/peer_review/peer_review.csv",
        },
        "processed": {
            "scimago": "data/processed_data/scimago.csv",
            "web_of_science": "data/processed_data/web_of_science.csv",
            "doaj": "data/processed_data/doaj.csv",
            "norwegian_list": "data/processed_data/norwegian_list.csv",
            "retraction_watch": "data/processed_data/retraction_watch.csv",
            "publisher": "data/processed_data/publisher_metadata.csv",
            "peer_review": "data/processed_data/peer_review.csv",
            "pubmed_journals": "data/external/pubmed-journals.csv",
        },
    },
    "transform": {
        "article_shard_dir": "data/temp_data/article_parquet",
        "article_shard_format": "parquet",
        "min_received": "2013-01-01",
        "default_shards": 64,
    },
    "aggregate": {
        "processed_parquet": "data/processed_data/processed.parquet",
        "processed_csv": "data/processed_data/processed.csv",
        "summary_dir": "data/processed_data/summaries",
        "filter_counts": "data/processed_data/filter_counts.csv",
    },
    "analysis": {
        "cwd": ".",
        "input": "data/processed_data/processed.parquet",
        "output_dir": "data/processed_data/analysis",
        "command": [
            "python",
            "pubdelays_analysis/outputs.py",
            "--input",
            "data/processed_data/processed.parquet",
            "--table-dir",
            "data/processed_data/analysis_tables",
            "--figure-data-dir",
            "data/processed_data/analysis_figures",
        ],
    },
    "validation": {
        "report_dir": "data/processed_data/validation_tables",
        "filtered_output": "data/processed_data/processed_validated.parquet",
        "min_article_date": "2016-01-01",
        "max_article_date": "2025-06-01",
        "min_delay_days": 1,
        "max_delay_days": 1095,
    },
}


def write_config(path: Path, values: dict[str, Any]) -> Path:
    def emit_table(prefix: str, mapping: dict[str, Any], lines: list[str]) -> None:
        scalars = {key: value for key, value in mapping.items() if not isinstance(value, dict)}
        if scalars:
            lines.append(f"[{prefix}]")
            for key, value in scalars.items():
                if isinstance(value, str):
                    lines.append(f'{key} = "{value}"')
                else:
                    lines.append(f"{key} = {value}")
            lines.append("")
        for key, value in mapping.items():
            if isinstance(value, dict):
                emit_table(f"{prefix}.{key}", value, lines)

    lines: list[str] = []
    for section, value in values.items():
        emit_table(section, value, lines)
    path.write_text("\n".join(lines), encoding="utf-8")
    return path


def config_copy() -> dict[str, Any]:
    return copy.deepcopy(VALID_CONFIG)


def test_load_config_accepts_custom_valid_config(tmp_path: Path) -> None:
    config_path = write_config(tmp_path / "valid.toml", config_copy())

    config = load_config(config_path)

    assert config.path("pipeline.manifest") == tmp_path / "data/manifests/pipeline.sqlite"


@pytest.mark.parametrize(
    ("mutate", "key", "message"),
    [
        (lambda values: values.pop("pubmed"), "pubmed", "missing required section"),
        (
            lambda values: values["transform"].__setitem__("default_shards", "64"),
            "transform.default_shards",
            "positive integer",
        ),
        (
            lambda values: values["transform"].__setitem__("article_shard_format", "json"),
            "transform.article_shard_format",
            "expected one of",
        ),
        (
            lambda values: values["transform"].__setitem__("min_received", "20130101"),
            "transform.min_received",
            "YYYY-MM-DD",
        ),
        (
            lambda values: values["validation"].__setitem__("min_delay_days", -1),
            "validation.min_delay_days",
            "non-negative integer",
        ),
    ],
)
def test_load_config_rejects_invalid_config(
    tmp_path: Path,
    mutate: Any,
    key: str,
    message: str,
) -> None:
    values = config_copy()
    mutate(values)
    config_path = write_config(tmp_path / "invalid.toml", values)

    with pytest.raises(ConfigError) as excinfo:
        load_config(config_path)

    error = str(excinfo.value)
    assert str(config_path) in error
    assert key in error
    assert message in error


def test_cli_config_error_reports_config_path_and_key(tmp_path: Path, capsys: pytest.CaptureFixture[str]) -> None:
    values = config_copy()
    values["aggregate"].pop("processed_csv")
    config_path = write_config(tmp_path / "invalid.toml", values)

    exit_code = main(["--config", str(config_path), "init-dirs"])

    captured = capsys.readouterr()
    assert exit_code == 2
    assert str(config_path) in captured.err
    assert "aggregate.processed_csv" in captured.err
