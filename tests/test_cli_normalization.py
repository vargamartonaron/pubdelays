from __future__ import annotations

from pathlib import Path

from pubdelays.cli import build_parser, main


def write_config(path: Path) -> Path:
    path.write_text(
        """
[pipeline]
manifest = "data/manifests/pipeline.sqlite"
parse_inputs = "data/manifests/parse_inputs.txt"
transform_inputs = "data/manifests/transform_inputs.txt"

[pubmed]
xml_dir = "data/raw_data/pubmed/xmls"
jsonl_dir = "data/temp_data/pubmed/jsonl"

[external.raw]
scimago_dir = "data/raw_data/scimago"
web_of_science_csv = "data/raw_data/web_of_science/wos.csv"
doaj_csv = "data/raw_data/directory_of_open_access_journals/doaj.csv"
norwegian_list_csv = "data/raw_data/norwegian_publication_indicator/npi.csv"
retraction_watch_csv = "data/raw_data/retraction_watch/retraction_watch.csv"
publisher_csv = "data/raw_data/publisher_metadata/publishers.csv"
peer_review_csv = "data/raw_data/peer_review/peer_review.csv"

[external.processed]
scimago = "data/processed_data/scimago.csv"
web_of_science = "data/processed_data/web_of_science.csv"
doaj = "data/processed_data/doaj.csv"
norwegian_list = "data/processed_data/norwegian_list.csv"
retraction_watch = "data/processed_data/retraction_watch.csv"
publisher = "data/processed_data/publisher_metadata.csv"
peer_review = "data/processed_data/peer_review.csv"
pubmed_journals = "data/external/pubmed-journals.csv"

[transform]
article_shard_dir = "data/temp_data/article_parquet"
article_shard_format = "parquet"
min_received = "2013-01-01"
default_shards = 2

[aggregate]
processed_parquet = "data/processed_data/processed.parquet"
processed_csv = "data/processed_data/processed.csv"
summary_dir = "data/processed_data/summaries"
filter_counts = "data/processed_data/filter_counts.csv"

[analysis]
cwd = "."
input = "data/processed_data/processed.parquet"
output_dir = "data/processed_data/analysis"
command = ["python", "pubdelays_analysis/outputs.py", "--input", "data/processed_data/processed.parquet", "--table-dir", "data/processed_data/analysis_tables", "--figure-data-dir", "data/processed_data/analysis_figures"]

[validation]
report_dir = "data/processed_data/validation_tables"
filtered_output = "data/processed_data/processed_validated.parquet"
min_article_date = "2016-01-01"
max_article_date = "2025-06-01"
min_delay_days = 1
max_delay_days = 1095
""".strip(),
        encoding="utf-8",
    )
    return path


def test_top_level_help_describes_main_workflow() -> None:
    parser = build_parser()
    help_text = parser.format_help()

    assert parser.prog == "pubdelays"
    assert "Main workflow" in help_text
    assert "external-all" in help_text
    assert "transform-shards" in help_text
    assert "manifest" in help_text
    assert "summary" in help_text


def test_parse_dry_run_does_not_write_outputs(tmp_path: Path, capsys: object) -> None:
    config = write_config(tmp_path / "config.toml")
    xml_dir = tmp_path / "data/raw_data/pubmed/xmls"
    xml_dir.mkdir(parents=True)
    (xml_dir / "pubmed25n0001.xml").write_text("<PubmedArticleSet />", encoding="utf-8")

    code = main(["--config", str(config), "parse", "--dry-run", "--jobs", "1"])
    output = capsys.readouterr().out

    assert code == 0
    assert "dry-run parse" in output
    assert not (tmp_path / "data/temp_data/pubmed/jsonl").exists()


def test_transform_shards_dry_run_does_not_write_input_list(tmp_path: Path, capsys: object) -> None:
    config = write_config(tmp_path / "config.toml")
    json_dir = tmp_path / "data/temp_data/pubmed/jsonl"
    json_dir.mkdir(parents=True)
    (json_dir / "records-1.jsonl").write_text("{}\n", encoding="utf-8")
    (json_dir / "records-2.jsonl").write_text("{}\n", encoding="utf-8")

    code = main(["--config", str(config), "transform-shards", "--dry-run", "--jobs", "1", "--limit", "1"])
    output = capsys.readouterr().out

    assert code == 0
    assert "files=1" in output
    assert not (tmp_path / "data/manifests/transform_inputs.txt").exists()


def test_transform_limit_arguments_are_accepted() -> None:
    parser = build_parser()

    transform = parser.parse_args(["transform", "--limit", "2"])
    transform_shards = parser.parse_args(["transform-shards", "--limit", "2"])
    list_inputs = parser.parse_args(
        ["list-inputs", "--input-dir", "data", "--output", "inputs.txt", "--limit", "2"]
    )

    assert transform.limit == 2
    assert transform_shards.limit == 2
    assert list_inputs.limit == 2
