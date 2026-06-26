from __future__ import annotations

from pathlib import Path

from pubdelays.cli import main


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


def create_required_inputs(root: Path) -> None:
    for directory in [
        "data/raw_data/pubmed/xmls",
        "data/raw_data/scimago",
        "data/raw_data/web_of_science",
        "data/raw_data/directory_of_open_access_journals",
        "data/raw_data/norwegian_publication_indicator",
        "data/raw_data/retraction_watch",
    ]:
        (root / directory).mkdir(parents=True, exist_ok=True)
    for file_path in [
        "data/raw_data/web_of_science/wos.csv",
        "data/raw_data/directory_of_open_access_journals/doaj.csv",
        "data/raw_data/norwegian_publication_indicator/npi.csv",
        "data/raw_data/retraction_watch/retraction_watch.csv",
    ]:
        (root / file_path).write_text("placeholder\n", encoding="utf-8")


def test_preflight_reports_required_missing_with_placement_hint(
    tmp_path: Path, capsys: object
) -> None:
    config = write_config(tmp_path / "config.toml")

    code = main(["--config", str(config), "preflight"])
    output = capsys.readouterr().out

    assert code == 1
    assert "missing required PubMed XML baseline/update files" in output
    assert "Put .xml.gz files from NCBI PubMed baseline/updatefiles here" in output
    assert "missing_required_inputs" in output


def test_preflight_warns_for_missing_optional_metadata_without_failing(
    tmp_path: Path, capsys: object
) -> None:
    config = write_config(tmp_path / "config.toml")
    create_required_inputs(tmp_path)

    code = main(["--config", str(config), "preflight"])
    output = capsys.readouterr().out

    assert code == 0
    assert "missing optional Publisher metadata raw CSV" in output
    assert "missing optional Peer-review metadata raw CSV" in output
    assert "missing_optional_inputs" in output
    assert "missing_required_inputs" in output


def test_list_inputs_writes_sorted_deterministic_xml_paths(tmp_path: Path) -> None:
    input_dir = tmp_path / "inputs"
    (input_dir / "b").mkdir(parents=True)
    (input_dir / "a").mkdir(parents=True)
    for path in [
        input_dir / "b" / "pubmed25n0002.xml.gz",
        input_dir / "a" / "pubmed25n0001.xml",
        input_dir / "b" / "ignore.txt",
    ]:
        path.write_text("x", encoding="utf-8")
    output = tmp_path / "inputs.txt"

    assert main(["list-inputs", "--input-dir", str(input_dir), "--output", str(output)]) == 0

    lines = output.read_text(encoding="utf-8").splitlines()
    assert lines == sorted(lines)
    assert lines == [
        str(input_dir / "a" / "pubmed25n0001.xml"),
        str(input_dir / "b" / "pubmed25n0002.xml.gz"),
    ]


def test_list_inputs_writes_sorted_deterministic_json_paths(tmp_path: Path) -> None:
    input_dir = tmp_path / "json"
    (input_dir / "b").mkdir(parents=True)
    (input_dir / "a").mkdir(parents=True)
    for path in [
        input_dir / "b" / "records.json",
        input_dir / "a" / "records.jsonl",
        input_dir / "a" / "ignore.txt",
    ]:
        path.write_text("x", encoding="utf-8")
    output = tmp_path / "json_inputs.txt"

    assert (
        main(
            [
                "list-inputs",
                "--kind",
                "json",
                "--input-dir",
                str(input_dir),
                "--output",
                str(output),
            ]
        )
        == 0
    )

    lines = output.read_text(encoding="utf-8").splitlines()
    assert lines == sorted(lines)
    assert lines == [
        str(input_dir / "a" / "records.jsonl"),
        str(input_dir / "b" / "records.json"),
    ]
