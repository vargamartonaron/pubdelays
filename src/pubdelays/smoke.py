"""Isolated live end-to-end smoke workflow for the publication-delay pipeline."""

from __future__ import annotations

import json
import subprocess
import sys
from dataclasses import dataclass
from pathlib import Path

import polars as pl

from pubdelays.config import PipelineConfig
from pubdelays.manifest import Manifest
from pubdelays.schema import validate_analysis_dataset_schema


@dataclass(frozen=True)
class LiveSmokeResult:
    workspace: Path
    config_path: Path
    debrief_path: Path
    rows: int
    sources: dict[str, str]


def _toml_text(config: PipelineConfig, workspace: Path, shards: int) -> str:
    def quoted(value: object) -> str:
        return json.dumps(str(value))

    manual = {
        "scimago_dir": config.path("external.raw.scimago_dir"),
        "web_of_science_csv": config.path("external.raw.web_of_science_csv"),
        "norwegian_list_csv": config.path("external.raw.norwegian_list_csv"),
        "publisher_csv": config.path("external.raw.publisher_csv"),
        "peer_review_csv": config.path("external.raw.peer_review_csv"),
    }
    raw = workspace / "data/raw_data"
    processed = workspace / "data/processed_data"
    temporary = workspace / "data/temp_data"
    manifests = workspace / "data/manifests"
    return f"""[pipeline]
manifest = {quoted(manifests / 'pipeline.sqlite')}
parse_inputs = {quoted(manifests / 'parse_inputs.txt')}
transform_inputs = {quoted(manifests / 'transform_inputs.txt')}

[pubmed]
xml_dir = {quoted(raw / 'pubmed/xmls')}
jsonl_dir = {quoted(temporary / 'pubmed/jsonl')}

[external.raw]
scimago_dir = {quoted(manual['scimago_dir'])}
web_of_science_csv = {quoted(manual['web_of_science_csv'])}
doaj_csv = {quoted(raw / 'directory_of_open_access_journals/doaj.csv')}
norwegian_list_csv = {quoted(manual['norwegian_list_csv'])}
retraction_watch_csv = {quoted(raw / 'retraction_watch/retraction_watch.csv')}
publisher_csv = {quoted(manual['publisher_csv'])}
peer_review_csv = {quoted(manual['peer_review_csv'])}

[external.download]
doaj_url = {quoted(config.get('external.download.doaj_url', 'https://doaj.org/csv'))}
retraction_watch_url = {quoted(config.get('external.download.retraction_watch_url', 'https://gitlab.com/crossref/retraction-watch-data/-/raw/main/retraction_watch.csv'))}
scimago_url_template = ""
publisher_url = ""

[external.processed]
scimago = {quoted(processed / 'scimago.csv')}
web_of_science = {quoted(processed / 'web_of_science.csv')}
doaj = {quoted(processed / 'doaj.csv')}
norwegian_list = {quoted(processed / 'norwegian_list.csv')}
retraction_watch = {quoted(processed / 'retraction_watch.csv')}
publisher = {quoted(processed / 'publisher_metadata.csv')}
peer_review = {quoted(processed / 'peer_review.csv')}
pubmed_journals = {quoted(workspace / 'data/external/pubmed-journals.csv')}

[transform]
article_shard_dir = {quoted(temporary / 'article_parquet')}
article_shard_format = "parquet"
min_received = "2013-01-01"
default_shards = {shards}

[aggregate]
processed_parquet = {quoted(processed / 'processed.parquet')}
processed_csv = {quoted(processed / 'processed.csv')}
summary_dir = {quoted(processed / 'summaries')}
filter_counts = {quoted(processed / 'filter_counts.csv')}

[quality]
report_dir = {quoted(processed / 'quality')}

[analysis]
cwd = {quoted(config.root)}
input = {quoted(processed / 'processed.parquet')}
output_dir = {quoted(processed / 'analysis')}
command = ["python", "pubdelays_analysis/outputs.py"]

[validation]
report_dir = {quoted(processed / 'validation_tables')}
filtered_output = {quoted(processed / 'processed_validated.parquet')}
excluded_output = {quoted(processed / 'processed_validation_excluded.parquet')}
min_article_date = "2016-01-01"
max_article_date = "2025-12-31"
min_delay_days = 1
max_delay_days = 1095

[slurm]
runner = "pubdelays"
log_dir = {quoted(workspace / 'logs/slurm')}
partition = ""
account = ""
qos = ""
max_array_size = 100
"""


def _run(config_path: Path, args: list[str], *, accepted: tuple[int, ...] = (0,)) -> int:
    command = [sys.executable, "-m", "pubdelays.cli", "--config", str(config_path), *args]
    completed = subprocess.run(command, check=False)
    if completed.returncode not in accepted:
        raise RuntimeError(f"smoke stage failed ({completed.returncode}): {' '.join(args)}")
    return completed.returncode


def _available_sources(config: PipelineConfig) -> dict[str, bool]:
    scimago = config.path("external.raw.scimago_dir")
    return {
        "scimago": all((scimago / f"scimagojr {year}.csv").is_file() for year in range(2015, 2025)),
        "web_of_science": config.path("external.raw.web_of_science_csv").is_file(),
        "norwegian_list": config.path("external.raw.norwegian_list_csv").is_file(),
        "publisher": config.path("external.raw.publisher_csv").is_file(),
        "peer_review": config.path("external.raw.peer_review_csv").is_file(),
    }


def run_live_smoke(
    config: PipelineConfig,
    workspace: Path,
    *,
    pubmed_files: int = 1,
    jobs: int = 1,
    shards: int = 2,
    resume: bool = False,
) -> LiveSmokeResult:
    """Run real downloads and all feasible stages in an isolated workspace."""
    workspace = Path(workspace).resolve()
    workspace.mkdir(parents=True, exist_ok=True)
    config_path = workspace / "smoke.toml"
    config_path.write_text(_toml_text(config, workspace, shards), encoding="utf-8")
    resume_arg = ["--resume"] if resume else []

    _run(config_path, ["init-dirs"])
    _run(
        config_path,
        ["download", "--source", "baseline", "--limit", str(pubmed_files * 2), "--newest", "--jobs", str(jobs), *resume_arg],
    )
    _run(config_path, ["download-external", "--source", "all", *resume_arg])

    sources: dict[str, str] = {"pubmed": "downloaded", "doaj": "downloaded", "retraction_watch": "downloaded"}
    _run(config_path, ["external-doaj", *resume_arg])
    _run(config_path, ["external-retraction-watch", *resume_arg])
    available = _available_sources(config)
    commands = {
        "scimago": "external-scimago",
        "web_of_science": "external-wos",
        "norwegian_list": "external-npi",
        "publisher": "external-publisher",
        "peer_review": "external-peer-review",
    }
    for source, command in commands.items():
        if available[source]:
            _run(config_path, [command, *resume_arg])
            sources[source] = "local_input_used"
        else:
            sources[source] = "source_not_supplied"

    _run(config_path, ["parse", "--jobs", str(jobs), "--format", "jsonl", *resume_arg])
    _run(config_path, ["validate"])
    _run(config_path, ["transform-shards", "--shards", str(shards), "--jobs", str(jobs), "--format", "parquet", *resume_arg])
    _run(config_path, ["validate-shards", "--shards", str(shards), "--format", "parquet"])
    _run(config_path, ["aggregate-all", "--shards", str(shards), "--format", "parquet", *resume_arg])
    _run(config_path, ["schema", "--input", str(workspace / "data/processed_data/processed.parquet")])
    validation_code = _run(config_path, ["validate-analysis"], accepted=(0, 1))
    _run(config_path, ["filter-counts", *resume_arg])
    _run(config_path, ["quality-report"])
    _run(config_path, ["summaries"])
    _run(config_path, ["provenance", "--format", "csv", "--output", str(workspace / "data/processed_data/variable_provenance.csv")])
    _run(config_path, ["manifest", "check"])

    parquet_path = workspace / "data/processed_data/processed.parquet"
    csv_path = workspace / "data/processed_data/processed.csv"
    valid, schema_errors = validate_analysis_dataset_schema(parquet_path)
    parquet = pl.read_parquet(parquet_path)
    csv_frame = pl.read_csv(csv_path, infer_schema=False)
    parity = parquet.to_dicts() == csv_frame.to_dicts()
    quality = pl.read_csv(
        workspace / "data/processed_data/quality/stage_and_join_debrief.csv",
        infer_schema=False,
    )
    validation_checks = pl.read_csv(
        workspace / "data/processed_data/validation_tables/validation_checks.csv",
        infer_schema=False,
    )
    allowed_validation_failures = {"range:article_date"}
    observed_validation_failures = set(
        validation_checks.filter(pl.col("status") != "pass")["check"].to_list()
    )
    expected_validation_failures = observed_validation_failures & allowed_validation_failures
    unexpected_validation_failures = validation_checks.filter(
        (pl.col("status") != "pass")
        & ~pl.col("check").is_in(allowed_validation_failures)
    )["check"].to_list()
    cardinality = quality.filter(pl.col("metric") == "cardinality_delta")
    cardinality_ok = not cardinality.height or all(value == "0" for value in cardinality["numerator"])
    manifest = Manifest(workspace / "data/manifests/pipeline.sqlite")
    debrief = {
        "workspace": str(workspace),
        "rows": parquet.height,
        "schema_valid": valid,
        "schema_errors": schema_errors,
        "csv_parquet_parity": parity,
        "join_cardinality_ok": cardinality_ok,
        "validation_exit_code": validation_code,
        "expected_validation_failures": sorted(expected_validation_failures),
        "unexpected_validation_failures": unexpected_validation_failures,
        "manifest_rows": len(manifest.rows(limit=100000)),
        "sources": sources,
    }
    debrief_path = workspace / "data/processed_data/smoke_debrief.json"
    debrief_path.write_text(json.dumps(debrief, indent=2) + "\n", encoding="utf-8")
    if not valid or not parity or not cardinality_ok or unexpected_validation_failures:
        raise RuntimeError(f"smoke quality gates failed; inspect {debrief_path}")
    return LiveSmokeResult(workspace, config_path, debrief_path, parquet.height, sources)
