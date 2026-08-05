"""Command-line interface for the PubMed/MEDLINE publication-delay pipeline.

The CLI is deliberately thin: XML parsing is in :mod:`pubdelays.parser`,
external metadata cleaning is in :mod:`pubdelays.external`, article-level
filtering/enrichment is in :mod:`pubdelays.transform`, and aggregation is in
:mod:`pubdelays.aggregate`.  All mutating stages use atomic output files and
write one manifest row.
"""

from __future__ import annotations

import argparse
import csv
import json
import os
import shlex
import sqlite3
import subprocess
import time
import urllib.request
from collections.abc import Callable
from concurrent.futures import ProcessPoolExecutor, ThreadPoolExecutor, as_completed
from dataclasses import dataclass
from datetime import date
from pathlib import Path
from typing import Any

import polars as pl

from pubdelays.aggregate import aggregate_articles, aggregate_outputs, collect_filter_counts
from pubdelays.config import ConfigError, PipelineConfig, load_config, resolve_config_path
from pubdelays.download import (
    DownloadError,
    contained_download_path,
    download_file,
    download_request,
    external_download_plans,
    index_links,
    verify_md5_file,
)
from pubdelays.download import (
    parse_md5_sidecar as parse_md5_sidecar,
)
from pubdelays.external import (
    download_official_exchange_rates,
    preprocess_doaj,
    preprocess_npi,
    preprocess_peer_review,
    preprocess_publisher,
    preprocess_retraction_watch,
    preprocess_scimago,
    preprocess_wos,
)
from pubdelays.external.common import write_frame
from pubdelays.fs import atomic_output_path, complete_file
from pubdelays.manifest import (
    Manifest,
    ManifestRow,
    default_worker,
    file_size,
    sha256_file,
    utc_now,
)
from pubdelays.parser.medline import parse_medline_xml
from pubdelays.paths import expected_input_paths, expected_output_paths
from pubdelays.provenance import render_provenance, validate_provenance, write_provenance
from pubdelays.quality import build_quality_report
from pubdelays.schema import (
    ANALYSIS_DATASET_VERSION,
    CANONICAL_ARTICLE_COLUMNS,
    FILTER_STAGES,
    validate_analysis_dataset_schema,
)
from pubdelays.shards import expected_article_shard_path, validate_article_shards
from pubdelays.slurm import (
    SlurmJob,
    SlurmQueryError,
    SlurmResources,
    SlurmSubmissionError,
    SlurmSubmitter,
    build_sbatch_script,
    query_max_array_size,
    submit_sbatch,
)
from pubdelays.smoke import run_live_smoke
from pubdelays.state import resolve_pubmed_state
from pubdelays.summaries import derive_summary_tables
from pubdelays.transform import ExternalInputs, transform_files
from pubdelays.ui import err, info, ok, print_kv_table, section, warn
from pubdelays.validation import compare_outputs, validate_analysis_output

PUBMED_BASE_URLS = {
    "baseline": "https://ftp.ncbi.nlm.nih.gov/pubmed/baseline/",
    "updatefiles": "https://ftp.ncbi.nlm.nih.gov/pubmed/updatefiles/",
}

DEFAULT_CONFIG = "config/default.toml"


@dataclass(frozen=True)
class ParseStats:
    input_path: str
    output_path: str
    records: int
    deleted: int
    skipped: bool = False


def cfg(args: argparse.Namespace) -> PipelineConfig:
    return load_config(getattr(args, "config", DEFAULT_CONFIG))


def cfg_path(args: argparse.Namespace, attr: str, key: str, default: str | None = None) -> Path:
    value = getattr(args, attr, None)
    if value not in (None, ""):
        return Path(value).expanduser()
    return cfg(args).path(key, default)


def manifest_path_from_args(args: argparse.Namespace) -> Path:
    value = getattr(args, "manifest", None)
    if value not in (None, ""):
        return Path(value)
    return cfg(args).path("pipeline.manifest", "data/manifests/pipeline.sqlite")


def manifest_from_args(args: argparse.Namespace) -> Manifest:
    return Manifest(manifest_path_from_args(args))


def elapsed(start: float) -> float:
    return round(time.time() - start, 3)


def maybe_sha256(path: Path | None, enabled: bool = True) -> str:
    if not enabled or path is None:
        return ""
    p = Path(path)
    if not p.exists() or not p.is_file():
        return ""
    return sha256_file(p)


def append_manifest(
    manifest: Manifest,
    *,
    stage: str,
    status: str,
    started_at: str,
    start_seconds: float,
    input_path: Path | None = None,
    output_path: Path | None = None,
    records: int | None = None,
    deleted: int | None = None,
    metadata: dict[str, Any] | None = None,
    error_message: str = "",
    checksum: bool = True,
) -> None:
    manifest.append(
        ManifestRow(
            stage=stage,
            status=status,
            input_path=str(input_path or ""),
            output_path=str(output_path or ""),
            input_sha256=maybe_sha256(input_path, checksum),
            output_sha256=maybe_sha256(output_path, checksum),
            input_bytes=file_size(input_path),
            output_bytes=file_size(output_path),
            records=records,
            deleted=deleted,
            started_at=started_at,
            finished_at=utc_now(),
            elapsed_seconds=elapsed(start_seconds),
            worker=default_worker(),
            metadata=metadata or {},
            error=error_message,
        )
    )


def list_xml_paths(path_dir: str | Path) -> list[Path]:
    path = Path(path_dir).expanduser()
    if not path.exists():
        return []
    suffixes = {".xml", ".nxml"}
    return sorted(
        candidate
        for candidate in path.rglob("*")
        if candidate.is_file() and (candidate.suffix in suffixes or candidate.name.endswith(".xml.gz"))
    )


def list_json_paths(path_dir: str | Path) -> list[Path]:
    path = Path(path_dir).expanduser()
    if not path.exists():
        return []
    return sorted(list(path.rglob("*.jsonl")) + list(path.rglob("*.json")))


def limit_paths(paths: list[Path], limit: int | None) -> list[Path]:
    return paths if limit is None else paths[:limit]


def output_path_for(input_path: Path, output_dir: Path, fmt: str) -> Path:
    extension = "jsonl" if fmt == "jsonl" else "json"
    return output_dir / f"{input_path.name}.{extension}"


def transform_output_path_for(input_path: Path, output_dir: Path, fmt: str = "parquet") -> Path:
    suffix = "parquet" if fmt == "parquet" else fmt
    return output_dir / f"{input_path.name}.{suffix}"


def filters_output_path_for(input_path: Path, output_dir: Path) -> Path:
    return output_dir / f"{input_path.name}.filters.csv"


def parse_one(
    input_path: str | Path,
    output_path: str | Path,
    *,
    fmt: str,
    resume: bool,
    year_info_only: bool,
    nlm_category: bool,
    author_list: bool,
    reference_list: bool,
    parse_mesh_subterms: bool,
    min_pub_year: int | None,
    recover: bool,
    manifest_path: str | Path | None = None,
    checksum: bool = True,
) -> ParseStats:
    input_path = Path(input_path)
    output_path = Path(output_path)
    output_path.parent.mkdir(parents=True, exist_ok=True)
    manifest = Manifest(Path(manifest_path)) if manifest_path else None
    started_at = utc_now()
    start_seconds = time.time()

    try:
        if resume and complete_file(output_path):
            stats = ParseStats(str(input_path), str(output_path), 0, 0, skipped=True)
            if manifest:
                append_manifest(
                    manifest,
                    stage="parse",
                    status="skipped",
                    input_path=input_path,
                    output_path=output_path,
                    records=0,
                    deleted=0,
                    started_at=started_at,
                    start_seconds=start_seconds,
                    metadata={
                        "format": fmt,
                        "reason": "existing_output",
                        "recover_malformed_xml": recover,
                    },
                    checksum=checksum,
                )
            return stats

        records = 0
        deleted = 0
        iterator = parse_medline_xml(
            input_path,
            year_info_only=year_info_only,
            nlm_category=nlm_category,
            author_list=author_list,
            reference_list=reference_list,
            parse_downto_mesh_subterms=parse_mesh_subterms,
            min_pub_year=min_pub_year,
            recover=recover,
        )

        with atomic_output_path(output_path) as tmp_path:
            if fmt == "jsonl":
                with tmp_path.open("w", encoding="utf-8") as handle:
                    for record in iterator:
                        records += 1
                        if record.get("delete"):
                            deleted += 1
                        handle.write(json.dumps(record, ensure_ascii=False, separators=(",", ":")))
                        handle.write("\n")
            else:
                data: list[dict[str, Any]] = []
                for record in iterator:
                    records += 1
                    if record.get("delete"):
                        deleted += 1
                    data.append(record)
                with tmp_path.open("w", encoding="utf-8") as handle:
                    json.dump(data, handle, ensure_ascii=False)
                    handle.write("\n")

        if manifest:
            append_manifest(
                manifest,
                stage="parse",
                status="success",
                input_path=input_path,
                output_path=output_path,
                records=records,
                deleted=deleted,
                started_at=started_at,
                start_seconds=start_seconds,
                metadata={
                    "format": fmt,
                    "min_pub_year": min_pub_year,
                    "recover_malformed_xml": recover,
                },
                checksum=checksum,
            )
        return ParseStats(str(input_path), str(output_path), records, deleted)
    except Exception as exc:
        if manifest:
            append_manifest(
                manifest,
                stage="parse",
                status="failed",
                input_path=input_path,
                output_path=output_path,
                records=0,
                deleted=0,
                started_at=started_at,
                start_seconds=start_seconds,
                metadata={"format": fmt, "recover_malformed_xml": recover},
                error_message=repr(exc),
                checksum=checksum,
            )
        raise


def parse_kwargs(args: argparse.Namespace) -> dict[str, Any]:
    manifest_path = cfg_path(args, "manifest", "pipeline.manifest")
    return {
        "fmt": args.format,
        "resume": args.resume,
        "year_info_only": args.year_info_only,
        "nlm_category": args.nlm_category,
        "author_list": args.author_list,
        "reference_list": args.reference_list,
        "parse_mesh_subterms": args.parse_mesh_subterms,
        "min_pub_year": args.min_pub_year,
        "recover": args.recover_malformed_xml,
        "manifest_path": manifest_path,
        "checksum": not args.no_checksum,
    }


def cmd_parse_one(args: argparse.Namespace) -> int:
    output = (
        Path(args.output)
        if args.output
        else output_path_for(
            Path(args.input),
            cfg_path(args, "output_dir", "pubmed.jsonl_dir"),
            args.format,
        )
    )
    try:
        stats = parse_one(Path(args.input), output, **parse_kwargs(args))
    except Exception as exc:
        err(f"parse failed for {args.input}: {exc}")
        return 1
    status = "skipped" if stats.skipped else "parsed"
    ok(f"{status} {stats.input_path} -> {stats.output_path}")
    print_kv_table({"records": stats.records, "deleted": stats.deleted})
    return 0


def cmd_parse(args: argparse.Namespace) -> int:
    source = getattr(args, "source", "legacy")
    input_key = {
        "baseline": "pubmed.baseline_xml_dir",
        "updatefiles": "pubmed.update_xml_dir",
        "legacy": "pubmed.xml_dir",
    }[source]
    output_key = {
        "baseline": "pubmed.baseline_jsonl_dir",
        "updatefiles": "pubmed.update_jsonl_dir",
        "legacy": "pubmed.jsonl_dir",
    }[source]
    input_dir = cfg_path(
        args, "input_dir", input_key, str(cfg(args).get("pubmed.xml_dir"))
    )
    output_dir = cfg_path(
        args, "output_dir", output_key, str(cfg(args).get("pubmed.jsonl_dir"))
    )
    xml_paths = list_xml_paths(input_dir)
    if not xml_paths:
        err(f"No XML files found in {input_dir}")
        return 1

    jobs = args.jobs if args.jobs is not None else max((os.cpu_count() or 2) - 1, 1)
    if getattr(args, "dry_run", False):
        info(f"dry-run parse files={len(xml_paths)} jobs={jobs} output={output_dir}")
        return 0
    kwargs = parse_kwargs(args)
    total_records = 0
    total_deleted = 0
    skipped = 0

    info(f"parse files={len(xml_paths)} jobs={jobs} output={output_dir}")
    if jobs == 1:
        for path in xml_paths:
            try:
                stats = parse_one(path, output_path_for(path, output_dir, args.format), **kwargs)
            except Exception as exc:
                err(f"parse failed for {path}: {exc}")
                return 1
            total_records += stats.records
            total_deleted += stats.deleted
            skipped += int(stats.skipped)
            ok(f"{'skipped' if stats.skipped else 'parsed'} {Path(stats.input_path).name}")
    else:
        with ProcessPoolExecutor(max_workers=jobs) as executor:
            futures = [
                executor.submit(
                    parse_one,
                    path,
                    output_path_for(path, output_dir, args.format),
                    **kwargs,
                )
                for path in xml_paths
            ]
            for future in as_completed(futures):
                try:
                    stats = future.result()
                except Exception as exc:
                    err(f"parse failed: {exc}")
                    return 1
                total_records += stats.records
                total_deleted += stats.deleted
                skipped += int(stats.skipped)
                ok(f"{'skipped' if stats.skipped else 'parsed'} {Path(stats.input_path).name}")

    print_kv_table(
        {
            "files": len(xml_paths),
            "skipped": skipped,
            "records": total_records,
            "deleted": total_deleted,
        }
    )
    return 0


def cmd_resolve_state(args: argparse.Namespace) -> int:
    """Resolve separately parsed baseline/update files into the live PubMed state."""
    manifest = manifest_from_args(args)
    started_at = utc_now()
    start_seconds = time.time()
    baseline = cfg_path(args, "baseline", "pubmed.baseline_jsonl_dir")
    updates = cfg_path(args, "updates", "pubmed.update_jsonl_dir")
    output_dir = cfg_path(args, "output_dir", "pubmed.resolved_jsonl_dir")
    state_db = cfg_path(args, "state_db", "pubmed.state_db")
    counts_output = cfg_path(args, "counts_output", "pubmed.state_counts")
    try:
        counts = resolve_pubmed_state(baseline, updates, output_dir, state_db=state_db)
        counts_output.parent.mkdir(parents=True, exist_ok=True)
        with atomic_output_path(counts_output) as temporary:
            temporary.write_text(
                json.dumps(counts, indent=2, sort_keys=True) + "\n", encoding="utf-8"
            )
        append_manifest(
            manifest,
            stage="resolve-pubmed-state",
            status="success",
            input_path=baseline,
            output_path=output_dir,
            records=counts["resolved_rows"],
            deleted=counts["deleted_pmids"],
            started_at=started_at,
            start_seconds=start_seconds,
            metadata={**counts, "updates": str(updates), "state_db": str(state_db)},
            checksum=False,
        )
        print_kv_table(counts)
        ok(f"wrote resolved PubMed state to {output_dir}")
        return 0
    except Exception as exc:
        append_manifest(
            manifest,
            stage="resolve-pubmed-state",
            status="failed",
            input_path=baseline,
            output_path=output_dir,
            started_at=started_at,
            start_seconds=start_seconds,
            metadata={"updates": str(updates), "state_db": str(state_db)},
            error_message=repr(exc),
            checksum=False,
        )
        raise


def validate_json_file(path: Path) -> tuple[bool, int]:
    count = 0
    try:
        if path.suffix == ".jsonl":
            with path.open("r", encoding="utf-8") as handle:
                for line in handle:
                    if not line.strip():
                        continue
                    json.loads(line)
                    count += 1
        else:
            with path.open("r", encoding="utf-8") as handle:
                data = json.load(handle)
                count = len(data) if isinstance(data, list) else 1
        return True, count
    except Exception as exc:
        err(f"INVALID {path}: {exc}")
        return False, count


def cmd_validate(args: argparse.Namespace) -> int:
    manifest = manifest_from_args(args)
    started_at = utc_now()
    start_seconds = time.time()
    input_path = Path(args.input) if args.input else cfg_path(args, "input", "pubmed.jsonl_dir")
    paths = list_json_paths(input_path) if input_path.is_dir() else [input_path]
    failures = 0
    total_records = 0
    for path in paths:
        ok_file, count = validate_json_file(path)
        total_records += count
        if ok_file:
            ok(f"{path}: {count} records")
        else:
            failures += 1
    append_manifest(
        manifest,
        stage="validate-json",
        status="failed" if failures else "success",
        input_path=input_path,
        records=total_records,
        deleted=failures,
        started_at=started_at,
        start_seconds=start_seconds,
        metadata={"files": len(paths), "failures": failures},
        checksum=not args.no_checksum,
    )
    print_kv_table({"files": len(paths), "records": total_records, "failures": failures})
    return 1 if failures else 0


def parse_j_medline(text: str) -> list[dict[str, str]]:
    keys = [
        "JrId",
        "JournalTitle",
        "MedAbbr",
        "ISSN (Print)",
        "ISSN (Online)",
        "IsoAbbr",
        "NlmId",
    ]
    records: list[dict[str, str]] = []
    current = dict.fromkeys(keys, "")
    for line in text.splitlines():
        if line.startswith("-"):
            if any(current.values()):
                records.append(dict(current))
            current = dict.fromkeys(keys, "")
            continue
        if ":" in line:
            key, value = map(str.strip, line.split(":", 1))
            if key in current:
                current[key] = value
    if any(current.values()):
        records.append(dict(current))
    return records


def cmd_journals(args: argparse.Namespace) -> int:
    manifest = manifest_from_args(args)
    started_at = utc_now()
    start_seconds = time.time()
    output = cfg_path(args, "output", "external.processed.pubmed_journals")
    request = download_request(args.url, accept="text/plain,text/csv,*/*")
    with urllib.request.urlopen(request) as response:
        text = response.read().decode("utf-8")
    rows = parse_j_medline(text)
    with atomic_output_path(output) as tmp_path:
        with tmp_path.open("w", newline="", encoding="utf-8") as handle:
            writer = csv.DictWriter(
                handle,
                fieldnames=[
                    "JrId",
                    "JournalTitle",
                    "MedAbbr",
                    "ISSN (Print)",
                    "ISSN (Online)",
                    "IsoAbbr",
                    "NlmId",
                ],
            )
            writer.writeheader()
            writer.writerows(rows)
    append_manifest(
        manifest,
        stage="journals",
        status="success",
        output_path=output,
        records=len(rows),
        started_at=started_at,
        start_seconds=start_seconds,
        metadata={"url": args.url},
        checksum=not args.no_checksum,
    )
    ok(f"wrote {len(rows)} journal rows to {output}")
    return 0


def cmd_download(args: argparse.Namespace) -> int:
    manifest = manifest_from_args(args)
    started_at = utc_now()
    start_seconds = time.time()
    base_url = PUBMED_BASE_URLS[args.source]
    output_key = (
        "pubmed.baseline_xml_dir" if args.source == "baseline" else "pubmed.update_xml_dir"
    )
    output_dir = cfg_path(
        args,
        "output_dir",
        output_key,
        str(cfg(args).get("pubmed.xml_dir", "data/raw_data/pubmed/xmls")),
    )
    try:
        links = index_links(base_url)
        if args.limit is not None:
            links = links[-args.limit :] if args.newest else links[: args.limit]
        download_paths = [(link, contained_download_path(output_dir, link)) for link in links]
    except (DownloadError, OSError, ValueError) as exc:
        append_manifest(
            manifest,
            stage="download",
            status="failed",
            output_path=output_dir,
            started_at=started_at,
            start_seconds=start_seconds,
            metadata={"source": args.source},
            error_message=str(exc),
            checksum=False,
        )
        err(str(exc))
        return 1

    jobs = max(args.jobs, 1)
    info(f"download source={args.source} files={len(links)} jobs={jobs} output={output_dir}")
    if getattr(args, "dry_run", False):
        info("dry-run download: no files or manifest rows will be written")
        return 0

    downloaded = 0
    skipped = 0
    download_errors: list[str] = []
    with ThreadPoolExecutor(max_workers=jobs) as executor:
        futures = [
            executor.submit(download_file, base_url + link, output_path, resume=args.resume)
            for link, output_path in download_paths
        ]
        for future in as_completed(futures):
            try:
                stats = future.result()
            except DownloadError as exc:
                download_errors.append(str(exc))
                err(str(exc))
                continue
            downloaded += int(stats.downloaded)
            skipped += int(stats.skipped)
            if stats.skipped:
                warn(f"skip {stats.output_path}")
            else:
                ok(f"downloaded {stats.output_path}")

    requested_md5_paths = sorted(
        output_path for _link, output_path in download_paths if output_path.suffix == ".md5" and output_path.exists()
    )
    md5_failures = [str(path) for path in requested_md5_paths if not verify_md5_file(path)]
    failures = [*download_errors, *md5_failures]
    append_manifest(
        manifest,
        stage="download",
        status="failed" if failures else "success",
        output_path=output_dir,
        records=downloaded,
        deleted=len(failures),
        started_at=started_at,
        start_seconds=start_seconds,
        metadata={
            "source": args.source,
            "links": len(links),
            "skipped": skipped,
            "failures": failures,
        },
        error_message="; ".join(failures),
        checksum=False,
    )
    if failures:
        err("download failures")
        for failure in failures:
            err(f"  {failure}")
        return 1
    ok(f"downloaded={downloaded} skipped={skipped} into {output_dir}; MD5 checks passed")
    return 0

def cmd_download_external(args: argparse.Namespace) -> int:
    manifest = manifest_from_args(args)
    started_at = utc_now()
    start_seconds = time.time()
    try:
        plans = external_download_plans(
            cfg(args), source=args.source, start_year=args.start_year, end_year=args.end_year
        )
    except RuntimeError as exc:
        err(str(exc))
        return 2
    if getattr(args, "dry_run", False):
        for plan in plans:
            print_kv_table({"source": plan.source, "url": plan.url, "output": str(plan.output_path)})
            print("---")
        return 0

    downloaded = 0
    skipped = 0
    failure = ""
    for plan in plans:
        try:
            stats = download_file(plan.url, plan.output_path, resume=args.resume, retries=args.retries, require_md5=False)
        except DownloadError as exc:
            failure = str(exc)
            err(failure)
            break
        downloaded += int(stats.downloaded)
        skipped += int(stats.skipped)
        ok(f"downloaded {plan.source}: {stats.output_path}") if stats.downloaded else warn(
            f"skip {plan.source}: {stats.output_path}"
        )
    append_manifest(
        manifest,
        stage="download-external",
        status="failed" if failure else "success",
        output_path=Path("data/raw_data"),
        records=downloaded,
        started_at=started_at,
        start_seconds=start_seconds,
        metadata={"sources": [plan.source for plan in plans], "skipped": skipped},
        error_message=failure,
        checksum=False,
    )
    return 1 if failure else 0


def cmd_exchange_rates(args: argparse.Namespace) -> int:
    manifest = manifest_from_args(args)
    started_at = utc_now()
    start_seconds = time.time()
    output = cfg_path(args, "output", "external.processed.exchange_rates")
    try:
        if args.resume and complete_file(output):
            warn(f"skip existing {output}")
            return 0
        records = download_official_exchange_rates(
            output, start_year=args.start_year, end_year=args.end_year
        )
        append_manifest(
            manifest,
            stage="exchange-rates",
            status="success",
            output_path=output,
            records=records,
            started_at=started_at,
            start_seconds=start_seconds,
            metadata={"start_year": args.start_year, "end_year": args.end_year},
            checksum=not args.no_checksum,
        )
        ok(f"wrote {records} official exchange-rate rows to {output}")
        return 0
    except Exception as exc:
        append_manifest(
            manifest,
            stage="exchange-rates",
            status="failed",
            output_path=output,
            started_at=started_at,
            start_seconds=start_seconds,
            error_message=repr(exc),
            checksum=False,
        )
        raise


def _optional_path(value: str | Path | None) -> Path | None:
    return Path(value) if value else None


def external_inputs_from_args(args: argparse.Namespace) -> ExternalInputs:
    config = cfg(args)
    return ExternalInputs(
        scimago=_optional_path(getattr(args, "scimago", None)) or config.path("external.processed.scimago"),
        web_of_science=_optional_path(getattr(args, "web_of_science", None))
        or config.path("external.processed.web_of_science"),
        doaj=_optional_path(getattr(args, "doaj", None)) or config.path("external.processed.doaj"),
        norwegian_list=_optional_path(getattr(args, "norwegian_list", None))
        or config.path("external.processed.norwegian_list"),
        retraction_watch=_optional_path(getattr(args, "retraction_watch", None))
        or config.path("external.processed.retraction_watch"),
        publisher=_optional_path(getattr(args, "publisher", None))
        or config.path("external.processed.publisher"),
        peer_review=_optional_path(getattr(args, "peer_review", None))
        or config.path("external.processed.peer_review"),
        exchange_rates=_optional_path(getattr(args, "exchange_rates", None))
        or (
            resolve_config_path(str(config.get("external.processed.exchange_rates")), config.root)
            if config.get("external.processed.exchange_rates")
            else None
        ),
    )


def min_received_from_args(args: argparse.Namespace) -> date:
    value = getattr(args, "min_received", None) or cfg(args).get("transform.min_received", "2013-01-01")
    return date.fromisoformat(str(value))


def cmd_transform_one(args: argparse.Namespace) -> int:
    manifest = manifest_from_args(args)
    started_at = utc_now()
    start_seconds = time.time()
    input_path = Path(args.input)
    output_path = Path(args.output)
    try:
        if args.resume and complete_file(output_path):
            append_manifest(
                manifest,
                stage="transform",
                status="skipped",
                input_path=input_path,
                output_path=output_path,
                started_at=started_at,
                start_seconds=start_seconds,
                metadata={"reason": "existing_output"},
                checksum=not args.no_checksum,
            )
            warn(f"skip existing {output_path}")
            return 0
        result = transform_files(
            input_path,
            output_path,
            filters_path=_optional_path(args.filters_output),
            external=external_inputs_from_args(args),
            min_received=min_received_from_args(args),
        )
        append_manifest(
            manifest,
            stage="transform",
            status="success",
            input_path=input_path,
            output_path=output_path,
            records=result.counts.get("final_rows", 0),
            started_at=started_at,
            start_seconds=start_seconds,
            metadata={
                "counts": dict(result.counts),
                "filters_path": str(result.filters_path or ""),
                "quality_path": str(result.quality_path or ""),
            },
            checksum=not args.no_checksum,
        )
        ok(f"wrote {result.output_path}")
        print_kv_table(result.counts)
        return 0
    except Exception as exc:
        append_manifest(
            manifest,
            stage="transform",
            status="failed",
            input_path=input_path,
            output_path=output_path,
            started_at=started_at,
            start_seconds=start_seconds,
            error_message=repr(exc),
            checksum=not args.no_checksum,
        )
        raise


def transform_worker(payload: dict[str, Any]) -> int:
    return cmd_transform_one(argparse.Namespace(**payload))


def cmd_transform(args: argparse.Namespace) -> int:
    """Compatibility command: one output file per input file.

    For full corpus work prefer ``transform-shards`` because it loads external
    metadata once per shard instead of once per PubMed XML file.
    """

    input_path = cfg_path(args, "input", "pubmed.jsonl_dir")
    output_dir = cfg_path(args, "output_dir", "transform.article_shard_dir")
    inputs = limit_paths(list_json_paths(input_path) if input_path.is_dir() else [input_path], args.limit)
    if not inputs:
        err(f"No JSON/JSONL files found in {input_path}")
        return 1
    jobs = args.jobs if args.jobs is not None else 1
    warn("transform processes one output per input; use transform-shards for the full corpus")
    info(f"transform files={len(inputs)} jobs={jobs} output_dir={output_dir}")

    payloads: list[dict[str, Any]] = []
    for path in inputs:
        payload = {key: value for key, value in vars(args).items() if key != "func"}
        payload["input"] = str(path)
        payload["output"] = str(transform_output_path_for(path, output_dir, args.format))
        payload["filters_output"] = str(filters_output_path_for(path, output_dir))
        payloads.append(payload)

    if jobs == 1:
        for payload in payloads:
            transform_worker(payload)
    else:
        with ProcessPoolExecutor(max_workers=jobs) as executor:
            for future in as_completed([executor.submit(transform_worker, payload) for payload in payloads]):
                future.result()
    return 0


def _preprocess_stage(
    args: argparse.Namespace,
    *,
    stage: str,
    input_path: Path,
    output_path: Path,
    func: Callable[[], int],
) -> int:
    manifest = manifest_from_args(args)
    started_at = utc_now()
    start_seconds = time.time()
    try:
        if args.resume and complete_file(output_path):
            append_manifest(
                manifest,
                stage=stage,
                status="skipped",
                input_path=input_path,
                output_path=output_path,
                started_at=started_at,
                start_seconds=start_seconds,
                metadata={"reason": "existing_output"},
                checksum=not args.no_checksum,
            )
            warn(f"skip existing {output_path}")
            return 0
        rows = func()
        append_manifest(
            manifest,
            stage=stage,
            status="success",
            input_path=input_path,
            output_path=output_path,
            records=rows,
            started_at=started_at,
            start_seconds=start_seconds,
            checksum=not args.no_checksum,
        )
        ok(f"{stage}: wrote {rows} rows to {output_path}")
        return 0
    except Exception as exc:
        append_manifest(
            manifest,
            stage=stage,
            status="failed",
            input_path=input_path,
            output_path=output_path,
            started_at=started_at,
            start_seconds=start_seconds,
            error_message=repr(exc),
            checksum=not args.no_checksum,
        )
        raise


def cmd_external_scimago(args: argparse.Namespace) -> int:
    input_dir = cfg_path(args, "input_dir", "external.raw.scimago_dir")
    output = cfg_path(args, "output", "external.processed.scimago")
    return _preprocess_stage(
        args,
        stage="external-scimago",
        input_path=input_dir,
        output_path=output,
        func=lambda: preprocess_scimago(
            input_dir, output, start_year=args.start_year, end_year=args.end_year
        ),
    )


def cmd_external_wos(args: argparse.Namespace) -> int:
    input_path = cfg_path(args, "input", "external.raw.web_of_science_csv")
    output = cfg_path(args, "output", "external.processed.web_of_science")
    return _preprocess_stage(
        args,
        stage="external-wos",
        input_path=input_path,
        output_path=output,
        func=lambda: preprocess_wos(input_path, output),
    )


def cmd_external_npi(args: argparse.Namespace) -> int:
    input_path = cfg_path(args, "input", "external.raw.norwegian_list_csv")
    output = cfg_path(args, "output", "external.processed.norwegian_list")
    return _preprocess_stage(
        args,
        stage="external-npi",
        input_path=input_path,
        output_path=output,
        func=lambda: preprocess_npi(input_path, output),
    )


def cmd_external_retraction_watch(args: argparse.Namespace) -> int:
    input_path = cfg_path(args, "input", "external.raw.retraction_watch_csv")
    output = cfg_path(args, "output", "external.processed.retraction_watch")
    return _preprocess_stage(
        args,
        stage="external-retraction-watch",
        input_path=input_path,
        output_path=output,
        func=lambda: preprocess_retraction_watch(input_path, output),
    )


def cmd_external_doaj(args: argparse.Namespace) -> int:
    input_path = cfg_path(args, "input", "external.raw.doaj_csv")
    output = cfg_path(args, "output", "external.processed.doaj")
    return _preprocess_stage(
        args,
        stage="external-doaj",
        input_path=input_path,
        output_path=output,
        func=lambda: preprocess_doaj(input_path, output),
    )


def cmd_external_publisher(args: argparse.Namespace) -> int:
    input_path = cfg_path(args, "publisher_input", "external.raw.publisher_csv")
    output = cfg_path(args, "publisher_output", "external.processed.publisher")
    return _preprocess_stage(
        args,
        stage="external-publisher",
        input_path=input_path,
        output_path=output,
        func=lambda: preprocess_publisher(input_path, output),
    )


def cmd_external_peer_review(args: argparse.Namespace) -> int:
    input_path = cfg_path(args, "peer_review_input", "external.raw.peer_review_csv")
    output = cfg_path(args, "peer_review_output", "external.processed.peer_review")
    return _preprocess_stage(
        args,
        stage="external-peer-review",
        input_path=input_path,
        output_path=output,
        func=lambda: preprocess_peer_review(input_path, output),
    )


def cmd_external_all(args: argparse.Namespace) -> int:
    section("external metadata")
    if getattr(args, "dry_run", False):
        config = cfg(args)
        print_kv_table(
            {
                "external-scimago": config.path("external.processed.scimago"),
                "external-wos": config.path("external.processed.web_of_science"),
                "external-doaj": config.path("external.processed.doaj"),
                "external-npi": config.path("external.processed.norwegian_list"),
                "external-retraction-watch": config.path("external.processed.retraction_watch"),
                "external-publisher": config.path("external.processed.publisher"),
                "external-peer-review": config.path("external.processed.peer_review"),
            }
        )
        info("dry-run external-all: no files or manifest rows will be written")
        return 0
    cmd_external_scimago(args)
    cmd_external_wos(args)
    cmd_external_doaj(args)
    cmd_external_npi(args)
    cmd_external_retraction_watch(args)
    publisher_input = cfg_path(args, "publisher_input", "external.raw.publisher_csv")
    if complete_file(publisher_input):
        cmd_external_publisher(args)
    else:
        warn(f"skip optional publisher metadata {publisher_input}")
    peer_review_input = cfg_path(args, "peer_review_input", "external.raw.peer_review_csv")
    if complete_file(peer_review_input):
        cmd_external_peer_review(args)
    else:
        warn(f"skip optional peer-review metadata {peer_review_input}")
    return 0


def cmd_aggregate(args: argparse.Namespace) -> int:
    manifest = manifest_from_args(args)
    started_at = utc_now()
    start_seconds = time.time()
    input_path = cfg_path(args, "input", "transform.article_shard_dir")
    output_path = cfg_path(args, "output", "aggregate.processed_parquet")
    try:
        if args.resume and complete_file(output_path):
            append_manifest(
                manifest,
                stage="aggregate",
                status="skipped",
                input_path=input_path,
                output_path=output_path,
                started_at=started_at,
                start_seconds=start_seconds,
                metadata={"reason": "existing_output"},
                checksum=not args.no_checksum,
            )
            warn(f"skip existing {output_path}")
            return 0
        rows = aggregate_articles(input_path, output_path)
        append_manifest(
            manifest,
            stage="aggregate",
            status="success",
            input_path=input_path,
            output_path=output_path,
            records=rows,
            started_at=started_at,
            start_seconds=start_seconds,
            checksum=not args.no_checksum,
        )
        ok(f"aggregate: wrote {rows} rows to {output_path}")
        return 0
    except Exception as exc:
        append_manifest(
            manifest,
            stage="aggregate",
            status="failed",
            input_path=input_path,
            output_path=output_path,
            started_at=started_at,
            start_seconds=start_seconds,
            error_message=repr(exc),
            checksum=not args.no_checksum,
        )
        raise


def _expected_shards_from_args(args: argparse.Namespace) -> int:
    value = getattr(args, "shards", None)
    if value is not None:
        return value
    return int(cfg(args).get("transform.default_shards", 64))


def _expected_shard_format_from_args(args: argparse.Namespace) -> str:
    value = getattr(args, "format", None)
    if value:
        return str(value)
    return str(cfg(args).get("transform.article_shard_format", "parquet"))


def cmd_validate_shards(args: argparse.Namespace) -> int:
    input_path = cfg_path(args, "input", "transform.article_shard_dir")
    result = validate_article_shards(
        input_path,
        expected_shards=_expected_shards_from_args(args),
        expected_format=_expected_shard_format_from_args(args),
    )
    if result.ok:
        ok(f"validate-shards: {len(result.shards)} complete {result.expected_format} shards in {input_path}")
        return 0
    for message in result.errors:
        err(message)
    return 1


def cmd_aggregate_all(args: argparse.Namespace) -> int:
    manifest = manifest_from_args(args)
    started_at = utc_now()
    start_seconds = time.time()
    input_path = cfg_path(args, "input", "transform.article_shard_dir")
    parquet_path = cfg_path(args, "parquet", "aggregate.processed_parquet")
    csv_path = cfg_path(args, "csv", "aggregate.processed_csv")
    outputs = [parquet_path, csv_path]
    validation_metadata: dict[str, object] = {}
    if getattr(args, "dry_run", False):
        info(f"dry-run aggregate-all input={input_path} parquet={parquet_path} csv={csv_path}")
        return 0
    if not args.allow_incomplete:
        validation = validate_article_shards(
            input_path,
            expected_shards=_expected_shards_from_args(args),
            expected_format=_expected_shard_format_from_args(args),
        )
        validation_metadata = {"shard_validation": validation.metadata}
        if not validation.ok:
            for message in validation.errors:
                err(message)
            append_manifest(
                manifest,
                stage="aggregate-all",
                status="failed",
                input_path=input_path,
                output_path=parquet_path,
                started_at=started_at,
                start_seconds=start_seconds,
                metadata=validation_metadata,
                error_message="incomplete article shard set",
                checksum=not args.no_checksum,
            )
            return 1
    try:
        if args.resume and all(complete_analysis_output(path) for path in outputs):
            append_manifest(
                manifest,
                stage="aggregate-all",
                status="skipped",
                input_path=input_path,
                output_path=parquet_path,
                started_at=started_at,
                start_seconds=start_seconds,
                metadata={
                    "reason": "existing_outputs",
                    "csv": str(csv_path),
                    **validation_metadata,
                },
                checksum=not args.no_checksum,
            )
            warn(f"skip existing {parquet_path} and {csv_path}")
            return 0
        rows = aggregate_outputs(input_path, outputs)
        append_manifest(
            manifest,
            stage="aggregate-all",
            status="success",
            input_path=input_path,
            output_path=parquet_path,
            records=rows,
            started_at=started_at,
            start_seconds=start_seconds,
            metadata={"csv": str(csv_path), **validation_metadata},
            checksum=not args.no_checksum,
        )
        ok(f"aggregate-all: wrote {rows} rows to {parquet_path} and {csv_path}")
        return 0
    except Exception as exc:
        append_manifest(
            manifest,
            stage="aggregate-all",
            status="failed",
            input_path=input_path,
            output_path=parquet_path,
            started_at=started_at,
            start_seconds=start_seconds,
            metadata={"csv": str(csv_path), **validation_metadata},
            error_message=repr(exc),
            checksum=not args.no_checksum,
        )
        raise


def cmd_list_inputs(args: argparse.Namespace) -> int:
    input_dir = Path(args.input_dir)
    if args.kind == "xml":
        paths = list_xml_paths(input_dir)
    elif args.kind == "json":
        paths = list_json_paths(input_dir)
    else:
        paths = sorted(input_dir.rglob(args.glob))
    paths = limit_paths(paths, args.limit)
    output = Path(args.output)
    with atomic_output_path(output) as tmp_path:
        tmp_path.write_text("".join(f"{path}\n" for path in paths), encoding="utf-8")
    ok(f"listed {len(paths)} {args.kind} paths in {output}")
    return 0


def cmd_init_dirs(args: argparse.Namespace) -> int:
    config = cfg(args)
    dirs = [
        config.path("pubmed.xml_dir"),
        config.path("pubmed.jsonl_dir"),
        config.path("external.raw.scimago_dir"),
        config.path("external.raw.web_of_science_csv").parent,
        config.path("external.raw.doaj_csv").parent,
        config.path("external.raw.norwegian_list_csv").parent,
        config.path("external.raw.retraction_watch_csv").parent,
        config.path("transform.article_shard_dir"),
        config.path("aggregate.processed_parquet").parent,
        config.path("pipeline.manifest").parent,
        config.path("external.processed.pubmed_journals").parent,
    ]
    for directory in dirs:
        directory.mkdir(parents=True, exist_ok=True)
    ok("created canonical data directories")
    return 0


def cmd_preflight(args: argparse.Namespace) -> int:
    config = cfg(args)
    failures = 0
    optional_missing = 0
    inputs = expected_input_paths(config)
    outputs = expected_output_paths(config)
    for expected in inputs:
        path = expected.path
        exists = path.is_dir() if expected.kind == "dir" else path.exists()
        if exists:
            ok(f"{expected.label}: {path}")
        elif expected.required:
            warn(f"missing required {expected.label}: {path} -- {expected.description}")
            failures += 1
        else:
            warn(f"missing optional {expected.label}: {path} -- {expected.description}")
            optional_missing += 1
    for expected in outputs:
        path = expected.path
        exists = path.is_dir() if expected.kind == "dir" else path.exists()
        ok(f"{expected.label}: {path}") if exists else warn(
            f"will create {expected.label}: {path} -- {expected.description}"
        )
    xml_count = (
        len(list_xml_paths(config.path("pubmed.xml_dir"))) if config.path("pubmed.xml_dir").exists() else 0
    )
    print_kv_table(
        {
            "xml_files": xml_count,
            "missing_required_inputs": failures,
            "missing_optional_inputs": optional_missing,
        }
    )
    return 1 if failures else 0


def read_input_list(path: Path) -> list[Path]:
    return [Path(line.strip()) for line in path.read_text(encoding="utf-8").splitlines() if line.strip()]


def transform_shard_payload_to_namespace(payload: dict[str, Any]) -> argparse.Namespace:
    return argparse.Namespace(**payload)


def complete_article_shard(path: Path) -> bool:
    """Return true when a transform shard is present and schema-current."""

    if not complete_file(path):
        return False
    try:
        valid, _errors = validate_analysis_dataset_schema(path)
    except Exception:  # noqa: BLE001 - stale/corrupt shards must be regenerated.
        return False
    return valid


def complete_analysis_output(path: Path) -> bool:
    """Return true when an aggregate output is present and schema-current."""

    if not complete_file(path):
        return False
    try:
        valid, _errors = validate_analysis_dataset_schema(path)
    except Exception:  # noqa: BLE001 - stale/corrupt outputs must be regenerated.
        return False
    return valid


def cmd_transform_shard(args: argparse.Namespace) -> int:
    manifest = manifest_from_args(args)
    started_at = utc_now()
    start_seconds = time.time()
    input_list = Path(args.input_list)
    paths = read_input_list(input_list)
    selected = [path for index, path in enumerate(paths) if index % args.shards == args.shard_index]
    output_dir = cfg_path(args, "output_dir", "transform.article_shard_dir")
    output_path = expected_article_shard_path(output_dir, args.shard_index, args.shards, args.format)
    filters_path = output_dir / f"articles-shard-{args.shard_index:05d}-of-{args.shards:05d}.filters.csv"
    quality_path = output_dir / f"articles-shard-{args.shard_index:05d}-of-{args.shards:05d}.quality.parquet"

    if not selected:
        if args.resume and complete_article_shard(output_path):
            append_manifest(
                manifest,
                stage="transform-shard",
                status="skipped",
                input_path=input_list,
                output_path=output_path,
                started_at=started_at,
                start_seconds=start_seconds,
                metadata={
                    "reason": "existing_output",
                    "shard_index": args.shard_index,
                    "shards": args.shards,
                    "inputs": 0,
                },
                checksum=not args.no_checksum,
            )
            warn(f"skip existing {output_path}")
            return 0
        counts = {stage: 0 for stage in FILTER_STAGES}
        write_frame(
            output_path,
            pl.DataFrame({col: [] for col in CANONICAL_ARTICLE_COLUMNS}),
        )
        write_frame(
            filters_path,
            pl.DataFrame(
                {
                    "stage": list(FILTER_STAGES),
                    "count": [0 for _ in FILTER_STAGES],
                    "dropped": [0 for _ in FILTER_STAGES],
                    "drop_reason": list(FILTER_STAGES),
                    "kept_percent": [100.0 for _ in FILTER_STAGES],
                }
            ),
        )
        write_frame(
            quality_path,
            pl.DataFrame(
                {
                    "record_type": [],
                    "checkpoint": [],
                    "source": [],
                    "year": [],
                    "variable": [],
                    "metric": [],
                    "numerator": [],
                    "denominator": [],
                    "value": [],
                }
            ),
        )
        append_manifest(
            manifest,
            stage="transform-shard",
            status="success",
            input_path=input_list,
            output_path=output_path,
            records=0,
            started_at=started_at,
            start_seconds=start_seconds,
            metadata={
                "counts": counts,
                "shard_index": args.shard_index,
                "shards": args.shards,
                "inputs": 0,
                "empty_selection": True,
            },
            checksum=not args.no_checksum,
        )
        ok(f"transform-shard {args.shard_index}/{args.shards}: wrote empty {output_path}")
        return 0
    if args.resume and complete_article_shard(output_path):
        append_manifest(
            manifest,
            stage="transform-shard",
            status="skipped",
            input_path=input_list,
            output_path=output_path,
            started_at=started_at,
            start_seconds=start_seconds,
            metadata={
                "reason": "existing_output",
                "shard_index": args.shard_index,
                "shards": args.shards,
            },
            checksum=not args.no_checksum,
        )
        warn(f"skip existing {output_path}")
        return 0

    result = transform_files(
        selected,
        output_path,
        filters_path=filters_path,
        external=external_inputs_from_args(args),
        min_received=min_received_from_args(args),
    )
    append_manifest(
        manifest,
        stage="transform-shard",
        status="success",
        input_path=input_list,
        output_path=output_path,
        records=result.counts.get("final_rows", 0),
        started_at=started_at,
        start_seconds=start_seconds,
        metadata={
            "counts": dict(result.counts),
            "shard_index": args.shard_index,
            "shards": args.shards,
            "inputs": len(selected),
            "quality_path": str(result.quality_path or ""),
        },
        checksum=not args.no_checksum,
    )
    ok(f"transform-shard {args.shard_index}/{args.shards}: wrote {result.output_path}")
    print_kv_table(result.counts)
    return 0


def transform_shard_worker(payload: dict[str, Any]) -> int:
    return cmd_transform_shard(transform_shard_payload_to_namespace(payload))


def cmd_transform_shards(args: argparse.Namespace) -> int:
    json_dir = cfg_path(args, "input_dir", "pubmed.jsonl_dir")
    manifest_dir = cfg_path(
        args,
        "input_list",
        "pipeline.transform_inputs",
        "data/manifests/transform_inputs.txt",
    ).parent
    manifest_dir.mkdir(parents=True, exist_ok=True)
    input_list = Path(args.input_list) if args.input_list else manifest_dir / "transform_inputs.txt"
    paths = limit_paths(list_json_paths(json_dir), args.limit)
    if not paths:
        err(f"No JSON/JSONL files found in {json_dir}")
        return 1
    if getattr(args, "dry_run", False):
        jobs = args.jobs if args.jobs is not None else min(args.shards, max((os.cpu_count() or 2) - 1, 1))
        info(
            f"dry-run transform-shards files={len(paths)} shards={args.shards} jobs={jobs} input_list={input_list}"
        )
        return 0
    with atomic_output_path(input_list) as tmp_path:
        tmp_path.write_text("".join(f"{path}\n" for path in paths), encoding="utf-8")
    jobs = args.jobs if args.jobs is not None else min(args.shards, max((os.cpu_count() or 2) - 1, 1))
    info(f"transform-shards files={len(paths)} shards={args.shards} jobs={jobs} input_list={input_list}")
    payloads = []
    for shard_index in range(args.shards):
        payload = {key: value for key, value in vars(args).items() if key != "func"}
        payload["input_list"] = str(input_list)
        payload["shard_index"] = shard_index
        payloads.append(payload)
    if jobs == 1:
        for payload in payloads:
            transform_shard_worker(payload)
    else:
        with ProcessPoolExecutor(max_workers=jobs) as executor:
            for future in as_completed(
                [executor.submit(transform_shard_worker, payload) for payload in payloads]
            ):
                future.result()
    return 0


def _print_manifest_rows(rows: list[dict[str, Any]], *, as_json: bool = False) -> int:
    if as_json:
        print(json.dumps(rows, ensure_ascii=False, sort_keys=True))
        return 0
    if not rows:
        warn("manifest is empty")
        return 0
    for row in rows:
        print_kv_table(row)
        print("---")
    return 0


def cmd_manifest(args: argparse.Namespace) -> int:
    return _print_manifest_rows(manifest_from_args(args).rows(limit=args.limit))


def cmd_manifest_summary(args: argparse.Namespace) -> int:
    rows = manifest_from_args(args).summary()
    return _print_manifest_rows(rows, as_json=args.json)


def cmd_manifest_failed(args: argparse.Namespace) -> int:
    rows = manifest_from_args(args).rows(limit=args.limit, status="failed")
    return _print_manifest_rows(rows, as_json=args.json)


def cmd_manifest_show(args: argparse.Namespace) -> int:
    rows = manifest_from_args(args).rows(limit=args.limit)
    return _print_manifest_rows(rows, as_json=args.json)


def cmd_manifest_retry_script(args: argparse.Namespace) -> int:
    rows = manifest_from_args(args).rows(limit=args.limit, status="failed")
    for row in reversed(rows):
        stage = row.get("stage", "")
        input_path = row.get("input_path", "")
        output_path = row.get("output_path", "")
        print(f"# retry {stage} failed run {row.get('id')}")
        print(f"# input={input_path} output={output_path}")
    return 0


def _manifest_integrity(path: Path) -> tuple[bool, str]:
    if not path.exists():
        return False, "missing"
    try:
        conn = sqlite3.connect(f"file:{path}?mode=ro", uri=True)
        try:
            row = conn.execute("PRAGMA integrity_check").fetchone()
        finally:
            conn.close()
    except sqlite3.DatabaseError as exc:
        return False, str(exc)
    except OSError as exc:
        return False, str(exc)
    result = str(row[0]) if row else "empty integrity_check result"
    return result.lower() == "ok", result


def _archive_manifest_files(path: Path, archive_dir: Path | None = None) -> list[Path]:
    stamp = utc_now().replace(":", "").replace("+", "Z")
    target_dir = archive_dir or path.parent
    target_dir.mkdir(parents=True, exist_ok=True)
    moved: list[Path] = []
    for candidate in (path, Path(f"{path}-wal"), Path(f"{path}-shm")):
        if not candidate.exists():
            continue
        archived = target_dir / f"{candidate.name}.corrupt.{stamp}"
        candidate.replace(archived)
        moved.append(archived)
    return moved


def cmd_manifest_check(args: argparse.Namespace) -> int:
    """Run SQLite integrity checks and optionally archive corrupt manifest files."""
    path = manifest_path_from_args(args)
    ok_integrity, detail = _manifest_integrity(path)
    if ok_integrity:
        print_kv_table({"manifest": str(path), "integrity": detail})
        return 0
    result: dict[str, Any] = {"manifest": str(path), "integrity": detail}
    if args.cleanup:
        archived = _archive_manifest_files(path, Path(args.archive_dir) if args.archive_dir else None)
        result["archived"] = [str(item) for item in archived]
    print_kv_table(result)
    return 1


def _read_manifest_rows(path: Path) -> list[ManifestRow]:
    with sqlite3.connect(f"file:{path}?mode=ro", uri=True) as conn:
        cur = conn.execute(
            """
            SELECT stage, status, input_path, output_path, input_sha256,
                   output_sha256, input_bytes, output_bytes, records, deleted,
                   started_at, finished_at, elapsed_seconds, worker,
                   metadata_json, error
            FROM runs
            ORDER BY id
            """
        )
        rows: list[ManifestRow] = []
        for row in cur.fetchall():
            rows.append(
                ManifestRow(
                    stage=row[0],
                    status=row[1],
                    input_path=row[2],
                    output_path=row[3],
                    input_sha256=row[4],
                    output_sha256=row[5],
                    input_bytes=row[6],
                    output_bytes=row[7],
                    records=row[8],
                    deleted=row[9],
                    started_at=row[10],
                    finished_at=row[11],
                    elapsed_seconds=row[12],
                    worker=row[13],
                    metadata=json.loads(str(row[14] or "{}")),
                    error=row[15],
                )
            )
        return rows


def cmd_manifest_collect(args: argparse.Namespace) -> int:
    """Append rows from per-task SLURM manifests into a consolidated manifest."""
    target = manifest_from_args(args)
    input_dir = Path(args.input_dir) if args.input_dir else manifest_path_from_args(args).parent / "slurm"
    files = sorted(input_dir.rglob(args.glob)) if input_dir.exists() else []
    merged = 0
    corrupt: list[str] = []
    for path in files:
        if path == target.path:
            continue
        ok_integrity, detail = _manifest_integrity(path)
        if not ok_integrity:
            corrupt.append(f"{path}: {detail}")
            if args.cleanup_corrupt:
                _archive_manifest_files(path, Path(args.archive_dir) if args.archive_dir else None)
            continue
        for row in _read_manifest_rows(path):
            target.append(row)
            merged += 1
    print_kv_table({"input_dir": str(input_dir), "files": len(files), "rows": merged, "corrupt": len(corrupt)})
    for item in corrupt:
        warn(item)
    return 1 if corrupt else 0


def cmd_compare_outputs(args: argparse.Namespace) -> int:
    output = Path(args.output)
    result = compare_outputs(Path(args.baseline), Path(args.candidate), output)
    ok(f"wrote differential validation report to {result.report_path}")
    print_kv_table({"differences": result.rows, **result.categories})
    return 0


def cmd_schema(args: argparse.Namespace) -> int:
    if args.input:
        valid, errors = validate_analysis_dataset_schema(Path(args.input))
        print_kv_table({"schema": ANALYSIS_DATASET_VERSION, "columns": len(CANONICAL_ARTICLE_COLUMNS)})
        if valid:
            ok(f"{args.input} matches {ANALYSIS_DATASET_VERSION}")
            return 0
        for error in errors:
            err(error)
        return 1
    print(ANALYSIS_DATASET_VERSION)
    for index, column in enumerate(CANONICAL_ARTICLE_COLUMNS, start=1):
        print(f"{index:02d}\t{column}")
    return 0


def cmd_provenance(args: argparse.Namespace) -> int:
    errors = validate_provenance()
    if errors:
        for error in errors:
            err(error)
        return 1
    if args.output:
        write_provenance(Path(args.output), args.format)
        ok(f"wrote variable provenance to {args.output}")
    else:
        print(render_provenance(args.format), end="")
    return 0


def cmd_smoke_live(args: argparse.Namespace) -> int:
    result = run_live_smoke(
        cfg(args),
        Path(args.workspace),
        pubmed_files=args.pubmed_files,
        jobs=args.jobs,
        shards=args.shards,
        resume=args.resume,
    )
    ok(f"live smoke passed with {result.rows} processed rows")
    print_kv_table(
        {
            "workspace": str(result.workspace),
            "debrief": str(result.debrief_path),
            "sources": result.sources,
        }
    )
    return 0


def cmd_summaries(args: argparse.Namespace) -> int:
    manifest = manifest_from_args(args)
    started_at = utc_now()
    start_seconds = time.time()
    input_path = cfg_path(args, "input", "aggregate.processed_parquet")
    output_dir = cfg_path(args, "output_dir", "aggregate.summary_dir")
    try:
        outputs = derive_summary_tables(input_path, output_dir)
        append_manifest(
            manifest,
            stage="summaries",
            status="success",
            input_path=input_path,
            output_path=output_dir,
            records=len(outputs),
            started_at=started_at,
            start_seconds=start_seconds,
            metadata={"tables": {key: str(value) for key, value in outputs.items()}},
            checksum=not args.no_checksum,
        )
        ok(f"wrote {len(outputs)} summary tables to {output_dir}")
        return 0
    except Exception as exc:
        append_manifest(
            manifest,
            stage="summaries",
            status="failed",
            input_path=input_path,
            output_path=output_dir,
            started_at=started_at,
            start_seconds=start_seconds,
            error_message=repr(exc),
            checksum=not args.no_checksum,
        )
        raise


def cmd_run_analysis(args: argparse.Namespace) -> int:
    manifest = manifest_from_args(args)
    started_at = utc_now()
    start_seconds = time.time()
    config = cfg(args)
    cwd = cfg_path(args, "cwd", "analysis.cwd")
    input_path = cfg_path(args, "input", "analysis.input")
    output_dir = cfg_path(args, "output_dir", "analysis.output_dir")
    command = list(getattr(args, "command", None) or config.get("analysis.command", []))
    if not command:
        raise ConfigError("analysis.command must contain at least one argument")
    output_dir.mkdir(parents=True, exist_ok=True)
    try:
        completed = subprocess.run(
            command,
            cwd=cwd,
            check=False,
            text=True,
            capture_output=True,
            env={**os.environ, "PUBDELAYS_ANALYSIS_INPUT": str(input_path), "PUBDELAYS_ANALYSIS_OUTPUT_DIR": str(output_dir)},
        )
        metadata = {
            "command": command,
            "cwd": str(cwd),
            "returncode": completed.returncode,
            "stdout_tail": completed.stdout[-4000:],
            "stderr_tail": completed.stderr[-4000:],
        }
        append_manifest(
            manifest,
            stage="run-analysis",
            status="success" if completed.returncode == 0 else "failed",
            input_path=input_path,
            output_path=output_dir,
            records=0,
            started_at=started_at,
            start_seconds=start_seconds,
            metadata=metadata,
            error_message="" if completed.returncode == 0 else completed.stderr[-4000:],
            checksum=not args.no_checksum,
        )
        if completed.stdout:
            print(completed.stdout, end="")
        if completed.stderr:
            err(completed.stderr.rstrip())
        return completed.returncode
    except Exception as exc:
        append_manifest(
            manifest,
            stage="run-analysis",
            status="failed",
            input_path=input_path,
            output_path=output_dir,
            started_at=started_at,
            start_seconds=start_seconds,
            error_message=repr(exc),
            checksum=not args.no_checksum,
        )
        raise


def cmd_validate_analysis(args: argparse.Namespace) -> int:
    manifest = manifest_from_args(args)
    started_at = utc_now()
    start_seconds = time.time()
    input_path = cfg_path(args, "input", "aggregate.processed_parquet")
    output_dir = cfg_path(args, "output_dir", "validation.report_dir")
    filtered_output = cfg_path(args, "filtered_output", "validation.filtered_output") if args.filtered_output != "" else None
    excluded_output = (
        cfg_path(args, "excluded_output", "validation.excluded_output", "data/processed_data/excluded_validation.parquet")
        if args.excluded_output != ""
        else None
    )
    config = cfg(args)
    try:
        result = validate_analysis_output(
            input_path,
            output_dir,
            filtered_output=filtered_output,
            excluded_output=excluded_output,
            min_article_date=date.fromisoformat(str(config.get("validation.min_article_date"))),
            max_article_date=date.fromisoformat(str(config.get("validation.max_article_date"))),
            min_delay_days=int(config.get("validation.min_delay_days")),
            max_delay_days=int(config.get("validation.max_delay_days")),
        )
        append_manifest(
            manifest,
            stage="validate-analysis",
            status="success",
            input_path=input_path,
            output_path=output_dir,
            records=result.rows_kept,
            started_at=started_at,
            start_seconds=start_seconds,
            metadata={
                "rows_in": result.rows_in,
                "rows_kept": result.rows_kept,
                "failed_checks": result.failed_checks,
                "tables": {key: str(value) for key, value in result.tables.items()},
                "filtered_output": str(result.filtered_output or ""),
                "excluded_output": str(result.excluded_output or ""),
            },
            checksum=not args.no_checksum,
        )
        ok(f"wrote {len(result.tables)} validation tables to {output_dir}")
        print_kv_table({"rows_in": result.rows_in, "rows_kept": result.rows_kept, "failed_checks": result.failed_checks})
        return 1 if args.strict_checks and result.failed_checks else 0
    except Exception as exc:
        append_manifest(
            manifest,
            stage="validate-analysis",
            status="failed",
            input_path=input_path,
            output_path=output_dir,
            started_at=started_at,
            start_seconds=start_seconds,
            error_message=repr(exc),
            checksum=not args.no_checksum,
        )
        raise


def cmd_filter_counts(args: argparse.Namespace) -> int:
    manifest = manifest_from_args(args)
    started_at = utc_now()
    start_seconds = time.time()
    input_path = cfg_path(args, "input", "transform.article_shard_dir")
    output_path = cfg_path(args, "output", "aggregate.filter_counts")
    try:
        final_dataset = cfg_path(args, "dataset", "aggregate.processed_parquet")
        records = collect_filter_counts(
            input_path,
            output_path,
            final_dataset=final_dataset if final_dataset.exists() else None,
        )
        append_manifest(
            manifest,
            stage="filter-counts",
            status="success",
            input_path=input_path,
            output_path=output_path,
            records=records,
            started_at=started_at,
            start_seconds=start_seconds,
            checksum=not args.no_checksum,
        )
        ok(f"wrote filter counts to {output_path}")
        return 0
    except Exception as exc:
        append_manifest(
            manifest,
            stage="filter-counts",
            status="failed",
            input_path=input_path,
            output_path=output_path,
            started_at=started_at,
            start_seconds=start_seconds,
            error_message=repr(exc),
            checksum=not args.no_checksum,
        )
        raise


def cmd_quality_report(args: argparse.Namespace) -> int:
    manifest = manifest_from_args(args)
    started_at = utc_now()
    start_seconds = time.time()
    input_dir = cfg_path(args, "input", "transform.article_shard_dir")
    dataset = cfg_path(args, "dataset", "aggregate.processed_parquet")
    output_dir = cfg_path(
        args, "output_dir", "quality.report_dir", "data/processed_data/quality"
    )
    try:
        result = build_quality_report(input_dir, dataset, output_dir)
        append_manifest(
            manifest,
            stage="quality-report",
            status="success",
            input_path=dataset,
            output_path=output_dir,
            records=result.rows,
            started_at=started_at,
            start_seconds=start_seconds,
            metadata={"input_dir": str(input_dir), "tables": {key: str(value) for key, value in result.tables.items()}},
            checksum=not args.no_checksum,
        )
        ok(f"wrote {len(result.tables)} quality tables to {output_dir}")
        return 0
    except Exception as exc:
        append_manifest(
            manifest,
            stage="quality-report",
            status="failed",
            input_path=dataset,
            output_path=output_dir,
            started_at=started_at,
            start_seconds=start_seconds,
            error_message=repr(exc),
            checksum=not args.no_checksum,
        )
        raise


SLURM_STAGE_CONFIG = {
    "download": "download",
    "download-external": "download_external",
    "external-all": "external_all",
    "parse": "parse",
    "parse-baseline": "parse",
    "parse-updatefiles": "parse",
    "resolve-state": "prepare_transform",
    "prepare-transform": "prepare_transform",
    "transform-shards": "transform_shards",
    "aggregate-all": "aggregate_all",
}


def slurm_resources(config: PipelineConfig, stage: str) -> SlurmResources:
    key = SLURM_STAGE_CONFIG[stage]
    prefix = f"slurm.resources.{key}"
    return SlurmResources(
        cpus_per_task=int(config.get(f"{prefix}.cpus_per_task", 1)),
        mem=str(config.get(f"{prefix}.mem", "4G")),
        time=str(config.get(f"{prefix}.time", "01:00:00")),
        partition=str(config.get("slurm.partition", "") or ""),
        account=str(config.get("slurm.account", "") or ""),
        qos=str(config.get("slurm.qos", "") or ""),
    )


def slurm_runner(args: argparse.Namespace, config: PipelineConfig) -> list[str]:
    return shlex.split(args.runner or str(config.get("slurm.runner", "uv run pubdelays")))


def slurm_log_dir(config: PipelineConfig, args: argparse.Namespace) -> Path:
    value = args.log_dir or str(config.get("slurm.log_dir", "logs/slurm"))
    return resolve_config_path(value, config.root)


def slurm_config_arg(args: argparse.Namespace) -> str:
    return str(Path(getattr(args, "config", DEFAULT_CONFIG)))


def write_path_list(path: Path, paths: list[Path]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with atomic_output_path(path) as tmp_path:
        tmp_path.write_text("".join(f"{item}\n" for item in paths), encoding="utf-8")


def _resolve_array_throttle(
    args: argparse.Namespace, config: PipelineConfig, array_spec: str
) -> int | None:
    """Return an array throttle value (%N) from CLI args, config, or SLURM query."""
    # CLI override takes precedence.
    throttle = getattr(args, "array_throttle", None)
    if throttle is not None:
        return throttle
    # Determine max_array_size: config -> SLURM query.
    max_array_size = int(config.get("slurm.max_array_size", 0) or 0)
    if max_array_size <= 0:
        max_array_size = query_max_array_size() or 0
    if max_array_size <= 0:
        return None
    # Parse the upper bound from specs like "0-63" or "0-100".
    upper = array_spec.split("-")[-1].strip()
    try:
        total = int(upper) + 1
    except ValueError:
        return None
    if total <= max_array_size:
        return None
    return max_array_size


def build_slurm_job(args: argparse.Namespace, stage: str) -> tuple[SlurmJob, dict[str, Any]]:
    config = cfg(args)
    runner = slurm_runner(args, config)
    log_dir = slurm_log_dir(config, args)
    resources = slurm_resources(config, stage)
    base = [*runner, "--config", slurm_config_arg(args)]
    metadata: dict[str, Any] = {"stage": stage}
    repo_setup = [f"cd {shlex.quote(str(config.root))}"]

    if stage == "download":
        command = [*base, "download", "--source", args.source, "--jobs", str(args.jobs), "--resume"]
        if args.limit is not None:
            command.extend(["--limit", str(args.limit)])
        if args.output_dir:
            command.extend(["--output-dir", args.output_dir])
        return SlurmJob(
            "pubdelays-download", command, resources, log_dir, dependency=args.dependency, setup=repo_setup
        ), metadata

    if stage == "download-external":
        command = [
            *base,
            "download-external",
            "--source",
            args.external_source,
            "--start-year",
            str(args.start_year),
            "--end-year",
            str(args.end_year),
            "--resume",
        ]
        return SlurmJob(
            "pubdelays-download-external",
            command,
            resources,
            log_dir,
            dependency=args.dependency,
            setup=repo_setup,
        ), metadata

    if stage == "external-all":
        command = [
            *base,
            "external-all",
            "--start-year",
            str(args.start_year),
            "--end-year",
            str(args.end_year),
            "--resume",
        ]
        return SlurmJob(
            "pubdelays-external", command, resources, log_dir, dependency=args.dependency, setup=repo_setup
        ), metadata

    if stage in {"parse", "parse-baseline", "parse-updatefiles"}:
        source = {
            "parse": "legacy",
            "parse-baseline": "baseline",
            "parse-updatefiles": "updatefiles",
        }[stage]
        input_key = {
            "legacy": "pubmed.xml_dir",
            "baseline": "pubmed.baseline_xml_dir",
            "updatefiles": "pubmed.update_xml_dir",
        }[source]
        output_key = {
            "legacy": "pubmed.jsonl_dir",
            "baseline": "pubmed.baseline_jsonl_dir",
            "updatefiles": "pubmed.update_jsonl_dir",
        }[source]
        input_dir = cfg_path(
            args, "input_dir", input_key, str(config.get("pubmed.xml_dir"))
        )
        output_dir = cfg_path(
            args, "output_dir", output_key, str(config.get("pubmed.jsonl_dir"))
        )
        default_list = config.path("pipeline.parse_inputs")
        input_list = (
            Path(args.input_list)
            if args.input_list
            else default_list.with_name(f"parse_{source}_inputs.txt")
        )
        paths = list_xml_paths(input_dir)
        metadata.update({"inputs": len(paths), "input_list": str(input_list)})
        if not paths and not args.dry_run:
            raise RuntimeError(f"No XML/XML.GZ files found in {input_dir}")
        if not args.dry_run:
            write_path_list(input_list, paths)
        manifest_dir = config.path("pipeline.manifest").parent / "slurm" / stage
        setup = [
            *repo_setup,
            'PUBDELAYS_ARRAY_TASK_OFFSET="${PUBDELAYS_ARRAY_TASK_OFFSET:-0}"',
            'PUBDELAYS_ARRAY_TASK_ID="$((SLURM_ARRAY_TASK_ID + PUBDELAYS_ARRAY_TASK_OFFSET))"',
            f"PUBDELAYS_STAGE_MANIFEST_DIR={shlex.quote(str(manifest_dir))}",
            'mkdir -p "$PUBDELAYS_STAGE_MANIFEST_DIR"',
            'PUBDELAYS_STAGE_MANIFEST="$PUBDELAYS_STAGE_MANIFEST_DIR/${SLURM_ARRAY_JOB_ID:-local}-${PUBDELAYS_ARRAY_TASK_ID}.sqlite"',
            f"INPUT_LIST={shlex.quote(str(input_list))}",
            'INPUT=$(sed -n "$((PUBDELAYS_ARRAY_TASK_ID + 1))p" "$INPUT_LIST")',
            '[[ -n "$INPUT" ]] || { echo "No input for PUBDELAYS_ARRAY_TASK_ID=$PUBDELAYS_ARRAY_TASK_ID" >&2; exit 2; }',
        ]
        command = (
            f'{shlex.join(base)} parse-one --input "$INPUT" --output-dir {shlex.quote(str(output_dir))} '
            '--format jsonl --parse-mesh-subterms --manifest "$PUBDELAYS_STAGE_MANIFEST" --resume'
        )
        array_spec = f"0-{max(len(paths), 1) - 1}"
        array_throttle = _resolve_array_throttle(args, config, array_spec)
        job = SlurmJob(
            f"pubdelays-{stage}",
            command,
            resources,
            log_dir,
            array=array_spec,
            array_throttle=array_throttle,
            dependency=args.dependency,
            setup=setup,
        )
        return job, metadata

    if stage == "resolve-state":
        command = [*base, "resolve-state", "--resume"]
        return SlurmJob(
            "pubdelays-resolve-state",
            command,
            resources,
            log_dir,
            dependency=args.dependency,
            setup=repo_setup,
        ), metadata

    if stage == "prepare-transform":
        input_dir = cfg_path(args, "input_dir", "pubmed.resolved_jsonl_dir")
        input_list = Path(args.input_list) if args.input_list else config.path("pipeline.transform_inputs")
        command = [
            *base,
            "list-inputs",
            "--kind",
            "json",
            "--input-dir",
            str(input_dir),
            "--output",
            str(input_list),
        ]
        if args.limit is not None:
            command.extend(["--limit", str(args.limit)])
        metadata.update({"input_dir": str(input_dir), "input_list": str(input_list)})
        return SlurmJob(
            "pubdelays-prepare-transform",
            command,
            resources,
            log_dir,
            dependency=args.dependency,
            setup=repo_setup,
        ), metadata

    if stage == "transform-shards":
        input_dir = cfg_path(args, "input_dir", "pubmed.resolved_jsonl_dir")
        output_dir = cfg_path(args, "output_dir", "transform.article_shard_dir")
        input_list = Path(args.input_list) if args.input_list else config.path("pipeline.transform_inputs")
        use_existing_list = bool(getattr(args, "use_existing_input_list", False))
        metadata.update({"input_list": str(input_list), "shards": args.shards})
        if not use_existing_list:
            paths = limit_paths(list_json_paths(input_dir), args.limit)
            metadata.update({"inputs": len(paths), "limit": args.limit})
            if not paths and not args.dry_run:
                raise RuntimeError(f"No JSON/JSONL files found in {input_dir}")
            if not args.dry_run:
                write_path_list(input_list, paths)
        manifest_dir = config.path("pipeline.manifest").parent / "slurm" / "transform-shards"
        setup = [
            *repo_setup,
            'PUBDELAYS_ARRAY_TASK_OFFSET="${PUBDELAYS_ARRAY_TASK_OFFSET:-0}"',
            'PUBDELAYS_ARRAY_TASK_ID="$((SLURM_ARRAY_TASK_ID + PUBDELAYS_ARRAY_TASK_OFFSET))"',
            f"PUBDELAYS_STAGE_MANIFEST_DIR={shlex.quote(str(manifest_dir))}",
            'mkdir -p "$PUBDELAYS_STAGE_MANIFEST_DIR"',
            'PUBDELAYS_STAGE_MANIFEST="$PUBDELAYS_STAGE_MANIFEST_DIR/${SLURM_ARRAY_JOB_ID:-local}-${PUBDELAYS_ARRAY_TASK_ID}.sqlite"',
        ]
        command = (
            f"{shlex.join(base)} transform-shard --input-list {shlex.quote(str(input_list))} "
            f'--output-dir {shlex.quote(str(output_dir))} --shard-index "$PUBDELAYS_ARRAY_TASK_ID" '
            f'--shards {args.shards} --format {shlex.quote(args.format)} '
            '--manifest "$PUBDELAYS_STAGE_MANIFEST" --resume'
        )
        array_spec = f"0-{args.shards - 1}"
        array_throttle = _resolve_array_throttle(args, config, array_spec)
        job = SlurmJob(
            "pubdelays-transform",
            command,
            resources,
            log_dir,
            array=array_spec,
            array_throttle=array_throttle,
            dependency=args.dependency,
            setup=setup,
        )
        return job, metadata

    if stage == "aggregate-all":
        command = [*base, "aggregate-all", "--resume"]
        if args.shards is not None:
            command.extend(["--shards", str(args.shards)])
        if args.format is not None:
            command.extend(["--format", args.format])
        return SlurmJob(
            "pubdelays-aggregate", command, resources, log_dir, dependency=args.dependency, setup=repo_setup
        ), metadata

    raise ValueError(f"unsupported SLURM stage: {stage}")


def _parse_array_upper(array_spec: str) -> int | None:
    """Parse the upper bound from an array spec like '0-63' or '0-100:2'."""
    range_part = array_spec.split("%")[0]
    upper_str = range_part.split("-")[-1].split(":")[0].strip()
    try:
        return int(upper_str)
    except ValueError:
        return None


def _split_array_chunks(upper: int, max_size: int) -> list[tuple[int, int]]:
    """Split array range 0..upper into chunks of at most max_size tasks.

    Returns a list of (start, end) tuples.
    """
    chunks: list[tuple[int, int]] = []
    start = 0
    while start <= upper:
        end = min(start + max_size - 1, upper)
        chunks.append((start, end))
        start = end + 1
    return chunks


def _split_job_array(job: SlurmJob, chunks: list[tuple[int, int]]) -> list[SlurmJob]:
    """Create one SlurmJob per array chunk with a suffixed name.

    Each emitted array starts at zero because some clusters reject task IDs
    greater than MaxArraySize - 1 even when the number of tasks is valid.
    """
    jobs: list[SlurmJob] = []
    total = len(chunks)
    width = len(str(total))
    for idx, (start, end) in enumerate(chunks, start=1):
        chunk_label = f"-chunk{idx:0{width}d}" if total > 1 else ""
        array_spec = f"0-{end - start}"
        setup = [f"PUBDELAYS_ARRAY_TASK_OFFSET={start}", *job.setup] if start else job.setup
        jobs.append(
            SlurmJob(
                name=f"{job.name}{chunk_label}",
                command=job.command,
                resources=job.resources,
                log_dir=job.log_dir,
                array=array_spec,
                array_throttle=job.array_throttle,
                dependency=job.dependency,
                setup=setup,
            )
        )
    return jobs


def emit_or_submit_slurm(args: argparse.Namespace, job: SlurmJob) -> list[str]:
    """Submit a SlurmJob, splitting the array if it exceeds MaxArraySize.

    Returns a list of submitted job IDs (empty list for dry-run).
    """
    # Determine the effective MaxArraySize.
    raw_max = getattr(args, "config_max_array_size", None)
    cfg_max = int(raw_max) if raw_max is not None else None
    max_size = cfg_max if cfg_max and cfg_max > 0 else query_max_array_size()

    # Check whether the array needs splitting.
    chunks: list[tuple[int, int]] | None = None
    if job.array and max_size and max_size > 0:
        upper = _parse_array_upper(job.array)
        if upper is not None and upper + 1 > max_size:
            chunks = _split_array_chunks(upper, max_size)

    if chunks is not None and len(chunks) > 1:
        # Split into multiple job arrays.
        split_jobs = _split_job_array(job, chunks)
        job_ids: list[str] = []
        for chunk_job in split_jobs:
            script = build_sbatch_script(chunk_job)
            if args.dry_run:
                print(script, end="")
                continue
            chunk_job.log_dir.mkdir(parents=True, exist_ok=True)
            try:
                job_id = submit_sbatch(script)
            except SlurmSubmissionError as exc:
                err(str(exc))
                if exc.stdout.strip():
                    err(f"sbatch stdout: {exc.stdout.strip()}")
                err(f"failed script for {chunk_job.name}:")
                err(script)
                if chunk_job.array and "array" in str(exc).lower():
                    err("hint: check SLURM MaxArraySize with: scontrol show config | grep MaxArraySize")
                    err("hint: use --array-throttle N to limit concurrent array tasks")
                    err("hint: set slurm.max_array_size in config to auto-throttle")
                raise
            ok(f"submitted {chunk_job.name} job_id={job_id}")
            job_ids.append(job_id)
        return job_ids

    # Single job (no splitting needed).
    script = build_sbatch_script(job)
    if args.dry_run:
        print(script, end="")
        return []
    job.log_dir.mkdir(parents=True, exist_ok=True)
    try:
        job_id = submit_sbatch(script)
    except SlurmSubmissionError as exc:
        err(str(exc))
        if exc.stdout.strip():
            err(f"sbatch stdout: {exc.stdout.strip()}")
        if job.array and "array" in str(exc).lower():
            err("hint: check SLURM MaxArraySize with: scontrol show config | grep MaxArraySize")
            err("hint: use --array-throttle N to limit concurrent array tasks")
            err("hint: set slurm.max_array_size in config to auto-throttle")
        raise
    ok(f"submitted {job.name} job_id={job_id}")
    return [job_id]


def cmd_slurm_submit(args: argparse.Namespace) -> int:
    try:
        job, metadata = build_slurm_job(args, args.stage)
    except RuntimeError as exc:
        err(str(exc))
        return 1
    if args.dry_run:
        info(f"dry-run slurm submit: {metadata}")
    try:
        job_ids = emit_or_submit_slurm(args, job)
    except SlurmSubmissionError:
        return 1
    if job_ids:
        info(f"submitted job ids: {', '.join(job_ids)}")
    return 0


def _build_dependency(job_ids: list[str]) -> str | None:
    """Build an afterok dependency string from one or more job IDs."""
    if not job_ids:
        return None
    return "afterok:" + ":".join(job_ids)


def cmd_slurm_workflow(args: argparse.Namespace) -> int:
    stages = [stage.strip() for stage in args.stages.split(",") if stage.strip()]
    dependency = args.dependency
    submitted: dict[str, str] = {}
    for stage in stages:
        stage_args = argparse.Namespace(**vars(args))
        stage_args.stage = stage
        stage_args.dependency = dependency
        stage_args.use_existing_input_list = stage == "transform-shards"
        if stage in {"parse", "parse-baseline", "parse-updatefiles"}:
            stage_args.input_dir = args.parse_input_dir
            stage_args.output_dir = args.parse_output_dir
        elif stage in {"prepare-transform", "transform-shards"}:
            stage_args.input_dir = args.transform_input_dir
            stage_args.output_dir = args.transform_output_dir
        job, _metadata = build_slurm_job(stage_args, stage)
        try:
            job_ids = emit_or_submit_slurm(stage_args, job)
        except SlurmSubmissionError:
            return 1
        if job_ids:
            submitted[stage] = ", ".join(job_ids)
            dependency = _build_dependency(job_ids)
    if submitted:
        print_kv_table(submitted)
    return 0


def cmd_slurm_status(args: argparse.Namespace) -> int:
    try:
        statuses = SlurmSubmitter().status(args.job_id)
    except SlurmQueryError as exc:
        err(str(exc))
        if exc.stdout.strip():
            err(f"sacct stdout: {exc.stdout.strip()}")
        return 1
    if not statuses:
        warn(f"no SLURM accounting rows found for {args.job_id}")
        return 1
    for status in statuses:
        print_kv_table(
            {
                "job_id": status.job_id,
                "state": status.state,
                "name": status.name,
                "reason": status.reason,
            }
        )
        print("---")
    return 0


def _dependency_blocked_statuses(statuses: list[Any]) -> list[Any]:
    return [
        status
        for status in statuses
        if status.state in {"PENDING", "PD"} and "dependency" in status.reason.lower()
    ]


def cmd_slurm_cleanup(args: argparse.Namespace) -> int:
    """Preview or cancel jobs blocked by failed SLURM dependencies."""
    try:
        statuses = SlurmSubmitter().status(args.job_id)
    except SlurmQueryError as exc:
        err(str(exc))
        if exc.stdout.strip():
            err(f"sacct stdout: {exc.stdout.strip()}")
        return 1
    targets = _dependency_blocked_statuses(statuses)
    for status in targets:
        print_kv_table(
            {"job_id": status.job_id, "state": status.state, "name": status.name, "reason": status.reason}
        )
        print("---")
    if not targets:
        warn(f"no dependency-blocked pending jobs found for {args.job_id}")
        return 0
    if not args.cancel:
        warn("dry run; add --cancel to scancel these jobs")
        return 0
    command = ["scancel", *[status.job_id for status in targets]]
    try:
        subprocess.run(command, check=True)
    except (OSError, subprocess.CalledProcessError) as exc:
        err(f"scancel failed: {exc}")
        return 1
    ok(f"cancelled {len(targets)} dependency-blocked jobs")
    return 0


def add_common_stage_args(parser: argparse.ArgumentParser) -> None:
    parser.add_argument(
        "--manifest",
        default=None,
        help="SQLite manifest path; defaults to config pipeline.manifest",
    )
    parser.add_argument("--no-checksum", action="store_true", help="skip SHA-256 manifest checksums")
    parser.add_argument("--resume", action="store_true", help="skip existing non-empty outputs")


def add_parse_options(parser: argparse.ArgumentParser) -> None:
    parser.add_argument("--format", choices=["jsonl", "json"], default="jsonl")
    parser.add_argument("--year-info-only", action="store_true")
    parser.add_argument("--nlm-category", action="store_true")
    parser.add_argument("--author-list", action="store_true")
    parser.add_argument("--reference-list", action="store_true")
    parser.add_argument("--parse-mesh-subterms", action="store_true")
    parser.add_argument("--min-pub-year", type=int, default=None)
    parser.add_argument(
        "--recover-malformed-xml",
        action="store_true",
        help="enable lxml best-effort recovery instead of failing on malformed XML",
    )
    add_common_stage_args(parser)


def add_dry_run_arg(parser: argparse.ArgumentParser) -> None:
    parser.add_argument(
        "--dry-run",
        action="store_true",
        help="show planned work without writing outputs or manifest rows",
    )


def add_external_args(parser: argparse.ArgumentParser) -> None:
    parser.add_argument("--scimago", default=None)
    parser.add_argument("--web-of-science", default=None)
    parser.add_argument("--doaj", default=None)
    parser.add_argument("--norwegian-list", default=None)
    parser.add_argument("--retraction-watch", default=None)
    parser.add_argument("--publisher", default=None)
    parser.add_argument("--peer-review", default=None, help="optional licensed peer-review metadata table")
    parser.add_argument("--exchange-rates", default=None, help="canonical official EUR rate table")
    parser.add_argument("--min-received", default=None)
    add_common_stage_args(parser)


def build_parser() -> argparse.ArgumentParser:
    parser = argparse.ArgumentParser(
        prog="pubdelays",
        description="Run the PubMed publication-delay pipeline.",
        epilog=(
            "Main workflow: init-dirs -> preflight -> download -> external-all -> "
            "parse -> validate -> transform-shards -> validate-shards -> aggregate-all -> manifest summary"
        ),
    )
    parser.add_argument(
        "--config",
        default=os.environ.get("PUBDELAYS_CONFIG", DEFAULT_CONFIG),
        help="TOML config path",
    )
    subparsers = parser.add_subparsers(dest="command", required=True)

    init_dirs = subparsers.add_parser("init-dirs", help="create canonical data directories")
    init_dirs.set_defaults(func=cmd_init_dirs)

    preflight = subparsers.add_parser("preflight", help="check expected raw-data locations before a run")
    preflight.set_defaults(func=cmd_preflight)

    parse_one_p = subparsers.add_parser("parse-one", help="parse one MEDLINE XML/XML.GZ file")
    parse_one_p.add_argument("--input", required=True)
    parse_one_p.add_argument("--output", default=None)
    parse_one_p.add_argument("--output-dir", default=None)
    add_parse_options(parse_one_p)
    parse_one_p.set_defaults(func=cmd_parse_one)

    parse_p = subparsers.add_parser(
        "parse", help="parse all MEDLINE XML/XML.GZ files in configured directory"
    )
    parse_p.add_argument("--input-dir", default=None)
    parse_p.add_argument("--output-dir", default=None)
    parse_p.add_argument(
        "--source", choices=["baseline", "updatefiles", "legacy"], default="legacy"
    )
    parse_p.add_argument("--jobs", type=int, default=None)
    add_dry_run_arg(parse_p)
    add_parse_options(parse_p)
    parse_p.set_defaults(func=cmd_parse)

    resolve_state = subparsers.add_parser(
        "resolve-state", help="apply PubMed update files and DeleteCitation records by PMID"
    )
    resolve_state.add_argument("--baseline", default=None)
    resolve_state.add_argument("--updates", default=None)
    resolve_state.add_argument("--output-dir", default=None)
    resolve_state.add_argument("--state-db", default=None)
    resolve_state.add_argument("--counts-output", default=None)
    add_common_stage_args(resolve_state)
    resolve_state.set_defaults(func=cmd_resolve_state)

    validate = subparsers.add_parser("validate", help="validate JSON or JSONL outputs")
    validate.add_argument("input", nargs="?", default=None)
    add_common_stage_args(validate)
    validate.set_defaults(func=cmd_validate)

    journals = subparsers.add_parser("journals", help="download and parse NLM J_Medline.txt")
    journals.add_argument("--url", default="https://ftp.ncbi.nlm.nih.gov/pubmed/J_Medline.txt")
    journals.add_argument("--output", default=None)
    add_common_stage_args(journals)
    journals.set_defaults(func=cmd_journals)

    transform_one = subparsers.add_parser("transform-one", help="transform one parsed JSON/JSONL file")
    transform_one.add_argument("--input", required=True)
    transform_one.add_argument("--output", required=True)
    transform_one.add_argument("--filters-output", default=None)
    add_external_args(transform_one)
    transform_one.set_defaults(func=cmd_transform_one)

    transform = subparsers.add_parser(
        "transform", help="one transform output per input file"
    )
    transform.add_argument("--input", default=None)
    transform.add_argument("--output-dir", default=None)
    transform.add_argument("--jobs", type=int, default=1)
    transform.add_argument("--limit", type=int, default=None, help="debug only: first N inputs")
    transform.add_argument("--format", choices=["parquet", "tsv", "csv"], default="parquet")
    add_external_args(transform)
    transform.set_defaults(func=cmd_transform)

    transform_shard = subparsers.add_parser(
        "transform-shard", help="transform one modulo shard of a JSONL input list"
    )
    transform_shard.add_argument("--input-list", required=True)
    transform_shard.add_argument("--output-dir", default=None)
    transform_shard.add_argument("--shard-index", type=int, required=True)
    transform_shard.add_argument("--shards", type=int, required=True)
    transform_shard.add_argument("--format", choices=["parquet", "tsv", "csv"], default="parquet")
    add_external_args(transform_shard)
    transform_shard.set_defaults(func=cmd_transform_shard)

    transform_shards = subparsers.add_parser(
        "transform-shards", help="local sharded transform; preferred bare-metal mode"
    )
    transform_shards.add_argument("--input-dir", default=None)
    transform_shards.add_argument("--input-list", default=None)
    transform_shards.add_argument("--output-dir", default=None)
    transform_shards.add_argument("--shards", type=int, default=64)
    transform_shards.add_argument("--jobs", type=int, default=None)
    transform_shards.add_argument("--limit", type=int, default=None, help="debug only: first N inputs")
    transform_shards.add_argument("--format", choices=["parquet", "tsv", "csv"], default="parquet")
    add_dry_run_arg(transform_shards)
    add_external_args(transform_shards)
    transform_shards.set_defaults(func=cmd_transform_shards)

    validate_shards = subparsers.add_parser(
        "validate-shards", help="validate article shard completeness before aggregation"
    )
    validate_shards.add_argument("--input", default=None)
    validate_shards.add_argument("--shards", type=int, default=None)
    validate_shards.add_argument("--format", choices=["parquet", "tsv", "csv"], default=None)
    validate_shards.set_defaults(func=cmd_validate_shards)

    aggregate = subparsers.add_parser("aggregate", help="aggregate article shards into one processed dataset")
    aggregate.add_argument("--input", default=None)
    aggregate.add_argument("--output", default=None)
    add_common_stage_args(aggregate)
    aggregate.set_defaults(func=cmd_aggregate)

    aggregate_all = subparsers.add_parser(
        "aggregate-all", help="aggregate once and write Parquet plus CSV outputs"
    )
    aggregate_all.add_argument("--input", default=None)
    aggregate_all.add_argument("--parquet", default=None)
    aggregate_all.add_argument("--csv", default=None)
    aggregate_all.add_argument("--shards", type=int, default=None)
    aggregate_all.add_argument("--format", choices=["parquet", "tsv", "csv"], default=None)
    aggregate_all.add_argument(
        "--allow-incomplete",
        action="store_true",
        help="aggregate discovered article shards without completeness validation",
    )
    add_dry_run_arg(aggregate_all)
    add_common_stage_args(aggregate_all)
    aggregate_all.set_defaults(func=cmd_aggregate_all)

    compare_outputs_p = subparsers.add_parser("compare-outputs", help="compare two processed outputs")
    compare_outputs_p.add_argument("--baseline", required=True)
    compare_outputs_p.add_argument("--candidate", required=True)
    compare_outputs_p.add_argument("--output", default="data/processed_data/validation/differential.csv")
    compare_outputs_p.set_defaults(func=cmd_compare_outputs)

    schema_cmd = subparsers.add_parser("schema", help="print or validate the canonical analysis schema")
    schema_cmd.add_argument("--input", default=None)
    schema_cmd.set_defaults(func=cmd_schema)

    provenance = subparsers.add_parser(
        "provenance", help="print or write canonical variable provenance"
    )
    provenance.add_argument("--format", choices=["markdown", "csv", "json"], default="markdown")
    provenance.add_argument("--output", default=None)
    provenance.set_defaults(func=cmd_provenance)

    smoke_live = subparsers.add_parser(
        "smoke-live", help="run an isolated live one-file end-to-end validation"
    )
    smoke_live.add_argument("--workspace", default="data/temp_data/smoke_live")
    smoke_live.add_argument("--pubmed-files", type=int, default=1)
    smoke_live.add_argument("--jobs", type=int, default=1)
    smoke_live.add_argument("--shards", type=int, default=2)
    smoke_live.add_argument("--resume", action="store_true")
    smoke_live.set_defaults(func=cmd_smoke_live)

    summaries = subparsers.add_parser(
        "summaries", help="derive lightweight summary tables from processed.parquet"
    )
    summaries.add_argument("--input", default=None)
    summaries.add_argument("--output-dir", default=None)
    add_common_stage_args(summaries)
    summaries.set_defaults(func=cmd_summaries)

    run_analysis = subparsers.add_parser(
        "run-analysis", help="run a configured study-specific analysis subprocess"
    )
    run_analysis.add_argument("--cwd", default=None)
    run_analysis.add_argument("--input", default=None)
    run_analysis.add_argument("--output-dir", default=None)
    run_analysis.add_argument("--command", nargs="+", default=None)
    add_common_stage_args(run_analysis)
    run_analysis.set_defaults(func=cmd_run_analysis)

    validate_analysis = subparsers.add_parser(
        "validate-analysis", help="validate final processed output and write validation tables"
    )
    validate_analysis.add_argument("--input", default=None)
    validate_analysis.add_argument("--output-dir", default=None)
    validate_analysis.add_argument(
        "--filtered-output",
        default=None,
        help="validated dataset path; pass an empty string to skip writing filtered output",
    )
    validate_analysis.add_argument(
        "--excluded-output",
        default=None,
        help="excluded validation rows path; pass an empty string to skip writing excluded output",
    )
    validate_analysis.add_argument(
        "--strict-checks",
        action="store_true",
        help="return nonzero when data-quality checks fail (reports are always written)",
    )
    add_common_stage_args(validate_analysis)
    validate_analysis.set_defaults(func=cmd_validate_analysis)

    filter_counts = subparsers.add_parser(
        "filter-counts", help="aggregate transform .filters.csv sidecars"
    )
    filter_counts.add_argument("--input", default=None)
    filter_counts.add_argument("--output", default=None)
    filter_counts.add_argument("--dataset", default=None, help="final aggregate for reconciliation")
    add_common_stage_args(filter_counts)
    filter_counts.set_defaults(func=cmd_filter_counts)

    quality_report = subparsers.add_parser(
        "quality-report", help="aggregate filter/join diagnostics and final variable quality"
    )
    quality_report.add_argument("--input", default=None, help="article shard directory")
    quality_report.add_argument("--dataset", default=None, help="final processed dataset")
    quality_report.add_argument("--output-dir", default=None)
    add_common_stage_args(quality_report)
    quality_report.set_defaults(func=cmd_quality_report)

    download = subparsers.add_parser(
        "download", help="download PubMed baseline/updatefiles with MD5 verification"
    )
    download.add_argument("--source", choices=sorted(PUBMED_BASE_URLS), default="baseline")
    download.add_argument("--output-dir", default=None)
    download.add_argument("--limit", type=int, default=None, help="debug only: first N links")
    download.add_argument(
        "--newest", action="store_true", help="apply --limit to the newest index links"
    )
    download.add_argument("--jobs", type=int, default=4)
    add_dry_run_arg(download)
    add_common_stage_args(download)
    download.set_defaults(func=cmd_download)

    download_external = subparsers.add_parser(
        "download-external", help="download public external metadata sources into raw-data paths"
    )
    download_external.add_argument(
        "--source",
        choices=["all", "doaj", "retraction-watch", "scimago", "publisher"],
        default="all",
    )
    download_external.add_argument("--start-year", type=int, default=2015)
    download_external.add_argument("--end-year", type=int, default=2025)
    download_external.add_argument("--retries", type=int, default=5)
    add_dry_run_arg(download_external)
    add_common_stage_args(download_external)
    download_external.set_defaults(func=cmd_download_external)

    exchange_rates = subparsers.add_parser(
        "exchange-rates", help="download official ECB and InforEuro EUR rates"
    )
    exchange_rates.add_argument("--output", default=None)
    exchange_rates.add_argument("--start-year", type=int, default=2013)
    exchange_rates.add_argument("--end-year", type=int, default=date.today().year)
    add_common_stage_args(exchange_rates)
    exchange_rates.set_defaults(func=cmd_exchange_rates)

    external_all = subparsers.add_parser("external-all", help="preprocess all local external metadata inputs")
    external_all.add_argument("--input-dir", default=None)  # accepted for common stage dispatch; ignored
    external_all.add_argument("--input", default=None)  # accepted for common stage dispatch; ignored
    external_all.add_argument("--output", default=None)  # accepted for common stage dispatch; ignored
    external_all.add_argument("--publisher-input", default=None)
    external_all.add_argument("--publisher-output", default=None)
    external_all.add_argument("--peer-review-input", default=None)
    external_all.add_argument("--peer-review-output", default=None)
    external_all.add_argument("--start-year", type=int, default=2015)
    external_all.add_argument("--end-year", type=int, default=2025)
    add_dry_run_arg(external_all)
    add_common_stage_args(external_all)
    external_all.set_defaults(func=cmd_external_all)

    scimago = subparsers.add_parser("external-scimago", help="clean raw Scimago yearly CSVs")
    scimago.add_argument("--input-dir", default=None)
    scimago.add_argument("--output", default=None)
    scimago.add_argument("--start-year", type=int, default=2015)
    scimago.add_argument("--end-year", type=int, default=2025)
    add_common_stage_args(scimago)
    scimago.set_defaults(func=cmd_external_scimago)

    wos = subparsers.add_parser("external-wos", help="clean a raw Scopus Source List CSV")
    wos.add_argument("--input", default=None)
    wos.add_argument("--output", default=None)
    add_common_stage_args(wos)
    wos.set_defaults(func=cmd_external_wos)

    doaj = subparsers.add_parser("external-doaj", help="clean raw DOAJ CSV")
    doaj.add_argument("--input", default=None)
    doaj.add_argument("--output", default=None)
    add_common_stage_args(doaj)
    doaj.set_defaults(func=cmd_external_doaj)

    publisher = subparsers.add_parser("external-publisher", help="clean raw publisher metadata CSV")
    publisher.add_argument("--publisher-input", default=None)
    publisher.add_argument("--publisher-output", default=None)
    add_common_stage_args(publisher)
    publisher.set_defaults(func=cmd_external_publisher)

    npi = subparsers.add_parser("external-npi", help="clean raw Norwegian Publication Indicator CSV")
    npi.add_argument("--input", default=None)
    npi.add_argument("--output", default=None)
    add_common_stage_args(npi)
    npi.set_defaults(func=cmd_external_npi)

    rw = subparsers.add_parser("external-retraction-watch", help="clean raw Retraction Watch CSV")
    rw.add_argument("--input", default=None)
    rw.add_argument("--output", default=None)
    add_common_stage_args(rw)
    rw.set_defaults(func=cmd_external_retraction_watch)

    peer_review = subparsers.add_parser("external-peer-review", help="clean raw private peer-review CSV")
    peer_review.add_argument("--peer-review-input", default=None)
    peer_review.add_argument("--peer-review-output", default=None)
    add_common_stage_args(peer_review)
    peer_review.set_defaults(func=cmd_external_peer_review)

    list_inputs = subparsers.add_parser("list-inputs", help="write an input file list for SLURM arrays")
    list_inputs.add_argument("--input-dir", required=True)
    list_inputs.add_argument("--output", required=True)
    list_inputs.add_argument("--kind", choices=["xml", "json", "glob"], default="xml")
    list_inputs.add_argument("--glob", default="*")
    list_inputs.add_argument("--limit", type=int, default=None, help="debug only: first N paths")
    list_inputs.set_defaults(func=cmd_list_inputs)

    slurm = subparsers.add_parser("slurm", help="submit pipeline stages to SLURM")
    slurm_sub = slurm.add_subparsers(dest="slurm_command", required=True)
    slurm_submit = slurm_sub.add_parser("submit", help="submit one pipeline stage with sbatch")
    slurm_submit.add_argument("stage", choices=sorted(SLURM_STAGE_CONFIG))
    slurm_submit.add_argument(
        "--runner", default=None, help="command used inside jobs; defaults to slurm.runner"
    )
    slurm_submit.add_argument(
        "--log-dir", default=None, help="SLURM log directory; defaults to slurm.log_dir"
    )
    slurm_submit.add_argument("--dependency", default=None, help="sbatch dependency, e.g. afterok:12345")
    slurm_submit.add_argument("--dry-run", action="store_true", help="print sbatch script without submitting")
    slurm_submit.add_argument("--source", choices=sorted(PUBMED_BASE_URLS), default="baseline")
    slurm_submit.add_argument(
        "--external-source",
        choices=["all", "doaj", "retraction-watch", "scimago", "publisher"],
        default="all",
        help="source for the download-external SLURM stage",
    )
    slurm_submit.add_argument("--jobs", type=int, default=4)
    slurm_submit.add_argument("--limit", type=int, default=None)
    slurm_submit.add_argument("--input-dir", default=None)
    slurm_submit.add_argument("--output-dir", default=None)
    slurm_submit.add_argument("--input-list", default=None)
    slurm_submit.add_argument("--shards", type=int, default=64)
    slurm_submit.add_argument("--format", choices=["parquet", "tsv", "csv"], default="parquet")
    slurm_submit.add_argument("--start-year", type=int, default=2015)
    slurm_submit.add_argument("--end-year", type=int, default=2025)
    slurm_submit.add_argument(
        "--array-throttle",
        type=int,
        default=None,
        help="max concurrent array tasks (%%N in #SBATCH --array); 0 disables",
    )
    slurm_submit.add_argument(
        "--max-array-size",
        type=int,
        default=None,
        dest="config_max_array_size",
        help="override SLURM MaxArraySize for validation; default: auto-detect via scontrol",
    )
    slurm_submit.set_defaults(func=cmd_slurm_submit)

    slurm_workflow = slurm_sub.add_parser(
        "workflow", help="submit parse, transform, and aggregate with dependencies"
    )
    slurm_workflow.add_argument(
        "--stages",
        default="parse-baseline,parse-updatefiles,resolve-state,prepare-transform,transform-shards,aggregate-all",
        help="comma-separated stages to chain with afterok dependencies",
    )
    slurm_workflow.add_argument("--runner", default=None)
    slurm_workflow.add_argument("--log-dir", default=None)
    slurm_workflow.add_argument("--dependency", default=None)
    slurm_workflow.add_argument("--dry-run", action="store_true")
    slurm_workflow.add_argument(
        "--input-dir", default=None, help="deprecated workflow alias; use stage-specific dirs"
    )
    slurm_workflow.add_argument(
        "--output-dir", default=None, help="deprecated workflow alias; use stage-specific dirs"
    )
    slurm_workflow.add_argument("--parse-input-dir", default=None)
    slurm_workflow.add_argument("--parse-output-dir", default=None)
    slurm_workflow.add_argument("--transform-input-dir", default=None)
    slurm_workflow.add_argument("--transform-output-dir", default=None)
    slurm_workflow.add_argument("--input-list", default=None)
    slurm_workflow.add_argument("--shards", type=int, default=64)
    slurm_workflow.add_argument("--format", choices=["parquet", "tsv", "csv"], default="parquet")
    slurm_workflow.add_argument("--source", choices=sorted(PUBMED_BASE_URLS), default="baseline")
    slurm_workflow.add_argument("--jobs", type=int, default=4)
    slurm_workflow.add_argument("--limit", type=int, default=None)
    slurm_workflow.add_argument("--start-year", type=int, default=2015)
    slurm_workflow.add_argument("--end-year", type=int, default=2025)
    slurm_workflow.add_argument(
        "--array-throttle",
        type=int,
        default=None,
        help="max concurrent array tasks (%%N in #SBATCH --array); 0 disables",
    )
    slurm_workflow.add_argument(
        "--max-array-size",
        type=int,
        default=None,
        dest="config_max_array_size",
        help="override SLURM MaxArraySize for validation; default: auto-detect via scontrol",
    )
    slurm_workflow.set_defaults(func=cmd_slurm_workflow)

    slurm_status = slurm_sub.add_parser("status", help="inspect SLURM accounting state for a job id")
    slurm_status.add_argument("job_id")
    slurm_status.set_defaults(func=cmd_slurm_status)

    slurm_cleanup = slurm_sub.add_parser("cleanup", help="find or cancel dependency-blocked SLURM jobs")
    slurm_cleanup.add_argument("job_id")
    slurm_cleanup.add_argument("--cancel", action="store_true", help="run scancel on dependency-blocked jobs")
    slurm_cleanup.set_defaults(func=cmd_slurm_cleanup)

    manifest = subparsers.add_parser("manifest", help="inspect manifest rows")
    manifest.add_argument("--manifest", default=None)
    manifest.add_argument("--limit", type=int, default=20)
    manifest.set_defaults(func=cmd_manifest)
    manifest_sub = manifest.add_subparsers(dest="manifest_command")

    manifest_summary = manifest_sub.add_parser("summary", help="summarize manifest rows")
    manifest_summary.add_argument("--manifest", default=None)
    manifest_summary.add_argument("--json", action="store_true")
    manifest_summary.set_defaults(func=cmd_manifest_summary)

    manifest_failed = manifest_sub.add_parser("failed", help="show failed manifest rows")
    manifest_failed.add_argument("--manifest", default=None)
    manifest_failed.add_argument("--limit", type=int, default=20)
    manifest_failed.add_argument("--json", action="store_true")
    manifest_failed.set_defaults(func=cmd_manifest_failed)

    manifest_show = manifest_sub.add_parser("show", help="show manifest rows")
    manifest_show.add_argument("--manifest", default=None)
    manifest_show.add_argument("--limit", type=int, default=20)
    manifest_show.add_argument("--json", action="store_true")
    manifest_show.set_defaults(func=cmd_manifest_show)

    manifest_retry = manifest_sub.add_parser("retry-script", help="emit comments for failed work to retry")
    manifest_retry.add_argument("--manifest", default=None)
    manifest_retry.add_argument("--limit", type=int, default=100)
    manifest_retry.set_defaults(func=cmd_manifest_retry_script)

    manifest_check = manifest_sub.add_parser("check", help="run SQLite integrity_check without sqlite3 CLI")
    manifest_check.add_argument("--manifest", default=None)
    manifest_check.add_argument("--cleanup", action="store_true", help="archive corrupt manifest files")
    manifest_check.add_argument("--archive-dir", default=None)
    manifest_check.set_defaults(func=cmd_manifest_check)

    manifest_collect = manifest_sub.add_parser("collect", help="merge per-task manifest SQLite files")
    manifest_collect.add_argument("--manifest", default=None)
    manifest_collect.add_argument("--input-dir", default=None)
    manifest_collect.add_argument("--glob", default="*.sqlite")
    manifest_collect.add_argument("--cleanup-corrupt", action="store_true")
    manifest_collect.add_argument("--archive-dir", default=None)
    manifest_collect.set_defaults(func=cmd_manifest_collect)

    return parser


def main(argv: list[str] | None = None) -> int:
    parser = build_parser()
    args = parser.parse_args(argv)
    try:
        return args.func(args)
    except ConfigError as exc:
        err(str(exc))
        return 2


if __name__ == "__main__":
    raise SystemExit(main())
