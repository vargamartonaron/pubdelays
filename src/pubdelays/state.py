"""Resolve PubMed baseline and update records into one deterministic live state."""

from __future__ import annotations

import json
import sqlite3
from collections.abc import Iterable
from pathlib import Path

from pubdelays.fs import atomic_output_path


def _jsonl_paths(path: Path) -> list[Path]:
    path = Path(path)
    if path.is_file():
        return [path]
    return sorted(path.glob("*.jsonl"))


def _records(path: Path) -> Iterable[tuple[int, dict[str, object]]]:
    with Path(path).open("r", encoding="utf-8") as handle:
        for record_order, line in enumerate(handle):
            if line.strip():
                yield record_order, json.loads(line)


def _write_jsonl(path: Path, records: Iterable[dict[str, object]]) -> int:
    count = 0
    with atomic_output_path(path) as temporary:
        with temporary.open("w", encoding="utf-8") as handle:
            for record in records:
                handle.write(json.dumps(record, ensure_ascii=False, separators=(",", ":")))
                handle.write("\n")
                count += 1
    return count


def resolve_pubmed_state(
    baseline: Path,
    updatefiles: Path,
    output_dir: Path,
    *,
    state_db: Path,
) -> dict[str, int]:
    """Apply updates/deletions by PMID and write resolved JSONL partitions.

    Baseline records are retained unless an update record exists for their PMID.
    Update files are applied in sorted filename order and record order; the last
    live update wins and a last deletion removes the PMID. Records without PMID
    are passed through for the downstream DOI/title fallback audit.
    """
    baseline_paths = _jsonl_paths(Path(baseline))
    update_paths = _jsonl_paths(Path(updatefiles))
    if not baseline_paths:
        raise FileNotFoundError(f"PubMed state resolution: no baseline JSONL files in {baseline}")
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    expected_outputs = {path.name for path in baseline_paths} | {"updates-resolved.jsonl"}
    for stale_path in output_dir.glob("*.jsonl"):
        if stale_path.name not in expected_outputs:
            stale_path.unlink()
    state_db = Path(state_db)
    state_db.parent.mkdir(parents=True, exist_ok=True)

    connection = sqlite3.connect(state_db)
    try:
        connection.execute("PRAGMA journal_mode=WAL")
        connection.execute("PRAGMA synchronous=NORMAL")
        connection.execute(
            "CREATE TABLE IF NOT EXISTS updates ("
            "pmid TEXT PRIMARY KEY, deleted INTEGER NOT NULL, record_json TEXT, "
            "source_file TEXT NOT NULL, source_order INTEGER NOT NULL, record_order INTEGER NOT NULL)"
        )
        connection.execute("DELETE FROM updates")
        update_rows = 0
        update_without_pmid: list[dict[str, object]] = []
        for source_order, path in enumerate(update_paths):
            batch: list[tuple[object, ...]] = []
            for record_order, record in _records(path):
                pmid = str(record.get("pmid") or "").strip()
                if not pmid:
                    update_without_pmid.append(record)
                    continue
                deleted = int(bool(record.get("delete")))
                payload = None if deleted else json.dumps(record, ensure_ascii=False, separators=(",", ":"))
                batch.append((pmid, deleted, payload, path.name, source_order, record_order))
                update_rows += 1
                if len(batch) >= 10_000:
                    connection.executemany(
                        "INSERT OR REPLACE INTO updates VALUES (?, ?, ?, ?, ?, ?)", batch
                    )
                    connection.commit()
                    batch.clear()
            if batch:
                connection.executemany(
                    "INSERT OR REPLACE INTO updates VALUES (?, ?, ?, ?, ?, ?)", batch
                )
                connection.commit()

        overridden_set = {
            str(row[0]) for row in connection.execute("SELECT pmid FROM updates")
        }
        baseline_rows = 0
        baseline_overridden = 0
        baseline_without_pmid = 0
        resolved_baseline_rows = 0
        for path in baseline_paths:
            source_path = path

            def kept_records(source_path: Path = path) -> Iterable[dict[str, object]]:
                nonlocal baseline_rows, baseline_overridden, baseline_without_pmid
                for _, record in _records(source_path):
                    baseline_rows += 1
                    pmid = str(record.get("pmid") or "").strip()
                    if not pmid:
                        baseline_without_pmid += 1
                        yield record
                    elif pmid in overridden_set:
                        baseline_overridden += 1
                    elif not record.get("delete"):
                        yield record

            resolved_baseline_rows += _write_jsonl(output_dir / source_path.name, kept_records())

        def live_updates() -> Iterable[dict[str, object]]:
            cursor = connection.execute(
                "SELECT record_json FROM updates WHERE deleted = 0 "
                "ORDER BY source_order, record_order"
            )
            for (payload,) in cursor:
                if payload:
                    yield json.loads(payload)
            yield from update_without_pmid

        live_update_rows = _write_jsonl(output_dir / "updates-resolved.jsonl", live_updates())
        deleted_pmids = int(
            connection.execute("SELECT COUNT(*) FROM updates WHERE deleted = 1").fetchone()[0]
        )
        distinct_updated_pmids = int(
            connection.execute("SELECT COUNT(*) FROM updates").fetchone()[0]
        )
    finally:
        connection.close()

    return {
        "baseline_rows": baseline_rows,
        "update_rows": update_rows,
        "distinct_updated_pmids": distinct_updated_pmids,
        "deleted_pmids": deleted_pmids,
        "baseline_overridden": baseline_overridden,
        "baseline_without_pmid": baseline_without_pmid,
        "update_without_pmid": len(update_without_pmid),
        "resolved_baseline_rows": resolved_baseline_rows,
        "live_update_rows": live_update_rows,
        "resolved_rows": resolved_baseline_rows + live_update_rows,
    }
