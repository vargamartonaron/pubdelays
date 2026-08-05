from __future__ import annotations

import json
from pathlib import Path

from pubdelays.state import resolve_pubmed_state


def _write(path: Path, rows: list[dict[str, object]]) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text("".join(json.dumps(row) + "\n" for row in rows), encoding="utf-8")


def _read_all(path: Path) -> list[dict[str, object]]:
    return [
        json.loads(line)
        for file_path in sorted(path.glob("*.jsonl"))
        for line in file_path.read_text(encoding="utf-8").splitlines()
        if line
    ]


def test_resolve_pubmed_state_applies_latest_update_and_deletion(tmp_path: Path) -> None:
    baseline = tmp_path / "baseline"
    updates = tmp_path / "updates"
    output = tmp_path / "resolved"
    _write(
        baseline / "base.jsonl",
        [{"pmid": "1", "title": "old"}, {"pmid": "2", "title": "delete me"}],
    )
    _write(
        updates / "a.jsonl",
        [{"pmid": "1", "title": "first update"}, {"pmid": "3", "title": "new"}],
    )
    _write(
        updates / "b.jsonl",
        [{"pmid": "1", "title": "latest update"}, {"pmid": "2", "delete": True}],
    )

    counts = resolve_pubmed_state(
        baseline, updates, output, state_db=tmp_path / "state.sqlite"
    )

    rows = _read_all(output)
    assert {(row["pmid"], row.get("title")) for row in rows} == {
        ("1", "latest update"),
        ("3", "new"),
    }
    assert counts["deleted_pmids"] == 1
    assert counts["baseline_overridden"] == 2
    assert counts["resolved_rows"] == 2


def test_resolve_pubmed_state_retains_records_without_pmid_for_fallback(tmp_path: Path) -> None:
    baseline = tmp_path / "baseline"
    updates = tmp_path / "updates"
    _write(baseline / "base.jsonl", [{"title": "baseline no id"}])
    _write(updates / "update.jsonl", [{"title": "update no id"}])

    counts = resolve_pubmed_state(
        baseline, updates, tmp_path / "resolved", state_db=tmp_path / "state.sqlite"
    )

    assert counts["baseline_without_pmid"] == 1
    assert counts["update_without_pmid"] == 1
    assert counts["resolved_rows"] == 2
