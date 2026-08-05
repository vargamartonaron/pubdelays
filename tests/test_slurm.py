from __future__ import annotations

import subprocess
from pathlib import Path
from typing import Any

import pytest

from pubdelays.cli import (
    _split_array_chunks,
    _split_job_array,
    build_parser,
    build_slurm_job,
    cmd_slurm_cleanup,
    emit_or_submit_slurm,
)
from pubdelays.slurm import (
    SlurmJob,
    SlurmResources,
    SlurmStatus,
    SlurmSubmissionError,
    SlurmSubmitter,
    build_sbatch_script,
    parse_sacct_status,
)


def test_sbatch_script_includes_array_resources_and_dependency(tmp_path: Path) -> None:
    job = SlurmJob(
        name="pubdelays-transform",
        command='uv run pubdelays transform-shard --shard-index "$SLURM_ARRAY_TASK_ID"',
        resources=SlurmResources(cpus_per_task=4, mem="24G", time="06:00:00", partition="cpu"),
        log_dir=tmp_path / "logs",
        array="0-63",
        dependency="afterok:123",
    )

    script = build_sbatch_script(job)

    assert "#SBATCH --array=0-63" in script
    assert "#SBATCH --dependency=afterok:123" in script
    assert "#SBATCH --cpus-per-task=4" in script
    assert "#SBATCH --mem=24G" in script
    assert "#SBATCH --partition=cpu" in script
    assert 'transform-shard --shard-index "$SLURM_ARRAY_TASK_ID"' in script


def test_submitter_raises_explicit_error_on_sbatch_failure(monkeypatch: pytest.MonkeyPatch) -> None:
    def fake_run(*_args: Any, **_kwargs: Any) -> subprocess.CompletedProcess[str]:
        return subprocess.CompletedProcess(
            args=["sbatch", "--parsable"], returncode=1, stdout="job hint", stderr="bad qos"
        )

    monkeypatch.setattr(subprocess, "run", fake_run)

    with pytest.raises(SlurmSubmissionError) as excinfo:
        SlurmSubmitter().submit("#!/usr/bin/env bash\nfalse\n")

    assert excinfo.value.returncode == 1
    assert excinfo.value.stdout == "job hint"
    assert excinfo.value.stderr == "bad qos"
    assert "bad qos" in str(excinfo.value)


def test_submitter_rejects_empty_parsable_job_id(monkeypatch: pytest.MonkeyPatch) -> None:
    def fake_run(*_args: Any, **_kwargs: Any) -> subprocess.CompletedProcess[str]:
        return subprocess.CompletedProcess(args=["sbatch", "--parsable"], returncode=0, stdout="", stderr="")

    monkeypatch.setattr(subprocess, "run", fake_run)

    with pytest.raises(SlurmSubmissionError, match="no parsable job id"):
        SlurmSubmitter().submit("#!/usr/bin/env bash\ntrue\n")


def test_submitter_returns_submission_trace(monkeypatch: pytest.MonkeyPatch) -> None:
    def fake_run(*_args: Any, **_kwargs: Any) -> subprocess.CompletedProcess[str]:
        return subprocess.CompletedProcess(
            args=["sbatch", "--parsable"], returncode=0, stdout="12345;cluster\n", stderr="queued"
        )

    monkeypatch.setattr(subprocess, "run", fake_run)

    submission = SlurmSubmitter().submit_details("#!/usr/bin/env bash\ntrue\n")

    assert submission.job_id == "12345;cluster"
    assert submission.command == ("sbatch", "--parsable")
    assert submission.stderr == "queued"
    assert "true" in submission.script


def test_emit_or_submit_slurm_creates_log_dir_before_sbatch(
    tmp_path: Path, monkeypatch: pytest.MonkeyPatch
) -> None:
    log_dir = tmp_path / "missing" / "logs"
    submitted_scripts: list[str] = []

    def fake_submit(script: str) -> str:
        assert log_dir.is_dir()
        submitted_scripts.append(script)
        return "12345"

    monkeypatch.setattr("pubdelays.cli.query_max_array_size", lambda: None)
    monkeypatch.setattr("pubdelays.cli.submit_sbatch", fake_submit)
    args = build_parser().parse_args(["slurm", "submit", "external-all"])
    job = SlurmJob(
        name="pubdelays-external",
        command=["pubdelays", "external-all"],
        resources=SlurmResources(cpus_per_task=1, mem="2G", time="00:10:00"),
        log_dir=log_dir,
    )

    job_ids = emit_or_submit_slurm(args, job)

    assert job_ids == ["12345"]
    assert submitted_scripts


def test_slurm_cleanup_cancels_dependency_blocked_jobs(monkeypatch: pytest.MonkeyPatch) -> None:
    statuses = [
        SlurmStatus("123", "FAILED", "pubdelays-parse", "None"),
        SlurmStatus("124", "PD", "pubdelays-transform", "DependencyNeverSatisfied"),
        SlurmStatus("125", "PENDING", "pubdelays-aggregate", "Dependency"),
    ]
    cancelled: list[list[str]] = []

    monkeypatch.setattr("pubdelays.cli.SlurmSubmitter.status", lambda _self, _job_id: statuses)
    monkeypatch.setattr("pubdelays.cli.subprocess.run", lambda command, check: cancelled.append(command))
    args = build_parser().parse_args(["slurm", "cleanup", "123", "--cancel"])

    code = cmd_slurm_cleanup(args)

    assert code == 0
    assert cancelled == [["scancel", "124", "125"]]


def test_parse_slurm_job_uses_per_task_manifest() -> None:
    args = build_parser().parse_args(["slurm", "submit", "parse", "--dry-run"])

    job, _metadata = build_slurm_job(args, "parse")

    assert isinstance(job.command, str)
    assert '--manifest "$PUBDELAYS_STAGE_MANIFEST"' in job.command
    assert any("PUBDELAYS_STAGE_MANIFEST_DIR" in line for line in job.setup)
    assert any("${SLURM_ARRAY_JOB_ID:-local}-${PUBDELAYS_ARRAY_TASK_ID}.sqlite" in line for line in job.setup)


def test_transform_slurm_job_uses_per_task_manifest_and_logical_shard_id() -> None:
    args = build_parser().parse_args(["slurm", "submit", "transform-shards", "--dry-run"])

    job, _metadata = build_slurm_job(args, "transform-shards")

    assert isinstance(job.command, str)
    assert '--shard-index "$PUBDELAYS_ARRAY_TASK_ID"' in job.command
    assert '--manifest "$PUBDELAYS_STAGE_MANIFEST"' in job.command
    assert any("transform-shards" in line for line in job.setup)
    assert any("${SLURM_ARRAY_JOB_ID:-local}-${PUBDELAYS_ARRAY_TASK_ID}.sqlite" in line for line in job.setup)


def test_split_job_array_restarts_task_ids_and_sets_input_offset(tmp_path: Path) -> None:
    job = SlurmJob(
        name="pubdelays-parse",
        command='uv run pubdelays parse-one --input "$INPUT"',
        resources=SlurmResources(cpus_per_task=1, mem="6G", time="04:00:00"),
        log_dir=tmp_path / "logs",
        array="0-1333",
        array_throttle=100,
        dependency="afterok:123",
        setup=[
            'PUBDELAYS_ARRAY_TASK_OFFSET="${PUBDELAYS_ARRAY_TASK_OFFSET:-0}"',
            'PUBDELAYS_ARRAY_TASK_ID="$((SLURM_ARRAY_TASK_ID + PUBDELAYS_ARRAY_TASK_OFFSET))"',
            'INPUT=$(sed -n "$((PUBDELAYS_ARRAY_TASK_ID + 1))p" "$INPUT_LIST")',
        ],
    )

    chunks = _split_array_chunks(1333, 1001)
    split_jobs = _split_job_array(job, chunks)

    assert [split_job.array for split_job in split_jobs] == ["0-1000", "0-332"]
    assert [split_job.dependency for split_job in split_jobs] == [
        "afterok:123",
        "afterok:123",
    ]
    assert "PUBDELAYS_ARRAY_TASK_OFFSET=1001" in split_jobs[1].setup[0]
    script = build_sbatch_script(split_jobs[1])
    assert "#SBATCH --array=0-332%100" in script
    assert 'INPUT=$(sed -n "$((PUBDELAYS_ARRAY_TASK_ID + 1))p" "$INPUT_LIST")' in script


def test_build_slurm_download_external_job_uses_configured_source() -> None:
    args = build_parser().parse_args(
        [
            "slurm",
            "submit",
            "download-external",
            "--external-source",
            "publisher",
            "--start-year",
            "2023",
            "--end-year",
            "2024",
            "--dry-run",
        ]
    )

    job, metadata = build_slurm_job(args, "download-external")

    assert metadata == {"stage": "download-external"}
    assert job.name == "pubdelays-download-external"
    assert "download-external" in job.command
    assert "--source" in job.command
    assert "publisher" in job.command
    assert "--start-year" in job.command
    assert "2023" in job.command


def test_parse_sacct_status_returns_typed_rows() -> None:
    statuses = parse_sacct_status("12345|COMPLETED|pubdelays-parse|None\n12345_7|FAILED|pubdelays-parse|OutOfMemory\n")

    assert statuses[0].job_id == "12345"
    assert statuses[0].state == "COMPLETED"
    assert statuses[1].job_id == "12345_7"
    assert statuses[1].reason == "OutOfMemory"
