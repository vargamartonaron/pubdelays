from __future__ import annotations

from pathlib import Path

from pubdelays.cli import build_parser
from pubdelays.config import load_config
from pubdelays.smoke import _available_sources, _toml_text


def test_smoke_config_is_isolated_and_loadable(tmp_path: Path) -> None:
    project = load_config("config/default.toml")
    workspace = tmp_path / "smoke"
    config_path = workspace / "smoke.toml"
    workspace.mkdir()
    config_path.write_text(_toml_text(project, workspace.resolve(), 2), encoding="utf-8")

    smoke = load_config(config_path)

    assert smoke.path("pubmed.xml_dir").is_relative_to(workspace.resolve())
    assert smoke.path("aggregate.processed_parquet").is_relative_to(workspace.resolve())
    assert smoke.path("external.raw.web_of_science_csv") == project.path(
        "external.raw.web_of_science_csv"
    )


def test_smoke_reports_optional_local_sources_without_inventing_data(tmp_path: Path) -> None:
    config_path = tmp_path / "config.toml"
    project = load_config("config/default.toml")
    config_path.write_text(_toml_text(project, tmp_path.resolve(), 2), encoding="utf-8")
    smoke = load_config(config_path)

    available = _available_sources(smoke)

    assert set(available) == {
        "scimago",
        "web_of_science",
        "norwegian_list",
        "publisher",
        "peer_review",
    }
    assert all(isinstance(value, bool) for value in available.values())


def test_cli_exposes_newest_download_and_live_smoke() -> None:
    parser = build_parser()

    download = parser.parse_args(["download", "--limit", "2", "--newest"])
    smoke = parser.parse_args(["smoke-live", "--pubmed-files", "1"])

    assert download.newest
    assert smoke.pubmed_files == 1
