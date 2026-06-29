"""Configuration loading and repository-relative path resolution."""

from __future__ import annotations

import re
import tomllib
from collections.abc import Mapping
from dataclasses import dataclass
from datetime import date
from pathlib import Path, PureWindowsPath
from typing import Any


class ConfigError(ValueError):
    """Raised when a pipeline TOML config violates the public config contract."""


@dataclass(frozen=True)
class PipelineConfig:
    """Loaded TOML configuration with repository-relative path resolution."""

    root: Path
    values: Mapping[str, Any]

    def get(self, dotted: str, default: Any = None) -> Any:
        """Return a dotted config value, or ``default`` when any part is missing."""
        current: Any = self.values
        for part in dotted.split("."):
            if not isinstance(current, Mapping) or part not in current:
                return default
            current = current[part]
        return current

    def path(self, dotted: str, default: str | None = None) -> Path:
        """Resolve a dotted config path relative to the repository root."""
        value = self.get(dotted, default)
        if value is None:
            raise KeyError(f"missing config path: {dotted}")
        return resolve_config_path(str(value), self.root)


def is_absolute_path_string(value: str) -> bool:
    """Return true for native absolute paths and Windows drive/UNC paths."""

    path = Path(value).expanduser()
    return path.is_absolute() or PureWindowsPath(value).is_absolute()


def resolve_config_path(value: str, root: Path) -> Path:
    """Resolve a config path without mangling Windows absolute path strings."""

    path = Path(value).expanduser()
    return path if is_absolute_path_string(value) else root / path


def discover_repo_root(start: Path | None = None) -> Path:
    """Find the nearest repository root marker above ``start`` or cwd."""
    current = (start or Path.cwd()).resolve()
    for candidate in (current, *current.parents):
        if (candidate / "pyproject.toml").exists() or (candidate / ".git").exists():
            return candidate
    return current


REQUIRED_PATH_KEYS = (
    "pipeline.manifest",
    "pipeline.parse_inputs",
    "pipeline.transform_inputs",
    "pubmed.xml_dir",
    "pubmed.jsonl_dir",
    "external.raw.scimago_dir",
    "external.raw.web_of_science_csv",
    "external.raw.doaj_csv",
    "external.raw.norwegian_list_csv",
    "external.raw.retraction_watch_csv",
    "external.raw.publisher_csv",
    "external.raw.peer_review_csv",
    "external.processed.scimago",
    "external.processed.web_of_science",
    "external.processed.doaj",
    "external.processed.norwegian_list",
    "external.processed.retraction_watch",
    "external.processed.publisher",
    "external.processed.peer_review",
    "external.processed.pubmed_journals",
    "transform.article_shard_dir",
    "transform.article_shard_format",
    "transform.min_received",
    "aggregate.processed_parquet",
    "aggregate.processed_csv",
    "aggregate.summary_dir",
    "aggregate.filter_counts",
    "analysis.cwd",
    "analysis.input",
    "analysis.output_dir",
    "validation.report_dir",
    "validation.filtered_output",
)

REQUIRED_SECTIONS = ("pipeline", "pubmed", "external", "transform", "aggregate", "analysis", "validation")
SUPPORTED_ARTICLE_SHARD_FORMATS = {"parquet", "tsv", "csv"}


def _config_error(config_path: Path, key: str, message: str) -> ConfigError:
    return ConfigError(f"{config_path}: invalid config key '{key}': {message}")


def _require_mapping(config_path: Path, values: Mapping[str, Any], key: str) -> Mapping[str, Any]:
    current: Any = values
    for part in key.split("."):
        if not isinstance(current, Mapping) or part not in current:
            raise _config_error(config_path, key, "missing required section")
        current = current[part]
    if not isinstance(current, Mapping):
        raise _config_error(config_path, key, "expected a table")
    return current


def _get_required(config_path: Path, values: Mapping[str, Any], key: str) -> Any:
    current: Any = values
    for part in key.split("."):
        if not isinstance(current, Mapping) or part not in current:
            raise _config_error(config_path, key, "missing required value")
        current = current[part]
    return current


def validate_config_values(config_path: Path, values: Mapping[str, Any]) -> None:
    """Validate the public TOML config boundary before any stage runs."""

    for section in REQUIRED_SECTIONS:
        _require_mapping(config_path, values, section)
    _require_mapping(config_path, values, "external.raw")
    _require_mapping(config_path, values, "external.processed")

    for key in REQUIRED_PATH_KEYS:
        value = _get_required(config_path, values, key)
        if not isinstance(value, str) or not value.strip():
            raise _config_error(config_path, key, "expected a non-empty string")

    shards = _get_required(config_path, values, "transform.default_shards")
    if not isinstance(shards, int) or isinstance(shards, bool) or shards < 1:
        raise _config_error(config_path, "transform.default_shards", "expected a positive integer")

    fmt = _get_required(config_path, values, "transform.article_shard_format")
    if fmt not in SUPPORTED_ARTICLE_SHARD_FORMATS:
        supported = ", ".join(sorted(SUPPORTED_ARTICLE_SHARD_FORMATS))
        raise _config_error(config_path, "transform.article_shard_format", f"expected one of: {supported}")

    for key in ("transform.min_received", "validation.min_article_date", "validation.max_article_date"):
        value = _get_required(config_path, values, key)
        if not isinstance(value, str) or not re.fullmatch(r"\d{4}-\d{2}-\d{2}", value):
            raise _config_error(config_path, key, "expected YYYY-MM-DD date")
        try:
            date.fromisoformat(value)
        except ValueError as exc:
            raise _config_error(config_path, key, "expected YYYY-MM-DD date") from exc

    min_delay = _get_required(config_path, values, "validation.min_delay_days")
    max_delay = _get_required(config_path, values, "validation.max_delay_days")
    for key, value in (
        ("validation.min_delay_days", min_delay),
        ("validation.max_delay_days", max_delay),
    ):
        if not isinstance(value, int) or isinstance(value, bool) or value < 0:
            raise _config_error(config_path, key, "expected a non-negative integer")
    if min_delay > max_delay:
        raise _config_error(config_path, "validation.min_delay_days", "expected <= validation.max_delay_days")


def load_config(path: Path | str = "config/default.toml") -> PipelineConfig:
    """Load, validate, and root-anchor a pipeline TOML config."""
    config_path = Path(path).expanduser()
    if not config_path.is_absolute():
        config_path = discover_repo_root() / config_path
    with config_path.open("rb") as handle:
        values = tomllib.load(handle)
    validate_config_values(config_path, values)
    return PipelineConfig(root=discover_repo_root(config_path.parent), values=values)
