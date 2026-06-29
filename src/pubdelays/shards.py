"""Article-shard discovery and completeness validation."""

from __future__ import annotations

import re
from dataclasses import dataclass
from pathlib import Path

from pubdelays.schema import validate_analysis_dataset_schema

SUPPORTED_ARTICLE_SHARD_FORMATS: tuple[str, ...] = ("parquet", "tsv", "csv")
ARTICLE_SHARD_RE = re.compile(
    r"^articles-shard-(?P<index>\d+)-of-(?P<total>\d+)\.(?P<format>[^.]+)$"
)


@dataclass(frozen=True)
class ArticleShard:
    """Parsed identity encoded in a canonical article shard filename."""

    path: Path
    index: int
    total: int
    format: str


@dataclass(frozen=True)
class ShardValidationResult:
    """Completeness and schema validation result for a shard directory."""

    input_path: Path
    expected_shards: int
    expected_format: str | None
    shards: tuple[ArticleShard, ...]
    errors: tuple[str, ...]

    @property
    def ok(self) -> bool:
        return not self.errors

    @property
    def metadata(self) -> dict[str, object]:
        return {
            "expected_shards": self.expected_shards,
            "expected_format": self.expected_format or "",
            "article_shards": len(self.shards),
            "errors": list(self.errors),
        }


def expected_article_shard_path(
    output_dir: Path, shard_index: int, shards: int, fmt: str
) -> Path:
    """Return the canonical output path for one transform shard."""
    return Path(output_dir) / f"articles-shard-{shard_index:05d}-of-{shards:05d}.{fmt}"


def parse_article_shard_name(path: Path) -> ArticleShard | None:
    """Parse a canonical article shard filename, returning ``None`` for sidecars."""
    match = ARTICLE_SHARD_RE.fullmatch(Path(path).name)
    if match is None:
        return None
    return ArticleShard(
        path=Path(path),
        index=int(match.group("index")),
        total=int(match.group("total")),
        format=match.group("format").lower(),
    )


def discover_article_shards(input_path: Path) -> list[ArticleShard]:
    """Return files with the canonical article-shard filename shape."""

    input_path = Path(input_path)
    candidates = [input_path] if input_path.is_file() else sorted(input_path.rglob("articles-shard-*"))
    shards: list[ArticleShard] = []
    for candidate in candidates:
        if not candidate.is_file():
            continue
        shard = parse_article_shard_name(candidate)
        if shard is not None:
            shards.append(shard)
    return sorted(shards, key=lambda shard: (shard.index, str(shard.path)))


def iter_article_paths(input_path: Path) -> list[Path]:
    """Discover article inputs without admitting transform sidecars.

    Directory discovery is limited to canonical transform-shard output names.
    Explicit file inputs remain supported for direct checks, but filter sidecars
    are never returned as article data.
    """

    input_path = Path(input_path)
    if input_path.is_dir():
        return [
            shard.path
            for shard in discover_article_shards(input_path)
            if shard.format in SUPPORTED_ARTICLE_SHARD_FORMATS
        ]
    if input_path.name.endswith(".filters.csv"):
        return []
    if input_path.suffix.lstrip(".").lower() in SUPPORTED_ARTICLE_SHARD_FORMATS:
        return [input_path]
    return []


def _schema_error(shard: ArticleShard) -> str | None:
    try:
        valid, errors = validate_analysis_dataset_schema(shard.path)
    except Exception as exc:  # noqa: BLE001 - surface Polars IO/schema failures.
        return f"unreadable shard {shard.path}: {exc}"
    if not valid:
        return f"schema-invalid shard {shard.path}: {'; '.join(errors)}"
    return None


def validate_article_shards(
    input_path: Path,
    *,
    expected_shards: int,
    expected_format: str | None = None,
) -> ShardValidationResult:
    """Validate a complete modulo transform-shard output set."""

    if expected_shards < 1:
        raise ValueError("expected_shards must be positive")
    if expected_format is not None:
        expected_format = expected_format.lower()

    shards = discover_article_shards(Path(input_path))
    errors: list[str] = []
    by_index: dict[int, list[ArticleShard]] = {index: [] for index in range(expected_shards)}

    for shard in shards:
        if shard.index >= expected_shards:
            errors.append(
                f"unexpected shard id {shard.index} in {shard.path}; "
                f"expected 0..{expected_shards - 1}"
            )
            continue
        if shard.total != expected_shards:
            errors.append(
                f"wrong total-shard value in {shard.path}: "
                f"of-{shard.total}, expected of-{expected_shards}"
            )
            continue
        if shard.format not in SUPPORTED_ARTICLE_SHARD_FORMATS:
            errors.append(
                f"wrong extension for {shard.path}: .{shard.format}; "
                f"expected one of {', '.join(SUPPORTED_ARTICLE_SHARD_FORMATS)}"
            )
            continue
        if expected_format is not None and shard.format != expected_format:
            errors.append(
                f"wrong format for {shard.path}: {shard.format}; "
                f"expected {expected_format}"
            )
            continue
        by_index[shard.index].append(shard)

    for index, matches in by_index.items():
        if not matches:
            errors.append(f"missing shard output for id {index}")
        elif len(matches) > 1:
            paths = ", ".join(str(match.path) for match in matches)
            errors.append(f"duplicate shard output for id {index}: {paths}")
        else:
            schema_error = _schema_error(matches[0])
            if schema_error:
                errors.append(schema_error)

    return ShardValidationResult(
        input_path=Path(input_path),
        expected_shards=expected_shards,
        expected_format=expected_format,
        shards=tuple(shards),
        errors=tuple(errors),
    )
