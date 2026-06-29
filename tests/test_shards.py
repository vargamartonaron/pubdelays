from __future__ import annotations

from pathlib import Path

import polars as pl

from pubdelays.cli import main
from pubdelays.schema import CANONICAL_ARTICLE_COLUMNS, FILTER_STAGES
from pubdelays.shards import validate_article_shards


def canonical_frame(rows: int = 1) -> pl.DataFrame:
    values = {col: [""] * rows for col in CANONICAL_ARTICLE_COLUMNS}
    if rows:
        values["title"] = [f"title {index}" for index in range(rows)]
    return pl.DataFrame(values)


def write_shard(directory: Path, index: int, total: int, fmt: str = "parquet", rows: int = 1) -> Path:
    path = directory / f"articles-shard-{index:05d}-of-{total:05d}.{fmt}"
    path.parent.mkdir(parents=True, exist_ok=True)
    df = canonical_frame(rows)
    if fmt == "parquet":
        df.write_parquet(path)
    elif fmt == "tsv":
        df.write_csv(path, separator="\t")
    else:
        df.write_csv(path)
    return path


def test_validate_shards_accepts_complete_local_and_slurm_style_filenames(tmp_path: Path) -> None:
    write_shard(tmp_path, 0, 2)
    write_shard(tmp_path, 1, 2)
    (tmp_path / "articles-shard-00000-of-00002.filters.csv").write_text(
        "stage,count\nraw_records,1\n", encoding="utf-8"
    )

    result = validate_article_shards(tmp_path, expected_shards=2, expected_format="parquet")

    assert result.ok
    assert [shard.index for shard in result.shards] == [0, 1]


def test_validate_shards_detects_missing_shard(tmp_path: Path) -> None:
    write_shard(tmp_path, 0, 2)

    result = validate_article_shards(tmp_path, expected_shards=2, expected_format="parquet")

    assert not result.ok
    assert "missing shard output for id 1" in result.errors


def test_validate_shards_detects_duplicate_shard(tmp_path: Path) -> None:
    write_shard(tmp_path, 0, 1, "parquet")
    write_shard(tmp_path, 0, 1, "csv")

    result = validate_article_shards(tmp_path, expected_shards=1)

    assert not result.ok
    assert any("duplicate shard output for id 0" in error for error in result.errors)


def test_validate_shards_detects_wrong_total(tmp_path: Path) -> None:
    write_shard(tmp_path, 0, 3, "parquet")

    result = validate_article_shards(tmp_path, expected_shards=2, expected_format="parquet")

    assert not result.ok
    assert any("wrong total-shard value" in error for error in result.errors)
    assert any("missing shard output for id 0" in error for error in result.errors)


def test_validate_shards_detects_wrong_format(tmp_path: Path) -> None:
    write_shard(tmp_path, 0, 1, "csv")

    result = validate_article_shards(tmp_path, expected_shards=1, expected_format="parquet")

    assert not result.ok
    assert any("wrong format" in error for error in result.errors)


def test_validate_shards_accepts_empty_canonical_parquet_shard(tmp_path: Path) -> None:
    write_shard(tmp_path, 0, 1, rows=0)

    result = validate_article_shards(tmp_path, expected_shards=1, expected_format="parquet")

    assert result.ok


def test_validate_shards_detects_schema_invalid_shard(tmp_path: Path) -> None:
    path = tmp_path / "articles-shard-00000-of-00001.parquet"
    pl.DataFrame({"title": ["missing most columns"]}).write_parquet(path)

    result = validate_article_shards(tmp_path, expected_shards=1, expected_format="parquet")

    assert not result.ok
    assert any("schema-invalid shard" in error for error in result.errors)


def test_validate_shards_detects_unexpected_stale_schema_column(tmp_path: Path) -> None:
    path = write_shard(tmp_path, 0, 1)
    df = pl.read_parquet(path).with_columns(pl.lit("").alias("is_psych"))
    df.write_parquet(path)

    result = validate_article_shards(tmp_path, expected_shards=1, expected_format="parquet")

    assert not result.ok
    assert any("unexpected columns: is_psych" in error for error in result.errors)


def test_aggregate_all_blocks_incomplete_shard_set(tmp_path: Path) -> None:
    write_shard(tmp_path, 0, 2)
    parquet = tmp_path / "processed.parquet"
    csv = tmp_path / "processed.csv"
    manifest = tmp_path / "manifest.sqlite"

    code = main(
        [
            "aggregate-all",
            "--input",
            str(tmp_path),
            "--parquet",
            str(parquet),
            "--csv",
            str(csv),
            "--manifest",
            str(manifest),
            "--shards",
            "2",
            "--format",
            "parquet",
        ]
    )

    assert code == 1
    assert not parquet.exists()
    assert not csv.exists()


def test_aggregate_all_resume_regenerates_stale_schema_outputs(tmp_path: Path) -> None:
    write_shard(tmp_path, 0, 1)
    parquet = tmp_path / "processed.parquet"
    csv = tmp_path / "processed.csv"
    manifest = tmp_path / "manifest.sqlite"
    stale_columns = [
        column
        for column in CANONICAL_ARTICLE_COLUMNS
        if column not in {"n_review_round", "peer_review_delay"}
    ]
    stale = pl.DataFrame({column: [""] for column in stale_columns}).with_columns(
        pl.lit("").alias("is_psych")
    )
    stale.write_parquet(parquet)
    stale.write_csv(csv)

    code = main(
        [
            "aggregate-all",
            "--input",
            str(tmp_path),
            "--parquet",
            str(parquet),
            "--csv",
            str(csv),
            "--manifest",
            str(manifest),
            "--shards",
            "1",
            "--format",
            "parquet",
            "--resume",
        ]
    )

    assert code == 0
    assert pl.read_parquet(parquet).columns == list(CANONICAL_ARTICLE_COLUMNS)
    assert pl.read_csv(csv).columns == list(CANONICAL_ARTICLE_COLUMNS)


def test_transform_shard_writes_empty_canonical_shard_for_empty_selection(
    tmp_path: Path,
) -> None:
    input_list = tmp_path / "transform_inputs.txt"
    input_list.write_text(f"{tmp_path / 'parsed.jsonl'}\n", encoding="utf-8")
    output_dir = tmp_path / "article_shards"

    code = main(
        [
            "transform-shard",
            "--input-list",
            str(input_list),
            "--output-dir",
            str(output_dir),
            "--shard-index",
            "1",
            "--shards",
            "2",
            "--manifest",
            str(tmp_path / "manifest.sqlite"),
        ]
    )

    output = output_dir / "articles-shard-00001-of-00002.parquet"
    filters = output_dir / "articles-shard-00001-of-00002.filters.csv"
    assert code == 0
    assert pl.read_parquet(output).columns == list(CANONICAL_ARTICLE_COLUMNS)
    assert pl.read_parquet(output).is_empty()
    assert pl.read_csv(filters)["count"].to_list() == [0] * len(FILTER_STAGES)


def test_transform_shard_resume_regenerates_stale_schema_shard(tmp_path: Path) -> None:
    parsed = tmp_path / "parsed.json"
    parsed.write_text("[]\n", encoding="utf-8")
    input_list = tmp_path / "transform_inputs.txt"
    input_list.write_text(f"{parsed}\n", encoding="utf-8")
    output_dir = tmp_path / "article_shards"
    output_dir.mkdir()
    output = output_dir / "articles-shard-00000-of-00001.parquet"
    stale_columns = [
        column
        for column in CANONICAL_ARTICLE_COLUMNS
        if column not in {"n_review_round", "peer_review_delay"}
    ]
    pl.DataFrame({column: [""] for column in stale_columns}).write_parquet(output)

    code = main(
        [
            "transform-shard",
            "--input-list",
            str(input_list),
            "--output-dir",
            str(output_dir),
            "--shard-index",
            "0",
            "--shards",
            "1",
            "--manifest",
            str(tmp_path / "manifest.sqlite"),
            "--resume",
        ]
    )

    assert code == 0
    rewritten = pl.read_parquet(output)
    assert rewritten.columns == list(CANONICAL_ARTICLE_COLUMNS)
    assert rewritten.is_empty()


def test_validate_shards_cli_uses_same_validator(tmp_path: Path) -> None:
    write_shard(tmp_path, 0, 1)

    code = main(
        [
            "validate-shards",
            "--input",
            str(tmp_path),
            "--shards",
            "1",
            "--format",
            "parquet",
        ]
    )

    assert code == 0
