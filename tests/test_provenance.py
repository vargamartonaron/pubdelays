from __future__ import annotations

import json
from pathlib import Path

from pubdelays.cli import main
from pubdelays.provenance import VARIABLE_PROVENANCE, render_provenance, validate_provenance
from pubdelays.schema import CANONICAL_ARTICLE_COLUMNS


def test_provenance_exactly_covers_canonical_schema() -> None:
    assert validate_provenance() == []
    assert [item.variable for item in VARIABLE_PROVENANCE] == list(CANONICAL_ARTICLE_COLUMNS)
    assert all(item.source and item.level and item.missingness for item in VARIABLE_PROVENANCE)


def test_provenance_renderers_and_cli(tmp_path: Path) -> None:
    assert "| variable | level |" in render_provenance("markdown")
    assert render_provenance("csv").startswith("variable,level,")
    assert len(json.loads(render_provenance("json"))) == len(CANONICAL_ARTICLE_COLUMNS)
    output = tmp_path / "provenance.csv"
    assert main(["provenance", "--format", "csv", "--output", str(output)]) == 0
    assert output.exists()
