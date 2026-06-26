from __future__ import annotations

from importlib.util import module_from_spec, spec_from_file_location
from pathlib import Path

import polars as pl

from pubdelays.schema import CANONICAL_ARTICLE_COLUMNS
from pubdelays.summaries import derive_summary_tables

_ANALYSIS_OUTPUTS = Path(__file__).resolve().parents[1] / "pubdelays_analysis" / "outputs.py"
_SPEC = spec_from_file_location("analysis_outputs", _ANALYSIS_OUTPUTS)
assert _SPEC is not None and _SPEC.loader is not None
_analysis_outputs = module_from_spec(_SPEC)
_SPEC.loader.exec_module(_analysis_outputs)
derive_analysis_outputs = _analysis_outputs.derive_analysis_outputs


def canonical_row(**updates: str) -> dict[str, str]:
    row = {column: "" for column in CANONICAL_ARTICLE_COLUMNS}
    row.update(
        {
            "journal": "Example Journal",
            "issn_linking": "12345678",
            "discipline": "health_sciences",
            "publisher": "Example Publisher",
            "publisher_group": "Example Group",
            "article_date": "2020-02-01",
            "acceptance_delay": "31",
            "publication_delay": "17",
        }
    )
    row.update(updates)
    return row


def test_derive_summary_tables_from_processed_parquet(tmp_path: Path) -> None:
    processed = tmp_path / "processed.parquet"
    pl.DataFrame(
        [
            canonical_row(title="A", publication_delay="17"),
            canonical_row(title="B", publication_delay="19"),
            canonical_row(
                title="C",
                journal="Other Journal",
                issn_linking="87654321",
                discipline="social_sciences_and_humanities",
                publisher="",
                publisher_group="",
                article_date="2021-03-01",
                publication_delay="40",
            ),
        ]
    ).write_parquet(processed)

    outputs = derive_summary_tables(processed, tmp_path / "summaries")

    assert set(outputs) == {"journal_year", "field_year", "publisher_year", "delay_distribution"}
    journal = pl.read_csv(outputs["journal_year"], infer_schema=False)
    assert journal.filter(pl.col("journal") == "Example Journal")["articles"].item() == "2"
    publisher = pl.read_csv(outputs["publisher_year"], infer_schema=False)
    assert publisher.height == 1
    assert publisher["publisher"].to_list() == ["Example Publisher"]
    distribution = pl.read_csv(outputs["delay_distribution"], infer_schema=False)
    assert set(distribution["article_year"].to_list()) == {"2020", "2021"}


def test_derive_analysis_outputs_include_analysis_tables_and_peer_review_data(tmp_path: Path) -> None:
    processed = tmp_path / "processed.parquet"
    pl.DataFrame(
        [
            canonical_row(
                title="A",
                is_covid="True",
                is_mega="False",
                open_access="True",
                is_retracted="False",
                quartile_year="Q1",
                npi_year="1",
                npi_discipline="Psychology",
                n_reviewers="2",
                n_review_round="1",
                peer_review_delay="21",
            ),
            canonical_row(
                title="B",
                journal="Other Journal",
                issn_linking="87654321",
                discipline="social_sciences_and_humanities",
                article_date="2021-03-01",
                publication_delay="40",
                is_covid="False",
                is_mega="False",
                open_access="False",
                is_retracted="True",
                quartile_year="Q2",
                npi_year="2",
                npi_discipline="Psychology",
                reason="Error",
            ),
        ]
    ).write_parquet(processed)

    outputs = derive_analysis_outputs(processed, tmp_path / "tables", tmp_path / "figures")

    assert "table:article_number_retracted" in outputs
    assert "table:summary_retracted_article" in outputs
    assert "figure_data:peer_review_delay_yearly" in outputs
    assert "figure:peer_review_delay_yearly" in outputs
    assert outputs["figure:peer_review_delay_yearly"].read_text(encoding="utf-8").startswith("<svg")
    assert pl.read_csv(outputs["table:delay_summary"], infer_schema=False).height == 1
    peer_review = pl.read_csv(outputs["figure_data:peer_review_delay_yearly"], infer_schema=False)
    assert peer_review["mean"].to_list() == ["21.0"]
