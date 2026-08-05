from __future__ import annotations

import json
from collections import Counter
from datetime import date
from pathlib import Path

import polars as pl

from pubdelays.schema import CANONICAL_ARTICLE_COLUMNS
from pubdelays.transform import ExternalInputs, transform_files
from pubdelays.transform.articles import first_stage_record, journal_metadata_eligible


def write_csv(path: Path, rows: list[dict[str, object]]) -> None:
    pl.DataFrame(rows).write_csv(path)


def read_output(path: Path) -> list[dict[str, object]]:
    if path.suffix == ".parquet":
        return pl.read_parquet(path).to_dicts()
    if path.suffix == ".tsv":
        return pl.read_csv(path, separator="\t").to_dicts()
    return pl.read_csv(path).to_dicts()


def test_transform_excludes_blank_title_before_deduplication(tmp_path: Path) -> None:
    parsed = tmp_path / "parsed.jsonl"
    parsed.write_text(
        json.dumps(
            {
                "title": " ",
                "journal": "Example Journal",
                "pubdate": "2020-03-01",
                "article_date": "2020-02-01",
                "history": {"received": "2020-01-01", "accepted": "2020-01-20"},
                "publication_types": "Journal Article",
                "issn_linking": "1234-567X",
            }
        )
        + "\n",
        encoding="utf-8",
    )

    result = transform_files(parsed, tmp_path / "articles.parquet")

    assert result.counts["raw_records"] == 1
    assert result.counts["has_required_parsed_fields"] == 0
    assert pl.read_parquet(tmp_path / "articles.parquet").is_empty()


def test_transform_files_counts_filters_and_enriches_schema(tmp_path: Path) -> None:
    records = [
        {
            "title": "Fast COVID replication study",
            "journal": "Example Journal",
            "pubdate": "2020-03-01",
            "article_date": "2020-02-01",
            "history": {
                "received": "2019-12-15",
                "accepted": "2020-01-15",
                "pubmed": "2020-02-03",
            },
            "publication_types": "D016428:Journal Article",
            "issn_linking": "1234-567X",
            "keywords": "covid;replication",
            "doi": "https://doi.org/10.1000/example",
            "pmid": "1",
            "delete": False,
        },
        {
            "title": "Impossible timing",
            "journal": "Example Journal",
            "pubdate": "2020-03-01",
            "article_date": "2020-02-01",
            "history": {"received": "2020-01-20", "accepted": "2020-01-10"},
            "publication_types": "D016428:Journal Article",
            "issn_linking": "1234-567X",
            "keywords": "",
            "doi": "10.1000/bad",
            "pmid": "2",
            "delete": False,
        },
    ]
    parsed = tmp_path / "parsed.jsonl"
    parsed.write_text("\n".join(json.dumps(record) for record in records) + "\n", encoding="utf-8")

    scimago = tmp_path / "scimago.csv"
    write_csv(
        scimago,
        [
            {
                "issn_linking": "1234567X",
                "quartile_2020": "Q1",
                "rank_2020": "10",
                "h_index_2020": "50",
                "scimago_categories": "Psychology|Medicine",
            }
        ],
    )
    wos = tmp_path / "wos.csv"
    write_csv(
        wos,
        [
            {
                "issn_linking": "1234567X",
                "asjc": "3203",
                "discipline": "social_sciences_and_humanities",
                "asjc_all": "3203|1000",
                "discipline_all": "social_sciences_and_humanities|multidisciplinary",
                "open_access_status": "Unpaywall Open Acess",
            }
        ],
    )
    npi = tmp_path / "npi.csv"
    write_csv(
        npi,
        [
            {
                "issn_linking": "1234567X",
                "npi_level_20": "1",
                "npi_discipline": "Psychology",
                "npi_field": "Psychology",
                "is_conference": "0",
                "is_series": "0",
                "established": "1999",
                "ceased": "2022",
                "country_of_publication": "United Kingdom",
            }
        ],
    )
    doaj = tmp_path / "doaj.csv"
    write_csv(
        doaj,
        [
            {
                "issn_linking": "1234567X",
                "does_the_journal_comply_to_doaj_s_definition_of_open_access": "Yes",
                "apc": "Yes",
                "apc_amount": "1000",
            }
        ],
    )
    retractions = tmp_path / "retraction_watch.csv"
    write_csv(
        retractions,
        [
            {
                "doi": "10.1000/example",
                "retraction_doi": "",
                "reason": "Error",
                "retraction_nature": "Retraction",
                "retraction_date": "2021-01-01",
                "original_date": "2020-02-02",
            }
        ],
    )
    publisher = tmp_path / "publisher.csv"
    write_csv(
        publisher,
        [
            {
                "issn_linking": "1234567X",
                "publisher": "Example Publisher",
                "publisher_group": "Example Group",
                "publisher_conflict": "False",
                "publisher_group_conflict": "False",
            }
        ],
    )
    peer_review = tmp_path / "peer_review.csv"
    write_csv(
        peer_review,
        [
            {
                "doi": "10.1000/example",
                "n_review_round": "2",
                "n_reviews": "4",
                "first_review_date": "2020-01-07",
                "last_review_date": "2020-01-10",
                "n_reviewers": "3",
                "date_first_accepted": "2020-01-05",
                "review_cycle_delay": "3",
                "review_finding_delay": "5",
                "first_decision_delay": "10",
                "final_decision_delay": "25",
                "first_review_delay": "7",
                "peer_review_delay": "31",
            }
        ],
    )

    output = tmp_path / "articles.parquet"
    filters = tmp_path / "filters.csv"
    result = transform_files(
        parsed,
        output,
        filters_path=filters,
        external=ExternalInputs(
            scimago=scimago,
            web_of_science=wos,
            doaj=doaj,
            norwegian_list=npi,
            retraction_watch=retractions,
            publisher=publisher,
            peer_review=peer_review,
        ),
    )

    assert result.counts["raw_records"] == 2
    assert result.counts["coherent_dates"] == 1
    assert result.counts["final_rows"] == 1
    rows = read_output(output)
    assert len(rows) == 1
    row = rows[0]
    assert row["acceptance_delay"] == "31"
    assert row["publication_delay"] == "17"
    assert row["is_covid"] == "True"
    assert row["is_retracted"] == "True"
    assert row["article_date"] == "2020-02-01"
    assert row["retraction_original_date"] == "2020-02-02"
    assert "is_psych" not in row
    assert row["quartile_year"] == "Q1"
    assert row["asjc_all"] == "3203|1000"
    assert row["discipline_all"] == "social_sciences_and_humanities|multidisciplinary"
    assert row["scimago_categories"] == "Psychology|Medicine"
    assert row["publisher"] == "Example Publisher"
    assert row["publisher_group"] == "Example Group"
    assert row["publisher_conflict"] == "False"
    assert row["n_review_round"] == "2"
    assert row["review_cycle_delay"] == "3"
    assert row["review_finding_delay"] == "21"
    assert row["first_decision_delay"] == "2"
    assert row["final_decision_delay"] == "5"
    assert row["first_review_delay"] == "23"
    assert row["peer_review_delay"] == "8"
    assert row["npi_year"] == "1"
    assert row["open_access"] == "True"
    assert {r["stage"] for r in pl.read_csv(filters).to_dicts()} >= {
        "raw_records",
        "final_rows",
    }
    assert result.quality_path is not None
    quality = pl.read_parquet(result.quality_path)
    assert {"missingness", "join"}.issubset(set(quality["record_type"]))
    retraction_join = quality.filter(
        (pl.col("source") == "retraction_watch")
        & (pl.col("year") == "__all__")
        & (pl.col("metric") == "matched")
    )
    assert retraction_join["numerator"].item() == 1
    supplied = quality.filter(
        (pl.col("source") == "retraction_watch")
        & (pl.col("year") == "__all__")
        & (pl.col("metric") == "source_supplied")
    )
    assert supplied["numerator"].item() == supplied["denominator"].item()


def test_peer_review_metadata_can_join_by_pmid_when_doi_is_absent(
    tmp_path: Path,
) -> None:
    parsed = tmp_path / "parsed.jsonl"
    parsed.write_text(
        json.dumps(
            {
                "title": "Lifecycle peer review example",
                "journal": "Example Journal",
                "pubdate": "2020-03-01",
                "article_date": "2020-02-01",
                "history": {"received": "2020-01-01", "accepted": "2020-01-20"},
                "publication_types": "D016428:Journal Article",
                "issn_linking": "1234-567X",
                "keywords": "",
                "doi": "",
                "pmid": "42",
                "delete": False,
            }
        )
        + "\n",
        encoding="utf-8",
    )
    peer_review = tmp_path / "peer_review.csv"
    write_csv(
        peer_review,
        [
            {
                "pmid": "42",
                "n_reviewers": "2",
                "peer_review_delay": "19",
            }
        ],
    )

    output = tmp_path / "articles.parquet"
    transform_files(
        parsed,
        output,
        external=ExternalInputs(peer_review=peer_review),
    )

    row = read_output(output)[0]
    assert row["doi"] == ""
    assert row["n_reviewers"] == "2"
    assert row["peer_review_delay"] == "19"


def test_pubdate_fallback_used_when_article_date_missing() -> None:
    counts = Counter()
    record = {
        "history": {"received": "2020-01-01", "accepted": "2020-02-01"},
        "journal": "Example Journal",
        "pubdate": "2020-03-01",
        "article_date": "",
        "publication_types": "D016428:Journal Article",
        "issn_linking": "1234-5678",
        "title": "Example article",
        "keywords": "",
        "doi": "10.123/example",
    }
    row = first_stage_record(record, counts)
    assert row is not None
    assert row["article_date"] == "2020-03-01"
    assert row["article_date_raw"] == ""
    assert row["publication_date_source"] == "pubdate"
    assert row["publication_delay"] == 29


def test_article_date_preferred_over_pubdate() -> None:
    counts = Counter()
    record = {
        "history": {"received": "2020-01-01", "accepted": "2020-02-01"},
        "journal": "Example Journal",
        "pubdate": "2020-04-01",
        "article_date": "2020-03-01",
        "publication_types": "D016428:Journal Article",
        "issn_linking": "1234-5678",
        "title": "Example article",
        "keywords": "",
        "doi": "10.123/example",
    }
    row = first_stage_record(record, counts)
    assert row is not None
    assert row["article_date"] == "2020-03-01"
    assert row["article_date_raw"] == "2020-03-01"
    assert row["publication_date_source"] == "article_date"


def test_ceased_journal_filter_uses_publication_year() -> None:
    assert journal_metadata_eligible(
        {
            "received": "2018-01-01",
            "article_date": "2019-01-01",
            "is_conference": "0",
            "ceased": "2020",
        },
        min_received=date(2013, 1, 1),
    )
    assert not journal_metadata_eligible(
        {
            "received": "2021-01-01",
            "article_date": "2021-03-01",
            "is_conference": "0",
            "ceased": "2020",
        },
        min_received=date(2013, 1, 1),
    )
    assert journal_metadata_eligible(
        {
            "received": "2021-01-01",
            "article_date": "2021-03-01",
            "is_conference": "0",
            "ceased": "",
        },
        min_received=date(2013, 1, 1),
    )



def test_transform_reads_jsonl_with_late_non_null_nested_fields(tmp_path: Path) -> None:
    parsed = tmp_path / "parsed.jsonl"
    records = [
        {
            "title": f"No dates {index}",
            "journal": "Example Journal",
            "pubdate": "2020-03-01",
            "article_date": None,
            "history": None,
            "publication_types": "",
            "issn_linking": "",
            "keywords": "",
            "doi": "",
            "delete": False,
        }
        for index in range(3)
    ]
    records.append(
        {
            "title": "Late valid record",
            "journal": "Example Journal",
            "pubdate": "2020-03-01",
            "article_date": "2020-02-15",
            "history": {"received": "2020-01-01", "accepted": "2020-02-01"},
            "publication_types": "D016428:Journal Article",
            "issn_linking": "0000-0001",
            "keywords": "",
            "doi": "10.555/LATE",
            "delete": False,
        }
    )
    parsed.write_text("\n".join(json.dumps(record) for record in records) + "\n", encoding="utf-8")

    result = transform_files(parsed, tmp_path / "articles.parquet", external=ExternalInputs())

    assert result.counts["raw_records"] == 4
    assert result.counts["final_rows"] == 1


def test_transform_retains_rows_with_missing_external_metadata(tmp_path: Path) -> None:
    parsed = tmp_path / "parsed.jsonl"
    parsed.write_text(
        json.dumps(
            {
                "title": "No metadata article",
                "journal": "Unknown Journal",
                "pubdate": "2020-03-01",
                "article_date": "2020-02-15",
                "history": {"received": "2020-01-01", "accepted": "2020-02-01"},
                "publication_types": "D016428:Journal Article",
                "issn_linking": "0000-0001",
                "keywords": "",
                "doi": "HTTPS://DOI.ORG/10.555/MISSING",
                "pmid": "9",
                "delete": False,
            }
        )
        + "\n",
        encoding="utf-8",
    )

    output = tmp_path / "articles.parquet"
    filters = tmp_path / "filters.csv"
    result = transform_files(parsed, output, filters_path=filters, external=ExternalInputs())

    rows = read_output(output)
    assert result.counts["final_rows"] == 1
    assert rows[0]["doi"] == "10.555/missing"
    assert rows[0]["quartile_year"] == ""
    assert rows[0]["open_access"] == "False"
    assert rows[0]["is_retracted"] == "False"
    assert pl.read_parquet(output).columns == list(CANONICAL_ARTICLE_COLUMNS)


def test_zero_or_negative_delay_rows_are_counted_before_drop(tmp_path: Path) -> None:
    parsed = tmp_path / "parsed.jsonl"
    records = [
        {
            "title": "Zero publication delay",
            "journal": "Example Journal",
            "pubdate": "2020-02-01",
            "article_date": "2020-02-01",
            "history": {"received": "2020-01-01", "accepted": "2020-02-01"},
            "publication_types": "D016428:Journal Article",
            "issn_linking": "1234-5678",
            "keywords": "",
            "doi": "10.zero/delay",
            "delete": False,
        },
        {
            "title": "Good delay",
            "journal": "Example Journal",
            "pubdate": "2020-03-01",
            "article_date": "2020-03-01",
            "history": {"received": "2020-01-01", "accepted": "2020-02-01"},
            "publication_types": "D016428:Journal Article",
            "issn_linking": "1234-5678",
            "keywords": "",
            "doi": "10.good/delay",
            "delete": False,
        },
    ]
    parsed.write_text("\n".join(json.dumps(record) for record in records) + "\n", encoding="utf-8")

    result = transform_files(parsed, tmp_path / "articles.parquet", external=ExternalInputs())

    assert result.counts["has_linking_issn"] == 2
    assert result.counts["coherent_dates"] == 1
    assert result.counts["final_rows"] == 1
