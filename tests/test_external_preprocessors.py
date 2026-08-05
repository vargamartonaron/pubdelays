from __future__ import annotations

from pathlib import Path

import polars as pl

from pubdelays.external import (
    preprocess_doaj,
    preprocess_npi,
    preprocess_peer_review,
    preprocess_publisher,
    preprocess_retraction_watch,
    preprocess_scimago,
    preprocess_wos,
)
from pubdelays.external.wos import discipline_for_asjc


def rows(path: Path) -> list[dict[str, object]]:
    return pl.read_csv(path, infer_schema=False).to_dicts()


def test_external_raw_reader_preserves_identifier_like_strings(tmp_path: Path) -> None:
    from pubdelays.external.common import read_csv_polars

    raw = tmp_path / "ids.csv"
    raw.write_text("issn,doi,asjc\n0123-4567,  HTTPS://DOI.ORG/10.X/ABC  ,03203\n", encoding="utf-8")

    df = read_csv_polars(raw)

    assert df["issn"].to_list() == ["0123-4567"]
    assert df["doi"].to_list() == ["  HTTPS://DOI.ORG/10.X/ABC  "]
    assert df["asjc"].to_list() == ["03203"]


def test_wos_preprocessor_splits_asjc_and_issn_then_preserves_all_codes(
    tmp_path: Path,
) -> None:
    raw = tmp_path / "wos.csv"
    raw.write_text(
        "Source Title,Print-ISSN,E-ISSN,Source Type,All Science Journal Classification Codes (ASJC),Open Access status\n"
        "Journal A,1234-567X,2222-3333,Journal,3203; 1000,Unpaywall Open Acess\n"
        "Book A,9999-9999,,Book,3203,No\n",
        encoding="utf-8",
    )
    out = tmp_path / "web_of_science.csv"
    assert preprocess_wos(raw, out) == 2
    data = rows(out)
    assert data[0]["issn_linking"] == "1234567X"
    assert data[0]["asjc"] == 3203 or data[0]["asjc"] == "3203"
    assert data[0]["discipline"] == "social_sciences_and_humanities"
    assert data[0]["asjc_all"] == "3203|1000"
    assert data[0]["discipline_all"] == "social_sciences_and_humanities|multidisciplinary"
    assert data[1]["issn_linking"] == "22223333"
    assert data[1]["asjc_all"] == "3203|1000"


def test_npi_preprocessor_removes_issn_hyphens_and_splits_print_online(
    tmp_path: Path,
) -> None:
    raw = tmp_path / "npi.csv"
    raw.write_text(
        "Original Title;Print ISSN;Online ISSN;Open Access;NPI Academic Discipline;NPI Scientific Field;"
        "Level 2025;Level 2024;Level 2023;Level 2022;Level 2021;Level 2020;Level 2019;Level 2018;Level 2017;Level 2016;Level 2015;"
        "Country of Publication;Language;Conference Proceedings;Series;Established;Ceased\n"
        "Journal A;1234-567X;8765-4321;DOAJ;Psychology;Psychology;2;1;1;1;1;1;1;1;1;1;1;NO;EN;0;0;1999;\n",
        encoding="utf-8",
    )
    out = tmp_path / "npi_out.csv"
    assert preprocess_npi(raw, out) == 2
    data = rows(out)
    assert data[0]["issn_linking"] == "1234567X"
    assert data[1]["issn_linking"] == "87654321"
    assert data[0]["npi_title"] == "Journal A"


def test_publisher_preprocessor_keeps_first_values_and_flags_conflicts(tmp_path: Path) -> None:
    raw = tmp_path / "publishers.csv"
    raw.write_text(
        "ISSN,Publisher,Publisher Group\n"
        "1234-5678,Elsevier,RELX\n"
        "1234-5678,Elsevier BV,RELX\n"
        "2222-3333,,\n",
        encoding="utf-8",
    )
    out = tmp_path / "publisher_metadata.csv"

    assert preprocess_publisher(raw, out) == 2
    data = rows(out)
    assert data[0]["issn_linking"] == "12345678"
    assert data[0]["publisher"] == "Elsevier"
    assert data[0]["publisher_group"] == "RELX"
    assert data[0]["publisher_conflict"] == "True"
    assert data[0]["publisher_group_conflict"] == "False"
    assert data[1]["publisher"] == ""


def test_retraction_watch_preprocessor_preserves_rows_for_quality_audit(tmp_path: Path) -> None:
    raw = tmp_path / "rw.csv"
    raw.write_text(
        "RetractionDate,OriginalPaperDate,Title,OriginalPaperDOI,RetractionDOI,RetractionNature,Reason\n"
        "01/02/2015 00:00,01/01/2013 00:00,Paper,  HTTPS://DOI.ORG/10.1/X  ,DOI: 10.1/R,Retraction,Error\n"
        "01/02/2014 00:00,01/01/2013 00:00,Old,10.1/y,10.1/ry,Retraction,Error\n",
        encoding="utf-8",
    )
    out = tmp_path / "rw_out.csv"
    assert preprocess_retraction_watch(raw, out) == 2
    data = rows(out)
    assert data[0]["doi"] == "10.1/x"
    assert data[0]["retraction_doi"] == "10.1/r"
    assert str(data[0]["retraction_date"]) == "2015-01-02"


def test_wos_preserves_asjc_identifier_text_until_classification(tmp_path: Path) -> None:
    raw = tmp_path / "wos.csv"
    raw.write_text(
        "Source Title,Print-ISSN,E-ISSN,Source Type,All Science Journal Classification Codes (ASJC),Open Access status\n"
        "Journal A,0123-4567,,Journal,03203; 1000,No\n",
        encoding="utf-8",
    )
    out = tmp_path / "wos_out.csv"

    assert preprocess_wos(raw, out) == 1
    data = rows(out)
    assert data[0]["issn_linking"] == "01234567"
    assert str(data[0]["asjc"]) == "03203"
    assert data[0]["asjc_all"] == "03203|1000"
    assert data[0]["discipline"] == "social_sciences_and_humanities"


def test_wos_discipline_boundaries_match_documented_case_when() -> None:
    assert discipline_for_asjc(1000) == "multidisciplinary"
    assert discipline_for_asjc(1111) == "life_sciences"
    assert discipline_for_asjc(3207) == "social_sciences_and_humanities"
    assert discipline_for_asjc(3616) == "health_sciences"


def test_scopus_source_list_accepts_current_issn_header(tmp_path: Path) -> None:
    raw = tmp_path / "scopus.csv"
    raw.write_text(
        "Source Title,ISSN,EISSN,Source Type,All Science Journal Classification Codes (ASJC),Open Access Status\n"
        "Journal A,1234-567X,2222-3333,Journal,2700,DOAJ\n",
        encoding="utf-8",
    )
    out = tmp_path / "scopus_out.csv"

    assert preprocess_wos(raw, out) == 2
    assert {row["issn_linking"] for row in rows(out)} == {"1234567X", "22223333"}


def test_scimago_preprocessor_preserves_multiple_categories(tmp_path: Path) -> None:
    for year in (2023, 2024):
        raw = tmp_path / f"scimagojr {year}.csv"
        raw.write_text(
            "Title;Issn;SJR Best Quartile;H index;Rank;SJR;Categories\n"
            'Journal A;"1234-567X, 2222-3333";Q1;50;10;2.5;"Psychology; Medicine"\n',
            encoding="utf-8",
        )
    out = tmp_path / "scimago.csv"

    assert preprocess_scimago(tmp_path, out, start_year=2023, end_year=2024) == 2
    data = rows(out)
    assert data[0]["issn_linking"] == "1234567X"
    assert data[0]["scimago_categories"] == "Psychology|Medicine"
    assert data[1]["scimago_categories"] == "Psychology|Medicine"


def test_scimago_preprocessor_uses_2025_snapshot(tmp_path: Path) -> None:
    for year, quartile in ((2024, "Q2"), (2025, "Q1")):
        (tmp_path / f"scimagojr {year}.csv").write_text(
            "Title;Issn;SJR Best Quartile;H index;Rank;SJR;Categories\n"
            f'Journal A;"1234-567X";{quartile};50;10;2.5;Medicine\n',
            encoding="utf-8",
        )
    out = tmp_path / "scimago.csv"

    assert preprocess_scimago(tmp_path, out, start_year=2024, end_year=2025) == 1
    assert rows(out)[0]["quartile_2025"] == "Q1"


def test_scimago_preprocessor_repairs_unescaped_publisher_quotes(tmp_path: Path) -> None:
    raw = tmp_path / "scimagojr 2024.csv"
    raw.write_text(
        "Rank;Sourceid;Title;Type;Issn;SJR;SJR Best Quartile;H index;Publisher;Categories;Areas\n"
        '1;123;Journal A;journal;"1234-567X";0,145;Q4;17;'
        '"Wei sheng yan jiu" bian ji bu";"Medicine (miscellaneous) (Q4)";Medicine\n',
        encoding="utf-8",
    )
    out = tmp_path / "scimago.csv"

    assert preprocess_scimago(tmp_path, out, start_year=2024, end_year=2024) == 1
    data = rows(out)
    assert data[0]["issn_linking"] == "1234567X"
    assert data[0]["scimago_categories"] == "Medicine (miscellaneous) (Q4)"


def test_peer_review_preprocessor_aggregates_raw_events_by_doi(tmp_path: Path) -> None:
    raw = tmp_path / "peer_review.csv"
    raw.write_text(
        "date_accepted,doi,date_reviewed,review_round\n"
        "2020-02-01, HTTPS://DOI.ORG/10.1000/Example ,2020-01-10,1\n"
        "2020-02-01,10.1000/example,2020-01-20,2\n"
        ",10.2000/other,2012-12-31,1\n"
        ",10.3000/future,2026-01-01,1\n",
        encoding="utf-8",
    )
    out = tmp_path / "peer_review_out.csv"

    assert preprocess_peer_review(raw, out) == 1
    data = rows(out)
    assert data[0] == {
        "doi": "10.1000/example",
        "n_review_round": "2",
        "n_reviews": "2",
        "first_review_date": "2020-01-10",
        "last_review_date": "2020-01-20",
        "n_reviewers": None,
        "date_first_accepted": "2020-02-01",
        "review_cycle_delay": "10",
    }


def test_doaj_preprocessor_selects_expected_columns(tmp_path: Path) -> None:
    raw = tmp_path / "doaj.csv"
    raw.write_text(
        "Journal title,Journal ISSN (print version),Journal EISSN (online version),Review process,APC,APC amount,Does the journal comply to DOAJ's definition of open access?,Ignored\n"
        "Journal A,1234-567X,8765-4321,Peer review,Yes,1000,Yes,x\n"
        "No ISSN,,,Peer review,No,0,Yes,x\n",
        encoding="utf-8",
    )
    out = tmp_path / "doaj_out.csv"
    assert preprocess_doaj(raw, out) == 2
    data = rows(out)
    assert data[0]["issn_linking"] == "1234567X"
    assert data[1]["issn_linking"] == "87654321"
    assert str(data[0]["apc_amount"]) == "1000"
    assert "ignored" not in data[0]
