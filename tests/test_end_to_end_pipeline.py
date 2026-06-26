from __future__ import annotations

import gzip
from pathlib import Path

import polars as pl

from pubdelays.cli import main
from pubdelays.manifest import Manifest
from pubdelays.schema import CANONICAL_ARTICLE_COLUMNS


def sample_xml() -> str:
    return """<?xml version="1.0" encoding="UTF-8"?>
<PubmedArticleSet>
  <PubmedArticle>
    <MedlineCitation>
      <PMID>1</PMID>
      <Article>
        <Journal><ISSN>1234-5678</ISSN><JournalIssue><PubDate><Year>2017</Year><Month>Jan</Month><Day>5</Day></PubDate></JournalIssue><Title>Example Journal</Title></Journal>
        <ArticleTitle>Example title</ArticleTitle>
        <ELocationID EIdType="doi">10.123/example</ELocationID>
        <ArticleDate><Year>2016</Year><Month>12</Month><Day>31</Day></ArticleDate>
        <PublicationTypeList><PublicationType UI="D016428">Journal Article</PublicationType></PublicationTypeList>
      </Article>
      <MedlineJournalInfo><ISSNLinking>1234-5678</ISSNLinking></MedlineJournalInfo>
    </MedlineCitation>
    <PubmedData><History><PubMedPubDate PubStatus="received"><Year>2016</Year><Month>10</Month><Day>1</Day></PubMedPubDate><PubMedPubDate PubStatus="accepted"><Year>2016</Year><Month>11</Month><Day>2</Day></PubMedPubDate></History></PubmedData>
  </PubmedArticle>
  <PubmedArticle>
    <MedlineCitation>
      <PMID>2</PMID>
      <Article>
        <Journal><ISSN>1234-5678</ISSN><JournalIssue><PubDate><Year>2017</Year><Month>Feb</Month><Day>1</Day></PubDate></JournalIssue><Title>Example Journal</Title></Journal>
        <ArticleTitle>Pubdate fallback title</ArticleTitle>
        <ELocationID EIdType="doi">10.123/fallback</ELocationID>
        <PublicationTypeList><PublicationType UI="D016428">Journal Article</PublicationType></PublicationTypeList>
      </Article>
      <MedlineJournalInfo><ISSNLinking>1234-5678</ISSNLinking></MedlineJournalInfo>
    </MedlineCitation>
    <PubmedData><History><PubMedPubDate PubStatus="received"><Year>2016</Year><Month>12</Month><Day>15</Day></PubMedPubDate><PubMedPubDate PubStatus="accepted"><Year>2017</Year><Month>1</Month><Day>2</Day></PubMedPubDate></History></PubmedData>
  </PubmedArticle>
  <PubmedArticle>
    <MedlineCitation>
      <PMID>3</PMID>
      <Article>
        <Journal><ISSN>1234-5678</ISSN><JournalIssue><PubDate><Year>2023</Year><Month>Jan</Month><Day>5</Day></PubDate></JournalIssue><Title>Example Journal</Title></Journal>
        <ArticleTitle>Ceased boundary title</ArticleTitle>
        <ELocationID EIdType="doi">10.123/ceased</ELocationID>
        <ArticleDate><Year>2023</Year><Month>1</Month><Day>5</Day></ArticleDate>
        <PublicationTypeList><PublicationType UI="D016428">Journal Article</PublicationType></PublicationTypeList>
      </Article>
      <MedlineJournalInfo><ISSNLinking>1234-5678</ISSNLinking></MedlineJournalInfo>
    </MedlineCitation>
    <PubmedData><History><PubMedPubDate PubStatus="received"><Year>2022</Year><Month>10</Month><Day>1</Day></PubMedPubDate><PubMedPubDate PubStatus="accepted"><Year>2022</Year><Month>11</Month><Day>2</Day></PubMedPubDate></History></PubmedData>
  </PubmedArticle>
  <DeleteCitation><PMID>4</PMID></DeleteCitation>
</PubmedArticleSet>
"""


def write_gz(path: Path, text: str) -> None:
    path.parent.mkdir(parents=True, exist_ok=True)
    with gzip.open(path, "wb") as handle:
        handle.write(text.encode("utf-8"))


def write_config(path: Path, values: dict[str, object]) -> Path:
    def emit(prefix: str, mapping: dict[str, object], lines: list[str]) -> None:
        scalars = {k: v for k, v in mapping.items() if not isinstance(v, dict)}
        if scalars:
            lines.append(f"[{prefix}]")
            for key, value in scalars.items():
                lines.append(f'{key} = "{value}"' if isinstance(value, str) else f"{key} = {value}")
            lines.append("")
        for key, value in mapping.items():
            if isinstance(value, dict):
                emit(f"{prefix}.{key}", value, lines)

    lines: list[str] = []
    for key, value in values.items():
        emit(key, value, lines)  # type: ignore[arg-type]
    path.write_text("\n".join(lines), encoding="utf-8")
    return path


def config_copy() -> dict[str, object]:
    return {
        "pipeline": {"manifest": "", "parse_inputs": "data/manifests/parse_inputs.txt", "transform_inputs": ""},
        "pubmed": {"xml_dir": "", "jsonl_dir": ""},
        "external": {
            "raw": {},
            "processed": {
                "scimago": "data/processed_data/scimago.csv",
                "web_of_science": "data/processed_data/web_of_science.csv",
                "doaj": "data/processed_data/doaj.csv",
                "norwegian_list": "data/processed_data/norwegian_list.csv",
                "retraction_watch": "data/processed_data/retraction_watch.csv",
                "publisher": "data/processed_data/publisher_metadata.csv",
                "peer_review": "data/processed_data/peer_review.csv",
                "pubmed_journals": "data/external/pubmed-journals.csv",
            },
        },
        "transform": {"article_shard_dir": "", "article_shard_format": "parquet", "min_received": "2013-01-01", "default_shards": 2},
        "aggregate": {
            "processed_parquet": "",
            "processed_csv": "",
            "summary_dir": "data/processed_data/summaries",
            "filter_counts": "data/processed_data/filter_counts.csv",
        },
        "analysis": {
            "cwd": ".",
            "input": "data/processed_data/processed.parquet",
            "output_dir": "data/processed_data/analysis",
            "command": ["python", "pubdelays_analysis/outputs.py"],
        },
        "validation": {
            "report_dir": "data/processed_data/validation_tables",
            "filtered_output": "data/processed_data/processed_validated.parquet",
            "min_article_date": "2016-01-01",
            "max_article_date": "2025-06-01",
            "min_delay_days": 1,
            "max_delay_days": 1095,
        },
    }


def configure(tmp_path: Path) -> Path:
    values = config_copy()
    values["pipeline"]["manifest"] = "data/manifests/pipeline.sqlite"
    values["pipeline"]["transform_inputs"] = "data/manifests/transform_inputs.txt"
    values["pubmed"]["xml_dir"] = "data/raw_data/pubmed/xmls"
    values["pubmed"]["jsonl_dir"] = "data/temp_data/pubmed/jsonl"
    values["transform"]["article_shard_dir"] = "data/temp_data/article_parquet"
    values["transform"]["default_shards"] = 2
    values["aggregate"]["processed_parquet"] = "data/processed_data/processed.parquet"
    values["aggregate"]["processed_csv"] = "data/processed_data/processed.csv"
    values["aggregate"]["summary_dir"] = "data/processed_data/summaries"
    raw = values["external"]["raw"]
    raw["scimago_dir"] = "data/raw_data/scimago"
    raw["web_of_science_csv"] = "data/raw_data/web_of_science/wos.csv"
    raw["doaj_csv"] = "data/raw_data/directory_of_open_access_journals/doaj.csv"
    raw["norwegian_list_csv"] = "data/raw_data/norwegian_publication_indicator/npi.csv"
    raw["retraction_watch_csv"] = "data/raw_data/retraction_watch/retraction_watch.csv"
    raw["publisher_csv"] = "data/raw_data/publisher_metadata/publishers.csv"
    raw["peer_review_csv"] = "data/raw_data/peer_review/peer_review.csv"
    return write_config(tmp_path / "config.toml", values)


def write_external_inputs(root: Path) -> None:
    scimago = root / "data/raw_data/scimago"
    scimago.mkdir(parents=True)
    header = "Title;Issn;SJR Best Quartile;H index;Rank;SJR\n"
    row = "Example Journal;1234-5678;Q1;50;10;1.2\n"
    for year in range(2015, 2025):
        (scimago / f"scimagojr {year}.csv").write_text(header + row, encoding="utf-8")

    (root / "data/raw_data/web_of_science").mkdir(parents=True)
    (root / "data/raw_data/web_of_science/wos.csv").write_text(
        "Source Title,Print-ISSN,E-ISSN,Source Type,All Science Journal Classification Codes (ASJC),Open Access status\n"
        "Example Journal,1234-5678,,Journal,3203,Unpaywall Open Acess\n",
        encoding="utf-8",
    )
    (root / "data/raw_data/directory_of_open_access_journals").mkdir(parents=True)
    (root / "data/raw_data/directory_of_open_access_journals/doaj.csv").write_text(
        "Journal title,Journal ISSN (print version),Journal EISSN (online version),Review process,APC,APC amount,Does the journal comply to DOAJ's definition of open access?\n"
        "Example Journal,1234-5678,,Peer review,Yes,1000,Yes\n",
        encoding="utf-8",
    )
    (root / "data/raw_data/norwegian_publication_indicator").mkdir(parents=True)
    (root / "data/raw_data/norwegian_publication_indicator/npi.csv").write_text(
        "Original Title;Print ISSN;Online ISSN;Open Access;NPI Academic Discipline;NPI Scientific Field;"
        "Level 2025;Level 2024;Level 2023;Level 2022;Level 2021;Level 2020;Level 2019;Level 2018;Level 2017;Level 2016;Level 2015;"
        "Country of Publication;Language;Conference Proceedings;Series;Established;Ceased\n"
        "Example Journal;1234-5678;;DOAJ;Psychology;Psychology;1;1;1;1;1;1;1;1;1;1;1;NO;EN;0;0;1999;2022\n",
        encoding="utf-8",
    )
    (root / "data/raw_data/retraction_watch").mkdir(parents=True)
    (root / "data/raw_data/retraction_watch/retraction_watch.csv").write_text(
        "RetractionDate,OriginalPaperDate,Title,OriginalPaperDOI,RetractionDOI,RetractionNature,Reason\n"
        "01/02/2021 00:00,12/31/2016 00:00,Example title,10.123/example,10.123/retract,Retraction,Error\n",
        encoding="utf-8",
    )
    (root / "data/raw_data/publisher_metadata").mkdir(parents=True)
    (root / "data/raw_data/publisher_metadata/publishers.csv").write_text(
        "ISSN,Publisher,Publisher Group\n"
        "1234-5678,Example Publisher,Example Group\n",
        encoding="utf-8",
    )


def test_tiny_end_to_end_pipeline(tmp_path: Path) -> None:
    config = configure(tmp_path)
    write_external_inputs(tmp_path)
    write_gz(tmp_path / "data/raw_data/pubmed/xmls/sample.xml.gz", sample_xml())

    common = ["--config", str(config)]
    assert main([*common, "init-dirs"]) == 0
    assert main([*common, "external-all", "--resume"]) == 0
    assert main([*common, "parse", "--jobs", "1", "--format", "jsonl", "--resume"]) == 0
    assert main([*common, "validate"]) == 0
    assert main([*common, "transform-shards", "--shards", "2", "--jobs", "1", "--resume"]) == 0
    assert main([*common, "validate-shards", "--shards", "2", "--format", "parquet"]) == 0
    assert main([*common, "aggregate-all", "--shards", "2", "--format", "parquet", "--resume"]) == 0

    processed = pl.read_parquet(tmp_path / "data/processed_data/processed.parquet")
    assert processed.columns == list(CANONICAL_ARTICLE_COLUMNS)
    assert processed.height == 2
    rows = {row["title"]: row for row in processed.to_dicts()}
    row = rows["Example title"]
    assert row["acceptance_delay"] == "32"
    assert row["publication_delay"] == "59"
    assert row["quartile_year"] == "Q1"
    assert "is_psych" not in row
    assert row["open_access"] == "True"
    assert row["publisher"] == "Example Publisher"
    assert row["publisher_group"] == "Example Group"
    assert row["is_retracted"] == "True"
    fallback = rows["Pubdate fallback title"]
    assert fallback["publication_date_source"] == "pubdate"
    assert fallback["publication_delay"] == "30"
    assert "Ceased boundary title" not in rows

    filters = pl.read_csv(
        tmp_path / "data/temp_data/article_parquet/articles-shard-00000-of-00002.filters.csv"
    )
    assert filters.filter(pl.col("stage") == "eligible_journal_metadata")["count"][0] == 2

    manifest_rows = Manifest(tmp_path / "data/manifests/pipeline.sqlite").rows(limit=100)
    stages = {row["stage"] for row in manifest_rows}
    assert {"parse", "transform-shard", "aggregate-all", "external-scimago"} <= stages
    assert (tmp_path / "data/temp_data/article_parquet/articles-shard-00001-of-00002.filters.csv").exists()
