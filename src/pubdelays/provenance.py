"""Canonical variable-level provenance for the public analysis dataset."""

from __future__ import annotations

import csv
import io
import json
from dataclasses import asdict, dataclass
from pathlib import Path

from pubdelays.fs import atomic_output_path
from pubdelays.schema import CANONICAL_ARTICLE_COLUMNS


@dataclass(frozen=True)
class VariableProvenance:
    variable: str
    level: str
    kind: str
    source: str
    source_fields: str
    join_key: str
    derivation: str
    units: str
    missingness: str


def _entry(
    variable: str,
    level: str,
    kind: str,
    source: str,
    source_fields: str,
    join_key: str = "",
    derivation: str = "Direct extraction",
    units: str = "text",
    missingness: str = "Empty when the source does not report the field",
) -> VariableProvenance:
    return VariableProvenance(
        variable, level, kind, source, source_fields, join_key, derivation, units, missingness
    )


VARIABLE_PROVENANCE: tuple[VariableProvenance, ...] = (
    _entry("pmid", "article", "direct", "PubMed/MEDLINE", "PMID", units="identifier", missingness="Normally populated; missing identifiers use DOI/title fallback identity and are audited"),
    _entry("is_covid", "article", "derived", "PubMed/MEDLINE", "ArticleTitle; KeywordList", derivation="Case-insensitive synonym match in title and keywords", units="True/False", missingness="Always populated; absent evidence is False"),
    _entry("received", "article", "direct", "PubMed/MEDLINE", "History/PubMedPubDate[@PubStatus='received']", units="ISO date", missingness="Articles without a usable receipt date are excluded after pre-filter auditing"),
    _entry("article_date", "article", "derived", "PubMed/MEDLINE", "ArticleDate; JournalIssue/PubDate", derivation="ArticleDate, otherwise PubDate", units="ISO date", missingness="Articles without either usable date are excluded after pre-filter auditing"),
    _entry("article_date_raw", "article", "direct", "PubMed/MEDLINE", "ArticleDate", units="ISO date", missingness="Empty when ArticleDate is absent; article_date may use PubDate"),
    _entry("publication_date_source", "article", "derived", "PubMed/MEDLINE", "ArticleDate; JournalIssue/PubDate", derivation="Records whether article_date came from article_date or pubdate", missingness="Empty only when no publication date is usable"),
    _entry("acceptance_delay", "article", "derived", "PubMed/MEDLINE", "received; accepted", derivation="Accepted date minus received date", units="days", missingness="Requires both dates and chronological coherence"),
    _entry("is_mega", "journal", "derived", "Study classification", "Fixed 21-ISSN megajournal list", "issn_linking", "Normalized linking ISSN membership", "True/False", "Always populated; non-membership is False"),
    _entry("issn_linking", "journal", "direct", "PubMed/MEDLINE", "MedlineJournalInfo/ISSNLinking", derivation="Remove punctuation and uppercase terminal X", units="normalized ISSN", missingness="Articles without a linking ISSN are excluded after pre-filter auditing"),
    _entry("h_index_year", "journal-year", "joined", "SCImago Journal Rank", "H index by snapshot year", "issn_linking", "Select article publication year through 2025", "index", "Empty when journal/year does not match SCImago"),
    _entry("open_access", "journal", "derived", "DOAJ; Scopus Source List; Norwegian Publication Indicator", "DOAJ compliance; Scopus open-access status; NPI DOAJ flag", "issn_linking", "True when any implemented source condition indicates open access", "True/False", "Always populated; False means no positive evidence in matched sources, not proof of closed access"),
    _entry("match_scimago", "article-journal", "derived", "SCImago Journal Rank", "ISSN match", "issn_linking", "Whether the article joined to the source", "True/False", "Always populated"),
    _entry("match_scopus", "article-journal", "derived", "Scopus Source List", "ISSN match", "issn_linking", "Whether the article joined to the source", "True/False", "Always populated"),
    _entry("match_doaj", "article-journal", "derived", "Directory of Open Access Journals", "ISSN match", "issn_linking", "Whether the article joined to the source", "True/False", "Always populated"),
    _entry("match_npi", "article-journal", "derived", "Norwegian Publication Indicator", "ISSN match", "issn_linking", "Whether the article joined to the source", "True/False", "Always populated"),
    _entry("match_publisher", "article-journal", "derived", "Publisher metadata", "ISSN match", "issn_linking", "Whether the article joined to the source", "True/False", "Always populated"),
    _entry("match_peer_review", "article", "derived", "Optional proprietary peer-review metadata", "DOI/PMID/title match", "doi, then pmid, then title", "Whether the article joined to the source", "True/False", "Always populated"),
    _entry("open_access_doaj_evidence", "article-journal", "derived", "Directory of Open Access Journals", "DOAJ compliance", "issn_linking", "Positive DOAJ open-access evidence", "True/False", "Always populated"),
    _entry("open_access_scopus_evidence", "article-journal", "derived", "Scopus Source List", "Open Access Status", "issn_linking", "Positive Unpaywall open-access label", "True/False", "Always populated"),
    _entry("open_access_npi_evidence", "article-journal", "derived", "Norwegian Publication Indicator", "Open Access", "issn_linking", "Positive DOAJ label", "True/False", "Always populated"),
    _entry("open_access_evidence_sources", "article-journal", "derived", "DOAJ; Scopus Source List; Norwegian Publication Indicator", "open-access evidence fields", "issn_linking", "Pipe-delimited sources supplying positive evidence", units="source labels", missingness="Empty when no source supplies positive evidence"),
    _entry("publication_delay", "article", "derived", "PubMed/MEDLINE", "accepted; article_date", derivation="Publication date minus accepted date", units="days", missingness="Requires acceptance and publication dates and chronological coherence"),
    _entry("publication_types", "article", "direct", "PubMed/MEDLINE", "PublicationTypeList", units="labels", missingness="Journal Article membership is required"),
    _entry("title", "article", "direct", "PubMed/MEDLINE", "ArticleTitle", units="text", missingness="Records without a title are excluded before deterministic title deduplication"),
    _entry("journal", "journal", "direct", "PubMed/MEDLINE", "Journal/Title", units="text", missingness="Required parsed field"),
    _entry("quartile_year", "journal-year", "joined", "SCImago Journal Rank", "SJR Best Quartile by year", "issn_linking", "Select article publication year through 2025", "Q1-Q4", "Empty when journal/year does not match"),
    _entry("rank_year", "journal-year", "joined", "SCImago Journal Rank", "Rank by year", "issn_linking", "Select article publication year through 2025", "rank", "Empty when journal/year does not match"),
    _entry("discipline", "journal", "derived", "Scopus Source List", "First ASJC code", "issn_linking", "Map first ASJC code to broad discipline", missingness="Empty when the Scopus journal match or ASJC is unavailable"),
    _entry("asjc", "journal", "joined", "Scopus Source List", "First ASJC code", "issn_linking", units="ASJC code", missingness="Empty when the Scopus journal match or ASJC is unavailable"),
    _entry("discipline_all", "journal", "derived", "Scopus Source List", "All ASJC codes", "issn_linking", "Map all ASJC codes to unique pipe-delimited disciplines", missingness="Empty when the Scopus journal match or ASJC is unavailable"),
    _entry("asjc_all", "journal", "joined", "Scopus Source List", "All ASJC codes", "issn_linking", "Unique pipe-delimited codes", "ASJC codes", "Empty when the Scopus journal match or ASJC is unavailable"),
    _entry("scimago_categories", "journal", "joined", "SCImago Journal Rank", "Categories", "issn_linking", units="pipe-delimited labels", missingness="Empty when journal does not match SCImago"),
    _entry("publisher", "journal", "joined", "Publisher metadata", "Publisher", "issn_linking", "First non-empty value after conflict audit", missingness="Empty when source is absent, unmatched, or blank"),
    _entry("publisher_group", "journal", "joined", "Publisher metadata", "Publisher group/parent", "issn_linking", "First non-empty value after conflict audit", missingness="Empty when source is absent, unmatched, or blank"),
    _entry("publisher_conflict", "journal", "derived", "Publisher metadata", "Publisher values per ISSN", "issn_linking", "True when normalized ISSN rows disagree", "True/False", "Empty when publisher metadata is unavailable"),
    _entry("publisher_group_conflict", "journal", "derived", "Publisher metadata", "Publisher-group values per ISSN", "issn_linking", "True when normalized ISSN rows disagree", "True/False", "Empty when publisher metadata is unavailable"),
    _entry("npi_discipline", "journal", "joined", "Norwegian Publication Indicator", "NPI academic discipline", "issn_linking", missingness="Empty when NPI is absent or unmatched"),
    _entry("npi_field", "journal", "joined", "Norwegian Publication Indicator", "NPI scientific field", "issn_linking", missingness="Empty when NPI is absent or unmatched"),
    _entry("npi_year", "journal-year", "joined", "Norwegian Publication Indicator", "Level 2015-2025", "issn_linking", "Select article publication year through 2025", "level 0-2", "Empty when journal/year does not match"),
    _entry("is_series", "journal", "joined", "Norwegian Publication Indicator", "Series", "issn_linking", units="source flag", missingness="Empty when NPI is absent or unmatched"),
    _entry("established", "journal", "joined", "Norwegian Publication Indicator", "Established", "issn_linking", units="year", missingness="Empty when NPI is absent or unmatched"),
    _entry("country", "journal", "joined", "Norwegian Publication Indicator or other journal metadata", "Country; country_of_publication", "issn_linking", "First available country value", missingness="Empty when no matched source supplies country"),
    _entry("keywords", "article", "direct", "PubMed/MEDLINE", "KeywordList", derivation="Semicolons normalized to commas", missingness="Empty when PubMed supplies no keywords"),
    _entry("apc", "journal", "joined", "Directory of Open Access Journals", "APC", "issn_linking", units="source value", missingness="Empty when DOAJ is absent, unmatched, or blank"),
    _entry("apc_amount", "journal", "joined", "Directory of Open Access Journals", "APC amount", "issn_linking", units="source amount", missingness="Empty when DOAJ is absent, unmatched, or blank"),
    _entry("apc_eur_proxy", "article-journal", "derived", "DOAJ plus ECB/European Commission exchange rates", "APC amount; publication date; FX rate", "issn_linking", "Median of all currency quotes converted at the latest official rate on or before publication date", "EUR", "Empty when no parseable quote or applicable rate exists; contemporary DOAJ snapshot is a proxy"),
    _entry("apc_eur_proxy_min", "article-journal", "derived", "DOAJ plus official exchange rates", "APC amount; FX rate", "issn_linking", "Minimum converted quote", "EUR", "As for apc_eur_proxy"),
    _entry("apc_eur_proxy_max", "article-journal", "derived", "DOAJ plus official exchange rates", "APC amount; FX rate", "issn_linking", "Maximum converted quote", "EUR", "As for apc_eur_proxy"),
    _entry("apc_quote_count", "article-journal", "derived", "Directory of Open Access Journals", "APC amount", "issn_linking", "Count all parseable amount/currency quotes", "count", "Zero when no quote is parseable"),
    _entry("apc_fx_sources", "article-journal", "derived", "ECB; European Commission InforEuro", "FX source", "currency and date", "Pipe-delimited rate sources used", units="source labels", missingness="Empty when conversion was not possible"),
    _entry("apc_fx_rate_start", "article-journal", "derived", "ECB; European Commission InforEuro", "FX date", "currency and date", "Earliest rate date used", "ISO date", "Empty when conversion was not possible"),
    _entry("apc_fx_rate_end", "article-journal", "derived", "ECB; European Commission InforEuro", "FX date", "currency and date", "Latest rate date used", "ISO date", "Empty when conversion was not possible"),
    _entry("apc_conversion_status", "article-journal", "derived", "DOAJ plus official exchange rates", "APC amount; FX availability", "currency and date", "Conversion coverage status", units="category", missingness="Always populated"),
    _entry("doi", "article", "direct", "PubMed/MEDLINE", "ELocationID DOI; ArticleIdList DOI", derivation="Lowercase and remove DOI URL/prefix", units="normalized DOI", missingness="Empty when PubMed supplies no DOI"),
    _entry("retraction_nature", "article", "joined", "Retraction Watch", "RetractionNature", "doi", missingness="Empty when DOI is missing/unmatched or field is blank"),
    _entry("reason", "article", "joined", "Retraction Watch", "Reason", "doi", missingness="Empty when DOI is missing/unmatched or field is blank"),
    _entry("retraction_date", "article", "joined", "Retraction Watch", "RetractionDate", "doi", units="ISO date", missingness="Empty when DOI is missing/unmatched or date is unavailable"),
    _entry("retraction_original_date", "article", "joined", "Retraction Watch", "OriginalPaperDate", "doi", units="ISO date", missingness="Empty when DOI is missing/unmatched or date is unavailable; never replaces PubMed date"),
    _entry("is_retracted", "article", "derived", "Retraction Watch", "RetractionNature; Reason", "doi", "True when either joined field is non-empty", "True/False", "Always populated; lack of matched evidence is False"),
    _entry("n_review_round", "article", "joined", "Optional proprietary peer-review metadata", "review_round", "doi, then pmid, then title", "Count distinct review rounds", "count", "Empty when source is not supplied or article is unmatched"),
    _entry("n_reviews", "article", "joined", "Optional proprietary peer-review metadata", "review events", "doi, then pmid, then title", "Count review events", "count", "Empty when source is not supplied or article is unmatched"),
    _entry("first_review_date", "article", "joined", "Optional proprietary peer-review metadata", "date_reviewed", "doi, then pmid, then title", "Minimum review date", "ISO date", "Empty when source is not supplied, unmatched, or missing"),
    _entry("last_review_date", "article", "joined", "Optional proprietary peer-review metadata", "date_reviewed", "doi, then pmid, then title", "Maximum review date", "ISO date", "Empty when source is not supplied, unmatched, or missing"),
    _entry("n_reviewers", "article", "reserved", "Optional proprietary peer-review metadata", "No reviewer identifier in current export", "doi, then pmid, then title", "Not derived from review-event count", "count", "Always empty for the current source; excluded from models and interpretation"),
    _entry("date_first_accepted", "article", "joined", "Optional proprietary peer-review metadata", "date_accepted", "doi, then pmid, then title", "Minimum accepted event date", "ISO date", "Empty when source is not supplied, unmatched, or missing"),
    _entry("review_cycle_delay", "article", "derived", "Optional proprietary peer-review metadata", "first and last review events", "doi, then pmid, then title", "Source-derived review-cycle duration", "days", "Empty when required events are unavailable"),
    _entry("review_finding_delay", "article", "derived", "PubMed plus optional peer-review metadata", "received; date_first_accepted", "doi, then pmid, then title", "First accepted event minus receipt; source fallback", "days", "Empty when required dates are unavailable"),
    _entry("first_decision_delay", "article", "derived", "Optional proprietary peer-review metadata", "date_first_accepted; first_review_date", "doi, then pmid, then title", "First review minus first accepted event; source fallback", "days", "Empty when required dates are unavailable"),
    _entry("final_decision_delay", "article", "derived", "PubMed plus optional peer-review metadata", "accepted; last_review_date", "doi, then pmid, then title", "PubMed acceptance minus last review; source fallback", "days", "Empty when required dates are unavailable"),
    _entry("first_review_delay", "article", "derived", "PubMed plus optional peer-review metadata", "received; first_review_date", "doi, then pmid, then title", "First review minus receipt; source fallback", "days", "Empty when required dates are unavailable"),
    _entry("peer_review_delay", "article", "derived", "PubMed plus optional peer-review metadata", "accepted; first_review_date", "doi, then pmid, then title", "PubMed acceptance minus first review; source fallback", "days", "Empty when required dates are unavailable"),
)


def validate_provenance() -> list[str]:
    names = [item.variable for item in VARIABLE_PROVENANCE]
    errors: list[str] = []
    missing = [name for name in CANONICAL_ARTICLE_COLUMNS if name not in names]
    extra = [name for name in names if name not in CANONICAL_ARTICLE_COLUMNS]
    duplicates = sorted({name for name in names if names.count(name) > 1})
    if missing:
        errors.append("missing provenance: " + ", ".join(missing))
    if extra:
        errors.append("unexpected provenance: " + ", ".join(extra))
    if duplicates:
        errors.append("duplicate provenance: " + ", ".join(duplicates))
    if names != list(CANONICAL_ARTICLE_COLUMNS):
        errors.append("provenance order differs from canonical schema")
    return errors


def render_provenance(format: str) -> str:
    rows = [asdict(item) for item in VARIABLE_PROVENANCE]
    if format == "json":
        return json.dumps(rows, indent=2, ensure_ascii=False) + "\n"
    if format == "csv":
        output = io.StringIO()
        writer = csv.DictWriter(output, fieldnames=list(rows[0]))
        writer.writeheader()
        writer.writerows(rows)
        return output.getvalue()
    headers = list(rows[0])
    lines = ["| " + " | ".join(headers) + " |", "| " + " | ".join("---" for _ in headers) + " |"]
    for row in rows:
        lines.append("| " + " | ".join(str(row[key]).replace("|", "\\|").replace("\n", " ") for key in headers) + " |")
    return "\n".join(lines) + "\n"


def write_provenance(path: Path, format: str) -> None:
    path = Path(path)
    path.parent.mkdir(parents=True, exist_ok=True)
    with atomic_output_path(path) as temporary:
        temporary.write_text(render_provenance(format), encoding="utf-8")
