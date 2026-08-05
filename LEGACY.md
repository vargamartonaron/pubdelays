# Legacy migration notes

The historical pipeline mixed shell scripts, patched `pubmed_parser`, Python JSON conversion, and R/dplyr scripts. The active implementation ports the behavior into `src/pubdelays/`; obsolete executable legacy sources and generated legacy outputs were removed after the migration was verified.

## Ported stages

| Historical stage | Active implementation |
| --- | --- |
| XML-to-JSON parser | `src/pubdelays/parser/medline.py` and `pubdelays parse` |
| External journal preprocessors | `src/pubdelays/external/` |
| Article processing | `src/pubdelays/transform/articles.py` |
| Aggregation | `src/pubdelays/aggregate.py` |

## Intended semantic preservation

The active transform preserves the old high-level sequence:

1. unnest MEDLINE/PubMed history dates;
2. require received and accepted dates;
3. keep publication type `Journal Article`;
4. normalize linking ISSN by removing punctuation and uppercasing `X`;
5. require coherent dates: `received < accepted < publication_date`;
6. compute acceptance and publication delays in days;
7. join Scimago, Web of Science, DOAJ, and NPI by `issn_linking`;
8. choose year-specific Scimago/NPI metadata using article publication year, with 2025 falling back to 2024 Scimago/NPI metadata;
9. mark psychology, megajournal, open-access, COVID, and retraction flags;
10. keep the first article per title.

## Intentional corrections

Three legacy defects are deliberately corrected:

1. If `article_date` is missing, `pubdate` is now allowed to supply the publication date for `publication_delay`. Legacy R filtered on `article_date` before its own `pubdate` fallback could contribute.
2. Ceased journals are filtered against the article publication year. Legacy `ceased = is.numeric(ceased)` destroyed the ceased-year information before filtering.
3. Retraction Watch's original-paper date is retained as enrichment metadata and no longer overwrites the PubMed publication date after delay calculation.
