# Data acquisition and snapshotting

The pipeline distinguishes automatically downloadable public inputs from manually
selected snapshots. Record the retrieval date, upstream URL or product release,
license, file checksum, and local filename for every supplied snapshot. Never use a
generated processed table as the source of truth.

## Public inputs

- **PubMed/MEDLINE:** run `pubdelays download --source baseline`. NLM describes the
  annual baseline as a complete snapshot and requires daily update files to be
  applied after the baseline, in numeric order, with revised and deleted citations
  replacing prior records: <https://pubmed.ncbi.nlm.nih.gov/download/>. For a small
  current check, use `--limit 2 --newest`; this selects the newest XML/MD5 pair.
- **DOAJ:** run `pubdelays download-external --source doaj`. The configured endpoint,
  <https://doaj.org/csv>, is DOAJ's public journal CSV and is refreshed within 30
  days: <https://doaj.org/docs/journal-csv>.
- **Retraction Watch:** run `pubdelays download-external --source retraction-watch`.
  Crossref documents the GitLab CSV as the complete dataset, updated each working
  day: <https://www.crossref.org/documentation/retrieve-metadata/retraction-watch/>.

Downloads are written under `data/raw_data/`; PubMed files are accepted only with a
matching MD5 sidecar. The manifest records stage inputs, outputs, counts, status,
checksums, and configuration.

## User-supplied journal snapshots

- **SCImago:** download the required annual journal-rank exports through SCImago's
  interactive export and place `scimagojr YYYY.csv` files in the configured
  `external.raw.scimago_dir`. The analysis expects 2015–2025 snapshots. Automated
  scripted requests may be rejected, so the repository does not pretend this is a
  stable unattended endpoint.
- **Scopus Source List:** obtain the source-list export appropriate to the study and
  place it at `external.raw.web_of_science_csv`. The config key is retained for
  backward compatibility. Preserve the workbook and the CSV exported from its
  `Scopus Sources` sheet so that the conversion remains auditable.
- **Norwegian Publication Indicator:** export the selected channel snapshot from the
  Norwegian Register and place it at `external.raw.norwegian_list_csv`. Preserve the
  export date and source metadata with the file.
- **Publisher metadata:** place the study-curated, versioned mapping at
  `external.raw.publisher_csv`. There is no canonical automatic provider configured;
  document its provenance and license explicitly.

The optional private peer-review input is deliberately omitted from this acquisition
guide. It is not a public source and must be governed under its own data-use terms.

Run `pubdelays preflight` before a full transform. Missing optional/manual inputs are
reported rather than silently fabricated. `pubdelays smoke-live` uses current PubMed,
DOAJ, and Retraction Watch downloads in `data/temp_data/smoke_live`, uses a manual
source only when the configured real file exists, and labels every unavailable source
`source_not_supplied` in `smoke_debrief.json`.
