---
title: Stage contracts
description: Command-level inputs, outputs, manifest rows, resume behavior, and failure boundaries.
icon: octicons/tasklist-16
---

# Stage contracts

This document is the source-of-truth contract for the active Python pipeline stages. Paths are repository-relative defaults from `config/default.toml`; CLI flags may override them without changing the stage semantics.

## Shared Rules

- Mutating data stages write through same-directory temporary files and atomically rename on success.
- `--resume` skips only when the expected output exists and is non-empty.
- Local manifested stages append to `data/manifests/pipeline.sqlite` unless `--manifest` overrides it.
- SLURM parse and transform array tasks use per-task manifests under `data/manifests/slurm/`; collect them after a run with `manifest collect` when a consolidated audit table is needed.
- Manifest rows use `status` values `success`, `skipped`, or `failed` where the command currently records failures.
- `records` means rows or records emitted by that stage; `deleted` is stage-specific and documented below.
- `init-dirs`, `preflight`, `list-inputs`, and `manifest` are inspection or helper commands and do not append manifest rows.

## Stage Table

| Command | Manifest stage | Inputs | Outputs | Row counts and metadata | Resume behavior |
| --- | --- | --- | --- | --- | --- |
| `init-dirs` | none | `config/default.toml` | Canonical raw, temp, processed, manifest, and external directories | No data rows; creates directories only | Idempotent directory creation |
| `preflight` | none | Configured raw paths and generated path defaults | Console readiness report | Reports XML file count and missing required inputs | No output to resume |
| `download` | `download` | NCBI PubMed `baseline` or `updatefiles` index | `.xml.gz` and `.md5` files under `data/raw_data/pubmed/xmls/` | `records` is downloaded file count; `deleted` is MD5 failure count; metadata records source, link count, skipped count, and failed sidecars | Skips existing non-empty files before download; still verifies present `.md5` sidecars |
| `external-scimago` | `external-scimago` | `data/raw_data/scimago/scimagojr <year>.csv` | `data/processed_data/scimago.csv` | `records` is normalized ISSN rows in the output | Skips complete output |
| `external-wos` | `external-wos` | `data/raw_data/web_of_science/wos.csv` | `data/processed_data/web_of_science.csv` | `records` is normalized journal ISSN rows in the output | Skips complete output |
| `external-doaj` | `external-doaj` | `data/raw_data/directory_of_open_access_journals/doaj_2025_05_15.csv` | `data/processed_data/doaj.csv` | `records` is normalized journal ISSN rows in the output | Skips complete output |
| `external-npi` | `external-npi` | `data/raw_data/norwegian_publication_indicator/norwegian_list_2025_05_14.csv` | `data/processed_data/norwegian_list.csv` | `records` is normalized journal ISSN rows in the output | Skips complete output |
| `external-retraction-watch` | `external-retraction-watch` | `data/raw_data/retraction_watch/retraction_watch.csv` | `data/processed_data/retraction_watch.csv` | `records` is DOI-addressable retraction metadata; missing dates are retained for downstream quality reporting | Skips complete output |
| `external-publisher` | `external-publisher` | `data/raw_data/publisher_metadata/publishers.csv` | `data/processed_data/publisher_metadata.csv` | `records` is normalized publisher ISSN rows in the output; conflict columns flag duplicate-key disagreements | Skips complete output |
| `external-all` | individual `external-*` rows | All configured external raw inputs | All configured external processed outputs | One manifest row per external preprocessor; missing optional publisher input is skipped without an output | Each preprocessor applies its own resume check |
| `journals` | `journals` | NLM `J_Medline.txt` URL | `data/external/pubmed-journals.csv` | `records` is parsed journal records; metadata records URL | No current resume shortcut |
| `parse-one` | `parse` | One `.xml` or `.xml.gz` MEDLINE file | One `.jsonl` or `.json` parsed shard | `records` is parsed citations, including deletion records; `deleted` is `DeleteCitation` record count; metadata records format, `min_pub_year`, and `recover_malformed_xml` | Skips complete output |
| `parse` | `parse` | All `.xml` and `.xml.gz` files under `data/raw_data/pubmed/xmls/` | One parsed shard per input under `data/temp_data/pubmed/jsonl/` | Same as `parse-one`, with one manifest row per input file | Per-file complete-output skip |
| `validate` | `validate-json` | Parsed `.jsonl` or `.json` file or directory | Console validation report only | `records` is valid records read before errors; `deleted` is failure count; metadata records file count and failures | No output to resume |
| `validate-shards` | none | Article shard directory, expected shard count, and expected format | Console validation report only | Validates expected shard IDs, duplicate outputs, encoded total count, format, readability, and canonical schema | No output to resume |
| `list-inputs` | none | Directory plus kind `xml`, `json`, or `glob` | Atomic path list, usually `data/manifests/parse_inputs.txt` or `data/manifests/transform_inputs.txt` | No manifest row; output line count is the listed input count | Rewrites the list atomically |
| `transform-one` | `transform` | One parsed `.jsonl` or `.json` input plus processed external metadata | One article shard and optional filter-count CSV | `records` is `final_rows`; metadata contains filter counts and filter-count path | Skips complete output |
| `transform` | `transform` | Parsed input file or all parsed files in `data/temp_data/pubmed/jsonl/` | One article shard per input in `data/temp_data/article_parquet/` | Same as `transform-one`; one-output-per-input mode | Per-output complete-file skip |
| `transform-shard` | `transform-shard` | `transform_inputs.txt`, shard index, shard count, processed external metadata, and optional peer-review metadata | `articles-shard-<index>-of-<shards>.<format>` plus `.filters.csv` and `.quality.parquet` | `records` is `final_rows`; metadata contains filter counts, quality path, `shard_index`, `shards`, and selected input count | Skips schema-current shard output |
| `transform-shards` | `transform-shard` | All parsed JSON/JSONL files in `data/temp_data/pubmed/jsonl/` | Atomic `transform_inputs.txt` and one output per expected shard, including empty canonical shards | One manifest row per shard | Rewrites input list; each shard applies its own resume check |
| `aggregate` | `aggregate` | Existing canonical article shards under `data/temp_data/article_parquet/` or one explicit shard | One processed dataset, default `data/processed_data/processed.parquet` | `records` is final deduplicated row count | Skips complete output |
| `aggregate-all` | `aggregate-all` | Complete canonical article shard set under `data/temp_data/article_parquet/` | `data/processed_data/processed.parquet` and `data/processed_data/processed.csv` | `records` is final deduplicated row count; metadata records CSV path and shard validation details | Skips only when both final outputs are complete after shard validation |
| `summaries` | `summaries` | `data/processed_data/processed.parquet` | CSV tables under `data/processed_data/summaries/` | `records` is derived table count; metadata records table paths | Rewrites summary outputs atomically |
| `run-analysis` | `run-analysis` | Configured analysis command plus `analysis.input` | Configured `analysis.output_dir` or whatever the subprocess writes | Manifest metadata records command, cwd, return code, and stdout/stderr tails | Delegates resume behavior to the subprocess |
| `validate-analysis` | `validate-analysis` | `data/processed_data/processed.parquet` | CSV tables under `data/processed_data/validation_tables/`, optional kept-row `processed_validated.parquet`, and excluded-row output | `records` is validation-kept rows; metadata records table paths, failed checks, filtered output, and excluded output | Rewrites validation outputs atomically |
| `filter-counts` | `filter-counts` | Transform `.filters.csv` sidecars under `data/temp_data/article_parquet/` | `data/processed_data/filter_counts.csv` | `records` is aggregate filter-count rows | Rewrites aggregate filter-count output atomically |
| `quality-report` | `quality-report` | Transform `.quality.parquet` sidecars and final processed dataset | `data/processed_data/quality/` | `records` is final dataset rows | Writes deterministic Parquet audit tables and CSV debriefs |
| `manifest` | none | `data/manifests/pipeline.sqlite` | Console report of recent rows | Displays recent manifest rows | No output to resume |

## Empty Inputs

- `parse` fails with exit code 1 when no XML files are found.
- `validate` over an empty directory records `validate-json` success with zero files and zero records.
- `transform_files` writes an empty canonical article shard when selected parsed records are empty or all filtered out.
- `transform-shard` writes an empty canonical article shard plus filter counts when its modulo selection is empty, and records a `transform-shard` success manifest row with `empty_selection=true`.
- `aggregate` writes an empty canonical dataset when the shard directory exists but contains no canonical article shards; a missing explicit input path raises an error.
- `aggregate-all` refuses missing, duplicate, wrong-total, wrong-format, unreadable, or schema-invalid shard sets unless `--allow-incomplete` is explicit.

## Malformed Inputs

- Malformed JSON or JSONL makes `validate` append a `validate-json` row with `status=failed` and return nonzero.
- External preprocessors append `status=failed` with error text when the preprocessor raises.
- `parse-one` and `parse` fail fast on malformed XML by default, append `status=failed` when XML parsing raises, and leave previous complete outputs intact through atomic writes. `--recover-malformed-xml` opts into lxml best-effort recovery.
- Transform and aggregate commands may raise on malformed inputs; not all of those code paths currently append failed manifest rows.

## Parse Output Formats

- `jsonl` is the preferred full-scale parse format because records are written one at a time.
- `json` writes a single JSON array and accumulates parsed records in memory; use it only for small fixtures or interoperability cases.

## Partial Shards And Aggregation

- Local `transform-shards` and SLURM `transform-shard` use the same modulo rule: an input at line index `i` belongs to shard `i % shards`.
- Expected shard output names encode both the shard index and total shard count.
- Directory aggregation only discovers canonical `articles-shard-*.parquet`, `articles-shard-*.tsv`, and `articles-shard-*.csv` data files; sidecars such as `*.filters.csv` are ignored.
- `validate-shards` is the shared local/SLURM completeness validator. It checks expected shard count and IDs, duplicate outputs, wrong `of-N` totals, wrong format, unreadable files, and canonical schema conformance.
- `aggregate-all` runs the same validator by default. Use `--allow-incomplete` only for deliberate partial debugging outputs.

## Manifest Expectations

Every mutating manifested stage should record:

```text
stage, status, input_path, output_path, input/output bytes, input/output SHA-256 when enabled, records, deleted when stage-specific, timestamps, worker, metadata, error
```

Skipped rows include metadata explaining the skip reason. Failure rows include `error` text where the command catches the exception.

`pubdelays manifest` prints recent rows. `manifest summary`, `manifest failed`, `manifest show --json`, and `manifest retry-script` provide lightweight audit and retry inspection without opening SQLite directly.

`manifest check` runs SQLite integrity checks through Python, which is useful on HPC systems without the `sqlite3` command. `manifest collect` appends rows from per-task manifests into the target manifest; it is intended as a one-time post-run audit collection, not a repeatedly idempotent synchronization step.
