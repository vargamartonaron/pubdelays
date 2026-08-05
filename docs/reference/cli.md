---
title: CLI reference
description: pubdelays command families, options, defaults, and examples.
icon: octicons/terminal-16
---

# CLI reference

The command is `pubdelays`. Put global options before the subcommand:

```bash
pubdelays --config config/default.toml <command>
```

`uv run python -m pubdelays.cli --help` reports this main workflow:

```text
init-dirs -> preflight -> download -> external-all -> parse -> validate -> transform-shards -> validate-shards -> aggregate-all -> manifest summary
```

## Command families

| Family | Commands | Main purpose |
| --- | --- | --- |
| Setup and inspection | `init-dirs`, `preflight`, `schema`, `provenance`, `smoke-live`, `list-inputs` | Prepare directories, check raw inputs, inspect schema/provenance, or run an isolated live check. |
| PubMed input | `download`, `parse-one`, `parse`, `validate`, `journals` | Fetch/parse PubMed XML and validate parser outputs. |
| External metadata | `download-external`, `external-all`, `external-scimago`, `external-wos`, `external-doaj`, `external-publisher`, `external-npi`, `external-retraction-watch` | Fetch public/configured metadata and normalize raw CSVs. |
| Transform and aggregate | `transform-one`, `transform`, `transform-shard`, `transform-shards`, `validate-shards`, `aggregate`, `aggregate-all`, `summaries` | Create article shards and final outputs. |
| Audit and comparison | `manifest`, `compare-outputs` | Inspect stage records or compare processed outputs. |
| Scheduler | `slurm submit`, `slurm workflow`, `slurm status`, `slurm cleanup` | Generate and submit SLURM jobs. |

## Common options

| Option | Commands | Meaning |
| --- | --- | --- |
| `--config` | global | TOML config path. |
| `--manifest` | mutating manifested stages | SQLite manifest path; defaults to `pipeline.manifest`. |
| `--no-checksum` | mutating manifested stages | Skip SHA-256 manifest checksums. |
| `--resume` | output-producing stages | Skip complete non-empty outputs. |
| `--dry-run` | expensive/planning stages | Show planned work without writing outputs or manifest rows. |

`--dry-run` is available on commands such as `download`, `download-external`, `external-all`, `parse`, `transform-shards`, and `aggregate-all`.

## Setup

```bash
pubdelays init-dirs
pubdelays preflight
pubdelays schema
pubdelays schema --input data/processed_data/processed.parquet
pubdelays provenance --format csv --output data/processed_data/variable_provenance.csv
pubdelays smoke-live --pubmed-files 1 --jobs 1 --shards 2
```

`provenance` validates exact variable coverage and emits source fields, join keys,
derivations, units, and missingness meanings. `smoke-live` runs current public
downloads and the feasible end-to-end pipeline in an isolated workspace; absent
manual sources are reported and never fabricated.

## Download and parse

```bash
pubdelays download --source baseline --jobs 4 --resume
pubdelays download --source updatefiles --jobs 4 --resume
pubdelays download --source baseline --limit 2 --newest --jobs 1
pubdelays download --source updatefiles --jobs 4 --resume
pubdelays parse --source baseline --jobs 16 --format jsonl --parse-mesh-subterms --resume
pubdelays parse --source updatefiles --jobs 16 --format jsonl --parse-mesh-subterms --resume
pubdelays resolve-state --resume
pubdelays validate data/temp_data/pubmed/resolved_jsonl
```

Parsing supports `--format jsonl` and `--format json`. JSONL is the full-scale default; JSON materializes one array and is for small fixtures or interoperability.

## External metadata

```bash
pubdelays download-external --source all --resume
pubdelays external-all --resume
pubdelays external-scimago --resume
pubdelays external-wos --resume
pubdelays external-doaj --resume
pubdelays external-npi --resume
pubdelays external-retraction-watch --resume
pubdelays external-publisher --resume
```

`download-external --source all` includes configured public sources. Scopus Source List and Norwegian Publication Indicator snapshots remain manual inputs in the default docs.

## Transform and aggregate

```bash
pubdelays transform-shards \
  --shards 64 \
  --jobs 16 \
  --format parquet \
  --resume
pubdelays validate-shards --shards 64 --format parquet
pubdelays aggregate-all --resume
pubdelays summaries --resume
pubdelays run-analysis --resume
pubdelays validate-analysis --resume
pubdelays filter-counts --resume
pubdelays quality-report
```

`run-analysis` runs the configured study-specific command from `[analysis]`, captures stdout/stderr, and records the subprocess status in the manifest. The core CLI does not encode analysis semantics such as GAMs or plot definitions.

`validate-analysis` writes final-output validation tables for delay outliers, missingness/range checks, missingness mechanisms, journal counts, article counts by time, COVID counts, and Scopus/NPI discipline agreement. Quality-check failures are reported but do not make the command fail unless `--strict-checks` is passed. It can also write kept rows to `processed_validated.parquet` and excluded rows to `processed_validation_excluded.parquet` instead of overwriting `processed.csv` in place.

`filter-counts` aggregates transform `.filters.csv` sidecars into one row-count/drop-count audit table.

`quality-report` aggregates `.quality.parquet` shard sidecars and describes the
final dataset. It writes source/year join coverage, pre/post-filter missingness,
all-variable completeness and distributions, and pairwise marginal missingness.

`transform` and `transform-shards` accept `--limit N` for small debug runs. `transform-shards` also accepts external path overrides and optional peer-review metadata:

```bash
pubdelays transform-shards \
  --peer-review path/to/private-peer-review.csv \
  --shards 64 \
  --resume
```

Run `external-peer-review` to clean the raw private event table from `external.raw.peer_review_csv` into `data/processed_data/peer_review.csv`. The transform then joins that processed peer-review table by `doi`, `pmid`, or `title` when those keys are available and derives the process-data-local delay columns from article and review dates. No private peer-review data is bundled with the repository.

## Manifest subcommands

```bash
pubdelays manifest --limit 20
pubdelays manifest summary
pubdelays manifest failed --limit 50
pubdelays manifest show --json
pubdelays manifest retry-script
pubdelays manifest check --manifest data/manifests/pipeline.sqlite
pubdelays manifest collect --manifest data/manifests/pipeline.sqlite --input-dir data/manifests/slurm
```

!!! note "Manifest collection"
    `manifest collect` appends per-task manifest rows into the target manifest. It is intended as a one-time post-run audit collection after SLURM arrays complete.

## SLURM subcommands

```bash
pubdelays slurm submit parse --dry-run
pubdelays slurm submit transform-shards --dry-run --shards 64
pubdelays slurm workflow --shards 64 --max-array-size 100
pubdelays slurm status <job-id>
pubdelays slurm cleanup <job-id>
pubdelays slurm cleanup <job-id> --cancel
```

`slurm cleanup` previews dependency-blocked jobs by default and calls `scancel` only with `--cancel`.
