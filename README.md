# Publication Delays

Authors: Dominik Dianovics, Marton A. Varga, Miklos Bognar, Balazs Aczel

This repository contains a reproducible research pipeline for studying publication and editorial delays in PubMed/MEDLINE records. The active implementation is Python-first and Polars-backed for tabular work. It supports local bare-metal runs, Nix, uv, resumable atomic outputs, SQLite manifests, and opt-in SLURM job arrays.

Read the published documentation first: https://martonaronvarga.github.io/pubdelays/

The source for that site lives in `docs/` and is built with Zensical:

```bash
uv sync --group dev
uv run zensical serve
```

## Active layout

```text
src/pubdelays/parser/medline.py      # streaming self-vendored MEDLINE XML parser
src/pubdelays/external/              # Polars preprocessors for Scimago, WoS, DOAJ, NPI, Retraction Watch
src/pubdelays/transform/articles.py  # Polars article filtering and enrichment
src/pubdelays/aggregate.py           # Polars aggregation into processed outputs
src/pubdelays/manifest.py            # SQLite manifest, WAL mode, process-safe append writes
src/pubdelays/cli.py                 # pubdelays CLI
config/default.toml                  # canonical paths and defaults
docs/reference/file-layout.md        # exact raw/generated data placement
docs/internals/stage-contracts.md    # stage inputs, outputs, manifests, and failure behavior
docs/reference/schemas.md            # final analysis schema and data dictionary
```

## Install

Python 3.12 is the supported runtime. Nix is the reference environment:

```bash
nix develop
pubdelays --help
pytest -q
```

Fallback for collaborators without Nix:

```bash
scripts/bootstrap_uv.sh
uv run pubdelays --help
uv run pytest -q
```

## Place data

Create directories:

```bash
pubdelays init-dirs
```

Place existing raw files as documented in `docs/reference/file-layout.md`:

```text
data/raw_data/pubmed/xmls/*.xml.gz
data/raw_data/scimago/scimagojr 2015.csv ... scimagojr 2024.csv
data/raw_data/web_of_science/wos.csv
data/raw_data/directory_of_open_access_journals/doaj_2025_05_15.csv
data/raw_data/norwegian_publication_indicator/norwegian_list_2025_05_14.csv
data/raw_data/retraction_watch/retraction_watch.csv
```

Check readiness:

```bash
pubdelays preflight
```

## Local full pipeline

```bash
JOBS=16 SHARDS=64 scripts/pipeline.sh
```

The local transform stage uses shard-level parallelism, not one job per JSON file. Each transform worker loads the external metadata once and processes many JSONL files.

Primary outputs:

```text
data/temp_data/pubmed/jsonl/         # parsed PubMed shards
data/temp_data/article_parquet/      # transformed article shards
data/processed_data/processed.parquet# preferred analysis dataset
data/processed_data/processed.csv    # export/collaboration dataset
data/manifests/pipeline.sqlite       # audit manifest
```

## Download PubMed data

Full baseline:

```bash
pubdelays download --source baseline --jobs 4 --resume
```

Smoke test:

```bash
pubdelays download --source baseline --limit 2 --jobs 2 --resume
```

Update files:

```bash
pubdelays download --source updatefiles --jobs 4 --resume
```

Downloads keep `.md5` sidecars and verify them after transfer. Baseline and updatefiles use the same NCBI layout: each `.xml.gz` file is paired with a same-directory `.xml.gz.md5` sidecar. With `--resume`, existing data files are skipped only when the data file is non-empty and its sidecar verifies; existing sidecars are reused as complete metadata files.

## Individual stages

All commands use `config/default.toml` by default and validate required sections, path keys, dates, and supported shard formats before running a stage. Stage inputs, outputs, manifest rows, resume behavior, and failure behavior are documented in `docs/internals/stage-contracts.md`. The top-level `--help` output lists the canonical workflow order; expensive workflow commands such as `download`, `parse`, `external-all`, `transform-shards`, and `aggregate-all` support `--dry-run` for planning without writing outputs or manifest rows. Override config paths with:

```bash
pubdelays --config path/to/config.toml <command>
```

External metadata:

```bash
pubdelays download-external --source all --resume
pubdelays external-all --resume
```

`download-external --source all --dry-run` lists configured public metadata downloads. DOAJ uses the public journal CSV at `https://doaj.org/csv`; Retraction Watch uses Crossref's public GitLab mirror. SCImago and publisher metadata are included automatically when `external.download.scimago_url_template` and `external.download.publisher_url` are set in the config. SCImago's interactive export URL can return HTTP 403 to scripted clients; use yearly local files or an internal `{year}` mirror instead. Web of Science and Norwegian Publication Indicator snapshots still require licensed/manual source selection, so their raw paths remain documented in `docs/reference/file-layout.md`.

Parse XML:

```bash
pubdelays parse --jobs 16 --format jsonl --parse-mesh-subterms --resume
```

Parsing fails on malformed XML by default. Use `--recover-malformed-xml` only for explicit best-effort salvage runs. `jsonl` is the preferred full-scale output because it streams records; `json` writes one array and accumulates records in memory.

Validate parsed JSONL:

```bash
pubdelays validate
```

Sharded transform:

```bash
pubdelays transform-shards --shards 64 --jobs 16 --format parquet --resume
```

Aggregate:

```bash
pubdelays aggregate-all --resume
```

For a single custom output format, use `aggregate --output path/to/output.parquet`.

Derive basic summaries, run the configured study-specific analysis process, write validation tables, and aggregate filter counts:

```bash
pubdelays summaries --resume
pubdelays run-analysis --resume
pubdelays validate-analysis --resume
pubdelays filter-counts --resume
```

The core CLI only orchestrates analysis as a subprocess. Study-specific tables, figures, and models live under `pubdelays_analysis/`. Optional private peer-review metadata defaults to `data/processed_data/peer_review.csv` or can be supplied with `--peer-review` during transform.

Inspect manifest:

```bash
pubdelays manifest --limit 20
```

## SLURM job arrays

SLURM is opt-in and submitted by the CLI, not by maintained wrapper scripts. Defaults live in `[slurm]` in `config/default.toml`: runner command, log directory, optional account/partition/qos, and per-stage CPU, memory, and walltime.

Inspect an `sbatch` script without writing input lists or submitting:

```bash
pubdelays slurm submit parse --dry-run
pubdelays slurm submit transform-shards --dry-run --shards 64
```

Submit individual stages:

```bash
pubdelays slurm submit download --source baseline
pubdelays slurm submit download-external --external-source all
pubdelays slurm submit external-all
pubdelays slurm submit parse
pubdelays slurm submit transform-shards --shards 64 --dependency afterok:<prepare-job-id>
pubdelays slurm submit aggregate-all --dependency afterok:<transform-job-id>
```

`parse` writes `data/manifests/parse_inputs.txt` and submits one XML file per array task. `transform-shards` writes or consumes `data/manifests/transform_inputs.txt` and submits one modulo shard per array task, so each worker loads external metadata once for many JSONL files.

For the common parse-to-aggregate chain, let the CLI submit dependent jobs:

```bash
pubdelays slurm workflow --shards 64
```

The workflow submits parse, transform-input preparation, transform, and aggregate jobs with `afterok` dependencies. Submitted jobs print SLURM job IDs; `sbatch` failures return nonzero with captured scheduler output instead of silently continuing. Inspect scheduler state with `pubdelays slurm status <job-id>`. Logs go to `logs/slurm/` by default.

## Manifest and resumability

Every mutating local stage writes a row to `data/manifests/pipeline.sqlite` with stage name, status, input/output paths, byte sizes, checksums when enabled, row counts, worker identity, timestamps, and error text.

SLURM parse and transform array tasks write per-task manifests under `data/manifests/slurm/` to avoid concurrent SQLite writes on shared filesystems. Use `pubdelays manifest collect --input-dir data/manifests/slurm` once after a run to build a consolidated audit manifest.

Outputs are written through same-directory temporary files and atomically renamed on success. A failed task leaves the previous completed output intact.

## Performance model

The main throughput path is:

```text
PubMed .xml.gz -> streaming parser -> JSONL shards -> Polars transform shards -> Parquet -> aggregate
```

Performance choices:

- no decompression of PubMed XML to disk;
- JSONL for parse shards, because it is append-free, line-oriented, resumable, and easy to validate;
- Parquet for transformed article shards and final analysis input;
- Polars for all tabular preprocessing, joins, and aggregation;
- SLURM modulo sharding for transform tasks, so each worker amortizes external metadata loading over many PubMed files;
- SQLite WAL manifest instead of shared progress text files.

## Correctness notes

Two semantic safeguards are covered by tests:

1. Missing `article_date` falls back to `pubdate` for `publication_delay`.
2. Ceased journals are filtered against the article publication year.
