---
title: Quickstart
description: Default local workflow from directories to processed outputs.
icon: octicons/play-16
---

# Quickstart

This page follows the default local workflow printed by `pubdelays --help`:

```text
init-dirs -> preflight -> download -> external-all -> parse -> validate -> transform-shards -> validate-shards -> aggregate-all -> manifest summary
```

## Prepare paths

```bash
pubdelays init-dirs
pubdelays preflight
```

`init-dirs` creates configured raw, temporary, processed, manifest, and external directories. `preflight` reports missing required raw inputs and counts PubMed XML/XML.GZ files.

## Place or download inputs

If raw data is already present, skip downloading and move to preprocessing. Otherwise:

```bash
pubdelays download --source baseline --jobs 4 --resume
pubdelays download-external --source all --resume
```

!!! note "External source limits"
    DOAJ and Retraction Watch have configured public URLs in `config/default.toml`. SCImago and publisher downloads run only when URL settings are provided. Scopus Source List and Norwegian Publication Indicator inputs require manual source selection.

## Run stages locally

```bash
pubdelays external-all --resume
pubdelays parse --jobs 16 --format jsonl --parse-mesh-subterms --resume
pubdelays validate
pubdelays transform-shards --shards 64 --jobs 16 --format parquet --resume
pubdelays validate-shards --shards 64 --format parquet
pubdelays aggregate-all --resume
pubdelays summaries --resume
pubdelays manifest summary
```

The repository wrapper runs the same core sequence with configurable parallelism:

```bash
JOBS=16 SHARDS=64 scripts/pipeline.sh
```

## Check outputs

```text
data/temp_data/pubmed/jsonl/          parsed PubMed shards
data/temp_data/article_parquet/       canonical transform shards
data/processed_data/processed.parquet preferred analysis dataset
data/processed_data/processed.csv     CSV export
data/manifests/pipeline.sqlite        audit manifest
```

Validate the final schema:

```bash
pubdelays schema --input data/processed_data/processed.parquet
```

For a 64-shard transform, this should print `64` when the run is complete:

```bash
find data/temp_data/article_parquet -name 'articles-shard-*.parquet' | wc -l
```

??? example "Plan a stage without writing outputs"
    Several expensive commands support `--dry-run`:

    ```bash
    pubdelays parse --dry-run
    pubdelays transform-shards --shards 64 --jobs 16 --dry-run
    pubdelays aggregate-all --dry-run
    ```
