---
title: Configuration reference
description: Default config keys and command consumers.
icon: octicons/gear-16
---

# Configuration reference

`config/default.toml` is the canonical default. `src/pubdelays/config.py` validates required sections, required path keys, supported article shard formats, and date-shaped settings before stages run.

## Default keys

| Key | Default | Used by |
| --- | --- | --- |
| `pipeline.manifest` | `data/manifests/pipeline.sqlite` | Manifested stages and `manifest` subcommands. |
| `pipeline.parse_inputs` | `data/manifests/parse_inputs.txt` | SLURM parse input lists. |
| `pipeline.transform_inputs` | `data/manifests/transform_inputs.txt` | Transform sharding and SLURM transform arrays. |
| `pubmed.xml_dir` | `data/raw_data/pubmed/xmls` | Download, parse, preflight. |
| `pubmed.jsonl_dir` | `data/temp_data/pubmed/jsonl` | Parse outputs, validate, transform inputs. |
| `transform.article_shard_dir` | `data/temp_data/article_parquet` | Transform outputs and shard validation. |
| `transform.article_shard_format` | `parquet` | Transform and aggregation default format. |
| `transform.min_received` | `2013-01-01` | Journal metadata eligibility filter. |
| `transform.default_shards` | `64` | Default expected shard count. |
| `aggregate.processed_parquet` | `data/processed_data/processed.parquet` | `aggregate-all`, `schema`, downstream analysis. |
| `aggregate.processed_csv` | `data/processed_data/processed.csv` | `aggregate-all` CSV export. |
| `aggregate.summary_dir` | `data/processed_data/summaries` | `summaries`. |
| `aggregate.filter_counts` | `data/processed_data/filter_counts.csv` | `filter-counts`. |
| `quality.report_dir` | `data/processed_data/quality` | Long-form Parquet quality tables and compact CSV debriefs. |
| `analysis.cwd` | `.` | Working directory for `run-analysis`. |
| `analysis.input` | `data/processed_data/processed.parquet` | Default input exposed to analysis subprocesses. |
| `analysis.output_dir` | `data/processed_data/analysis` | Default output directory for `run-analysis`. |
| `analysis.command` | `python pubdelays_analysis/outputs.py ...` | Study-specific analysis command. |
| `validation.report_dir` | `data/processed_data/validation_tables` | `validate-analysis`. |
| `validation.filtered_output` | `data/processed_data/processed_validated.parquet` | Rows kept by `validate-analysis`. |
| `validation.excluded_output` | `data/processed_data/processed_validation_excluded.parquet` | Rows excluded by final validation bounds. |
| `validation.min_article_date` / `validation.max_article_date` | `2016-01-01` / `2025-06-01` | Final-output validation window. |
| `validation.min_delay_days` / `validation.max_delay_days` | `1` / `1095` | Final-output delay validation bounds. |

## External raw and processed paths

External raw paths live under `[external.raw]`; normalized outputs live under `[external.processed]`. Transform commands read processed outputs unless a CLI override such as `--scimago` or `--doaj` is supplied. Peer-review metadata is optional and private; when present, put the raw table at `external.raw.peer_review_csv` or a cleaned table at `external.processed.peer_review`, or pass `--peer-review` directly.

!!! warning "Licensed/manual sources"
    Default public URLs exist for DOAJ and Retraction Watch. Web of Science and Norwegian Publication Indicator paths are declared but the source snapshots are not downloaded by the repository.

## SLURM resources

`[slurm]` sets the runner, log directory, and optional scheduler account fields. `[slurm.resources.<stage>]` sets `cpus_per_task`, `mem`, and `time` for stage-specific job scripts.
