---
title: Configuration
description: Reader guide to config/default.toml and path override behavior.
icon: octicons/gear-16
---

# Configuration

The default configuration is `config/default.toml`. Pass an alternate file with the global option before the command:

```bash
pubdelays --config config/default.toml preflight
```

Relative paths in the config are repository-relative because `PipelineConfig.path()` resolves them from the config root logic in `src/pubdelays/config.py`.

## Important sections

| Section | Keys documented here | Used by |
| --- | --- | --- |
| `[pipeline]` | `manifest`, `parse_inputs`, `transform_inputs` | Manifest commands, SLURM lists, transform sharding |
| `[pubmed]` | `xml_dir`, `jsonl_dir` | Download, parse, validate |
| `[external.raw]` | SCImago, WoS, DOAJ, NPI, Retraction Watch, publisher, optional peer-review raw paths | `external-*`, `preflight` |
| `[external.processed]` | Normalized lookup outputs, including optional peer-review metadata | Transform joins |
| `[transform]` | `article_shard_dir`, `article_shard_format`, `min_received`, `default_shards` | Transform and shard validation |
| `[aggregate]` | `processed_parquet`, `processed_csv`, `summary_dir`, `filter_counts` | Aggregation, summaries, filter-count aggregation |
| `[analysis]` | `cwd`, `input`, `output_dir`, `command` | Generic `run-analysis` subprocess orchestration |
| `[validation]` | Validation report/output paths and date/delay bounds | `validate-analysis` |
| `[slurm]` and `[slurm.resources.*]` | runner, logs, array cap, stage resources | SLURM script generation |

## Minimal path override pattern

Prefer a copied config for site-specific paths:

```bash
cp config/default.toml config/local.toml
pubdelays --config config/local.toml preflight
```

CLI path flags are stage overrides. For example, `transform-shards` can override the input and output directories without changing the config file:

```bash
pubdelays transform-shards \
  --input-dir data/temp_data/pubmed/jsonl \
  --output-dir data/temp_data/article_parquet \
  --shards 64 \
  --format parquet
```

!!! warning "Keep defaults synchronized"
    When a default path changes, update `config/default.toml`, [file layout](../reference/file-layout.md), [stage contracts](../internals/stage-contracts.md), and smoke-test commands together.
