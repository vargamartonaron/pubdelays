# Analysis workspace

This tree contains study-specific analysis code that consumes the processed PubMed publication-delay dataset.

The core pipeline can run study analysis scripts generically with `pubdelays run-analysis`, which records subprocess status in the manifest but does not encode analysis semantics.

## Analysis outputs

```bash
PYTHONPATH=src python pubdelays_analysis/outputs.py \
  --input data/processed_data/processed.parquet \
  --table-dir data/processed_data/analysis_tables \
  --figure-data-dir data/processed_data/analysis_figures
```
