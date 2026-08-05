# Analysis workspace

This tree contains study-specific analysis code that consumes the processed PubMed publication-delay dataset.

The core pipeline can run study analysis scripts generically with `pubdelays run-analysis`, which records subprocess status in the manifest but does not encode analysis semantics.

## Retrospective boosting models

The CatBoost workflow uses temporal development/validation/test partitions, journal-grouped cross-validation, historical-median baselines, subgroup diagnostics, SHAP summaries, and an exclusion sensitivity for retracted records:

```bash
nix develop .#model --command python -m pubdelays_analysis.boosting \
  --input data/processed_data/processed_validated.parquet \
  --output-dir data/processed_data/boosting \
  --outcome both
```

This is an association/prediction workflow. SHAP values are not causal estimates.

## Analysis outputs

```bash
PYTHONPATH=src python pubdelays_analysis/outputs.py \
  --input data/processed_data/processed.parquet \
  --table-dir data/processed_data/analysis_tables \
  --figure-data-dir data/processed_data/analysis_figures
```
