"""Retrospective CatBoost models for acceptance and publication delays."""

from __future__ import annotations

import argparse
import json
from dataclasses import asdict, dataclass
from pathlib import Path
from typing import TYPE_CHECKING, Any

import polars as pl

if TYPE_CHECKING:
    import pandas as pd


CATEGORICAL_FEATURES: tuple[str, ...] = (
    "is_mega",
    "open_access",
    "is_covid",
    "journal",
    "discipline",
    "apc",
    "country",
    "publication_date_source",
    "quartile_year",
    "asjc",
    "is_series",
    "is_retracted",
)

NUMERIC_FEATURES: tuple[str, ...] = (
    "h_index_year",
    "rank_year",
    "established",
    "n_review_round",
    "n_reviewers",
    "npi_year",
    "time_numeric",
    "weekday",
    "day_of_year",
    "month_of_year",
    "article_year",
)

FEATURES = (*CATEGORICAL_FEATURES, *NUMERIC_FEATURES)
DERIVED_FEATURES = {
    "time_numeric",
    "weekday",
    "day_of_year",
    "month_of_year",
    "article_year",
}


@dataclass(frozen=True)
class BoostingConfig:
    seed: int = 20250805
    learning_rate: float = 0.03
    depth: int = 8
    iterations: int = 5000
    early_stopping_rounds: int = 50
    shap_max_rows: int = 10000


def _read(path: Path) -> pl.DataFrame:
    if path.suffix == ".parquet":
        return pl.read_parquet(path)
    return pl.read_csv(path, infer_schema=False)


def prepare_model_frame(
    path: Path,
    outcome: str,
    *,
    sample_fraction: float = 1.0,
    seed: int = 20250805,
) -> pl.DataFrame:
    """Prepare the declared retrospective feature set and temporal split fields."""
    if outcome not in {"acceptance_delay", "publication_delay"}:
        raise ValueError(f"unsupported outcome: {outcome}")
    if not 0 < sample_fraction <= 1:
        raise ValueError("sample_fraction must be in (0, 1]")
    df = _read(Path(path))
    required = {"title", "article_date", outcome, *(set(FEATURES) - DERIVED_FEATURES)}
    missing = sorted(required - set(df.columns))
    if missing:
        raise ValueError("model input missing columns: " + ", ".join(missing))
    date = pl.col("article_date").str.strptime(pl.Date, "%Y-%m-%d", strict=False)
    frame = (
        df.with_columns(
            date.alias("_date"),
            pl.col(outcome).cast(pl.Float64, strict=False).alias("target"),
        )
        .with_columns(
            pl.col("_date").dt.year().alias("article_year"),
            pl.col("_date").cast(pl.Int32).alias("time_numeric"),
            pl.col("_date").dt.weekday().alias("weekday"),
            pl.col("_date").dt.ordinal_day().alias("day_of_year"),
            pl.col("_date").dt.month().alias("month_of_year"),
        )
        .filter(
            pl.col("_date").is_between(pl.date(2016, 1, 1), pl.date(2025, 12, 31))
            & pl.col("target").is_between(1, 1095)
        )
        .with_columns(
            *[
                pl.col(column)
                .cast(pl.Utf8, strict=False)
                .replace("", "__MISSING__")
                .fill_null("__MISSING__")
                .alias(column)
                for column in CATEGORICAL_FEATURES
            ],
            *[
                pl.col(column).cast(pl.Float64, strict=False).alias(column)
                for column in NUMERIC_FEATURES
            ],
        )
        .select("title", "article_date", "target", *FEATURES)
        .sort("article_date", "title")
    )
    if sample_fraction < 1 and frame.height:
        frame = frame.sample(fraction=sample_fraction, shuffle=True, seed=seed).sort(
            "article_date", "title"
        )
    return frame


def temporal_partitions(frame: pl.DataFrame) -> dict[str, pl.DataFrame]:
    return {
        "train": frame.filter(pl.col("article_year") <= 2023),
        "validation": frame.filter(pl.col("article_year") == 2024),
        "test": frame.filter(pl.col("article_year") == 2025),
    }


def _metrics(actual: Any, predicted: Any) -> dict[str, float]:
    import numpy as np

    actual = np.asarray(actual, dtype=float)
    predicted = np.asarray(predicted, dtype=float)
    residual = actual - predicted
    denominator = np.sum((actual - np.mean(actual)) ** 2)
    return {
        "mae": float(np.mean(np.abs(residual))),
        "median_absolute_error": float(np.median(np.abs(residual))),
        "rmse": float(np.sqrt(np.mean(residual**2))),
        "r2": float(1 - np.sum(residual**2) / denominator) if denominator else float("nan"),
    }


def _xy(frame: pl.DataFrame) -> tuple[Any, Any]:
    import pandas as pd

    pandas = pd.DataFrame(frame.select(*FEATURES).to_dict(as_series=False))
    return pandas, frame["target"].to_numpy()


def _params(config: BoostingConfig, iterations: int | None = None) -> dict[str, Any]:
    return {
        "loss_function": "RMSE",
        "eval_metric": "RMSE",
        "learning_rate": config.learning_rate,
        "depth": config.depth,
        "iterations": iterations or config.iterations,
        "random_seed": config.seed,
        "allow_writing_files": False,
        "verbose": False,
    }


def _plot_outputs(output_dir: Path, predictions: pl.DataFrame, importance: pl.DataFrame) -> None:
    import matplotlib.pyplot as plt

    actual = predictions["actual"].to_numpy()
    predicted = predictions["predicted"].to_numpy()
    figure, axis = plt.subplots(figsize=(6, 6))
    axis.hexbin(actual, predicted, gridsize=70, bins="log", mincnt=1)
    bounds = [0, max(float(max(actual, default=1)), float(max(predicted, default=1)))]
    axis.plot(bounds, bounds, "r--")
    axis.set(xlabel="Actual delay (days)", ylabel="Predicted delay (days)")
    figure.tight_layout()
    figure.savefig(output_dir / "actual_vs_predicted.svg")
    plt.close(figure)

    figure, axis = plt.subplots(figsize=(7, 5))
    axis.hexbin(actual, actual - predicted, gridsize=70, bins="log", mincnt=1)
    axis.axhline(0, color="red", linestyle="--")
    axis.set(xlabel="Actual delay (days)", ylabel="Residual (days)")
    figure.tight_layout()
    figure.savefig(output_dir / "residuals.svg")
    plt.close(figure)

    top = importance.sort("importance").tail(25)
    figure, axis = plt.subplots(figsize=(8, 6))
    axis.barh(top["feature"].to_list(), top["importance"].to_list())
    axis.set(xlabel="CatBoost feature importance")
    figure.tight_layout()
    figure.savefig(output_dir / "feature_importance.svg")
    plt.close(figure)


def fit_outcome(
    input_path: Path,
    output_dir: Path,
    outcome: str,
    *,
    config: BoostingConfig | None = None,
    sample_fraction: float = 1.0,
    exclude_retracted: bool = False,
) -> dict[str, Path]:
    """Fit, evaluate, explain, and persist one outcome model."""
    import numpy as np
    import pandas as pd
    from catboost import CatBoostRegressor, Pool
    from sklearn.model_selection import GroupKFold

    config = config or BoostingConfig()
    analysis_name = outcome + ("_excluding_retracted" if exclude_retracted else "")
    output_dir = Path(output_dir) / analysis_name
    output_dir.mkdir(parents=True, exist_ok=True)
    frame = prepare_model_frame(
        input_path, outcome, sample_fraction=sample_fraction, seed=config.seed
    )
    if exclude_retracted:
        frame = frame.filter(pl.col("is_retracted") != "True")
    parts = temporal_partitions(frame)
    if any(parts[name].is_empty() for name in ("train", "validation", "test")):
        counts = {name: value.height for name, value in parts.items()}
        raise ValueError(f"temporal model partitions require non-empty 2016-2023/2024/2025 data: {counts}")

    x_train, y_train = _xy(parts["train"])
    x_validation, y_validation = _xy(parts["validation"])
    x_test, y_test = _xy(parts["test"])
    model = CatBoostRegressor(**_params(config))
    model.fit(
        x_train,
        y_train,
        cat_features=list(CATEGORICAL_FEATURES),
        eval_set=(x_validation, y_validation),
        early_stopping_rounds=config.early_stopping_rounds,
    )
    best_iterations = max(int(model.get_best_iteration()) + 1, 1)

    development = pl.concat([parts["train"], parts["validation"]]).sort("article_date", "title")
    x_development, y_development = _xy(development)
    final_model = CatBoostRegressor(**_params(config, best_iterations))
    final_model.fit(
        x_development,
        y_development,
        cat_features=list(CATEGORICAL_FEATURES),
    )
    predicted = np.asarray(final_model.predict(x_test), dtype=float)

    historical_median = float(np.median(y_development))
    journal_medians = development.group_by("journal").agg(pl.col("target").median().alias("median"))
    journal_lookup = dict(zip(journal_medians["journal"], journal_medians["median"], strict=True))
    journal_baseline = np.asarray(
        [journal_lookup.get(value, historical_median) for value in parts["test"]["journal"]],
        dtype=float,
    )
    metric_rows: list[dict[str, Any]] = []
    for name, values in (
        ("catboost", predicted),
        ("global_historical_median", np.repeat(historical_median, len(y_test))),
        ("journal_historical_median", journal_baseline),
    ):
        metric_rows.append({"model": name, "split": "test_2025", "rows": len(y_test), **_metrics(y_test, values)})

    test_with_predictions = parts["test"].with_columns(
        pl.Series("_predicted", predicted),
        pl.col("journal").is_in(development["journal"].implode()).alias("_journal_seen"),
    )
    for variable in ("discipline", "open_access", "is_mega", "_journal_seen"):
        for key, group in test_with_predictions.group_by(variable):
            value = key[0] if isinstance(key, tuple) else key
            metric_rows.append(
                {
                    "model": "catboost",
                    "split": "test_2025_subgroup",
                    "subgroup_variable": variable.removeprefix("_"),
                    "subgroup_value": str(value),
                    "rows": group.height,
                    **_metrics(group["target"], group["_predicted"]),
                }
            )

    unique_groups = parts["train"]["journal"].n_unique()
    folds = min(4, unique_groups)
    if folds >= 2:
        groups = parts["train"]["journal"].to_numpy()
        for fold, (train_index, holdout_index) in enumerate(
            GroupKFold(n_splits=folds).split(x_train, y_train, groups), start=1
        ):
            fold_model = CatBoostRegressor(**_params(config, best_iterations))
            fold_model.fit(
                x_train.iloc[train_index],
                y_train[train_index],
                cat_features=list(CATEGORICAL_FEATURES),
            )
            fold_prediction = fold_model.predict(x_train.iloc[holdout_index])
            metric_rows.append(
                {
                    "model": "catboost",
                    "split": f"journal_group_fold_{fold}",
                    "rows": len(holdout_index),
                    **_metrics(y_train[holdout_index], fold_prediction),
                }
            )

    predictions = pl.DataFrame(
        {
            "title": parts["test"]["title"],
            "article_date": parts["test"]["article_date"],
            "journal": parts["test"]["journal"],
            "actual": y_test,
            "predicted": predicted,
            "residual": y_test - predicted,
        }
    )
    importance = pl.DataFrame(
        {"feature": list(FEATURES), "importance": final_model.get_feature_importance()}
    ).sort("importance", descending=True)

    shap_n = min(config.shap_max_rows, len(x_test))
    shap_sample = x_test.sample(n=shap_n, random_state=config.seed) if shap_n else x_test
    shap_values = final_model.get_feature_importance(
        Pool(shap_sample, cat_features=list(CATEGORICAL_FEATURES)), type="ShapValues"
    )
    shap_frame: pd.DataFrame = pd.DataFrame(shap_values[:, :-1], columns=FEATURES)
    shap_frame.insert(0, "row_index", shap_sample.index.to_numpy())
    shap_importance = pl.DataFrame(
        {
            "feature": list(FEATURES),
            "mean_absolute_shap": np.mean(np.abs(shap_values[:, :-1]), axis=0),
        }
    ).sort("mean_absolute_shap", descending=True)

    metrics_path = output_dir / "metrics.csv"
    predictions_path = output_dir / "predictions.parquet"
    importance_path = output_dir / "feature_importance.csv"
    shap_path = output_dir / "shap_values.parquet"
    shap_importance_path = output_dir / "shap_importance.csv"
    model_path = output_dir / "model.cbm"
    manifest_path = output_dir / "model_manifest.json"
    missingness_path = output_dir / "feature_missingness.csv"
    pl.DataFrame(metric_rows).write_csv(metrics_path)
    predictions.write_parquet(predictions_path)
    importance.write_csv(importance_path)
    pl.DataFrame(shap_frame.to_dict(orient="list")).write_parquet(shap_path)
    shap_importance.write_csv(shap_importance_path)
    missing_rows = []
    for split, split_frame in parts.items():
        for feature in FEATURES:
            expression = (
                pl.col(feature).eq("__MISSING__")
                if feature in CATEGORICAL_FEATURES
                else pl.col(feature).is_null() | pl.col(feature).is_nan()
            )
            missing = split_frame.select(expression.sum()).item()
            missing_rows.append(
                {
                    "split": split,
                    "feature": feature,
                    "rows": split_frame.height,
                    "missing": missing,
                    "missing_ratio": missing / split_frame.height if split_frame.height else None,
                }
            )
    pl.DataFrame(missing_rows).write_csv(missingness_path)
    final_model.save_model(model_path)
    manifest_path.write_text(
        json.dumps(
            {
                "outcome": outcome,
                "exclude_retracted": exclude_retracted,
                "config": asdict(config),
                "features": list(FEATURES),
                "categorical_features": list(CATEGORICAL_FEATURES),
                "best_iterations": best_iterations,
                "rows": {name: value.height for name, value in parts.items()},
                "interpretation": "retrospective predictive association; SHAP values are not causal",
            },
            indent=2,
        )
        + "\n",
        encoding="utf-8",
    )
    _plot_outputs(output_dir, predictions, importance)
    return {
        "metrics": metrics_path,
        "predictions": predictions_path,
        "importance": importance_path,
        "shap_values": shap_path,
        "shap_importance": shap_importance_path,
        "model": model_path,
        "manifest": manifest_path,
        "missingness": missingness_path,
    }


def main(argv: list[str] | None = None) -> int:
    parser = argparse.ArgumentParser(description="Fit retrospective CatBoost delay models.")
    parser.add_argument("--input", default="data/processed_data/processed_validated.parquet")
    parser.add_argument("--output-dir", default="data/processed_data/boosting")
    parser.add_argument(
        "--outcome",
        choices=["acceptance_delay", "publication_delay", "both"],
        default="both",
    )
    parser.add_argument("--seed", type=int, default=20250805)
    parser.add_argument("--sample-fraction", type=float, default=1.0)
    parser.add_argument("--iterations", type=int, default=5000)
    parser.add_argument("--shap-max-rows", type=int, default=10000)
    args = parser.parse_args(argv)
    config = BoostingConfig(
        seed=args.seed, iterations=args.iterations, shap_max_rows=args.shap_max_rows
    )
    outcomes = (
        ("acceptance_delay", "publication_delay")
        if args.outcome == "both"
        else (args.outcome,)
    )
    for outcome in outcomes:
        fit_outcome(
            Path(args.input),
            Path(args.output_dir),
            outcome,
            config=config,
            sample_fraction=args.sample_fraction,
        )
        fit_outcome(
            Path(args.input),
            Path(args.output_dir),
            outcome,
            config=config,
            sample_fraction=args.sample_fraction,
            exclude_retracted=True,
        )
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
