"""Analysis publication-delay analysis outputs.

This script is intentionally outside the core `pubdelays` CLI package. Run it
through `pubdelays run-analysis` or directly when study-specific analysis needs
to evolve independently from data processing.
"""

from __future__ import annotations

from pathlib import Path
from xml.sax.saxutils import escape

import polars as pl

from pubdelays.external.common import write_frame

ANALYSIS_TABLES: tuple[str, ...] = (
    "article_number_all",
    "article_number_covid",
    "article_number_discipline",
    "article_number_megajournal",
    "article_number_npi",
    "article_number_open_access",
    "article_number_quartile",
    "article_number_retracted",
    "month_table",
    "delay_summary",
    "delay_summary_yearly",
    "delay_summary_quartile",
    "delay_summary_discipline",
    "delay_summary_is_covid",
    "delay_summary_is_mega",
    "delay_summary_open_access",
    "summary_logical_variables",
    "summary_numeric_variables",
    "summary_quartile_analysis",
    "summary_npi_analysis",
    "summary_retracted_article",
    "summary_retracted_journals",
)

FIGURE_DATASETS: tuple[str, ...] = (
    "yearly_distribution",
    "monthly_distribution",
    "discipline_distribution",
    "quartile_distribution",
    "journal_article_number_distribution",
    "acceptance_delay_distribution",
    "acceptance_delay_overall_loess_plot",
    "acceptance_delay_overall_rainplot",
    "publication_delay_distribution",
    "acceptance_delay_percentiles",
    "delay_timeline",
    "delay_timeline_filtered",
    "delay_timeline_percent",
    "simple_gam_plot",
    "peer_review_delay_yearly",
    "reviewer_number_yearly",
    "review_round_number_yearly",
    "review_cycle_delay_yearly",
    "review_finding_delay_yearly",
    "first_review_delay_yearly",
    "first_decision_delay_yearly",
    "final_decision_delay_yearly",
    "peer_review_per_discipline",
    "peer_review_per_npi",
    "peer_review_per_quartile",
)


def _scan_processed(path: Path) -> pl.LazyFrame:
    path = Path(path)
    if path.suffix == ".parquet":
        return pl.scan_parquet(path)
    if path.suffix == ".tsv":
        return pl.scan_csv(path, separator="\t", infer_schema=False)
    return pl.scan_csv(path, infer_schema=False)


def _read_processed(path: Path) -> pl.DataFrame:
    path = Path(path)
    if path.suffix == ".parquet":
        return pl.read_parquet(path)
    if path.suffix == ".tsv":
        return pl.read_csv(path, separator="\t", infer_schema=False)
    return pl.read_csv(path, infer_schema=False)


def _ensure_columns(df: pl.DataFrame, columns: tuple[str, ...]) -> pl.DataFrame:
    for column in columns:
        if column not in df.columns:
            df = df.with_columns(pl.lit("").alias(column))
    return df


def _analysis_frame(path: Path) -> pl.DataFrame:
    needed = (
        "journal",
        "issn_linking",
        "discipline",
        "publisher",
        "publisher_group",
        "article_date",
        "acceptance_delay",
        "publication_delay",
        "is_covid",
        "is_mega",
        "open_access",
        "is_retracted",
        "quartile_year",
        "npi_year",
        "npi_discipline",
        "reason",
        "retraction_nature",
        "n_review_round",
        "n_reviews",
        "peer_review_delay",
        "review_cycle_delay",
        "review_finding_delay",
        "first_review_delay",
        "first_decision_delay",
        "final_decision_delay",
    )
    return (
        _ensure_columns(_read_processed(path), needed)
        .with_columns(
            pl.col("article_date").cast(pl.Utf8, strict=False).str.slice(0, 4).alias("article_year"),
            pl.col("article_date").cast(pl.Utf8, strict=False).str.slice(0, 7).alias("article_month"),
            pl.col("acceptance_delay").cast(pl.Float64, strict=False).alias("acceptance_delay_days"),
            pl.col("publication_delay").cast(pl.Float64, strict=False).alias("publication_delay_days"),
            pl.col("n_review_round").cast(pl.Float64, strict=False).alias("n_review_round_num"),
            pl.col("peer_review_delay").cast(pl.Float64, strict=False).alias("peer_review_delay_days"),
            pl.col("review_cycle_delay").cast(pl.Float64, strict=False).alias("review_cycle_delay_days"),
            pl.col("review_finding_delay").cast(pl.Float64, strict=False).alias("review_finding_delay_days"),
            pl.col("first_review_delay").cast(pl.Float64, strict=False).alias("first_review_delay_days"),
            pl.col("first_decision_delay").cast(pl.Float64, strict=False).alias("first_decision_delay_days"),
            pl.col("final_decision_delay").cast(pl.Float64, strict=False).alias("final_decision_delay_days"),
        )
        .filter(pl.col("article_year").str.contains(r"^\d{4}$"))
    )


def _base_frame(path: Path) -> pl.DataFrame:
    return _analysis_frame(path).select(
        "journal",
        "issn_linking",
        "discipline",
        "publisher",
        "publisher_group",
        "article_date",
        "article_year",
        "acceptance_delay_days",
        "publication_delay_days",
    )


def _text(df: pl.DataFrame) -> pl.DataFrame:
    return df.with_columns(pl.all().cast(pl.Utf8, strict=False).fill_null(""))


def _summary(df: pl.DataFrame, keys: list[str]) -> pl.DataFrame:
    return _text(
        df.group_by(keys, maintain_order=True)
        .agg(
            pl.len().alias("articles"),
            pl.col("acceptance_delay_days").mean().round(2).alias("acceptance_delay_mean_days"),
            pl.col("publication_delay_days").mean().round(2).alias("publication_delay_mean_days"),
        )
        .sort(keys)
    )


def _article_number(df: pl.DataFrame, keys: list[str]) -> pl.DataFrame:
    return _text(df.group_by(keys, maintain_order=True).agg(pl.len().alias("articles")).sort(keys))


def _delay_summary(df: pl.DataFrame, keys: list[str] | None = None) -> pl.DataFrame:
    group_keys = keys or []
    agg = [
        pl.len().alias("articles"),
        pl.col("acceptance_delay_days").mean().round(2).alias("acceptance_delay_mean_days"),
        pl.col("acceptance_delay_days").median().alias("acceptance_delay_median_days"),
        pl.col("acceptance_delay_days").quantile(0.25).alias("acceptance_delay_p25_days"),
        pl.col("acceptance_delay_days").quantile(0.75).alias("acceptance_delay_p75_days"),
        pl.col("publication_delay_days").mean().round(2).alias("publication_delay_mean_days"),
        pl.col("publication_delay_days").median().alias("publication_delay_median_days"),
        pl.col("publication_delay_days").quantile(0.25).alias("publication_delay_p25_days"),
        pl.col("publication_delay_days").quantile(0.75).alias("publication_delay_p75_days"),
        (pl.col("acceptance_delay_days") <= 30).sum().alias("acceptance_delay_le_30_days"),
        (pl.col("acceptance_delay_days") <= 60).sum().alias("acceptance_delay_le_60_days"),
        (pl.col("acceptance_delay_days") <= 180).sum().alias("acceptance_delay_le_180_days"),
    ]
    if group_keys:
        return _text(df.group_by(group_keys, maintain_order=True).agg(*agg).sort(group_keys))
    return _text(df.select(*agg))


def _variable_summary(df: pl.DataFrame, columns: tuple[str, ...]) -> pl.DataFrame:
    rows = []
    for column in columns:
        if column not in df.columns:
            continue
        series = df[column]
        rows.append(
            {
                "variable": column,
                "non_missing": str(series.drop_nulls().len()),
                "missing": str(series.null_count()),
                "unique": str(series.n_unique()),
            }
        )
    return pl.DataFrame(rows or [{"variable": "", "non_missing": "0", "missing": "0", "unique": "0"}])


def _peer_review_metric(df: pl.DataFrame, metric: str, keys: list[str]) -> pl.DataFrame:
    return _text(
        df.filter(pl.col(metric).is_not_null())
        .group_by(keys, maintain_order=True)
        .agg(
            pl.len().alias("articles"),
            pl.col(metric).mean().round(2).alias("mean"),
            pl.col(metric).median().alias("median"),
        )
        .sort(keys)
    )


def derive_summary_tables(processed_path: Path, output_dir: Path) -> dict[str, Path]:
    """Create lightweight CSV summary tables from the final processed dataset."""
    output_dir = Path(output_dir)
    output_dir.mkdir(parents=True, exist_ok=True)
    df = _base_frame(processed_path)

    tables = {
        "journal_year": _summary(df, ["journal", "issn_linking", "article_year"]),
        "field_year": _summary(df, ["discipline", "article_year"]),
        "publisher_year": _summary(
            df.filter((pl.col("publisher") != "") | (pl.col("publisher_group") != "")),
            ["publisher_group", "publisher", "article_year"],
        ),
        "delay_distribution": _delay_summary(df, ["article_year"]),
    }

    outputs: dict[str, Path] = {}
    for name, table in tables.items():
        path = output_dir / f"{name}.csv"
        write_frame(path, table)
        outputs[name] = path
    return outputs


def _write_svg_placeholder(path: Path, title: str, table: pl.DataFrame) -> None:
    """Write a small dependency-free SVG preview for figure datasets."""
    numeric_columns = [
        column
        for column in table.columns
        if table[column].cast(pl.Float64, strict=False).drop_nulls().len() > 0
    ]
    value_column = numeric_columns[-1] if numeric_columns else None
    values = (
        table[value_column].cast(pl.Float64, strict=False).fill_null(0).to_list()[:24]
        if value_column
        else []
    )
    width = 720
    height = 360
    margin = 44
    max_value = max(values) if values else 1.0
    max_value = max_value if max_value > 0 else 1.0
    bars: list[str] = []
    if values:
        gap = 4
        bar_width = max((width - 2 * margin - gap * (len(values) - 1)) / len(values), 1)
        for index, value in enumerate(values):
            bar_height = (height - 2 * margin) * max(value, 0) / max_value
            x = margin + index * (bar_width + gap)
            y = height - margin - bar_height
            bars.append(
                f'<rect x="{x:.2f}" y="{y:.2f}" width="{bar_width:.2f}" height="{bar_height:.2f}" fill="#28536b" />'
            )
    else:
        bars.append(
            f'<text x="{margin}" y="{height / 2}" font-size="16" fill="#555">No numeric data available</text>'
        )
    svg = "\n".join(
        [
            f'<svg xmlns="http://www.w3.org/2000/svg" width="{width}" height="{height}" viewBox="0 0 {width} {height}">',
            '<rect width="100%" height="100%" fill="#f8f4ec" />',
            f'<text x="{margin}" y="28" font-family="serif" font-size="22" fill="#1f2d2f">{escape(title)}</text>',
            f'<text x="{margin}" y="50" font-family="sans-serif" font-size="12" fill="#555">Preview generated from CSV data; use the CSV for publication plots.</text>',
            f'<line x1="{margin}" y1="{height - margin}" x2="{width - margin}" y2="{height - margin}" stroke="#333" />',
            f'<line x1="{margin}" y1="{margin}" x2="{margin}" y2="{height - margin}" stroke="#333" />',
            *bars,
            "</svg>",
        ]
    )
    path.parent.mkdir(parents=True, exist_ok=True)
    path.write_text(svg, encoding="utf-8")


def derive_analysis_outputs(processed_path: Path, table_dir: Path, figure_data_dir: Path) -> dict[str, Path]:
    """Create analysis analysis tables and dependency-free SVG previews."""
    table_dir = Path(table_dir)
    figure_data_dir = Path(figure_data_dir)
    table_dir.mkdir(parents=True, exist_ok=True)
    figure_data_dir.mkdir(parents=True, exist_ok=True)
    df = _analysis_frame(processed_path)

    tables: dict[str, pl.DataFrame] = {
        "article_number_all": _article_number(df, ["article_year"]),
        "article_number_covid": _article_number(df, ["article_year", "is_covid"]),
        "article_number_discipline": _article_number(df, ["article_year", "discipline"]),
        "article_number_megajournal": _article_number(df, ["article_year", "is_mega"]),
        "article_number_npi": _article_number(df, ["article_year", "npi_year"]),
        "article_number_open_access": _article_number(df, ["article_year", "open_access"]),
        "article_number_quartile": _article_number(df, ["article_year", "quartile_year"]),
        "article_number_retracted": _article_number(df, ["article_year", "is_retracted"]),
        "month_table": _delay_summary(df, ["article_month"]),
        "delay_summary": _delay_summary(df),
        "delay_summary_yearly": _delay_summary(df, ["article_year"]),
        "delay_summary_quartile": _delay_summary(df, ["quartile_year"]),
        "delay_summary_discipline": _delay_summary(df, ["discipline"]),
        "delay_summary_is_covid": _delay_summary(df, ["is_covid"]),
        "delay_summary_is_mega": _delay_summary(df, ["is_mega"]),
        "delay_summary_open_access": _delay_summary(df, ["open_access"]),
        "summary_logical_variables": _variable_summary(df, ("is_covid", "is_mega", "open_access", "is_retracted")),
        "summary_numeric_variables": _variable_summary(
            df,
            (
                "acceptance_delay_days",
                "publication_delay_days",
                "n_review_round_num",
                "peer_review_delay_days",
            ),
        ),
        "summary_quartile_analysis": _delay_summary(df, ["journal", "issn_linking", "quartile_year"]),
        "summary_npi_analysis": _delay_summary(df, ["journal", "issn_linking", "npi_year"]),
        "summary_retracted_article": _delay_summary(df.filter(pl.col("is_retracted") == "True")),
        "summary_retracted_journals": _delay_summary(df.filter(pl.col("is_retracted") == "True"), ["journal"]),
    }

    figures: dict[str, pl.DataFrame] = {
        "yearly_distribution": tables["article_number_all"],
        "monthly_distribution": _article_number(df, ["article_month"]),
        "discipline_distribution": _article_number(df, ["discipline"]),
        "quartile_distribution": _article_number(df, ["quartile_year"]),
        "journal_article_number_distribution": _article_number(df, ["journal", "issn_linking"]),
        "acceptance_delay_distribution": _text(df.select("article_year", "acceptance_delay_days")),
        "acceptance_delay_overall_loess_plot": _delay_summary(df, ["article_month"]),
        "acceptance_delay_overall_rainplot": _text(df.select("article_year", "acceptance_delay_days")),
        "publication_delay_distribution": _text(df.select("article_year", "publication_delay_days")),
        "delay_timeline": _delay_summary(df, ["article_month"]),
        "delay_timeline_filtered": _delay_summary(df.filter(pl.col("acceptance_delay_days") <= 180), ["article_month"]),
        "delay_timeline_percent": _text(
            df.group_by("article_month", maintain_order=True)
            .agg(
                pl.len().alias("articles"),
                (pl.col("acceptance_delay_days") <= 30).sum().alias("accepted_within_30_days"),
                (pl.col("acceptance_delay_days") <= 60).sum().alias("accepted_within_60_days"),
                (pl.col("acceptance_delay_days") <= 180).sum().alias("accepted_within_180_days"),
            )
            .with_columns(
                (pl.col("accepted_within_30_days") / pl.col("articles") * 100).round(2).alias("pct_30_days"),
                (pl.col("accepted_within_60_days") / pl.col("articles") * 100).round(2).alias("pct_60_days"),
                (pl.col("accepted_within_180_days") / pl.col("articles") * 100).round(2).alias("pct_180_days"),
            )
            .sort("article_month")
        ),
        "simple_gam_plot": _text(
            df.select(
                "article_date",
                "article_year",
                "article_month",
                "acceptance_delay_days",
                "is_covid",
                "discipline",
                "journal",
            )
        ),
        "acceptance_delay_percentiles": _text(
            df.group_by("article_month", maintain_order=True)
            .agg(
                pl.col("acceptance_delay_days").quantile(0.25).alias("p25"),
                pl.col("acceptance_delay_days").median().alias("p50"),
                pl.col("acceptance_delay_days").quantile(0.75).alias("p75"),
            )
            .sort("article_month")
        ),
        "peer_review_delay_yearly": _peer_review_metric(df, "peer_review_delay_days", ["article_year"]),
        "review_round_number_yearly": _peer_review_metric(df, "n_review_round_num", ["article_year"]),
        "review_cycle_delay_yearly": _peer_review_metric(df, "review_cycle_delay_days", ["article_year"]),
        "review_finding_delay_yearly": _peer_review_metric(df, "review_finding_delay_days", ["article_year"]),
        "first_review_delay_yearly": _peer_review_metric(df, "first_review_delay_days", ["article_year"]),
        "first_decision_delay_yearly": _peer_review_metric(df, "first_decision_delay_days", ["article_year"]),
        "final_decision_delay_yearly": _peer_review_metric(df, "final_decision_delay_days", ["article_year"]),
        "peer_review_per_discipline": _peer_review_metric(df, "peer_review_delay_days", ["discipline"]),
        "peer_review_per_npi": _peer_review_metric(df, "peer_review_delay_days", ["npi_year"]),
        "peer_review_per_quartile": _peer_review_metric(df, "peer_review_delay_days", ["quartile_year"]),
    }

    outputs: dict[str, Path] = {}
    for name, table in tables.items():
        path = table_dir / f"{name}.csv"
        write_frame(path, table)
        outputs[f"table:{name}"] = path
    for name, table in figures.items():
        path = figure_data_dir / f"{name}.csv"
        write_frame(path, table)
        outputs[f"figure_data:{name}"] = path
        svg_path = figure_data_dir / f"{name}.svg"
        _write_svg_placeholder(svg_path, name.replace("_", " ").title(), table)
        outputs[f"figure:{name}"] = svg_path
    return outputs


def main(argv: list[str] | None = None) -> int:
    import argparse

    parser = argparse.ArgumentParser(description="Generate analysis analysis outputs.")
    parser.add_argument("--input", default="data/processed_data/processed.parquet")
    parser.add_argument("--table-dir", default="data/processed_data/analysis_tables")
    parser.add_argument("--figure-data-dir", default="data/processed_data/analysis_figures")
    args = parser.parse_args(argv)
    outputs = derive_analysis_outputs(Path(args.input), Path(args.table_dir), Path(args.figure_data_dir))
    print(f"wrote {len(outputs)} analysis outputs")
    return 0


if __name__ == "__main__":
    raise SystemExit(main())
