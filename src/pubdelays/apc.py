"""Parse DOAJ APC quotes and convert them to a documented EUR proxy."""

from __future__ import annotations

import bisect
import re
from collections import defaultdict
from datetime import date
from pathlib import Path
from statistics import median

import polars as pl

from pubdelays.external.common import scan_tabular

QUOTE_PATTERN = re.compile(r"(?P<amount>\d+(?:[.,]\d+)?)\s*(?P<currency>[A-Za-z]{3})")
APC_DERIVED_COLUMNS = (
    "apc_eur_proxy",
    "apc_eur_proxy_min",
    "apc_eur_proxy_max",
    "apc_quote_count",
    "apc_fx_sources",
    "apc_fx_rate_start",
    "apc_fx_rate_end",
    "apc_conversion_status",
)


def parse_apc_quotes(value: object) -> list[tuple[float, str]]:
    """Return every ``amount ISO4217`` quote found in a source value."""
    text = "" if value is None else str(value)
    return [
        (float(match.group("amount").replace(",", ".")), match.group("currency").upper())
        for match in QUOTE_PATTERN.finditer(text)
    ]


def _rate_index(path: Path | None) -> dict[str, list[tuple[date, float, str]]]:
    if path is None or not Path(path).exists():
        return {}
    rates = scan_tabular(Path(path)).collect()
    required = {"date", "currency", "rate", "source"}
    missing = required - set(rates.columns)
    if missing:
        raise ValueError(f"APC exchange rates missing columns: {', '.join(sorted(missing))}")
    index: dict[str, list[tuple[date, float, str]]] = defaultdict(list)
    for row in rates.select(*sorted(required)).iter_rows(named=True):
        try:
            rate_date = date.fromisoformat(str(row["date"])[:10])
            rate = float(row["rate"])
        except (TypeError, ValueError):
            continue
        if rate > 0:
            index[str(row["currency"]).upper()].append(
                (rate_date, rate, str(row["source"]))
            )
    for values in index.values():
        values.sort(key=lambda item: item[0])
    return dict(index)


def _previous_rate(
    index: dict[str, list[tuple[date, float, str]]], currency: str, on_date: date
) -> tuple[date, float, str] | None:
    if currency == "EUR":
        return on_date, 1.0, "EUR_identity"
    values = index.get(currency, [])
    # Daily ECB observations take precedence. InforEuro is used only when the
    # requested currency/date has no prior ECB observation.
    for preferred in ("ecb", "inforeuro"):
        candidates = [item for item in values if preferred in item[2].lower()]
        position = bisect.bisect_right([item[0] for item in candidates], on_date) - 1
        if position >= 0:
            return candidates[position]
    position = bisect.bisect_right([item[0] for item in values], on_date) - 1
    return values[position] if position >= 0 else None


def attach_apc_eur(
    df: pl.DataFrame, rate_path: Path | None, *, date_column: str = "publication_date"
) -> pl.DataFrame:
    """Add EUR proxy columns without discarding raw APC text or unmatched rows.

    The canonical rate is units of foreign currency per EUR. The latest rate on
    or before the publication date is used. Multiple DOAJ currency quotes are
    converted separately and summarized by their median, minimum, and maximum.
    """
    if "apc_amount" not in df.columns:
        return df.with_columns(*(pl.lit("").alias(name) for name in APC_DERIVED_COLUMNS))
    index = _rate_index(rate_path)
    pairs = df.select("apc_amount", date_column).unique(maintain_order=True)
    rows: list[dict[str, object]] = []
    for item in pairs.iter_rows(named=True):
        raw_date = item[date_column]
        on_date = raw_date if isinstance(raw_date, date) else None
        quotes = parse_apc_quotes(item["apc_amount"])
        converted: list[float] = []
        sources: set[str] = set()
        rate_dates: list[date] = []
        for amount, currency in quotes:
            found = _previous_rate(index, currency, on_date) if on_date else None
            if found is None:
                continue
            rate_date, rate, source = found
            converted.append(amount / rate)
            sources.add(source)
            rate_dates.append(rate_date)
        if not quotes:
            status = "no_parseable_quote" if str(item["apc_amount"] or "").strip() else "no_quote"
        elif len(converted) == len(quotes):
            status = "converted"
        elif converted:
            status = "partially_converted"
        else:
            status = "missing_rate"
        rows.append(
            {
                "apc_amount": item["apc_amount"],
                date_column: raw_date,
                "apc_eur_proxy": median(converted) if converted else None,
                "apc_eur_proxy_min": min(converted) if converted else None,
                "apc_eur_proxy_max": max(converted) if converted else None,
                "apc_quote_count": len(quotes),
                "apc_fx_sources": "|".join(sorted(sources)),
                "apc_fx_rate_start": min(rate_dates).isoformat() if rate_dates else "",
                "apc_fx_rate_end": max(rate_dates).isoformat() if rate_dates else "",
                "apc_conversion_status": status,
            }
        )
    lookup = pl.DataFrame(rows) if rows else pl.DataFrame()
    if lookup.is_empty():
        return df.with_columns(*(pl.lit("").alias(name) for name in APC_DERIVED_COLUMNS))
    lookup = lookup.with_columns(
        pl.col("apc_amount").cast(df.schema["apc_amount"]),
        pl.col(date_column).cast(df.schema[date_column]),
    )
    joined = df.join(lookup, on=["apc_amount", date_column], how="left", coalesce=True)
    return joined.with_columns(
        pl.col("apc_quote_count").fill_null(0),
        pl.col("apc_fx_sources").fill_null(""),
        pl.col("apc_fx_rate_start").fill_null(""),
        pl.col("apc_fx_rate_end").fill_null(""),
        pl.col("apc_conversion_status")
        .fill_null(
            pl.when(
                pl.col("apc_amount").is_null()
                | (pl.col("apc_amount").cast(pl.Utf8).str.strip_chars() == "")
            )
            .then(pl.lit("no_quote"))
            .otherwise(pl.lit("conversion_failed"))
        )
        .alias("apc_conversion_status"),
    )
