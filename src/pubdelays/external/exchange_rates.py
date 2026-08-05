"""Acquire canonical official EUR exchange-rate observations."""

from __future__ import annotations

import csv
import io
import json
import urllib.parse
import urllib.request
from datetime import date
from pathlib import Path

import polars as pl

from pubdelays.download import download_request
from pubdelays.external.common import write_frame

ECB_URL = "https://data-api.ecb.europa.eu/service/data/EXR/D..EUR.SP00.A"
INFOEURO_URL = "https://ec.europa.eu/budg/inforeuro/api/public/monthly-rates"


def _read_url(url: str) -> bytes:
    with urllib.request.urlopen(download_request(url), timeout=120) as response:
        return response.read()


def download_official_exchange_rates(
    output: Path, *, start_year: int = 2013, end_year: int | None = None
) -> int:
    """Download ECB daily rates and InforEuro monthly fallback into one table."""
    end_year = end_year or date.today().year
    query = urllib.parse.urlencode(
        {
            "startPeriod": f"{start_year}-01-01",
            "endPeriod": f"{end_year}-12-31",
            "format": "csvdata",
            "detail": "dataonly",
        }
    )
    ecb_rows = csv.DictReader(io.StringIO(_read_url(f"{ECB_URL}?{query}").decode("utf-8")))
    rows: list[dict[str, object]] = [
        {
            "date": row["TIME_PERIOD"],
            "currency": row["CURRENCY"],
            "rate": row["OBS_VALUE"],
            "source": "ECB_daily",
            "frequency": "daily",
        }
        for row in ecb_rows
        if row.get("TIME_PERIOD") and row.get("CURRENCY") and row.get("OBS_VALUE")
    ]
    for year in range(start_year, end_year + 1):
        for month in range(1, 13):
            if date(year, month, 1) > date.today().replace(day=1):
                break
            payload = json.loads(
                _read_url(f"{INFOEURO_URL}?{urllib.parse.urlencode({'year': year, 'month': month})}")
            )
            for item in payload:
                currency = str(item.get("isoA3Code") or "").upper()
                rate = item.get("value")
                if currency and rate is not None:
                    rows.append(
                        {
                            "date": f"{year:04d}-{month:02d}-01",
                            "currency": currency,
                            "rate": rate,
                            "source": "InforEuro_monthly",
                            "frequency": "monthly",
                        }
                    )
    frame = (
        pl.DataFrame(rows)
        .with_columns(
            pl.col("date").cast(pl.Utf8),
            pl.col("currency").cast(pl.Utf8),
            pl.col("rate").cast(pl.Float64, strict=False),
        )
        .filter(pl.col("rate").is_not_null() & (pl.col("rate") > 0))
        .unique(subset=["date", "currency", "source"], maintain_order=True)
        .sort("date", "currency", "source")
    )
    return write_frame(Path(output), frame)
