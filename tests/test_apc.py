from __future__ import annotations

from datetime import date
from pathlib import Path

import polars as pl

from pubdelays.apc import attach_apc_eur, parse_apc_quotes


def test_parse_apc_quotes_preserves_all_currencies() -> None:
    assert parse_apc_quotes("2490 EUR; 2850 USD; 2290 GBP") == [
        (2490.0, "EUR"),
        (2850.0, "USD"),
        (2290.0, "GBP"),
    ]


def test_attach_apc_eur_uses_previous_available_publication_date_rate(tmp_path: Path) -> None:
    rates = tmp_path / "rates.csv"
    pl.DataFrame(
        {
            "date": ["2020-01-02", "2020-01-03"],
            "currency": ["USD", "GBP"],
            "rate": [1.2, 0.8],
            "source": ["ECB_daily", "InforEuro_monthly"],
        }
    ).write_csv(rates)
    frame = pl.DataFrame(
        {
            "publication_date": [date(2020, 1, 4)],
            "apc_amount": ["120 EUR; 120 USD; 80 GBP"],
        }
    )

    result = attach_apc_eur(frame, rates).row(0, named=True)

    assert result["apc_quote_count"] == 3
    assert result["apc_eur_proxy_min"] == 100.0
    assert result["apc_eur_proxy"] == 100.0
    assert result["apc_eur_proxy_max"] == 120.0
    assert result["apc_fx_sources"] == "ECB_daily|EUR_identity|InforEuro_monthly"
    assert result["apc_conversion_status"] == "converted"
