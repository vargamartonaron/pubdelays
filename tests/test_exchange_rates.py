from __future__ import annotations

import json
from pathlib import Path

import polars as pl
import pytest

from pubdelays.external.exchange_rates import download_official_exchange_rates


def test_download_official_exchange_rates_normalizes_both_sources(
    monkeypatch: pytest.MonkeyPatch, tmp_path: Path
) -> None:
    ecb = b"KEY,FREQ,CURRENCY,CURRENCY_DENOM,EXR_TYPE,EXR_SUFFIX,TIME_PERIOD,OBS_VALUE\nEXR.D.USD.EUR.SP00.A,D,USD,EUR,SP00,A,2020-01-02,1.12\n"
    inforeuro = json.dumps(
        [{"isoA3Code": "ABC", "value": 2.5, "country": "Example"}]
    ).encode()

    def fake_read(url: str) -> bytes:
        return ecb if "data-api.ecb" in url else inforeuro

    monkeypatch.setattr("pubdelays.external.exchange_rates._read_url", fake_read)
    output = tmp_path / "rates.csv"

    rows = download_official_exchange_rates(output, start_year=2020, end_year=2020)

    frame = pl.read_csv(output)
    assert rows == 13
    assert set(frame["source"]) == {"ECB_daily", "InforEuro_monthly"}
    assert frame.filter(pl.col("source") == "ECB_daily")["rate"].item() == 1.12
