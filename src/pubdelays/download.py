"""Download helpers for PubMed and external metadata sources."""

from __future__ import annotations

import hashlib
import re
import time
import urllib.error
import urllib.request
from dataclasses import dataclass
from pathlib import Path

from pubdelays.config import PipelineConfig
from pubdelays.fs import atomic_output_path, complete_file

DEFAULT_DOWNLOAD_HEADERS = {
    "User-Agent": "pubdelays/0.1 (+https://github.com/martonaronvarga/pubdelays)",
    "Accept": "*/*",
}
DEFAULT_DOWNLOAD_ACCEPT = "application/gzip,application/octet-stream,text/csv,text/plain,*/*"


@dataclass(frozen=True)
class DownloadStats:
    """Outcome for one downloaded or resumed remote file."""

    link: str
    output_path: str
    downloaded: bool
    skipped: bool = False


@dataclass(frozen=True)
class ExternalDownloadPlan:
    """Resolved public/configured metadata download target."""

    source: str
    url: str
    output_path: Path


class DownloadError(RuntimeError):
    """Raised when a remote file cannot be downloaded after retries."""


def download_request(url: str, *, accept: str = "*/*") -> urllib.request.Request:
    """Build a request with a project user agent and broad Accept header."""
    headers = {**DEFAULT_DOWNLOAD_HEADERS, "Accept": accept}
    return urllib.request.Request(url, headers=headers)


def index_links(url: str) -> list[str]:
    """Return safe PubMed archive links from an NCBI directory listing."""
    request = download_request(url, accept="text/html,*/*")
    with urllib.request.urlopen(request) as response:
        html = response.read().decode("utf-8", errors="replace")
    return sorted(set(re.findall(r'href="([^"/]+\.(?:gz|md5))"', html)))


def contained_download_path(output_dir: Path, link: str) -> Path:
    """Resolve a remote href while rejecting absolute or escaping paths."""
    if Path(link).is_absolute():
        raise ValueError(f"unsafe absolute download link: {link}")
    output_root = output_dir.resolve()
    output_path = output_dir / link
    try:
        output_path.resolve().relative_to(output_root)
    except ValueError as exc:
        raise ValueError(f"unsafe download link outside output directory: {link}") from exc
    return output_path


def download_file(
    url: str,
    output_path: Path,
    *,
    resume: bool,
    retries: int = 5,
    require_md5: bool = True,
    timeout: int = 120,
) -> DownloadStats:
    """Download one file atomically, optionally resuming complete existing outputs."""
    output_path.parent.mkdir(parents=True, exist_ok=True)
    if resume and complete_file(output_path):
        if not require_md5 or output_path.suffix == ".md5" or complete_download(output_path):
            return DownloadStats(url, str(output_path), downloaded=False, skipped=True)

    last: BaseException | None = None
    attempts = max(retries, 1)
    for attempt in range(attempts):
        try:
            request = download_request(url, accept=DEFAULT_DOWNLOAD_ACCEPT)
            with atomic_output_path(output_path) as tmp_path:
                with (
                    urllib.request.urlopen(request, timeout=timeout) as response,
                    tmp_path.open("wb") as handle,
                ):
                    while chunk := response.read(8 * 1024 * 1024):
                        handle.write(chunk)
            return DownloadStats(url, str(output_path), downloaded=True, skipped=False)
        except (urllib.error.URLError, TimeoutError, OSError) as exc:
            last = exc
            if attempt < attempts - 1:
                time.sleep(min(2**attempt, 30))
    raise DownloadError(f"failed to download {url}: {last!r}")


def md5sum(path: Path) -> str:
    digest = hashlib.md5(usedforsecurity=False)
    with Path(path).open("rb") as handle:
        while chunk := handle.read(8 * 1024 * 1024):
            digest.update(chunk)
    return digest.hexdigest()


def parse_md5_sidecar(content: str) -> tuple[str, str] | None:
    content = content.strip()
    if not content:
        return None
    ncbi_match = re.match(
        r"MD5\s*\((?P<filename>[^)]+)\)\s*=\s*(?P<md5>[0-9a-fA-F]{32})",
        content,
    )
    if ncbi_match:
        return ncbi_match.group("md5").lower(), ncbi_match.group("filename")
    unix_match = re.match(r"(?P<md5>[0-9a-fA-F]{32})\s+\*?(?P<filename>.+)", content)
    if unix_match:
        return unix_match.group("md5").lower(), Path(unix_match.group("filename")).name
    return None


def verify_md5_file(md5_path: Path) -> bool:
    parsed = parse_md5_sidecar(md5_path.read_text(encoding="utf-8"))
    if parsed is None:
        return False
    expected, filename = parsed
    data_path = md5_path.parent / Path(filename).name
    return data_path.exists() and md5sum(data_path) == expected


def expected_md5_sidecar(data_path: Path) -> Path:
    return Path(f"{data_path}.md5")


def verify_download_pair(data_path: Path) -> bool:
    sidecar = expected_md5_sidecar(data_path)
    return sidecar.exists() and verify_md5_file(sidecar)


def complete_download(data_path: Path) -> bool:
    return complete_file(data_path) and verify_download_pair(data_path)


def external_download_plans(
    config: PipelineConfig, *, source: str, start_year: int, end_year: int
) -> list[ExternalDownloadPlan]:
    """Resolve configured external metadata download URLs into concrete file targets."""
    scimago_template = str(config.get("external.download.scimago_url_template", ""))
    publisher_url = str(config.get("external.download.publisher_url", ""))
    if source == "all":
        requested = ["doaj", "retraction-watch"]
        if scimago_template:
            requested.append("scimago")
        if publisher_url:
            requested.append("publisher")
    else:
        requested = [source]

    plans: list[ExternalDownloadPlan] = []
    if "doaj" in requested:
        plans.append(
            ExternalDownloadPlan(
                "doaj",
                str(config.get("external.download.doaj_url", "https://doaj.org/csv")),
                config.path("external.raw.doaj_csv"),
            )
        )
    if "retraction-watch" in requested:
        plans.append(
            ExternalDownloadPlan(
                "retraction-watch",
                str(
                    config.get(
                        "external.download.retraction_watch_url",
                        "https://gitlab.com/crossref/retraction-watch-data/-/raw/main/retraction_watch.csv",
                    )
                ),
                config.path("external.raw.retraction_watch_csv"),
            )
        )
    if "scimago" in requested:
        if not scimago_template:
            raise RuntimeError(
                "external.download.scimago_url_template is empty; configure a working SCImago yearly URL template"
            )
        if "{year}" not in scimago_template:
            raise RuntimeError(
                "external.download.scimago_url_template must contain {year}; use a yearly mirror or file URL"
            )
        for year in range(start_year, end_year + 1):
            plans.append(
                ExternalDownloadPlan(
                    "scimago",
                    scimago_template.format(year=year),
                    config.path("external.raw.scimago_dir") / f"scimagojr {year}.csv",
                )
            )
    if "publisher" in requested:
        if not publisher_url:
            raise RuntimeError("external.download.publisher_url is empty; configure a publisher metadata CSV URL")
        plans.append(ExternalDownloadPlan("publisher", publisher_url, config.path("external.raw.publisher_csv")))
    return plans
