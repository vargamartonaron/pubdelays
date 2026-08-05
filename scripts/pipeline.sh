#!/usr/bin/env bash
set -euo pipefail

ROOT="${ROOT:-$(git rev-parse --show-toplevel 2>/dev/null || pwd)}"
CONFIG="${CONFIG:-$ROOT/config/default.toml}"
RUN="${RUN:-pubdelays}"
JOBS="${JOBS:-$(nproc 2>/dev/null || echo 4)}"
SHARDS="${SHARDS:-64}"

cd "$ROOT"

"$RUN" --config "$CONFIG" init-dirs
"$RUN" --config "$CONFIG" preflight
"$RUN" --config "$CONFIG" external-all --resume
"$RUN" --config "$CONFIG" journals --resume
"$RUN" --config "$CONFIG" parse --jobs "$JOBS" --format jsonl --parse-mesh-subterms --resume
"$RUN" --config "$CONFIG" validate
"$RUN" --config "$CONFIG" transform-shards --shards "$SHARDS" --jobs "$JOBS" --format parquet --resume
"$RUN" --config "$CONFIG" aggregate-all --resume
"$RUN" --config "$CONFIG" filter-counts --resume
"$RUN" --config "$CONFIG" quality-report
