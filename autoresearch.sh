#!/usr/bin/env bash
# Autoresearch benchmark wrapper for the 4Clojure subset bench.
# Outputs METRIC name=value lines parsed by run_experiment.
set -euo pipefail

here="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$here"

INSTANCES="${BENCH_INSTANCES:-8}"
TAG="ar-$(date +%s)"

export FOURCLOJURE_MAX_INSTANCES="$INSTANCES"
export ITER_TAG="$TAG"
export VIS_MODEL="${VIS_MODEL:-glm-5.1}"
export VIS_PROVIDER="${VIS_PROVIDER:-zai-coding-plan}"
export VIS_REQUEST_BUDGET="${VIS_REQUEST_BUDGET:-$INSTANCES}"

# The run_subset.sh script already emits METRIC lines via metrics.py.
./dev/benches/4clojure/run_subset.sh
