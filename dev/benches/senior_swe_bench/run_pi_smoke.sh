#!/usr/bin/env bash
set -euo pipefail
here="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

export VIS_PROVIDER="${VIS_PROVIDER:-zai-coding-plan}"
export VIS_MODEL="${VIS_MODEL:-glm-5.2}"
export VIS_BENCH_AGENT="${VIS_BENCH_AGENT:-dev.benches.senior_swe_bench.agent:PiGlm52Agent}"
export VIS_BENCH_REASONING_EFFORT="${VIS_BENCH_REASONING_EFFORT:-high}"
export VIS_BENCH_AGENT_MODEL="${VIS_BENCH_AGENT_MODEL:-zai/$VIS_MODEL}"
export VIS_BENCH_AGENT_LABEL="${VIS_BENCH_AGENT_LABEL:-pi.dev}"
export VIS_BENCH_AGENT_VERSION="${VIS_BENCH_AGENT_VERSION:-0.73.1}"
export VIS_BENCH_AGENT_ENDPOINT="${VIS_BENCH_AGENT_ENDPOINT:-https://api.z.ai/api/coding/paas/v4}"
export VIS_BENCH_AGENT_ROUTE="${VIS_BENCH_AGENT_ROUTE:-zai-coding-plan}"
export RUN_ID="${RUN_ID:-pi-zai-$(date -u +%Y%m%d-%H%M%S)}"

exec "$here/run_smoke.sh" "$@"
