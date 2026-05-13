#!/usr/bin/env bash
# Inner-loop benchmark: solve frozen SWE-bench Lite subset (N=30) with Vis
# pinned to zai-coding-plan / glm-5.1, then emit METRIC lines.
set -euo pipefail

repo_root="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
cd "$repo_root"

export VIS_PROVIDER="${VIS_PROVIDER:-zai-coding-plan}"
export VIS_MODEL="${VIS_MODEL:-glm-5.1}"
export SWEBENCH_SUBSET="${SWEBENCH_SUBSET:-inner}"

# ── 1. Pre-checks (fast, <2s) ────────────────────────────────────────────
# Vis must start. If the JAR/SCI is broken, fail before burning quota.
if ! ./bin/vis run --db :memory "(+ 1 1)" >/dev/null 2>&1; then
  echo "vis smoke test failed — aborting iteration" >&2
  exit 1
fi
# Provider must be authenticated.
./bin/vis providers status "$VIS_PROVIDER" >/dev/null 2>&1 \
  || { echo "$VIS_PROVIDER not authed" >&2; exit 1; }
# Frozen subset must exist.
test -f dev/swe-bench/instances.json \
  || { echo "dev/swe-bench/instances.json missing — run subset.py first" >&2; exit 1; }

# ── 2. Run the subset ────────────────────────────────────────────────────
iter_tag="iter-$(date +%Y%m%d-%H%M%S)"
ITER_TAG="$iter_tag" ./dev/swe-bench/run_subset.sh

# ── 3. Emit metrics ──────────────────────────────────────────────────────
summary="dev/swe-bench/results/latest-summary.json"
test -f "$summary" || { echo "no summary.json produced" >&2; exit 1; }

# Primary metric first, then secondaries; one extra echo per error category.
python - "$summary" <<'PY'
import json, sys
s = json.load(open(sys.argv[1]))
def emit(k, v):
    if v is None: return
    print(f"METRIC {k}={v}")
emit("resolved_pct",      s["resolved_pct"])         # PRIMARY
emit("resolved_count",    s["resolved"])
emit("patches_emitted",   s["patches_emitted"])
emit("empty_patches",     s["empty_patches"])
emit("errors",            s["errors"])
emit("median_seconds",    s["median_seconds"])
emit("p95_seconds",       s["p95_seconds"])
emit("tokens_total",      s["tokens_total"])
emit("cost_usd",          s["cost_usd"])
emit("instances_run",     s["instances_run"])
for cat, n in (s.get("error_categories") or {}).items():
    emit(f"err_{cat.replace('-','_')}", n)
PY
