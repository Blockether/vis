#!/usr/bin/env bash
# Drive Vis through a SWE-bench subset, evaluate, and emit summary.json.
#
# Env:
#   SWEBENCH_SUBSET   inner|full     which task set (default: inner)
#   VIS_MODEL         model name     (default: glm-5.1)
#   VIS_PROVIDER      provider id    (default: zai-coding-plan)
#   VIS_REQUEST_BUDGET requests cap  (default: 300 inner, 2500 full)
#   ITER_TAG          string used in output dir (default: timestamp)
set -euo pipefail

here="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -P "$here/../.." && pwd)"

SUBSET="${SWEBENCH_SUBSET:-inner}"
ITER_TAG="${ITER_TAG:-$(date +%Y%m%d-%H%M%S)}"
export VIS_MODEL="${VIS_MODEL:-glm-5.1}"
export VIS_PROVIDER="${VIS_PROVIDER:-zai-coding-plan}"
export VIS_BIN="${VIS_BIN:-$repo_root/bin/vis}"

case "$SUBSET" in
  inner) INSTANCES="$here/instances.json"; DEFAULT_BUDGET=300;  DATASET="princeton-nlp/SWE-bench_Lite" ;;
  full)  INSTANCES="";                       DEFAULT_BUDGET=2500; DATASET="princeton-nlp/SWE-bench_Lite" ;;
  *) echo "unknown SWEBENCH_SUBSET=$SUBSET" >&2; exit 2 ;;
esac
BUDGET="${VIS_REQUEST_BUDGET:-$DEFAULT_BUDGET}"

out_dir="$here/results/$ITER_TAG"
mkdir -p "$out_dir"
preds_file="$out_dir/predictions.jsonl"
: > "$preds_file"

# Activate venv if present (recommended; see dev/swe-bench/README.md)
if [[ -f "$here/.venv/bin/activate" ]]; then
  # shellcheck disable=SC1091
  source "$here/.venv/bin/activate"
fi

# Build the working instance list as a single JSON array on stdout-of-python.
load_instances() {
  if [[ -n "$INSTANCES" ]]; then
    cat "$INSTANCES"
  else
    python - <<PY
from datasets import load_dataset
import json, sys
ds = load_dataset("$DATASET", split="test")
json.dump([
    {"instance_id": r["instance_id"], "repo": r["repo"],
     "base_commit": r["base_commit"], "problem_statement": r["problem_statement"],
     "version": r.get("version")}
    for r in ds
], sys.stdout)
PY
  fi
}

count_total=$(load_instances | python -c 'import json,sys; print(len(json.load(sys.stdin)))')
echo "[run_subset] subset=$SUBSET instances=$count_total budget=$BUDGET model=$VIS_MODEL" >&2

# Track Z.ai remaining requests before/after so we can attribute cost.
quota_before=$("$VIS_BIN" providers status "$VIS_PROVIDER" 2>/dev/null \
  | awk -F'[ /]+' '/request quota/ {for(i=1;i<=NF;i++) if($i~/^[0-9.]+$/){print $i; exit}}' \
  || true)

n_done=0
load_instances | python -c '
import json, sys
for inst in json.load(sys.stdin):
    print(json.dumps(inst))
' | while IFS= read -r line; do
  iid=$(printf '%s' "$line" | python -c 'import json,sys;print(json.load(sys.stdin)["instance_id"])')
  echo "[$((n_done+1))/$count_total] $iid" >&2
  printf '%s' "$line" | python "$here/vis_agent.py" >> "$preds_file" || true
  n_done=$((n_done+1))

  # Budget check (best-effort, based on n_done since z.ai status is slow to refresh).
  if (( n_done >= BUDGET / 5 )); then
    echo "[run_subset] budget guard: solved $n_done instances, stopping early" >&2
    break
  fi
done

# Evaluate with the official SWE-bench harness.
echo "[run_subset] evaluating $preds_file" >&2
python -m swebench.harness.run_evaluation \
  --dataset_name "$DATASET" \
  --predictions_path "$preds_file" \
  --max_workers 4 \
  --run_id "$ITER_TAG" \
  --report_dir "$out_dir" >&2 || true

# Reduce to a single summary.json the metrics script consumes.
python "$here/metrics.py" \
  --predictions "$preds_file" \
  --report-dir "$out_dir" \
  --run-id "$ITER_TAG" \
  --total "$count_total" \
  --out "$out_dir/summary.json"

# Surface the latest summary to a stable path autoresearch.sh reads.
ln -sf "$out_dir/summary.json" "$here/results/latest-summary.json"
echo "[run_subset] wrote $out_dir/summary.json" >&2
