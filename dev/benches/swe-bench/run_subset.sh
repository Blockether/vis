#!/usr/bin/env bash
# Drive Vis through a SWE-bench subset, evaluate, and emit summary.json.
#
# Env:
#   SWEBENCH_SUBSET        inner|lite|full  task set (default: inner; lite/full = all Lite)
#   SWEBENCH_MAX_INSTANCES N                local smoke cap after loading the set
#   SWEBENCH_EVALUATE      0|1              run official evaluator (default: 1)
#   SWEBENCH_EVAL_WORKERS  N                evaluator workers (default: 4)
#   SWEBENCH_ARTIFACT_DIR  dir              per-instance Vis traces (default: results/<iter>/artifacts)
#   PYTHON                 Python executable (default: python in venv/PATH, else python3)
#   VIS_MODEL              model name       (default: glm-5.1)
#   VIS_PROVIDER           provider id      (default: zai-coding-plan)
#   VIS_REQUEST_BUDGET     requests cap     (default: 300 inner, 2500 lite/full)
#   ITER_TAG               output dir tag   (default: timestamp)
set -euo pipefail

here="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -P "$here/../../.." && pwd)"

SUBSET="${SWEBENCH_SUBSET:-inner}"
ITER_TAG="${ITER_TAG:-$(date +%Y%m%d-%H%M%S)}"
export VIS_MODEL="${VIS_MODEL:-glm-5.1}"
export VIS_PROVIDER="${VIS_PROVIDER:-zai-coding-plan}"
export VIS_BIN="${VIS_BIN:-$repo_root/bin/vis}"

case "$SUBSET" in
  inner)      INSTANCES="$here/instances.json"; DEFAULT_BUDGET=300;  DATASET="princeton-nlp/SWE-bench_Lite" ;;
  lite|full)  INSTANCES="";                       DEFAULT_BUDGET=2500; DATASET="princeton-nlp/SWE-bench_Lite" ;;
  *) echo "unknown SWEBENCH_SUBSET=$SUBSET (expected inner, lite, or full)" >&2; exit 2 ;;
esac
BUDGET="${VIS_REQUEST_BUDGET:-$DEFAULT_BUDGET}"
export SWEBENCH_MAX_INSTANCES="${SWEBENCH_MAX_INSTANCES:-}"
out_dir="$here/results/$ITER_TAG"
preds_file="$out_dir/predictions.jsonl"
export SWEBENCH_ARTIFACT_DIR="${SWEBENCH_ARTIFACT_DIR:-$out_dir/artifacts}"

# Activate venv if present (recommended; see dev/benches/swe-bench/README.md)
if [[ -f "$here/.venv/bin/activate" ]]; then
  # shellcheck disable=SC1091
  source "$here/.venv/bin/activate"
fi
if [[ -z "${PYTHON:-}" ]]; then
  if command -v python >/dev/null 2>&1; then
    PYTHON=python
  else
    PYTHON=python3
  fi
fi

if [[ -n "$INSTANCES" && ! -f "$INSTANCES" ]]; then
  echo "missing frozen inner subset: $INSTANCES" >&2
  echo "run: $PYTHON dev/benches/swe-bench/subset.py --split lite --n 30 --seed 42 --out $INSTANCES" >&2
  exit 2
fi

mkdir -p "$out_dir" "$SWEBENCH_ARTIFACT_DIR"
: > "$preds_file"

# Build the working instance list as a single JSON array on stdout-of-python.
load_instances_raw() {
  if [[ -n "$INSTANCES" ]]; then
    cat "$INSTANCES"
  else
    "$PYTHON" - <<PY
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

load_instances() {
  load_instances_raw | "$PYTHON" -c '
import json, os, sys
items = json.load(sys.stdin)
limit = os.environ.get("SWEBENCH_MAX_INSTANCES")
if limit:
    items = items[:int(limit)]
json.dump(items, sys.stdout)
'
}

count_total=$(load_instances | "$PYTHON" -c 'import json,sys; print(len(json.load(sys.stdin)))')
echo "[run_subset] subset=$SUBSET instances=$count_total budget=$BUDGET model=$VIS_MODEL" >&2

# Track Z.ai remaining requests before/after so we can attribute cost.
quota_before=$("$VIS_BIN" providers status "$VIS_PROVIDER" 2>/dev/null \
  | awk -F'[ /]+' '/request quota/ {for(i=1;i<=NF;i++) if($i~/^[0-9.]+$/){print $i; exit}}' \
  || true)

n_done=0
load_instances | "$PYTHON" -c '
import json, sys
for inst in json.load(sys.stdin):
    print(json.dumps(inst))
' | while IFS= read -r line; do
  iid=$(printf '%s' "$line" | "$PYTHON" -c 'import json,sys;print(json.load(sys.stdin)["instance_id"])')
  echo "[$((n_done+1))/$count_total] $iid" >&2
  printf '%s' "$line" | "$PYTHON" "$here/vis_agent.py" >> "$preds_file" || true
  n_done=$((n_done+1))

  # Budget check (best-effort, based on n_done since z.ai status is slow to refresh).
  if (( n_done >= BUDGET / 5 )); then
    echo "[run_subset] budget guard: solved $n_done instances, stopping early" >&2
    break
  fi
done

# Evaluate with the official SWE-bench harness.
if [[ "${SWEBENCH_EVALUATE:-1}" == "0" ]]; then
  echo "[run_subset] SWEBENCH_EVALUATE=0; skipping official evaluator" >&2
else
  echo "[run_subset] evaluating $preds_file" >&2
  # SWE-bench 2.x accepts --report_dir but some versions still write the final
  # <model>.<run_id>.json report to cwd. Run from out_dir so all artifacts stay
  # with this iteration either way.
  (
    cd "$out_dir"
    "$PYTHON" -m swebench.harness.run_evaluation \
      --dataset_name "$DATASET" \
      --predictions_path "$preds_file" \
      --max_workers "${SWEBENCH_EVAL_WORKERS:-4}" \
      --run_id "$ITER_TAG" \
      --report_dir "$out_dir" >&2 || true
  )
fi

# Reduce to a single summary.json the metrics script consumes.
"$PYTHON" "$here/metrics.py" \
  --predictions "$preds_file" \
  --report-dir "$out_dir" \
  --run-id "$ITER_TAG" \
  --total "$count_total" \
  --out "$out_dir/summary.json"

# Surface the latest summary to a stable path autoresearch.sh reads.
ln -sf "$out_dir/summary.json" "$here/results/latest-summary.json"
echo "[run_subset] wrote $out_dir/summary.json" >&2
