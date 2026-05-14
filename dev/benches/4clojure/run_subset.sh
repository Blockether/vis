#!/usr/bin/env bash
# Drive Vis through a 4Clojure subset, evaluate answers, and emit summary.json.
set -euo pipefail

here="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
repo_root="$(cd -P "$here/../../.." && pwd)"

SUBSET="${FOURCLOJURE_SUBSET:-inner}"
ITER_TAG="${ITER_TAG:-$(date +%Y%m%d-%H%M%S)}"
export VIS_MODEL="${VIS_MODEL:-glm-5.1}"
export VIS_PROVIDER="${VIS_PROVIDER:-zai-coding-plan}"
export VIS_BIN="${VIS_BIN:-$repo_root/bin/vis}"
export FOURCLOJURE_MAX_INSTANCES="${FOURCLOJURE_MAX_INSTANCES:-}"

case "$SUBSET" in
  inner) INSTANCES="$here/instances.json"; DEFAULT_BUDGET=80 ;;
  all)   INSTANCES="$here/problems.json";  DEFAULT_BUDGET=300 ;;
  *) echo "unknown FOURCLOJURE_SUBSET=$SUBSET (expected inner or all)" >&2; exit 2 ;;
esac
BUDGET="${VIS_REQUEST_BUDGET:-$DEFAULT_BUDGET}"
out_dir="$here/results/$ITER_TAG"
preds_file="$out_dir/predictions.jsonl"
export FOURCLOJURE_TRACE_DIR="$out_dir/traces"

if [[ -f "$here/.venv/bin/activate" ]]; then
  # shellcheck disable=SC1091
  source "$here/.venv/bin/activate"
fi
if [[ -z "${PYTHON:-}" ]]; then
  if command -v python >/dev/null 2>&1; then PYTHON=python; else PYTHON=python3; fi
fi

if [[ ! -f "$INSTANCES" ]]; then
  echo "missing 4Clojure problem set: $INSTANCES" >&2
  echo "run: $PYTHON dev/benches/4clojure/download_problems.py --out $here/problems.json" >&2
  echo "then: $PYTHON dev/benches/4clojure/subset.py --n 30 --seed 42 --out $here/instances.json" >&2
  exit 2
fi

mkdir -p "$out_dir" "$FOURCLOJURE_TRACE_DIR"
: > "$preds_file"

load_instances() {
  "$PYTHON" - <<PY
import json, os
items = json.load(open("$INSTANCES"))
limit = os.environ.get("FOURCLOJURE_MAX_INSTANCES")
if limit:
    items = items[:int(limit)]
print(json.dumps(items, ensure_ascii=False))
PY
}

count_total=$(load_instances | "$PYTHON" -c 'import json,sys; print(len(json.load(sys.stdin)))')
echo "[4clojure] subset=$SUBSET instances=$count_total budget=$BUDGET model=$VIS_MODEL" >&2
echo "[4clojure] traces=$FOURCLOJURE_TRACE_DIR" >&2

n_done=0
load_instances | "$PYTHON" -c '
import json, sys
for inst in json.load(sys.stdin):
    print(json.dumps(inst, ensure_ascii=False))
' | while IFS= read -r line; do
  pid=$(printf '%s' "$line" | "$PYTHON" -c 'import json,sys;print(json.load(sys.stdin)["id"])')
  echo "[$((n_done+1))/$count_total] 4clojure-$pid" >&2
  printf '%s' "$line" | "$PYTHON" "$here/vis_agent.py" >> "$preds_file" || true
  n_done=$((n_done+1))

  if (( n_done >= BUDGET )); then
    echo "[4clojure] budget guard: solved $n_done instances, stopping early" >&2
    break
  fi
done

"$PYTHON" "$here/metrics.py" \
  --predictions "$preds_file" \
  --total "$count_total" \
  --out "$out_dir/summary.json"

ln -sf "$out_dir/summary.json" "$here/results/latest-summary.json"
echo "[4clojure] wrote $out_dir/summary.json" >&2
