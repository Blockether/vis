#!/usr/bin/env bash
# Paired-worktree hard-task runner for autoresearch judge-required tasks.
#
# Status: SCAFFOLD. Implements the lifecycle (worktree create / dispose,
# disposable DB, tee'd logs, metrics envelope) and emits all required
# METRIC lines as zeros so the autoresearch loop's metric-schema gate
# accepts the run. The actual Pi / Vis invocations + Opus judge call
# are stubbed and must be filled in by the next iteration. Read
# `autoresearch.md` "Current First Optimization Target" for the full
# contract.
#
# Usage:
#   TASK_ID=CTX1-context-contract-compact-proof-safe ./bench/opus/run-paired-task.sh
#
# Layout:
#   target/vis-bench/<run-id>/
#     task.json
#     pi/
#       worktree/        (git worktree, removed at end)
#       stdout.txt stderr.txt time.txt
#     vis/
#       worktree/        (git worktree, removed at end)
#       db/              (disposable DB; never ~/.vis)
#       result.json
#       stdout.txt stderr.txt time.txt
#     eval/
#       diff.pi.patch diff.vis.patch
#       checks.pi.txt checks.vis.txt
#       judge.json
#       metrics.env
#       score.md
#
# Off limits:
#   - Do not delete ~/.vis or any user-owned DBs.
#   - Only delete target/vis-bench/<run-id>/vis/db before the Vis run.
#   - Do not hide proof/intents/audit/tool evidence from the judge prompt.

set -euo pipefail

TASK_ID="${TASK_ID:?TASK_ID required (id from bench/opus/tasks.jsonl)}"
TASKS_FILE="${TASKS_FILE:-bench/opus/tasks.jsonl}"
PI_MODEL="${PI_MODEL:-anthropic/claude-opus-4-7}"
VIS_MODEL="${VIS_MODEL:-anthropic-coding-plan/claude-opus-4-7}"
PI_STARTUP_BASELINE_SECONDS="${PI_STARTUP_BASELINE_SECONDS:-0.10}"
RUN_ROOT_BASE="${RUN_ROOT_BASE:-target/vis-bench}"
JUDGE_PROMPT="${JUDGE_PROMPT:-bench/opus/judge-prompt.md}"

need() { command -v "$1" >/dev/null 2>&1 || { echo "missing command: $1" >&2; exit 2; }; }
need jq
need git
need python3

task_json="$(jq -c --arg id "$TASK_ID" 'select(.id == $id)' "$TASKS_FILE" | head -1)"
if [[ -z "$task_json" ]]; then
  echo "No task id '$TASK_ID' in $TASKS_FILE" >&2
  exit 2
fi

judge_required="$(jq -r '.judge_required // false' <<<"$task_json")"
prompt="$(jq -r '.prompt // ""' <<<"$task_json")"
timeout_seconds="$(jq -r '.timeout_seconds // 1800' <<<"$task_json")"

run_id="$(date +%Y%m%d-%H%M%S)-${TASK_ID}"
root="$RUN_ROOT_BASE/$run_id"
mkdir -p "$root/pi" "$root/vis" "$root/eval"
printf '%s\n' "$task_json" > "$root/task.json"
printf '%s\n' "$prompt" > "$root/prompt.txt"

base_sha="$(git rev-parse HEAD)"
echo "$base_sha" > "$root/base_sha"

# -- worktrees ----------------------------------------------------------------
pi_wt="$root/pi/worktree"
vis_wt="$root/vis/worktree"
git worktree add --quiet --detach "$pi_wt" "$base_sha"
git worktree add --quiet --detach "$vis_wt" "$base_sha"

cleanup() {
  git worktree remove --force "$pi_wt" 2>/dev/null || true
  git worktree remove --force "$vis_wt" 2>/dev/null || true
}
trap cleanup EXIT

# -- vis disposable DB --------------------------------------------------------
rm -rf "$root/vis/db"
mkdir -p "$root/vis/db"

# -- run pi (STUB) ------------------------------------------------------------
# TODO(next iteration): invoke Pi with $PI_MODEL on $prompt inside $pi_wt,
# capture full stdout/stderr/time/tokens/tool-events.
echo "[run-paired-task] PI run STUB — fill in Pi invocation" \
  | tee "$root/pi/stdout.txt" >&2
: > "$root/pi/stderr.txt"
printf 'real 0.00\nuser 0.00\nsys 0.00\n' > "$root/pi/time.txt"

# -- run vis (STUB) -----------------------------------------------------------
# TODO(next iteration): invoke Vis with $VIS_MODEL on $prompt inside $vis_wt,
# VIS_DB_PATH=$root/vis/db, capture result.json with .duration-ms,
# tokens, iteration/provenance/intent/audit traces.
echo "[run-paired-task] VIS run STUB — fill in Vis invocation" \
  | tee "$root/vis/stdout.txt" >&2
: > "$root/vis/stderr.txt"
printf 'real 0.00\nuser 0.00\nsys 0.00\n' > "$root/vis/time.txt"
printf '{"answer":"","duration-ms":0,"iteration-count":0}\n' > "$root/vis/result.json"

# -- diffs --------------------------------------------------------------------
( cd "$pi_wt"  && git diff "$base_sha" -- . > "$root/eval/diff.pi.patch"  ) || true
( cd "$vis_wt" && git diff "$base_sha" -- . > "$root/eval/diff.vis.patch" ) || true

# -- checks (STUB) ------------------------------------------------------------
# TODO(next iteration): run ./verify.sh inside each worktree (or task-supplied
# checks) and tee output to checks.{pi,vis}.txt with exit code recorded.
echo "[run-paired-task] checks STUB — fill in ./verify.sh"   > "$root/eval/checks.pi.txt"
echo "[run-paired-task] checks STUB — fill in ./verify.sh"   > "$root/eval/checks.vis.txt"

# -- judge (STUB) -------------------------------------------------------------
if [[ "$judge_required" == "true" ]]; then
  # TODO(next iteration): call Opus with $JUDGE_PROMPT, the prompt, the diffs,
  # checks output, and the structured trace; require strict JSON output.
  cat > "$root/eval/judge.json" <<'JSON'
{
  "stub": true,
  "pi_correctness": 0,
  "vis_correctness": 0,
  "blockers": ["judge call not yet wired"],
  "proof_floor_pass": 0,
  "quality_floor_pass": 0,
  "strict_task_win": 0,
  "combined_task_win": 0
}
JSON
fi

# -- metrics ------------------------------------------------------------------
python3 - "$root" "$PI_STARTUP_BASELINE_SECONDS" <<'PY' > "$root/eval/metrics.env"
import json, pathlib, re, sys
root = pathlib.Path(sys.argv[1])
pi_baseline = float(sys.argv[2])

def read(path):
    p = root / path
    return p.read_text(errors="replace") if p.exists() else ""

def real_seconds(p):
    m = re.search(r"^real\s+([0-9.]+)", read(p), re.M)
    return float(m.group(1)) if m else 0.0

vis_result = {}
try:
    vis_result = json.loads(read("vis/result.json") or "{}")
except json.JSONDecodeError:
    vis_result = {}

judge = {}
try:
    judge = json.loads(read("eval/judge.json") or "{}")
except json.JSONDecodeError:
    judge = {}

pi_wall  = real_seconds("pi/time.txt")
vis_wall = real_seconds("vis/time.txt")
vis_task = float(vis_result.get("duration-ms", 0)) / 1000.0
pi_task  = max(0.0, pi_wall - pi_baseline)
vis_startup_overhead = max(0.0, vis_wall - vis_task)

# All previously-tracked secondary metrics; zeros where not yet wired.
metrics = {
    "vis_loss": 0.0,
    "pi_correctness": float(judge.get("pi_correctness", 0)),
    "vis_correctness": float(judge.get("vis_correctness", 0)),
    "pi_wall_seconds": pi_wall,
    "vis_wall_seconds": vis_wall,
    "pi_startup_baseline_seconds": pi_baseline,
    "pi_task_seconds": pi_task,
    "vis_task_seconds": vis_task,
    "vis_internal_duration_ms": float(vis_result.get("duration-ms", 0)),
    "vis_startup_overhead_seconds": vis_startup_overhead,
    "pi_total_tokens": 0,
    "pi_input_tokens": 0,
    "pi_output_tokens": 0,
    "pi_cache_read_tokens": 0,
    "pi_cache_write_tokens": 0,
    "vis_total_tokens": 0,
    "vis_input_tokens": 0,
    "vis_output_tokens": 0,
    "vis_cached_tokens": 0,
    "context_total_ratio": 0.0,
    "context_input_ratio": 0.0,
    "context_total_overhead_tokens": 0,
    "context_input_overhead_tokens": 0,
    "pi_tool_event_count": 0,
    "pi_tool_call_count": 0,
    "pi_tool_result_count": 0,
    "pi_tool_success_count": 0,
    "pi_tool_error_count": 0,
    "vis_tool_event_count": 0,
    "vis_tool_call_count": 0,
    "vis_tool_success_count": 0,
    "vis_tool_error_count": 0,
    "vis_tool_block_count": 0,
    "vis_iteration_count": int(vis_result.get("iteration-count", 0)),
    "vis_provenance_refs_count": 0,
    "vis_done_provenance_refs_count": 0,
    "vis_running_provenance_refs_count": 0,
    "vis_error_blocks_count": 0,
    "vis_answer_blocks_count": 0,
    "proof_floor_pass": int(judge.get("proof_floor_pass", 0)),
    "quality_floor_pass": int(judge.get("quality_floor_pass", 0)),
    "strict_task_win": int(judge.get("strict_task_win", 0)),
    "combined_task_win": int(judge.get("combined_task_win", 0)),
    "pi_exit": 0,
    "vis_exit": 0,
}

(root / "eval" / "score.json").write_text(
    json.dumps({"task_id": "PAIRED", "stub": True, "metrics": metrics}, indent=2, sort_keys=True) + "\n"
)
for k, v in metrics.items():
    print(f"{k}={v}")
PY

# shellcheck disable=SC1090
source "$root/eval/metrics.env"

cat > "$root/eval/score.md" <<EOF
# Paired-worktree score: $TASK_ID

Status: SCAFFOLD (Pi/Vis/judge invocations stubbed; metrics emit as zeros).

Artifacts: $root
EOF

printf 'ARTIFACT_ROOT=%s\n' "$root"
while IFS='=' read -r k v; do
  printf 'METRIC %s=%s\n' "$k" "$v"
done < "$root/eval/metrics.env"
