#!/usr/bin/env bash
set -euo pipefail

TASK_ID="${TASK_ID:-S0-hi}"
TASKS_FILE="${TASKS_FILE:-bench/opus/tasks.jsonl}"
PI_MODEL="${PI_MODEL:-anthropic/claude-opus-4-7}"
VIS_MODEL="${VIS_MODEL:-anthropic-coding-plan/claude-opus-4-7}"
PI_STARTUP_BASELINE_SECONDS="${PI_STARTUP_BASELINE_SECONDS:-0.10}"
RUN_ROOT_BASE="${RUN_ROOT_BASE:-target/vis-bench}"

need() {
  command -v "$1" >/dev/null 2>&1 || { echo "missing command: $1" >&2; exit 2; }
}
need jq
need python3
need pi

task_json="$(jq -c --arg id "$TASK_ID" 'select(.id == $id)' "$TASKS_FILE" | head -1)"
if [[ -z "$task_json" ]]; then
  echo "No task id '$TASK_ID' in $TASKS_FILE" >&2
  exit 2
fi

prompt="$(jq -r '.prompt' <<<"$task_json")"
expected="$(jq -r '.expected_answer // empty' <<<"$task_json")"
pi_tools="$(jq -r '.pi_tools // "none"' <<<"$task_json")"
timeout_seconds="$(jq -r '.timeout_seconds // 180' <<<"$task_json")"
run_id="$(date +%Y%m%d-%H%M%S)-${TASK_ID}"
root="$RUN_ROOT_BASE/$run_id"
mkdir -p "$root/pi" "$root/vis" "$root/eval"
printf '%s\n' "$prompt" > "$root/prompt.txt"
printf '%s\n' "$task_json" > "$root/task.json"

# Pi run. Pi startup is tiny but not zero, so subtract a fixed baseline for task-speed metrics.
pi_cmd=(pi --mode json --model "$PI_MODEL" --no-session -p "$prompt")
if [[ "$pi_tools" == "none" ]]; then
  pi_cmd+=(--no-tools)
else
  pi_cmd+=(--tools "$pi_tools")
fi

set +e
/usr/bin/time -p -o "$root/pi/time.txt" timeout "$timeout_seconds" "${pi_cmd[@]}" > "$root/pi/events.jsonl" 2> "$root/pi/stderr.txt"
pi_exit=$?
set -e
printf '%s\n' "$pi_exit" > "$root/pi/exit_code"

jq -r 'select(.type == "message_end" and .message.role == "assistant") | .message.content[]? | select(.type == "text") | .text' \
  "$root/pi/events.jsonl" > "$root/pi/output.txt" 2>/dev/null || :
jq -s 'map(select(.type == "message_end" and .message.role == "assistant") | .message.usage) | last // {}' \
  "$root/pi/events.jsonl" > "$root/pi/metrics.tokens.json" 2>/dev/null || echo '{}' > "$root/pi/metrics.tokens.json"

# Vis run. Always delete disposable benchmark DB before the run.
rm -rf "$root/vis/db"
set +e
/usr/bin/time -p -o "$root/vis/time.txt" timeout "$timeout_seconds" \
  bin/vis run --json --model "$VIS_MODEL" --db "$root/vis/db" "$prompt" \
  > "$root/vis/result.json" 2> "$root/vis/stderr.txt"
vis_exit=$?
set -e
printf '%s\n' "$vis_exit" > "$root/vis/exit_code"

jq -r '.answer // empty' "$root/vis/result.json" > "$root/vis/output.txt" 2>/dev/null || :
jq '.tokens // {}' "$root/vis/result.json" > "$root/vis/metrics.tokens.json" 2>/dev/null || echo '{}' > "$root/vis/metrics.tokens.json"

python3 - "$root" "$expected" "$PI_STARTUP_BASELINE_SECONDS" <<'PY' > "$root/eval/metrics.env"
import json, re, sys, pathlib
root = pathlib.Path(sys.argv[1])
expected = sys.argv[2]
pi_startup = float(sys.argv[3])

def read(path):
    p = root / path
    return p.read_text(errors="replace") if p.exists() else ""

def load_json(path, default):
    try:
        return json.loads(read(path))
    except Exception:
        return default

def real_seconds(path):
    m = re.search(r"^real\s+([0-9.]+)", read(path), re.M)
    return float(m.group(1)) if m else 0.0

def clean_answer(s):
    return s.strip()

pi_exit = int(clean_answer(read("pi/exit_code")) or "1")
vis_exit = int(clean_answer(read("vis/exit_code")) or "1")
pi_answer = clean_answer(read("pi/output.txt"))
vis_answer = clean_answer(read("vis/output.txt"))
pi_wall = real_seconds("pi/time.txt")
vis_wall = real_seconds("vis/time.txt")
pi_task = max(0.0, pi_wall - pi_startup)
vis_result = load_json("vis/result.json", {})
vis_duration_ms = float(vis_result.get("duration-ms") or vis_result.get("duration_ms") or 0.0)
vis_task = vis_duration_ms / 1000.0
pi_usage = load_json("pi/metrics.tokens.json", {})
vis_tokens_obj = load_json("vis/metrics.tokens.json", {})
pi_events = []
for line in read("pi/events.jsonl").splitlines():
    try:
        pi_events.append(json.loads(line))
    except Exception:
        pass
pi_total_tokens = float(pi_usage.get("totalTokens") or pi_usage.get("total_tokens") or 0)
pi_input_tokens = float(pi_usage.get("input") or pi_usage.get("input_tokens") or 0)
pi_output_tokens = float(pi_usage.get("output") or pi_usage.get("output_tokens") or 0)
pi_cache_read = float(pi_usage.get("cacheRead") or pi_usage.get("cache_read") or 0)
pi_cache_write = float(pi_usage.get("cacheWrite") or pi_usage.get("cache_write") or 0)
vis_total_tokens = float(vis_tokens_obj.get("total") or 0)
vis_input_tokens = float(vis_tokens_obj.get("input") or 0)
vis_output_tokens = float(vis_tokens_obj.get("output") or 0)
vis_cached_tokens = float(vis_tokens_obj.get("cached") or 0)
context_total_ratio = (vis_total_tokens / pi_total_tokens) if pi_total_tokens > 0 else 0.0
context_input_ratio = (vis_input_tokens / pi_input_tokens) if pi_input_tokens > 0 else 0.0
context_total_overhead_tokens = max(0.0, vis_total_tokens - pi_total_tokens)
context_input_overhead_tokens = max(0.0, vis_input_tokens - pi_input_tokens)

pi_tool_event_count = sum(1 for e in pi_events if "tool" in str(e.get("type", "")).lower())
pi_tool_call_count = 0
pi_tool_result_count = 0
pi_tool_error_count = 0
for e in pi_events:
    msg = e.get("message") or {}
    for item in msg.get("content") or []:
        typ = item.get("type")
        if typ in {"tool_use", "server_tool_use"}:
            pi_tool_call_count += 1
        if typ == "tool_result":
            pi_tool_result_count += 1
            if item.get("is_error") or item.get("error"):
                pi_tool_error_count += 1
    tool_results = e.get("toolResults") or e.get("tool_results") or []
    pi_tool_result_count += len(tool_results)
    for result in tool_results:
        if isinstance(result, dict) and (result.get("error") or result.get("isError") or result.get("is_error") or result.get("status") in {"error", "failed", "timeout"}):
            pi_tool_error_count += 1
pi_tool_success_count = max(0, pi_tool_result_count - pi_tool_error_count)

vis_blocks = []
for iteration in vis_result.get("trace") or []:
    vis_blocks.extend(iteration.get("blocks") or [])
vis_provenance_refs_count = sum(1 for b in vis_blocks if ((b.get("provenance") or {}).get("ref")))
vis_done_provenance_refs_count = sum(1 for b in vis_blocks if ((b.get("provenance") or {}).get("status") == "done"))
vis_running_provenance_refs_count = sum(1 for b in vis_blocks if ((b.get("provenance") or {}).get("status") == "running"))
vis_error_blocks_count = sum(1 for b in vis_blocks if b.get("error"))
vis_answer_blocks_count = sum(1 for b in vis_blocks if ((b.get("provenance") or {}).get("op") == "vis/answer"))
vis_tool_events = []
for b in vis_blocks:
    vis_tool_events.extend(b.get("tool-events") or b.get("tool_events") or [])
vis_tool_event_count = len(vis_tool_events)
vis_tool_block_count = sum(1 for b in vis_blocks if str((b.get("provenance") or {}).get("op") or "").startswith("v/"))
vis_tool_call_count = max(vis_tool_event_count, vis_tool_block_count)
vis_tool_error_count = 0
for ev in vis_tool_events:
    if isinstance(ev, dict) and (ev.get("error") or ev.get("timeout?") or ev.get("timeout") or ev.get("status") in {"error", "failed", "timeout"}):
        vis_tool_error_count += 1
vis_tool_success_count = max(0, vis_tool_call_count - vis_tool_error_count)
vis_iteration_count = float(vis_result.get("iteration-count") or vis_result.get("iteration_count") or 0)
# Minimal proof/evidence health metric from the Vis trace. Hard proof tasks can add an Opus judge later,
# but this catches prompt/context optimizations that strip all provenance from the JSON result.
vis_trace_health = 1 if (vis_exit == 0 and vis_provenance_refs_count > 0 and vis_answer_blocks_count > 0 and vis_running_provenance_refs_count == 0 and vis_error_blocks_count == 0) else 0

if expected:
    pi_correct = 100.0 if pi_exit == 0 and pi_answer == expected else 0.0
    vis_correct = 100.0 if vis_exit == 0 and vis_answer == expected else 0.0
else:
    # No judge yet; successful non-empty output gets a provisional pass.
    pi_correct = 100.0 if pi_exit == 0 and pi_answer else 0.0
    vis_correct = 100.0 if vis_exit == 0 and vis_answer else 0.0

speed_penalty = max(0.0, (vis_task / pi_task) - 1.0) * 10.0 if pi_task > 0 else 0.0
# Context usage is a first-class optimization target. Penalize input-token bloat separately
# because it dominates latency/cost and shows whether Vis is dragging irrelevant context.
token_penalty = (
    (max(0.0, context_total_ratio - 1.0) * 5.0 if pi_total_tokens > 0 else 0.0)
    # Keep input-token bloat first-class without letting Pi cache accounting make
    # tiny prompts explode the whole score by thousands of points.
    + (context_input_overhead_tokens / 1000.0)
)
proof_penalty = 0.0 if vis_trace_health else 25.0
quality_penalty = (100.0 - vis_correct) + max(0.0, pi_correct - vis_correct) * 2.0
vis_loss = quality_penalty + speed_penalty + token_penalty + proof_penalty
quality_floor_pass = 1 if vis_correct >= pi_correct and vis_correct >= 100.0 else 0
proof_floor_pass = vis_trace_health
strict_task_win = 1 if quality_floor_pass and proof_floor_pass and vis_task <= pi_task and vis_total_tokens <= pi_total_tokens else 0
combined_task_win = 1 if quality_floor_pass and proof_floor_pass and vis_loss <= 5.0 else 0

metrics = {
    "vis_loss": vis_loss,
    "pi_correctness": pi_correct,
    "vis_correctness": vis_correct,
    "pi_wall_seconds": pi_wall,
    "vis_wall_seconds": vis_wall,
    "pi_startup_baseline_seconds": pi_startup,
    "pi_task_seconds": pi_task,
    "vis_task_seconds": vis_task,
    "vis_internal_duration_ms": vis_duration_ms,
    "vis_startup_overhead_seconds": max(0.0, vis_wall - vis_task),
    "pi_total_tokens": pi_total_tokens,
    "pi_input_tokens": pi_input_tokens,
    "pi_output_tokens": pi_output_tokens,
    "pi_cache_read_tokens": pi_cache_read,
    "pi_cache_write_tokens": pi_cache_write,
    "vis_total_tokens": vis_total_tokens,
    "vis_input_tokens": vis_input_tokens,
    "vis_output_tokens": vis_output_tokens,
    "vis_cached_tokens": vis_cached_tokens,
    "context_total_ratio": context_total_ratio,
    "context_input_ratio": context_input_ratio,
    "context_total_overhead_tokens": context_total_overhead_tokens,
    "context_input_overhead_tokens": context_input_overhead_tokens,
    "pi_tool_event_count": pi_tool_event_count,
    "pi_tool_call_count": pi_tool_call_count,
    "pi_tool_result_count": pi_tool_result_count,
    "pi_tool_success_count": pi_tool_success_count,
    "pi_tool_error_count": pi_tool_error_count,
    "vis_tool_event_count": vis_tool_event_count,
    "vis_tool_call_count": vis_tool_call_count,
    "vis_tool_success_count": vis_tool_success_count,
    "vis_tool_error_count": vis_tool_error_count,
    "vis_tool_block_count": vis_tool_block_count,
    "vis_iteration_count": vis_iteration_count,
    "vis_provenance_refs_count": vis_provenance_refs_count,
    "vis_done_provenance_refs_count": vis_done_provenance_refs_count,
    "vis_running_provenance_refs_count": vis_running_provenance_refs_count,
    "vis_error_blocks_count": vis_error_blocks_count,
    "vis_answer_blocks_count": vis_answer_blocks_count,
    "proof_floor_pass": proof_floor_pass,
    "quality_floor_pass": quality_floor_pass,
    "strict_task_win": strict_task_win,
    "combined_task_win": combined_task_win,
    "pi_exit": pi_exit,
    "vis_exit": vis_exit,
}
(root / "eval" / "score.json").write_text(json.dumps({
    "expected_answer": expected,
    "pi_answer": pi_answer,
    "vis_answer": vis_answer,
    "metrics": metrics,
}, indent=2, sort_keys=True) + "\n")
for k, v in metrics.items():
    print(f"{k}={v}")
PY

# shellcheck disable=SC1090
source "$root/eval/metrics.env"

cat > "$root/eval/score.md" <<EOF
# Opus bench score: $TASK_ID

- Pi model: $PI_MODEL
- Vis model: $VIS_MODEL
- Expected: $expected
- Pi answer: $(tr '\n' ' ' < "$root/pi/output.txt")
- Vis answer: $(tr '\n' ' ' < "$root/vis/output.txt")
- Pi task seconds: $pi_task_seconds
- Vis task seconds: $vis_task_seconds
- Vis wall seconds: $vis_wall_seconds
- Vis internal duration ms: $vis_internal_duration_ms
- Vis startup overhead seconds: $vis_startup_overhead_seconds
EOF

printf 'ARTIFACT_ROOT=%s\n' "$root"
while IFS='=' read -r k v; do
  printf 'METRIC %s=%s\n' "$k" "$v"
done < "$root/eval/metrics.env"
