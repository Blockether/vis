#!/usr/bin/env bash
# Paired-worktree hard-task runner for autoresearch judge-required tasks.
#
# Runs Pi and Vis from the same commit in isolated git worktrees, captures
# diffs/checks/tokens/tool evidence, asks an Opus judge for strict JSON, then
# emits METRIC lines for pi-autoresearch.

set -euo pipefail

TASK_ID="${TASK_ID:?TASK_ID required (id from bench/opus/tasks.jsonl)}"
TASKS_FILE="${TASKS_FILE:-bench/opus/tasks.jsonl}"
PI_MODEL="${PI_MODEL:-anthropic/claude-opus-4-7}"
VIS_MODEL="${VIS_MODEL:-anthropic-coding-plan/claude-opus-4-7}"
JUDGE_MODEL="${JUDGE_MODEL:-anthropic/claude-opus-4-7}"
PI_STARTUP_BASELINE_SECONDS="${PI_STARTUP_BASELINE_SECONDS:-0.10}"
RUN_ROOT_BASE="${RUN_ROOT_BASE:-target/vis-bench}"
JUDGE_PROMPT="${JUDGE_PROMPT:-bench/opus/judge-prompt.md}"
CHECK_TIMEOUT_SECONDS="${CHECK_TIMEOUT_SECONDS:-900}"
JUDGE_TIMEOUT_SECONDS="${JUDGE_TIMEOUT_SECONDS:-600}"
# Keep judge prompt bounded while preserving enough evidence for hard-task scoring.
DIFF_MAX_CHARS="${DIFF_MAX_CHARS:-120000}"
LOG_MAX_CHARS="${LOG_MAX_CHARS:-40000}"
TRACE_MAX_CHARS="${TRACE_MAX_CHARS:-80000}"

need() { command -v "$1" >/dev/null 2>&1 || { echo "missing command: $1" >&2; exit 2; }; }
need jq
need git
need python3
need pi

repo_root="$(git rev-parse --show-toplevel)"
cd "$repo_root"

task_json="$(jq -c --arg id "$TASK_ID" 'select(.id == $id)' "$TASKS_FILE" | head -1)"
if [[ -z "$task_json" ]]; then
  echo "No task id '$TASK_ID' in $TASKS_FILE" >&2
  exit 2
fi

judge_required="$(jq -r '.judge_required // false' <<<"$task_json")"
prompt="$(jq -r '.prompt // ""' <<<"$task_json")"
pi_tools="$(jq -r '.pi_tools // "read,bash,edit,write,grep,find,ls"' <<<"$task_json")"
timeout_seconds="$(jq -r '.timeout_seconds // 1800' <<<"$task_json")"

run_id="$(date +%Y%m%d-%H%M%S)-${TASK_ID}"
root="$repo_root/$RUN_ROOT_BASE/$run_id"
mkdir -p "$root/pi" "$root/vis" "$root/eval"
printf '%s\n' "$task_json" > "$root/task.json"
printf '%s\n' "$prompt" > "$root/prompt.txt"
cp "$JUDGE_PROMPT" "$root/eval/judge-prompt.md"

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

write_task_file() {
  local wt="$1"
  cat > "$wt/BENCH_TASK.md" <<EOF
# Autoresearch paired task: $TASK_ID

## Prompt

$prompt

## Task JSON

\`\`\`json
$task_json
\`\`\`
EOF
}
write_task_file "$pi_wt"
write_task_file "$vis_wt"

# -- vis disposable DB --------------------------------------------------------
rm -rf "$root/vis/db"
mkdir -p "$root/vis/db"

# -- run pi -------------------------------------------------------------------
pi_cmd=(pi --mode json --model "$PI_MODEL" --no-session -p "$prompt")
if [[ "$pi_tools" == "none" ]]; then
  pi_cmd+=(--no-tools)
else
  pi_cmd+=(--tools "$pi_tools")
fi

set +e
(
  cd "$pi_wt"
  /usr/bin/time -p -o "$root/pi/time.txt" timeout "$timeout_seconds" "${pi_cmd[@]}" \
    > "$root/pi/events.jsonl" 2> "$root/pi/stderr.txt"
)
pi_exit=$?
set -e
printf '%s\n' "$pi_exit" > "$root/pi/exit_code"

jq -r 'select(.type == "message_end" and .message.role == "assistant") | .message.content[]? | select(.type == "text") | .text' \
  "$root/pi/events.jsonl" > "$root/pi/output.txt" 2>/dev/null || :
jq -s 'map(select(.type == "message_end" and .message.role == "assistant") | .message.usage) | last // {}' \
  "$root/pi/events.jsonl" > "$root/pi/metrics.tokens.json" 2>/dev/null || echo '{}' > "$root/pi/metrics.tokens.json"

# -- run vis ------------------------------------------------------------------
set +e
(
  cd "$vis_wt"
  VIS_DB_PATH="$root/vis/db" VIS_CRAC=0 \
    /usr/bin/time -p -o "$root/vis/time.txt" timeout "$timeout_seconds" \
    ./bin/vis run --json --trace --model "$VIS_MODEL" --db "$root/vis/db" "$prompt" \
    > "$root/vis/result.json" 2> "$root/vis/stderr.txt"
)
vis_exit=$?
set -e
printf '%s\n' "$vis_exit" > "$root/vis/exit_code"

jq -r '.answer // empty' "$root/vis/result.json" > "$root/vis/output.txt" 2>/dev/null || :
jq '.tokens // {}' "$root/vis/result.json" > "$root/vis/metrics.tokens.json" 2>/dev/null || echo '{}' > "$root/vis/metrics.tokens.json"

# -- task checks --------------------------------------------------------------
run_checks() {
  local side="$1"
  local wt="$2"
  local out="$root/eval/checks.$side.txt"
  local code_file="$root/eval/checks.$side.exit_code"
  : > "$out"
  local count
  count="$(jq '.checks // [] | length' <<<"$task_json")"
  if [[ "$count" == "0" ]]; then
    printf '[run-paired-task] no task checks supplied; running ./verify.sh --quick\n' >> "$out"
    set +e
    ( cd "$wt" && timeout "$CHECK_TIMEOUT_SECONDS" ./verify.sh --quick ) >> "$out" 2>&1
    local ec=$?
    set -e
    printf '%s\n' "$ec" > "$code_file"
    return 0
  fi
  local overall=0
  local i cmd ec
  for ((i=0; i<count; i++)); do
    cmd="$(jq -r --argjson i "$i" '.checks[$i]' <<<"$task_json")"
    printf '\n$ %s\n' "$cmd" >> "$out"
    set +e
    ( cd "$wt" && timeout "$CHECK_TIMEOUT_SECONDS" bash -lc "$cmd" ) >> "$out" 2>&1
    ec=$?
    set -e
    printf '[exit %s]\n' "$ec" >> "$out"
    if [[ "$ec" != "0" ]]; then overall="$ec"; fi
  done
  printf '%s\n' "$overall" > "$code_file"
}
run_checks pi "$pi_wt"
run_checks vis "$vis_wt"

# -- diffs --------------------------------------------------------------------
( cd "$pi_wt"  && git diff --binary "$base_sha" -- . > "$root/eval/diff.pi.patch"  ) || true
( cd "$vis_wt" && git diff --binary "$base_sha" -- . > "$root/eval/diff.vis.patch" ) || true
( cd "$pi_wt"  && git status --short > "$root/pi/status.txt"  ) || true
( cd "$vis_wt" && git status --short > "$root/vis/status.txt" ) || true

# -- judge --------------------------------------------------------------------
if [[ "$judge_required" == "true" ]]; then
  python3 - "$root" "$DIFF_MAX_CHARS" "$LOG_MAX_CHARS" "$TRACE_MAX_CHARS" <<'PY' > "$root/eval/judge-input.md"
import json, pathlib, sys
root = pathlib.Path(sys.argv[1])
diff_max = int(sys.argv[2]); log_max = int(sys.argv[3]); trace_max = int(sys.argv[4])

def read(rel, limit=None):
    p = root / rel
    text = p.read_text(errors="replace") if p.exists() else ""
    if limit is not None and len(text) > limit:
        half = max(1, limit // 2)
        return text[:half] + f"\n\n[... truncated {len(text) - limit} chars ...]\n\n" + text[-half:]
    return text

def json_block(obj):
    return json.dumps(obj, indent=2, sort_keys=True)

try:
    task = json.loads(read("task.json"))
except Exception:
    task = {}
try:
    vis_result = json.loads(read("vis/result.json"))
except Exception:
    vis_result = {"raw_result_prefix": read("vis/result.json", trace_max)}
vis_trace = vis_result.get("trace")
vis_trace_json = json_block(vis_trace) if vis_trace is not None else read("vis/result.json", trace_max)
if len(vis_trace_json) > trace_max:
    vis_trace_json = vis_trace_json[:trace_max//2] + f"\n\n[... truncated {len(vis_trace_json) - trace_max} chars ...]\n\n" + vis_trace_json[-trace_max//2:]

print(read("eval/judge-prompt.md"))
print("\n# Benchmark artifacts to judge\n")
print("Return only strict JSON matching the required schema. Use booleans for *_pass/*_win fields.")
print("\n## Task JSON\n```json\n" + json_block(task) + "\n```")
print("\n## Original prompt\n```text\n" + read("prompt.txt") + "\n```")
print("\n## Pi final answer\n```text\n" + read("pi/output.txt", log_max) + "\n```")
print("\n## Vis final answer\n```text\n" + read("vis/output.txt", log_max) + "\n```")
print("\n## Pi status\n```text\n" + read("pi/status.txt", log_max) + "\n```")
print("\n## Vis status\n```text\n" + read("vis/status.txt", log_max) + "\n```")
print("\n## Pi git diff\n```diff\n" + read("eval/diff.pi.patch", diff_max) + "\n```")
print("\n## Vis git diff\n```diff\n" + read("eval/diff.vis.patch", diff_max) + "\n```")
print("\n## Pi checks\n```text\n" + read("eval/checks.pi.txt", log_max) + "\n```")
print("\n## Vis checks\n```text\n" + read("eval/checks.vis.txt", log_max) + "\n```")
print("\n## Pi stderr\n```text\n" + read("pi/stderr.txt", log_max) + "\n```")
print("\n## Vis stderr\n```text\n" + read("vis/stderr.txt", log_max) + "\n```")
print("\n## Timing files\n```text\n[pi]\n" + read("pi/time.txt") + "\n[vis]\n" + read("vis/time.txt") + "\n```")
print("\n## Token files\n```json\n{\n  \"pi\": " + (read("pi/metrics.tokens.json") or "{}") + ",\n  \"vis\": " + (read("vis/metrics.tokens.json") or "{}") + "\n}\n```")
print("\n## Vis trace/result evidence\n```json\n" + vis_trace_json + "\n```")
PY

  set +e
  /usr/bin/time -p -o "$root/eval/judge.time.txt" timeout "$JUDGE_TIMEOUT_SECONDS" \
    pi --mode json --model "$JUDGE_MODEL" --no-session --no-tools -p "@$root/eval/judge-input.md" \
    > "$root/eval/judge-events.jsonl" 2> "$root/eval/judge.stderr.txt"
  judge_exit=$?
  set -e
  printf '%s\n' "$judge_exit" > "$root/eval/judge.exit_code"

  python3 - "$root" <<'PY'
import json, pathlib, re, sys
root = pathlib.Path(sys.argv[1])
text_parts = []
for line in (root / "eval" / "judge-events.jsonl").read_text(errors="replace").splitlines():
    try:
        event = json.loads(line)
    except Exception:
        continue
    msg = event.get("message") or {}
    if event.get("type") == "message_end" and msg.get("role") == "assistant":
        for item in msg.get("content") or []:
            if item.get("type") == "text":
                text_parts.append(item.get("text") or "")
raw = "\n".join(text_parts).strip()
(root / "eval" / "judge.raw.txt").write_text(raw + "\n")

def extract_json(s):
    s = s.strip()
    if s.startswith("```"):
        s = re.sub(r"^```(?:json)?\s*", "", s)
        s = re.sub(r"\s*```$", "", s)
    try:
        return json.loads(s)
    except Exception:
        pass
    m = re.search(r"\{.*\}", s, re.S)
    if m:
        return json.loads(m.group(0))
    raise ValueError("judge output did not contain JSON")
try:
    obj = extract_json(raw)
except Exception as e:
    obj = {
        "pi_correctness": 0,
        "vis_correctness": 0,
        "pi_context_score": 0,
        "vis_context_score": 0,
        "pi_speed_score": 0,
        "vis_speed_score": 0,
        "pi_code_quality": 0,
        "vis_code_quality": 0,
        "pi_structural_edit_score": 0,
        "vis_structural_edit_score": 0,
        "pi_proof_honesty_score": 0,
        "vis_proof_honesty_score": 0,
        "quality_floor_pass": False,
        "proof_floor_pass": False,
        "strict_task_win": False,
        "combined_task_win": False,
        "fatal_violations": ["judge_json_parse_failed"],
        "pi_notes": [],
        "vis_notes": [],
        "reason": str(e),
    }
(root / "eval" / "judge.json").write_text(json.dumps(obj, indent=2, sort_keys=True) + "\n")
PY
else
  cat > "$root/eval/judge.json" <<'JSON'
{
  "pi_correctness": 0,
  "vis_correctness": 0,
  "quality_floor_pass": false,
  "proof_floor_pass": false,
  "strict_task_win": false,
  "combined_task_win": false,
  "reason": "judge not required"
}
JSON
fi

# -- metrics ------------------------------------------------------------------
python3 - "$root" "$PI_STARTUP_BASELINE_SECONDS" "$TASK_ID" <<'PY' > "$root/eval/metrics.env"
import json, pathlib, re, sys
root = pathlib.Path(sys.argv[1])
pi_baseline = float(sys.argv[2])
task_id = sys.argv[3]

def read(path):
    p = root / path
    return p.read_text(errors="replace") if p.exists() else ""

def load_json(path, default):
    try:
        return json.loads(read(path))
    except Exception:
        return default

def real_seconds(p):
    m = re.search(r"^real\s+([0-9.]+)", read(p), re.M)
    return float(m.group(1)) if m else 0.0

def num(obj, key, default=0.0):
    try:
        v = obj.get(key, default)
        if isinstance(v, bool):
            return 1.0 if v else 0.0
        return float(v if v is not None else default)
    except Exception:
        return float(default)

def flag(obj, key):
    v = obj.get(key, False)
    if isinstance(v, str):
        return 1 if v.lower() in {"1", "true", "yes", "pass", "passed"} else 0
    return 1 if bool(v) else 0

pi_exit = int((read("pi/exit_code").strip() or "1"))
vis_exit = int((read("vis/exit_code").strip() or "1"))
pi_checks_exit = int((read("eval/checks.pi.exit_code").strip() or "1"))
vis_checks_exit = int((read("eval/checks.vis.exit_code").strip() or "1"))
pi_wall  = real_seconds("pi/time.txt")
vis_wall = real_seconds("vis/time.txt")
pi_task  = max(0.0, pi_wall - pi_baseline)
vis_result = load_json("vis/result.json", {})
vis_task = float(vis_result.get("duration-ms") or vis_result.get("duration_ms") or 0) / 1000.0
vis_startup_overhead = max(0.0, vis_wall - vis_task)
judge = load_json("eval/judge.json", {})

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
context_total_overhead_tokens = vis_total_tokens - pi_total_tokens
context_input_overhead_tokens = vis_input_tokens - pi_input_tokens

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
vis_iteration_count = int(vis_result.get("iteration-count") or vis_result.get("iteration_count") or 0)
vis_trace_health = 1 if (vis_exit == 0 and vis_provenance_refs_count > 0 and vis_answer_blocks_count > 0 and vis_running_provenance_refs_count == 0 and vis_error_blocks_count == 0) else 0

pi_correctness = num(judge, "pi_correctness")
vis_correctness = num(judge, "vis_correctness")
# If there is no valid judge signal, fall back to observed execution/check failure.
if pi_correctness == 0 and pi_exit == 0 and pi_checks_exit == 0 and read("eval/diff.pi.patch"):
    pi_correctness = 1.0
if vis_correctness == 0 and vis_exit == 0 and vis_checks_exit == 0 and read("eval/diff.vis.patch"):
    vis_correctness = 1.0
quality_floor_pass = flag(judge, "quality_floor_pass")
proof_floor_pass = flag(judge, "proof_floor_pass")
strict_task_win = flag(judge, "strict_task_win")
combined_task_win = flag(judge, "combined_task_win")
if not proof_floor_pass and vis_trace_health and num(judge, "vis_proof_honesty_score") >= 80:
    proof_floor_pass = 1

speed_penalty = max(0.0, (vis_task / pi_task) - 1.0) * 10.0 if pi_task > 0 and vis_task > 0 else 0.0
token_penalty = 0.0
if pi_total_tokens > 0 and vis_total_tokens > 0:
    token_penalty += max(0.0, context_total_ratio - 1.0) * 5.0
if pi_input_tokens > 0 and vis_input_tokens > 0:
    token_penalty += max(0.0, context_input_overhead_tokens) / 1000.0
proof_penalty = 0.0 if proof_floor_pass else 25.0
quality_penalty = (100.0 - vis_correctness) + max(0.0, pi_correctness - vis_correctness) * 2.0
check_penalty = (15.0 if vis_exit != 0 else 0.0) + (10.0 if vis_checks_exit != 0 else 0.0)
vis_loss = max(0.0, quality_penalty + speed_penalty + token_penalty + proof_penalty + check_penalty)

metrics = {
    "vis_loss": vis_loss,
    "pi_correctness": pi_correctness,
    "vis_correctness": vis_correctness,
    "pi_wall_seconds": pi_wall,
    "vis_wall_seconds": vis_wall,
    "pi_startup_baseline_seconds": pi_baseline,
    "pi_task_seconds": pi_task,
    "vis_task_seconds": vis_task,
    "vis_internal_duration_ms": vis_task * 1000.0,
    "vis_startup_overhead_seconds": vis_startup_overhead,
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
    "proof_floor_pass": int(proof_floor_pass),
    "quality_floor_pass": int(quality_floor_pass),
    "strict_task_win": int(strict_task_win),
    "combined_task_win": int(combined_task_win),
    "pi_exit": pi_exit,
    "vis_exit": vis_exit,
    "pi_checks_exit": pi_checks_exit,
    "vis_checks_exit": vis_checks_exit,
    "judge_exit": int((read("eval/judge.exit_code").strip() or "0")),
    "pi_context_score": num(judge, "pi_context_score"),
    "vis_context_score": num(judge, "vis_context_score"),
    "pi_speed_score": num(judge, "pi_speed_score"),
    "vis_speed_score": num(judge, "vis_speed_score"),
    "pi_code_quality": num(judge, "pi_code_quality"),
    "vis_code_quality": num(judge, "vis_code_quality"),
    "pi_proof_honesty_score": num(judge, "pi_proof_honesty_score"),
    "vis_proof_honesty_score": num(judge, "vis_proof_honesty_score"),
}
(root / "eval" / "score.json").write_text(json.dumps({
    "task_id": task_id,
    "base_sha": read("base_sha").strip(),
    "pi_exit": pi_exit,
    "vis_exit": vis_exit,
    "pi_checks_exit": pi_checks_exit,
    "vis_checks_exit": vis_checks_exit,
    "judge": judge,
    "metrics": metrics,
}, indent=2, sort_keys=True) + "\n")
for k, v in metrics.items():
    print(f"{k}={v}")
PY

# shellcheck disable=SC1090
source "$root/eval/metrics.env"

cat > "$root/eval/score.md" <<EOF
# Paired-worktree score: $TASK_ID

- Base: $base_sha
- Pi model: $PI_MODEL
- Vis model: $VIS_MODEL
- Judge model: $JUDGE_MODEL
- Pi exit/checks: $pi_exit / $pi_checks_exit
- Vis exit/checks: $vis_exit / $vis_checks_exit
- Pi correctness: $pi_correctness
- Vis correctness: $vis_correctness
- Proof floor pass: $proof_floor_pass
- Quality floor pass: $quality_floor_pass
- Strict task win: $strict_task_win
- Combined task win: $combined_task_win
- Pi task seconds: $pi_task_seconds
- Vis task seconds: $vis_task_seconds
- Pi tokens: $pi_total_tokens
- Vis tokens: $vis_total_tokens
- Artifacts: $root
EOF

printf 'ARTIFACT_ROOT=%s\n' "$root"
while IFS='=' read -r k v; do
  printf 'METRIC %s=%s\n' "$k" "$v"
done < "$root/eval/metrics.env"
