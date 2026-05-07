#!/usr/bin/env bash
set -euo pipefail

TASK_ID="${TASK_ID:-VTUI-open-missing-conversation}"
TASKS_FILE="${CLI_TASKS_FILE:-bench/opus/cli-tasks.jsonl}"
RUN_ROOT_BASE="${RUN_ROOT_BASE:-target/vis-bench}"

need() { command -v "$1" >/dev/null 2>&1 || { echo "missing command: $1" >&2; exit 2; }; }
need jq
need python3

task_json="$(jq -c --arg id "$TASK_ID" 'select(.id == $id)' "$TASKS_FILE" | head -1)"
if [[ -z "$task_json" ]]; then
  echo "No CLI task id '$TASK_ID' in $TASKS_FILE" >&2
  exit 2
fi

run_id="$(date +%Y%m%d-%H%M%S)-${TASK_ID}"
root="$RUN_ROOT_BASE/$run_id"
mkdir -p "$root/cli" "$root/eval"
printf '%s\n' "$task_json" > "$root/task.json"

expected_exit="$(jq -r '.expected_exit // 0' <<<"$task_json")"
expected_contains="$(jq -r '.expected_contains // empty' <<<"$task_json")"
timeout_seconds="$(jq -r '.timeout_seconds // 60' <<<"$task_json")"
db_reset="$(jq -r '.db_reset // true' <<<"$task_json")"
mapfile -t cmd < <(jq -r '.cmd[]' <<<"$task_json")
printf '%q ' "${cmd[@]}" > "$root/cli/command.txt"
printf '\n' >> "$root/cli/command.txt"

if [[ "$db_reset" == "true" ]]; then
  rm -rf "$root/cli/db"
fi

set +e
VIS_MEASURE=1 VIS_CRAC=0 VIS_DB_PATH="$root/cli/db" \
  /usr/bin/time -p -o "$root/cli/time.txt" \
  timeout "$timeout_seconds" "${cmd[@]}" \
  > "$root/cli/stdout.txt" 2> "$root/cli/stderr.txt"
exit_code=$?
set -e
printf '%s\n' "$exit_code" > "$root/cli/exit_code"

python3 - "$root" "$expected_exit" "$expected_contains" <<'PY' > "$root/eval/metrics.env"
import json, pathlib, re, sys
root = pathlib.Path(sys.argv[1])
expected_exit = int(sys.argv[2])
expected_contains = sys.argv[3]

def read(path):
    p = root / path
    return p.read_text(errors="replace") if p.exists() else ""

def real_seconds():
    m = re.search(r"^real\s+([0-9.]+)", read("cli/time.txt"), re.M)
    return float(m.group(1)) if m else 0.0

stdout = read("cli/stdout.txt")
stderr = read("cli/stderr.txt")
combined = stdout + "\n" + stderr
exit_code = int((read("cli/exit_code").strip() or "999"))
seconds = real_seconds()
contains_ok = (expected_contains in combined) if expected_contains else True
exit_ok = (exit_code == expected_exit)
correctness = 100.0 if exit_ok and contains_ok else 0.0
measure_lines = [line for line in combined.splitlines() if line.startswith("[vis measure]")]
manifest_require_ms = 0.0
for line in measure_lines:
    if "jvm:manifest require" in line:
        m = re.search(r" ([0-9.]+) ms", line)
        if m:
            manifest_require_ms += float(m.group(1))
full_discovery_ms = 0.0
for line in measure_lines:
    if "jvm:discover-all+extensions" in line:
        m = re.search(r" ([0-9.]+) ms", line)
        if m:
            full_discovery_ms = float(m.group(1))
vis_loss = seconds + (0.0 if correctness == 100.0 else 100.0)
metrics = {
    "vis_loss": vis_loss,
    "cli_wall_seconds": seconds,
    "cli_correctness": correctness,
    "cli_exit_code": exit_code,
    "cli_expected_exit": expected_exit,
    "cli_expected_contains_ok": 1 if contains_ok else 0,
    "cli_measure_lines_count": len(measure_lines),
    "cli_manifest_require_ms_sum": manifest_require_ms,
    "cli_discover_all_extensions_ms": full_discovery_ms,
}
(root / "eval" / "score.json").write_text(json.dumps({"stdout_head": stdout[:2000], "stderr_head": stderr[:4000], "metrics": metrics}, indent=2, sort_keys=True) + "\n")
for k, v in metrics.items():
    print(f"{k}={v}")
PY

# shellcheck disable=SC1090
source "$root/eval/metrics.env"
cat > "$root/eval/score.md" <<EOF
# Vis CLI speed score: $TASK_ID

- Command: $(cat "$root/cli/command.txt")
- Exit: $cli_exit_code (expected $cli_expected_exit)
- Correctness: $cli_correctness
- Wall/task seconds: $cli_wall_seconds
- Measure lines: $cli_measure_lines_count
- Manifest require ms sum: $cli_manifest_require_ms_sum
- Discover all extensions ms: $cli_discover_all_extensions_ms
EOF

printf 'ARTIFACT_ROOT=%s\n' "$root"
while IFS='=' read -r k v; do
  printf 'METRIC %s=%s\n' "$k" "$v"
done < "$root/eval/metrics.env"
