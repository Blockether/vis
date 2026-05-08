#!/usr/bin/env bash
# Dispatch paired opencode-vs-Vis benchmark tasks.
set -euo pipefail

TASK_ID="${TASK_ID:?TASK_ID required}"
TASKS_FILE="${TASKS_FILE:-bench/opencode/tasks.jsonl}"

task_json="$(jq -c --arg id "$TASK_ID" 'select(.id == $id)' "$TASKS_FILE" | head -1)"
if [[ -z "$task_json" ]]; then
  echo "No task id '$TASK_ID' in $TASKS_FILE" >&2
  exit 2
fi

benchmark="$(jq -r '.benchmark // ""' <<<"$task_json")"
case "$benchmark" in
  terminal-bench)
    exec bench/opencode/run-terminal-bench-task.sh
    ;;
  *)
    echo "Unsupported benchmark '$benchmark' for task '$TASK_ID'" >&2
    exit 2
    ;;
esac
