#!/usr/bin/env bash
set -euo pipefail

# Primary Opus bench entrypoint for pi-autoresearch.
#
# Routes by TASK_ID (default S0-hi):
#   - bench/opus/tasks.jsonl
#       - judge_required tasks  -> bench/opus/run-paired-task.sh
#         (CTX1, CLJEXT1, PYEXT1, GAME1, ...)
#       - everything else        -> bench/opus/run-task.sh
#         (S0-hi, C*, exact-answer / inspect tasks)
#   - bench/opus/cli-tasks.jsonl -> bench/opus/run-cli-task.sh
#         CLI / TUI startup tasks. NOT an active optimization target
#         anymore (see autoresearch.md "Out of scope"); kept routable
#         for reproducibility of past wins.
TASK_ID="${TASK_ID:-S0-hi}"

if jq -e --arg id "$TASK_ID" 'select(.id == $id)' bench/opus/cli-tasks.jsonl >/dev/null; then
  exec bench/opus/run-cli-task.sh
fi

if jq -e --arg id "$TASK_ID" 'select(.id == $id) | .judge_required == true' \
     bench/opus/tasks.jsonl >/dev/null; then
  exec bench/opus/run-paired-task.sh
fi

exec bench/opus/run-task.sh
