#!/usr/bin/env bash
set -euo pipefail

# Primary Opus bench entrypoint for pi-autoresearch.
# Agent comparison tasks live in bench/opus/tasks.jsonl.
# Vis CLI speed tasks live in bench/opus/cli-tasks.jsonl.
TASK_ID="${TASK_ID:-S0-hi}"
if jq -e --arg id "$TASK_ID" 'select(.id == $id)' bench/opus/cli-tasks.jsonl >/dev/null; then
  exec bench/opus/run-cli-task.sh
else
  exec bench/opus/run-task.sh
fi
