#!/usr/bin/env bash
set -euo pipefail

# Primary Opus bench entrypoint for pi-autoresearch.
# Override TASK_ID to run another JSONL task, e.g. TASK_ID=C2-vis-duration-metric ./autoresearch.sh
exec bench/opus/run-task.sh
