#!/usr/bin/env bash
set -euo pipefail

# Primary autoresearch entrypoint: opencode vs Vis on Dockerized benchmark tasks.
TASK_ID="${TASK_ID:-TB-hello-world}"
export TASK_ID

exec bench/opencode/run-paired-task.sh
