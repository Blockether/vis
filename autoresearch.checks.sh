#!/usr/bin/env bash
set -euo pipefail

# Correctness backpressure for kept Vis changes.
# Keep quiet on success; errors surface through run_experiment checks output.
./verify.sh --quick >/tmp/vis-autoresearch-verify.log 2>&1 || {
  tail -80 /tmp/vis-autoresearch-verify.log
  exit 1
}
