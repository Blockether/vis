#!/usr/bin/env bash
# probes/run.sh -- run the D14 workspace + slash sweep against a real
# provider. See probes/workspace-slash-sweep.md for the scenario list
# and pass/fail criteria.
#
# Usage:
#   ./probes/run.sh <provider-id>            # run scenarios 03 / 04 / 06 / 07
#                                              (the ones that need an LLM)
#   ./probes/run.sh <provider-id> 03 07      # subset
#
# Scenarios 01 / 02 / 05 / 08 / 09 / 10 are out-of-LLM checks (TUI
# palette, Telegram menu, hook fixture, JGit verification). Run those
# manually per the sweep doc.
#
# Outputs land in target/probe/logs/<scenario>.log (full --full-trace-stream).

set -euo pipefail

PROVIDER="${1:-}"
if [[ -z "$PROVIDER" ]]; then
  echo "usage: $0 <provider-id> [scenario-ids...]" >&2
  exit 2
fi
shift || true

declare -A SCENARIOS=(
  ["03"]="/workspace list"
  ["04"]="Trigger a slash whose run-fn declares :slash/specs."
  ["06"]="Call (v/probe-emit) -- the symbol emits a :task envelope."
  ["07"]="What does v/snapshot say? Keep the answer to one paragraph."
)

mkdir -p target/probe/logs

scenario_ids=("$@")
if [[ ${#scenario_ids[@]} -eq 0 ]]; then
  scenario_ids=(03 04 06 07)
fi

for sid in "${scenario_ids[@]}"; do
  prompt="${SCENARIOS[$sid]:-}"
  if [[ -z "$prompt" ]]; then
    echo "[probe $sid] unknown scenario id; skipping" >&2
    continue
  fi
  log="target/probe/logs/${sid}.log"
  echo "[probe $sid] running against $PROVIDER -> $log"
  bin/vis --provider "$PROVIDER" \
    --persist \
    --full-trace-stream \
    "$prompt" \
    >"$log" 2>&1 || echo "[probe $sid] non-zero exit (inspect log)"
done

echo "done; results: target/probe/logs/"
