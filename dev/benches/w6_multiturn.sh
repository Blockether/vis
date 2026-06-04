#!/usr/bin/env bash
# W6 — multi-turn measurement scenario.
#
# Drives `bin/vis` through a 3-turn, SAME-FILE edit session (the shape the
# b117af1a teardown showed wasting iterations on re-location) so the engine
# signals can be scored with `benches.ctx-metrics` against the frozen baseline
# (task-set!=0, fact-set!=0, forms-per-iter={1 N}, locate-waste=13).
#
# Turn 1 creates a persisted session; turns 2-3 continue it with --session-id.
# Prints the session UUID on the last line for scoring.
#
#   VIS_PROVIDER / VIS_MODEL override the model (default zai-coding-plan/glm-5.1).
set -euo pipefail

VIS_BIN="${VIS_BIN:-$(cd "$(dirname "${BASH_SOURCE[0]}")/../.." && pwd)/bin/vis}"
PROVIDER="${VIS_PROVIDER:-zai-coding-plan}"
MODEL="${VIS_MODEL:-glm-5.1}"
TURN_TIMEOUT="${W6_TURN_TIMEOUT:-300}"

work="$(mktemp -d)"
echo "[w6] workdir=$work model=$PROVIDER/$MODEL" >&2

run_turn() {  # $1 = prompt ; optional $2 = session-id
  local prompt="$1" sid="${2:-}"
  ( cd "$work"
    if [[ -n "$sid" ]]; then
      timeout "$TURN_TIMEOUT" "$VIS_BIN" --json --session-id "$sid" \
        --provider "$PROVIDER" --model "$MODEL" "$prompt"
    else
      timeout "$TURN_TIMEOUT" "$VIS_BIN" --json --persist \
        --provider "$PROVIDER" --model "$MODEL" "$prompt"
    fi )
}

sid_of() { python3 -c 'import json,sys; print(json.load(sys.stdin).get("session-id") or "")'; }

echo "[w6] turn 1/3 — create + add" >&2
out1="$(run_turn 'Create a new file calc.clj with namespace calc and a function (defn add [a b]) that returns a + b. Load it in the REPL to verify it works.')"
SID="$(printf '%s' "$out1" | sid_of)"
echo "[w6] session=$SID" >&2
[[ -z "$SID" ]] && { echo "[w6] FAILED: no session id from turn 1" >&2; echo "$out1" >&2; exit 1; }

echo "[w6] turn 2/3 — add subtract (same file)" >&2
run_turn 'Add a function (defn subtract [a b]) to calc.clj that returns a - b. Verify it loads.' "$SID" >/dev/null

echo "[w6] turn 3/3 — rename add->sum (same file)" >&2
run_turn 'Rename the add function to sum in calc.clj (keep the same behavior). Verify it loads.' "$SID" >/dev/null

echo "[w6] DONE session=$SID" >&2
echo "$SID"
