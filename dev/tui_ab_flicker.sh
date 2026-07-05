#!/usr/bin/env bash
# Live A/B flicker gate for the TUI render fast paths.
#
# FLICKER at rest = the fast render path leaving the screen in a different
# state than a full repaint would. We measure it end-to-end through a real
# terminal grid (tmux):
#
#   run A: normal          — fast paths active (scroll/partial-live/header)
#   run B: VIS_FORCE_FULL_FRAME=1 — every frame is a full repaint (oracle)
#
# Both runs load the SAME restored session and get the SAME deterministic
# scroll sequence, capturing the pane after each ease settles. If A and B
# are byte-identical at every resting position, the fast path is visually
# equivalent to a full repaint — no flicker, no position mismatch.
#
# Usage: dev/tui_ab_flicker.sh [positions]
set -uo pipefail
cd "$(dirname "$0")/.."
OUT=/tmp/tui-ab; rm -rf "$OUT"; mkdir -p "$OUT"
POS=${1:-8}
WUP=$'\033[<64;100;25M'
WDN=$'\033[<65;100;25M'

run() {                 # run <label> <extra-env>
  local label="$1" env="$2" sess="ab-$1"
  tmux kill-session -t "$sess" 2>/dev/null; sleep 1
  tmux new-session -d -s "$sess" -x 200 -y 50 -c "$PWD"
  tmux send-keys -t "$sess" "cd '$PWD' && export VIS_NO_DEV_CHECKOUT=1 $env && bin/vis channels tui --jvm 2>/tmp/ab-$label.err" C-m
  # Wait for boot.
  for i in $(seq 1 40); do
    tmux capture-pane -t "$sess" -p 2>/dev/null | grep -q "help (C-x h)" && break
    sleep 1
  done
  sleep 2
  # Scroll to the TOP first (deterministic start), let it settle.
  for i in $(seq 1 40); do tmux send-keys -t "$sess" -l "$WUP"; done
  sleep 1
  tmux capture-pane -t "$sess" -p 2>/dev/null > "$OUT/$label.p0.txt"
  # Then step DOWN a page at a time, capturing after each ease settles.
  for p in $(seq 1 "$POS"); do
    for i in $(seq 1 5); do tmux send-keys -t "$sess" -l "$WDN"; done
    sleep 1     # let the smooth-scroll ease fully settle before capture
    tmux capture-pane -t "$sess" -p 2>/dev/null > "$OUT/$label.p$p.txt"
  done
  tmux kill-session -t "$sess" 2>/dev/null
}

echo "== run A: fast paths active =="
run A ""
echo "== run B: forced full frames (oracle) =="
run B "VIS_FORCE_FULL_FRAME=1"

# Compare only the MESSAGE BODY (rows 4..N-3): the scroll fast path owns the
# transcript viewport + scrollbar. The header band (rows 1-3) carries
# background-tab spinners and the footer (last 2 rows) a live clock — both are
# nondeterministic across two separate launches and are NOT touched by the
# scroll path, so stripping them isolates what we're actually testing.
body() { sed -e '1,3d' -e '$d' -e '$d' "$1"; }
echo "== diff A vs B (message body) at each resting scroll position =="
fail=0
for p in $(seq 0 "$POS"); do
  if [ -f "$OUT/A.p$p.txt" ] && [ -f "$OUT/B.p$p.txt" ]; then
    if diff -q <(body "$OUT/A.p$p.txt") <(body "$OUT/B.p$p.txt") >/dev/null; then
      echo "  p$p: IDENTICAL"
    else
      echo "  p$p: DIFFERS  (fast path != full repaint)"
      diff <(body "$OUT/A.p$p.txt") <(body "$OUT/B.p$p.txt") | head -24
      fail=1
    fi
  else
    echo "  p$p: MISSING capture"; fail=1
  fi
done
[ "$fail" = 0 ] && echo "AB-FLICKER: PASS — fast path is pixel-identical to full repaint at every position." \
                || echo "AB-FLICKER: FAIL — see diffs above."
exit $fail
