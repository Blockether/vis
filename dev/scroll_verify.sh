#!/usr/bin/env bash
# Canonical flicker gate for the TUI scroll fast path (render-scroll-frame!).
#
# The reliable test is SAME-STATE, in-process: VIS_SCROLL_VERIFY makes the
# render loop, right after painting a scroll frame, re-render the EXACT same
# db through the FULL painter and compare the transcript-band back buffer
# cell-for-cell (verify-scroll-frame! in screen.clj). Because both renders are
# of the same state at the same instant, it is immune to the async
# height-warming timing that makes a cross-run pane A/B flaky.
#
# It warms the session first (so height estimates are resolved), then scrolls
# up and down and reports SCROLL-VERIFY OK vs MISMATCH counts. Any MISMATCH is
# a real flicker/position bug and is printed with its cell + chars.
#
# Usage: dev/scroll_verify.sh
set -uo pipefail
cd "$(dirname "$0")/.."
: > ~/.vis/vis.log
tmux kill-session -t sv 2>/dev/null; sleep 1
tmux new-session -d -s sv -x 200 -y 50 -c "$PWD"
tmux send-keys -t sv "cd '$PWD' && export VIS_NO_DEV_CHECKOUT=1 VIS_SCROLL_VERIFY=1 && bin/vis channels tui --jvm 2>/tmp/sv.err" C-m
booted=0
for i in $(seq 1 50); do
  tmux capture-pane -t sv -p 2>/dev/null | grep -q "help (C-x h)" && { booted=1; break; }
  sleep 1
done
[ "$booted" = 0 ] && { echo "NO BOOT"; tail -20 /tmp/sv.err; tmux kill-session -t sv 2>/dev/null; exit 2; }
sleep 1
WUP=$'\033[<64;100;25M'; WDN=$'\033[<65;100;25M'
# Phase 1: warm the whole transcript (resolve estimate->real heights).
for b in $(seq 1 30); do for i in $(seq 1 6); do tmux send-keys -t sv -l "$WUP"; done; sleep 0.3; done
for b in $(seq 1 30); do for i in $(seq 1 6); do tmux send-keys -t sv -l "$WDN"; done; sleep 0.3; done
sleep 2
# Phase 2: the graded verify pass — up then down, pausing so the verify runs.
for b in $(seq 1 15); do for i in $(seq 1 5); do tmux send-keys -t sv -l "$WUP"; done; sleep 0.4; done
for b in $(seq 1 12); do for i in $(seq 1 5); do tmux send-keys -t sv -l "$WDN"; done; sleep 0.4; done
sleep 1
tmux kill-session -t sv 2>/dev/null

ok=$(grep -c "SCROLL-VERIFY OK" ~/.vis/vis.log)
miss=$(grep -c "SCROLL-VERIFY MISMATCH" ~/.vis/vis.log)
echo "SCROLL-VERIFY: $ok OK, $miss MISMATCH"
if [ "$miss" -gt 0 ]; then
  echo "MISMATCH details (fast path diverged from full repaint):"
  grep "SCROLL-VERIFY MISMATCH" ~/.vis/vis.log | head -12
  exit 1
fi
[ "$ok" -eq 0 ] && { echo "WARN: no scroll frames verified — did the fast path fire?"; exit 3; }
echo "PASS — scroll fast path is byte-identical to a full repaint across $ok frames."
