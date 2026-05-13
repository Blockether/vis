#!/usr/bin/env bash
# Backpressure for autoresearch: Vis itself must stay correct.
# Output is truncated to last 80 lines by the runner — keep it minimal.
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"

# Unit + integration tests
clojure -M:test 2>&1 | tail -60

# Linter — only surface errors/warnings
clojure -M:lint 2>&1 | grep -iE 'error|warn' || true
