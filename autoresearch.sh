#!/usr/bin/env bash
# Autoresearch driver. Picks 3 4Clojure problems, runs Vis on each
# in a fresh subprocess, judges with eval_one.clj, emits METRIC lines.
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"
exec python3 dev/benches/4clojure/autoresearch_runner.py "$@"
