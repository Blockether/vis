#!/usr/bin/env bash
set -euo pipefail
python3 -m unittest discover -s dev/benches/swe-bench/tests
python3 -m unittest discover -s dev/benches/4clojure/tests
