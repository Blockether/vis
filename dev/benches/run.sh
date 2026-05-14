#!/usr/bin/env bash
set -euo pipefail

here="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
bench="${1:-}"
if [[ -z "$bench" ]]; then
  echo "usage: $0 <swe-bench|4clojure> [args...]" >&2
  exit 2
fi
shift

case "$bench" in
  swe-bench) exec "$here/swe-bench/run_subset.sh" "$@" ;;
  4clojure)  exec "$here/4clojure/run_subset.sh" "$@" ;;
  *) echo "unknown bench: $bench (expected swe-bench or 4clojure)" >&2; exit 2 ;;
esac
