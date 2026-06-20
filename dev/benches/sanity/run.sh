#!/usr/bin/env bash
# Vis sanity harness — thin wrapper over run.py.
#   dev/benches/sanity/run.sh <provider/model> [run.py args...]
# e.g.
#   dev/benches/sanity/run.sh zai-anthropic/glm-5-turbo
#   dev/benches/sanity/run.sh openai-codex/gpt-5.5 --task python-compute --repeat 3
set -euo pipefail
here="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
exec python3 "$here/run.py" "$@"
