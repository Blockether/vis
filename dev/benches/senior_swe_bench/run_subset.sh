#!/usr/bin/env bash
set -euo pipefail
here="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
subset_name="${1:-public-5}"
if [[ -f "$subset_name" ]]; then
  subset="$(cd -P "$(dirname "$subset_name")" && pwd)/$(basename "$subset_name")"
  subset_slug="$(python3 - <<'PY' "$subset"
import json
import re
import sys
from pathlib import Path

data = json.loads(Path(sys.argv[1]).read_text())
name = str(data.get("name") or Path(sys.argv[1]).stem)
print(re.sub(r"[^A-Za-z0-9_.-]+", "-", name).strip("-") or "subset")
PY
)"
else
  subset="$here/subsets/${subset_name%.json}.json"
  subset_slug="${subset_name%.json}"
fi
if [[ ! -f "$subset" ]]; then
  echo "missing subset: $subset" >&2
  exit 2
fi
subset_run_id="${SUBSET_RUN_ID:-$(date -u +%Y%m%d-%H%M%S)}"
aggregate_dir="$here/results/subsets"
aggregate_out="$aggregate_dir/$subset_slug-$subset_run_id.json"
manifest="$(mktemp)"
python3 - <<'PY' "$subset_slug" "$subset" "$manifest"
import json
import sys

json.dump(
    {"subset_name": sys.argv[1], "subset_path": sys.argv[2], "runs": []},
    open(sys.argv[3], "w"),
    indent=2,
)
PY
status=0
while IFS= read -r task_id; do
  tmp_subset="$(mktemp)"
  python3 - <<'PY' "$task_id" "$tmp_subset"
import json, sys
json.dump({'name':'single-task','task_ids':[sys.argv[1]]}, open(sys.argv[2], 'w'), indent=2)
PY
  run_id="$subset_slug-$subset_run_id-$task_id"
  set +e
  RUN_ID="$run_id" SUBSET_FILE="$tmp_subset" "$here/run_smoke.sh"
  task_status=$?
  set -e
  rm -f "$tmp_subset"
  python3 - <<'PY' "$manifest" "$task_id" "$here/results/$run_id" "$task_status"
import json
import sys
from pathlib import Path

path = Path(sys.argv[1])
data = json.loads(path.read_text())
data.setdefault("runs", []).append(
    {"task_id": sys.argv[2], "run_dir": sys.argv[3], "exit_status": int(sys.argv[4])}
)
path.write_text(json.dumps(data, indent=2) + "\n")
PY
  if [[ "$task_status" != "0" ]]; then
    status="$task_status"
    if [[ "${VIS_BENCH_CONTINUE_ON_FAILURE:-0}" != "1" ]]; then
      break
    fi
  fi
done < <(python3 - <<'PY' "$subset"
import json, sys
for x in json.load(open(sys.argv[1]))['task_ids']: print(x)
PY
)
python3 "$here/summarize_subset.py" \
  --subset-name "$subset_slug" \
  --subset-path "$subset" \
  --manifest "$manifest" \
  --out "$aggregate_out" >/dev/null
rm -f "$manifest"
echo "wrote subset summary $aggregate_out"
exit "$status"
