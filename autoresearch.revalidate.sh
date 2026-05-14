#!/usr/bin/env bash
# Run the bench at two offsets different from the current row count, to
# guard against overfitting to the rotation slice the candidate change
# was scored on. Outputs aggregated METRIC lines that mix the slices.
#
# Usage: ./autoresearch.revalidate.sh [offset_a offset_b]
set -euo pipefail
cd "$(dirname "${BASH_SOURCE[0]}")"

current_rows=$(wc -l < autoresearch.jsonl 2>/dev/null | awk '{print $1}')
current_rows=${current_rows:-0}
off_a="${1:-$((current_rows + 7))}"
off_b="${2:-$((current_rows + 13))}"

stamp=$(date +%Y%m%d-%H%M%S)
out_base="dev/benches/4clojure/autoresearch/revalidate-$stamp"
mkdir -p "$out_base"

python3 - "$off_a" "$off_b" "$out_base" <<'PY'
import json, os, subprocess, sys, statistics
off_a, off_b, out_base = int(sys.argv[1]), int(sys.argv[2]), sys.argv[3]
summaries = []
for label, off in [("a", off_a), ("b", off_b)]:
    out_dir = os.path.join(out_base, f"slice-{label}-off{off}")
    env = dict(os.environ)
    env["AUTORESEARCH_OFFSET"] = str(off)
    env["AUTORESEARCH_OUT"]    = out_dir
    print(f"[revalidate] slice {label} offset={off} → {out_dir}", file=sys.stderr, flush=True)
    proc = subprocess.run(
        ["python3", "dev/benches/4clojure/autoresearch_runner.py"],
        env=env,
        check=False,
    )
    if proc.returncode != 0:
        print(f"[revalidate] slice {label} exit={proc.returncode}", file=sys.stderr)
    with open(os.path.join(out_dir, "summary.json")) as f:
        summaries.append(json.load(f))

iter_score    = sum(s["iter_score"] for s in summaries)
pass_count    = sum(s["pass_count"] for s in summaries)
pass_rate     = round(100.0 * pass_count / sum(s["task_count"] for s in summaries), 2)
cost_total    = round(sum(s["total_cost_usd"] for s in summaries), 6)
tokens_total  = sum(s["total_tokens"] for s in summaries)
mean_prompt   = int(statistics.mean([s["mean_prompt_chars"] for s in summaries]))
wall_seconds  = round(sum(s["wall_seconds"] for s in summaries), 2)

combined = {
    "slices":          summaries,
    "iter_score":      iter_score,
    "pass_count":      pass_count,
    "pass_rate":       pass_rate,
    "total_cost_usd":  cost_total,
    "total_tokens":    tokens_total,
    "mean_prompt_chars": mean_prompt,
    "wall_seconds":    wall_seconds,
}
with open(os.path.join(out_base, "summary.json"), "w") as f:
    json.dump(combined, f, indent=2)

for k in ("iter_score","pass_count","pass_rate","total_cost_usd","total_tokens","mean_prompt_chars","wall_seconds"):
    print(f"METRIC revalidate_{k}={combined[k]}")
PY
