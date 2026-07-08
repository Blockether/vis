#!/usr/bin/env bash
set -euo pipefail
here="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

usage() {
  cat <<'EOF'
Usage:
  run_eval.sh [smoke|public-5|overnight|full] [--dry-run] [subset options]

Common examples:
  ./dev/benches/senior_swe_bench/run_eval.sh overnight --max-tasks 10
  ./dev/benches/senior_swe_bench/run_eval.sh overnight --dry-run --max-tasks 12
  ./dev/benches/senior_swe_bench/run_eval.sh full

Subset options are passed to make_subset.py:
  --max-tasks N
  --seed SEED
  --repo REPO
  --segment investigate|design
  --task-type bug|feature|performance|migration
  --include-task TASK_ID
  --exclude-task TASK_ID

The script creates a generated subset under results/generated-subsets/ and then
delegates to run_subset.sh. Existing Vis/verifier env vars are passed through.
EOF
}

mode="${1:-overnight}"
if [[ "$mode" == "-h" || "$mode" == "--help" ]]; then
  usage
  exit 0
fi
if [[ "$mode" == --* ]]; then
  mode="overnight"
else
  shift || true
fi
case "$mode" in
  smoke|public-5|overnight|full) ;;
  *)
    echo "unknown eval mode: $mode" >&2
    usage >&2
    exit 2
    ;;
esac

dry_run=0
subset_args=()
while [[ $# -gt 0 ]]; do
  case "$1" in
    -h|--help)
      usage
      exit 0
      ;;
    --dry-run)
      dry_run=1
      shift
      ;;
    *)
      subset_args+=("$1")
      shift
      ;;
  esac
done

env_file_has_key() {
  local file="$1"
  local key="$2"
  [[ -f "$file" ]] && grep -Eq "^[[:space:]]*(export[[:space:]]+)?${key}=" "$file"
}

if [[ "${VIS_BENCH_VERIFIER_PROVIDER:-}" == "lmstudio" ]]; then
  judge_model="${VIS_BENCH_VERIFIER_JUDGE_MODEL:-${SSB_OVERRIDE_ALL_JUDGE_MODEL:-${VIS_BENCH_VERIFIER_MODEL:-}}}"
  classifier_model="${VIS_BENCH_VERIFIER_CLASSIFIER_MODEL:-${SSB_OVERRIDE_CLASSIFIER_MODEL:-${VIS_BENCH_VERIFIER_MODEL:-}}}"
  va_model="${VIS_BENCH_VERIFIER_VA_MODEL:-${SSB_OVERRIDE_VA_MODEL:-${FORCE_VA_MODEL:-${VIS_BENCH_VERIFIER_MODEL:-$judge_model}}}}"
  if [[ -z "$judge_model" || -z "$classifier_model" || -z "$va_model" ]]; then
    cat >&2 <<'EOF'
VIS_BENCH_VERIFIER_PROVIDER=lmstudio requires a verifier model.

Set one of:
  export LM_STUDIO_MODEL_ID='the exact id from LM Studio /v1/models'
  VIS_BENCH_VERIFIER_MODEL='the exact id from LM Studio /v1/models' ./dev/benches/senior_swe_bench/run_eval.sh overnight

Or set both:
  VIS_BENCH_VERIFIER_JUDGE_MODEL='...'
  VIS_BENCH_VERIFIER_CLASSIFIER_MODEL='...'

For validation-agent-only overrides:
  VIS_BENCH_VERIFIER_VA_MODEL='...'
EOF
    exit 2
  fi
elif [[ "${VIS_BENCH_VERIFIER_PROVIDER:-}" == "zai" ]]; then
  verifier_env_file="${VIS_BENCH_VERIFIER_ENV_FILE:-}"
  if [[ -z "${OPENAI_API_KEY:-}" && -z "${ZAI_API_KEY:-}" ]] \
    && ! env_file_has_key "$verifier_env_file" "OPENAI_API_KEY" \
    && ! env_file_has_key "$verifier_env_file" "ZAI_API_KEY"; then
    cat >&2 <<'EOF'
VIS_BENCH_VERIFIER_PROVIDER=zai requires ZAI_API_KEY or OPENAI_API_KEY.

Set one of:
  export ZAI_API_KEY='your-z.ai-api-token'
  export OPENAI_API_KEY='your-z.ai-api-token'

Or put one of these keys in VIS_BENCH_VERIFIER_ENV_FILE.
The wrapper defaults the verifier judge/classifier/validation-agent model to glm-5.2.
EOF
    exit 2
  fi
fi

run_id="${SUBSET_RUN_ID:-$(date -u +%Y%m%d-%H%M%S)}"
subset_dir="$here/results/generated-subsets"
subset_file="$subset_dir/$mode-$run_id.json"

python3 "$here/make_subset.py" "$mode" --out "$subset_file" "${subset_args[@]}"

python3 - <<'PY' "$subset_file"
import json
import sys
from pathlib import Path

path = Path(sys.argv[1])
data = json.loads(path.read_text())
print(f"subset: {path}")
print(f"name: {data.get('name')}")
print(f"tasks: {len(data.get('task_ids', []))}")
for task in data.get("tasks", []):
    print(
        "  {task_id}\t{segment}\t{task_type}\t{repo}".format(
            task_id=task.get("task_id"),
            segment=task.get("segment") or "-",
            task_type=task.get("task_type") or "-",
            repo=task.get("repo") or "-",
        )
    )
PY

if [[ "$dry_run" == "1" ]]; then
  echo "dry-run: not starting Harbor"
  exit 0
fi

export VIS_BENCH_CONTINUE_ON_FAILURE="${VIS_BENCH_CONTINUE_ON_FAILURE:-1}"
SUBSET_RUN_ID="$run_id" "$here/run_subset.sh" "$subset_file"
