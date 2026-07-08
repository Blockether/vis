#!/usr/bin/env bash
set -euo pipefail
here="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
# uv installs tools here by default; keep scripts usable in non-login shells.
export PATH="$HOME/.local/bin:$PATH"
repo_root="$(cd -P "$here/../../.." && pwd)"
run_id="${RUN_ID:-$(date -u +%Y%m%d-%H%M%S)}"
out="$here/results/$run_id"
subset="${SUBSET_FILE:-$here/subsets/smoke.json}"
lock="$here/dataset.lock.json"
artifact="${VIS_BENCH_ARTIFACT:-$repo_root/target/bench/vis-agent.tar.gz}"
export VIS_BENCH_ARTIFACT="$artifact"
export VIS_MODEL="${VIS_MODEL:-glm-5.2}"
export VIS_PROVIDER="${VIS_PROVIDER:-zai-coding-plan}"
export PYTHONPATH="$repo_root${PYTHONPATH:+:$PYTHONPATH}"
remote_home="${VIS_BENCH_REMOTE_HOME:-/tmp/vis-home}"
remote_home_mount="${VIS_BENCH_REMOTE_HOME_MOUNT:-}"
remote_home_mount_mode="${VIS_BENCH_REMOTE_HOME_MOUNT_MODE:-rw}"
requested_prepare_python_dev_image="${VIS_BENCH_PREPARE_PYTHON_DEV_IMAGE:-auto}"
task_image_override="${VIS_BENCH_TASK_IMAGE:-}"
preflight_only="${VIS_BENCH_PREFLIGHT_ONLY:-0}"
verifier_provider="${VIS_BENCH_VERIFIER_PROVIDER:-}"
verifier_env_file="${VIS_BENCH_VERIFIER_ENV_FILE:-}"
verifier_model="${VIS_BENCH_VERIFIER_MODEL:-}"
verifier_judge_model="${VIS_BENCH_VERIFIER_JUDGE_MODEL:-${SSB_OVERRIDE_ALL_JUDGE_MODEL:-$verifier_model}}"
verifier_classifier_model="${VIS_BENCH_VERIFIER_CLASSIFIER_MODEL:-${SSB_OVERRIDE_CLASSIFIER_MODEL:-$verifier_model}}"
verifier_va_model="${VIS_BENCH_VERIFIER_VA_MODEL:-${SSB_OVERRIDE_VA_MODEL:-${FORCE_VA_MODEL:-$verifier_model}}}"
verifier_va_harness="${VIS_BENCH_VERIFIER_VA_HARNESS:-${SSB_OVERRIDE_VA_HARNESS:-${FORCE_VA_HARNESS:-}}}"
verifier_openai_base_url="${VIS_BENCH_VERIFIER_OPENAI_BASE_URL:-${OPENAI_BASE_URL:-${OPENAI_API_BASE:-${ZAI_API_BASE:-}}}}"
verifier_tool_choice_compat="${VIS_BENCH_VERIFIER_TOOL_CHOICE_COMPAT:-}"
verifier_response_format_compat="${VIS_BENCH_VERIFIER_RESPONSE_FORMAT_COMPAT:-}"
verifier_tool_choice_compat_disabled=0
verifier_response_format_compat_disabled=0
verifier_timeout_multiplier="${VIS_BENCH_VERIFIER_TIMEOUT_MULTIPLIER:-}"
export VIS_BENCH_REMOTE_HOME="$remote_home"

fail_pre_harbor() {
  local failure_class="$1"
  local message="$2"
  mkdir -p "$out"
  python3 - <<'PY' "$out/preflight-failure.json" "$run_id" "$failure_class" "$message"
import json
import sys
from pathlib import Path

path = Path(sys.argv[1])
path.write_text(json.dumps({
    "run_id": sys.argv[2],
    "failure_class": sys.argv[3],
    "message": sys.argv[4],
    "stage": "pre-harbor",
}, indent=2, sort_keys=True) + "\n")
PY
  echo "$message" >&2
  exit 2
}

if [[ "$preflight_only" != "0" && "$preflight_only" != "1" ]]; then
  fail_pre_harbor "preflight_only_invalid" "VIS_BENCH_PREFLIGHT_ONLY must be 0 or 1, got '$preflight_only'"
fi

if [[ "${1:-}" == "--check" ]]; then
  harbor run --help >/dev/null
  python3 "$here/list_tasks.py" --subset "${SUBSET_FILE:-subsets/smoke.json}" >/dev/null
  echo "ok"
  exit 0
fi

mkdir -p "$out/harbor-output" "$out/vis-traces" "$out/patches"
cp "$lock" "$out/dataset.lock.json"

task_id="$(python3 - <<'PY' "$subset"
import json, sys
print(json.load(open(sys.argv[1]))['task_ids'][0])
PY
)"
if ! prepare_python_dev_image="$(python3 - <<'PY' "$here" "$task_id" "$requested_prepare_python_dev_image" "$task_image_override" "${VIS_BENCH_INSTALL_ONLY:-0}" 2>&1
import importlib.util
import sys
from pathlib import Path

here = Path(sys.argv[1])
task_id, requested, task_image, install_only_raw = sys.argv[2:6]
spec = importlib.util.spec_from_file_location("preflight", here / "preflight.py")
preflight = importlib.util.module_from_spec(spec)
sys.modules["preflight"] = preflight
spec.loader.exec_module(preflight)  # type: ignore[union-attr]
try:
    print(preflight.resolve_python_dev_image_mode(task_id, requested, task_image, install_only_raw == "1"))
except preflight.PreflightError as exc:
    print(str(exc), file=sys.stderr)
    raise SystemExit(2)
PY
)"; then
  fail_pre_harbor "python_dev_image_mode_invalid" "$prepare_python_dev_image"
fi

preflight_args=(--artifact "$artifact" --json)
if [[ -n "${VIS_BENCH_CONFIG:-}" ]]; then
  preflight_args+=(--config "$VIS_BENCH_CONFIG")
elif [[ -n "$remote_home_mount" && -f "$remote_home_mount/.vis/config.edn" ]]; then
  preflight_args+=(--config "$remote_home_mount/.vis/config.edn")
fi
if [[ "${VIS_BENCH_INSTALL_ONLY:-0}" != "1" ]]; then
  preflight_args+=(--require-config)
  if [[ "${VIS_BENCH_ALLOW_NO_CONFIG:-0}" == "1" ]]; then
    preflight_args+=(--allow-no-config)
  fi
fi
if ! python3 "$here/preflight.py" "${preflight_args[@]}" > "$out/preflight.json"; then
  preflight_message="$(python3 - <<'PY' "$out/preflight.json"
import json
import sys
from pathlib import Path

path = Path(sys.argv[1])
try:
    data = json.loads(path.read_text())
except Exception:
    print("run preflight failed; fix the artifact/config before starting Harbor")
else:
    print(data.get("error") or "run preflight failed; fix the artifact/config before starting Harbor")
PY
)"
  fail_pre_harbor "preflight_failed" "$preflight_message"
fi

if ! guard_message="$(python3 - <<'PY' "$here" "${VIS_BENCH_CONFIG:-}" "$remote_home_mount" "${VIS_BENCH_ALLOW_CONFIG_UPLOAD_WITH_HOME_MOUNT:-0}" 2>&1
import importlib.util
import sys
from pathlib import Path

here = Path(sys.argv[1])
spec = importlib.util.spec_from_file_location("preflight", here / "preflight.py")
preflight = importlib.util.module_from_spec(spec)
sys.modules["preflight"] = preflight
spec.loader.exec_module(preflight)  # type: ignore[union-attr]
try:
    preflight.validate_config_mount_combination(
        sys.argv[2],
        sys.argv[3],
        allow_config_with_mount=sys.argv[4] == "1",
    )
except preflight.PreflightError as exc:
    print(str(exc), file=sys.stderr)
    raise SystemExit(2)
PY
)"; then
  fail_pre_harbor "config_mount_combination_invalid" "$guard_message"
fi

env_file_has_key() {
  local file="$1"
  local key="$2"
  [[ -f "$file" ]] && grep -Eq "^[[:space:]]*(export[[:space:]]+)?${key}=" "$file"
}

env_file_value() {
  local file="$1"
  local key="$2"
  [[ -f "$file" ]] || return 1
  python3 - <<'PY' "$file" "$key"
import re
import sys
from pathlib import Path

path = Path(sys.argv[1])
key = re.escape(sys.argv[2])
pattern = re.compile(rf"^\s*(?:export\s+)?{key}\s*=\s*(.*)\s*$")
for line in path.read_text(errors="replace").splitlines():
    match = pattern.match(line)
    if not match:
        continue
    value = match.group(1).strip()
    if len(value) >= 2 and value[0] == value[-1] and value[0] in {"'", '"'}:
        value = value[1:-1]
    print(value)
    raise SystemExit(0)
raise SystemExit(1)
PY
}

ensure_openai_slug() {
  local model="$1"
  if [[ -z "$model" || "$model" == openai/* ]]; then
    printf '%s' "$model"
  else
    printf 'openai/%s' "$model"
  fi
}

if [[ -n "$verifier_env_file" && ! -f "$verifier_env_file" ]]; then
  fail_pre_harbor "verifier_env_file_missing" "VIS_BENCH_VERIFIER_ENV_FILE does not exist: $verifier_env_file"
fi
if [[ -z "$verifier_openai_base_url" && -n "$verifier_env_file" ]]; then
  verifier_openai_base_url="$(env_file_value "$verifier_env_file" OPENAI_BASE_URL || true)"
fi
if [[ -z "$verifier_openai_base_url" && -n "$verifier_env_file" ]]; then
  verifier_openai_base_url="$(env_file_value "$verifier_env_file" OPENAI_API_BASE || true)"
fi
if [[ -z "$verifier_openai_base_url" && -n "$verifier_env_file" ]]; then
  verifier_openai_base_url="$(env_file_value "$verifier_env_file" ZAI_API_BASE || true)"
fi

if [[ -n "$verifier_provider" && "$verifier_provider" != "openai" && "$verifier_provider" != "lmstudio" && "$verifier_provider" != "zai" ]]; then
  fail_pre_harbor "verifier_provider_invalid" \
    "VIS_BENCH_VERIFIER_PROVIDER must be unset, openai, lmstudio, or zai; got '$verifier_provider'"
fi
if [[ "$verifier_tool_choice_compat" == "none" || "$verifier_tool_choice_compat" == "0" ]]; then
  verifier_tool_choice_compat_disabled=1
  verifier_tool_choice_compat=""
fi
if [[ "$verifier_response_format_compat" == "none" || "$verifier_response_format_compat" == "0" ]]; then
  verifier_response_format_compat_disabled=1
  verifier_response_format_compat=""
fi
if [[ "$verifier_provider" == "lmstudio" ]]; then
  verifier_openai_base_url="${verifier_openai_base_url:-http://host.docker.internal:1234/v1}"
  if [[ "$verifier_tool_choice_compat_disabled" != "1" ]]; then
    verifier_tool_choice_compat="${verifier_tool_choice_compat:-required}"
  fi
  verifier_timeout_multiplier="${verifier_timeout_multiplier:-3}"
  verifier_va_model="${verifier_va_model:-$verifier_judge_model}"
  verifier_judge_model="$(ensure_openai_slug "$verifier_judge_model")"
  verifier_classifier_model="$(ensure_openai_slug "$verifier_classifier_model")"
  verifier_va_model="$(ensure_openai_slug "$verifier_va_model")"
  if [[ -z "$verifier_judge_model" || -z "$verifier_classifier_model" || -z "$verifier_va_model" ]]; then
    fail_pre_harbor "verifier_lmstudio_model_missing" \
      "VIS_BENCH_VERIFIER_PROVIDER=lmstudio requires VIS_BENCH_VERIFIER_MODEL, or judge/classifier/validation-agent model overrides."
  fi
elif [[ "$verifier_provider" == "openai" ]]; then
  verifier_va_model="${verifier_va_model:-$verifier_judge_model}"
  if [[ -z "${OPENAI_API_KEY:-}" ]] && ! env_file_has_key "$verifier_env_file" "OPENAI_API_KEY"; then
    fail_pre_harbor "verifier_openai_api_key_missing" \
      "VIS_BENCH_VERIFIER_PROVIDER=openai requires OPENAI_API_KEY in the host environment or VIS_BENCH_VERIFIER_ENV_FILE."
  fi
  if [[ -z "$verifier_judge_model" ]]; then
    fail_pre_harbor "verifier_openai_judge_model_missing" \
      "VIS_BENCH_VERIFIER_PROVIDER=openai requires VIS_BENCH_VERIFIER_JUDGE_MODEL or SSB_OVERRIDE_ALL_JUDGE_MODEL, for example openai/<judge-model>."
  fi
  if [[ -z "$verifier_classifier_model" ]]; then
    fail_pre_harbor "verifier_openai_classifier_model_missing" \
      "VIS_BENCH_VERIFIER_PROVIDER=openai requires VIS_BENCH_VERIFIER_CLASSIFIER_MODEL or SSB_OVERRIDE_CLASSIFIER_MODEL, for example openai/<classifier-model>."
  fi
elif [[ "$verifier_provider" == "zai" ]]; then
  verifier_openai_base_url="${verifier_openai_base_url:-https://api.z.ai/api/paas/v4/}"
  if [[ "$verifier_tool_choice_compat_disabled" != "1" ]]; then
    verifier_tool_choice_compat="${verifier_tool_choice_compat:-required}"
  fi
  if [[ "$verifier_tool_choice_compat_disabled" != "1" && "$verifier_response_format_compat_disabled" != "1" ]]; then
    verifier_response_format_compat="${verifier_response_format_compat:-json_object}"
  fi
  verifier_judge_model="${verifier_judge_model:-${verifier_model:-glm-5.2}}"
  verifier_classifier_model="${verifier_classifier_model:-${verifier_model:-glm-5.2}}"
  verifier_va_model="${verifier_va_model:-$verifier_judge_model}"
  verifier_judge_model="$(ensure_openai_slug "$verifier_judge_model")"
  verifier_classifier_model="$(ensure_openai_slug "$verifier_classifier_model")"
  verifier_va_model="$(ensure_openai_slug "$verifier_va_model")"
  if [[ -z "${OPENAI_API_KEY:-}" && -z "${ZAI_API_KEY:-}" ]] \
    && ! env_file_has_key "$verifier_env_file" "OPENAI_API_KEY" \
    && ! env_file_has_key "$verifier_env_file" "ZAI_API_KEY"; then
    fail_pre_harbor "verifier_zai_api_key_missing" \
      "VIS_BENCH_VERIFIER_PROVIDER=zai requires ZAI_API_KEY or OPENAI_API_KEY in the host environment or VIS_BENCH_VERIFIER_ENV_FILE."
  fi
fi
if [[ -n "$verifier_tool_choice_compat" && "$verifier_tool_choice_compat" != "required" ]]; then
  fail_pre_harbor "verifier_tool_choice_compat_invalid" \
    "VIS_BENCH_VERIFIER_TOOL_CHOICE_COMPAT must be unset, none, or required; got '$verifier_tool_choice_compat'"
fi
if [[ -n "$verifier_response_format_compat" && "$verifier_response_format_compat" != "json_schema" && "$verifier_response_format_compat" != "json_object" ]]; then
  fail_pre_harbor "verifier_response_format_compat_invalid" \
    "VIS_BENCH_VERIFIER_RESPONSE_FORMAT_COMPAT must be unset, none, json_schema, or json_object; got '$verifier_response_format_compat'"
fi
if [[ -n "$verifier_timeout_multiplier" ]]; then
  if ! python3 - <<'PY' "$verifier_timeout_multiplier"
import math
import sys

try:
    value = float(sys.argv[1])
except ValueError:
    raise SystemExit(1)
if not math.isfinite(value) or value <= 0:
    raise SystemExit(1)
PY
  then
    fail_pre_harbor "verifier_timeout_multiplier_invalid" \
      "VIS_BENCH_VERIFIER_TIMEOUT_MULTIPLIER must be a positive number; got '$verifier_timeout_multiplier'"
  fi
fi

mounts_json=""
if [[ -n "$remote_home_mount" ]]; then
  if ! mounts_json="$(python3 - <<'PY' "$here" "$remote_home_mount" "$remote_home" "$remote_home_mount_mode" 2>&1
import importlib.util
import json
import sys
from pathlib import Path

here = Path(sys.argv[1])
spec = importlib.util.spec_from_file_location("preflight", here / "preflight.py")
preflight = importlib.util.module_from_spec(spec)
sys.modules["preflight"] = preflight
spec.loader.exec_module(preflight)  # type: ignore[union-attr]
try:
    print(json.dumps(preflight.build_remote_home_mounts(sys.argv[2], sys.argv[3], sys.argv[4])))
except preflight.PreflightError as exc:
    print(str(exc), file=sys.stderr)
    raise SystemExit(2)
PY
)"; then
    fail_pre_harbor "remote_home_mount_invalid" "$mounts_json"
  fi
fi
config_delivery="$(python3 - <<'PY' "$here" "${VIS_BENCH_CONFIG:-}" "$remote_home_mount"
import importlib.util
import sys
from pathlib import Path

here = Path(sys.argv[1])
spec = importlib.util.spec_from_file_location("preflight", here / "preflight.py")
preflight = importlib.util.module_from_spec(spec)
sys.modules["preflight"] = preflight
spec.loader.exec_module(preflight)  # type: ignore[union-attr]
print(preflight.resolve_config_delivery(sys.argv[2], sys.argv[3]))
PY
)"

dataset_copy="$out/dataset"
cat > "$out/command.json" <<EOF
{
  "command": ["harbor", "run", "--yes", "--path", "$dataset_copy/tasks", "--agent", "dev.benches.senior_swe_bench.agent:VisInstalledAgent", "--model", "$VIS_MODEL"],
  "task_ids": ["$task_id"],

  "run_id": "$run_id",
  "vis_provider": "$VIS_PROVIDER",
  "vis_model": "$VIS_MODEL",
  "vis_bench_artifact": "$artifact",
  "vis_bench_remote_home": "$remote_home",
  "vis_bench_remote_home_mount": "$remote_home_mount",
  "vis_bench_remote_home_mount_mode": "$remote_home_mount_mode",
  "vis_bench_config": "${VIS_BENCH_CONFIG:-}",
  "vis_bench_config_delivery": "$config_delivery",
  "vis_bench_verifier_provider": "$verifier_provider",
  "vis_bench_verifier_env_file": "$verifier_env_file",
  "vis_bench_verifier_openai_base_url": "$verifier_openai_base_url",
  "vis_bench_verifier_judge_model": "$verifier_judge_model",
  "vis_bench_verifier_classifier_model": "$verifier_classifier_model",
  "vis_bench_verifier_va_model": "$verifier_va_model",
  "vis_bench_verifier_va_harness": "$verifier_va_harness",
  "vis_bench_verifier_tool_choice_compat": "$verifier_tool_choice_compat",
  "vis_bench_verifier_response_format_compat": "$verifier_response_format_compat",
  "vis_bench_verifier_timeout_multiplier": "$verifier_timeout_multiplier",
  "vis_bench_task_image": "$task_image_override",
  "vis_bench_prepare_python_dev_image_requested": "$requested_prepare_python_dev_image",
  "vis_bench_prepare_python_dev_image": "$prepare_python_dev_image",
  "vis_bench_preflight_only": "$preflight_only",
  "install_only": "${VIS_BENCH_INSTALL_ONLY:-0}"
}
EOF

# Harbor task-id filtering has moved during Senior SWE-Bench development. Prefer
# a temp dataset copy with only the selected task to keep smoke runs stable.
python3 "$here/list_tasks.py" --subset "$subset" --json > "$out/selected-tasks.json"
python3 - <<'PY' "$here" "$dataset_copy" "$task_id" "$subset"
import json, pathlib, shutil, subprocess, sys
here=pathlib.Path(sys.argv[1]); dest=pathlib.Path(sys.argv[2]); task_id=sys.argv[3]
lock=json.loads((here/'dataset.lock.json').read_text())
cache=pathlib.Path.home()/'.cache'/'vis'/'senior-swe-bench'/lock['dataset_repo'].replace('/','__')/lock['dataset_commit']
if not cache.exists():
    subprocess.run([sys.executable, str(here/'list_tasks.py'), '--subset', str(pathlib.Path(sys.argv[4]) if len(sys.argv) > 4 else 'subsets/smoke.json')], check=True)
if dest.exists(): shutil.rmtree(dest)
shutil.copytree(cache, dest, ignore=shutil.ignore_patterns('.git'))
for task in (dest/'tasks').iterdir():
    if task.name != task_id:
        shutil.rmtree(task)
PY

if [[ -n "$verifier_tool_choice_compat" ]]; then
  verifier_adapt_args=(
    --dataset-copy "$dataset_copy"
    --tool-choice-compat "$verifier_tool_choice_compat"
    --out "$out/verifier-adaptations.json"
  )
  if [[ -n "$verifier_response_format_compat" ]]; then
    verifier_adapt_args+=(--response-format-compat "$verifier_response_format_compat")
  fi
  if ! verifier_adaptation_output="$(python3 "$here/verifier_adapt.py" "${verifier_adapt_args[@]}" 2>&1)"; then
    fail_pre_harbor "verifier_adaptation_failed" "$verifier_adaptation_output"
  fi
  printf '%s\n' "$verifier_adaptation_output" > "$out/verifier-adaptations.stdout.txt"
fi

task_toml="$dataset_copy/tasks/$task_id/task.toml"
task_base_image="$(python3 - <<'PY' "$task_toml"
import re
import sys
from pathlib import Path

for line in Path(sys.argv[1]).read_text().splitlines():
    match = re.match(r'\s*(?:base_image|docker_image)\s*=\s*"([^"]+)"', line)
    if match:
        print(match.group(1))
        raise SystemExit(0)
raise SystemExit("task.toml has no environment base_image/docker_image")
PY
)"
selected_task_image="$task_image_override"
if [[ "$prepare_python_dev_image" == "1" ]]; then
  image_tag="${VIS_BENCH_PYTHON_DEV_IMAGE:-vis-senior-swe-bench/$task_id:python-dev}"
  task_environment_dir="$dataset_copy/tasks/$task_id/environment"
  task_dockerfile="$task_environment_dir/Dockerfile"
  if [[ "$preflight_only" != "1" && -f "$task_dockerfile" ]]; then
    python3 - <<'PY' "$task_dockerfile"
import sys
from pathlib import Path

path = Path(sys.argv[1])
text = path.read_text()
if "python3-dev" not in text:
    lines = text.splitlines(keepends=True)
    for i, line in enumerate(lines):
        if "build-essential" in line and line.rstrip().endswith("\\"):
            lines.insert(i + 1, "        python3-dev \\\n")
            break
    else:
        raise SystemExit(f"cannot find apt install block in {path}")
    path.write_text("".join(lines))
PY
    docker build -t "$image_tag" "$task_environment_dir"
  elif [[ "$preflight_only" != "1" ]]; then
    docker build \
      --build-arg "BASE_IMAGE=${VIS_BENCH_TASK_BASE_IMAGE:-$task_base_image}" \
      -t "$image_tag" \
      -f "$here/docker/python-dev.Dockerfile" \
      "$here"
  fi
  selected_task_image="$image_tag"
fi

if [[ -n "$selected_task_image" ]]; then
  python3 - <<'PY' "$task_toml" "$selected_task_image"
import re
import sys
from pathlib import Path

path = Path(sys.argv[1])
image = sys.argv[2]
lines = path.read_text().splitlines(keepends=True)
for i, line in enumerate(lines):
    newline = "\n" if line.endswith("\n") else ""
    body = line[:-1] if newline else line
    match = re.match(r'(\s*(?:base_image|docker_image)\s*=\s*)"[^"]+"(.*)$', body)
    if match:
        lines[i] = f'{match.group(1)}"{image}"{match.group(2)}{newline}'
        break
else:
    for i, line in enumerate(lines):
        if line.strip() == "[environment]":
            lines.insert(i + 1, f'base_image = "{image}"\n')
            break
    else:
        raise SystemExit("task.toml has no [environment] section")
path.write_text("".join(lines))
PY
fi

python3 - <<'PY' "$out/command.json" "$task_base_image" "$selected_task_image" "$mounts_json"
import json
import sys
from pathlib import Path

path = Path(sys.argv[1])
data = json.loads(path.read_text())
data["task_base_image"] = sys.argv[2]
data["selected_task_image"] = sys.argv[3]
data["harbor_mounts"] = json.loads(sys.argv[4]) if sys.argv[4] else []
path.write_text(json.dumps(data, indent=2) + "\n")
PY

if [[ "$preflight_only" == "1" ]]; then
  echo "preflight-only: Harbor and Docker were not started" > "$out/preflight-only.txt"
  echo "wrote $out (preflight-only)"
  exit 0
fi

docker_preflight="$out/docker-preflight.txt"
if ! command -v docker >/dev/null 2>&1; then
  message="docker command is not available. Install Docker and retry."
  echo "$message" > "$docker_preflight"
  fail_pre_harbor "docker_command_unavailable" "$message"
fi
if ! docker info > "$docker_preflight" 2>&1; then
  message="$(cat "$docker_preflight")
Docker preflight failed; start Docker and retry before launching Harbor."
  fail_pre_harbor "docker_daemon_unavailable" "$message"
fi

jobs_before="$out/jobs-before.txt"
find jobs -mindepth 1 -maxdepth 1 -type d -print 2>/dev/null | sort > "$jobs_before" || true

set +e
harbor_args=(
  --yes \
  --path "$dataset_copy/tasks" \
  --agent dev.benches.senior_swe_bench.agent:VisInstalledAgent \
  --model "$VIS_MODEL" \
  --artifact /logs/artifacts \
  --agent-env "PYTHONPATH=$PYTHONPATH" \
  --agent-env "HARBOR_TASK_ID=$task_id" \
  --agent-env "HARBOR_ARTIFACTS_DIR=/logs/artifacts" \
  --agent-env "VIS_BENCH_REMOTE_HOME=$remote_home" \
  --agent-env "VIS_PROVIDER=$VIS_PROVIDER" \
  --agent-env "VIS_MODEL=$VIS_MODEL"
)
if [[ -n "$verifier_timeout_multiplier" ]]; then
  harbor_args+=(--verifier-timeout-multiplier "$verifier_timeout_multiplier")
fi
if [[ -n "$verifier_env_file" ]]; then
  harbor_args+=(--env-file "$verifier_env_file")
fi
if [[ "$verifier_provider" == "lmstudio" ]] && [[ -z "${OPENAI_API_KEY:-}" ]] && ! env_file_has_key "$verifier_env_file" "OPENAI_API_KEY"; then
  harbor_args+=(--verifier-env "OPENAI_API_KEY=lmstudio")
fi
if [[ "$verifier_provider" == "zai" ]] && [[ -z "${OPENAI_API_KEY:-}" ]] && ! env_file_has_key "$verifier_env_file" "OPENAI_API_KEY"; then
  if [[ -n "${ZAI_API_KEY:-}" ]]; then
    harbor_args+=(--verifier-env "OPENAI_API_KEY=$ZAI_API_KEY")
  elif env_file_has_key "$verifier_env_file" "ZAI_API_KEY"; then
    harbor_args+=(--verifier-env "OPENAI_API_KEY=$(env_file_value "$verifier_env_file" ZAI_API_KEY)")
  fi
fi
if [[ -n "$verifier_openai_base_url" ]]; then
  harbor_args+=(--verifier-env "OPENAI_BASE_URL=$verifier_openai_base_url")
  harbor_args+=(--verifier-env "OPENAI_API_BASE=$verifier_openai_base_url")
fi
if [[ -n "$verifier_judge_model" ]]; then
  harbor_args+=(--verifier-env "SSB_OVERRIDE_ALL_JUDGE_MODEL=$verifier_judge_model")
fi
if [[ -n "$verifier_classifier_model" ]]; then
  harbor_args+=(--verifier-env "SSB_OVERRIDE_CLASSIFIER_MODEL=$verifier_classifier_model")
fi
if [[ -n "$verifier_va_model" ]]; then
  harbor_args+=(--verifier-env "SSB_OVERRIDE_VA_MODEL=$verifier_va_model")
fi
if [[ -n "$verifier_va_harness" ]]; then
  harbor_args+=(--verifier-env "SSB_OVERRIDE_VA_HARNESS=$verifier_va_harness")
fi
if [[ -n "$verifier_tool_choice_compat" ]]; then
  harbor_args+=(--verifier-env "SSB_OPENAI_COMPAT_TOOL_CHOICE=$verifier_tool_choice_compat")
  harbor_args+=(--verifier-env "SSB_OPENAI_COMPAT_PARSE_CONTENT_JSON=1")
  harbor_args+=(--verifier-env "SSB_OPENAI_COMPAT_RESPONSE_FORMAT=${verifier_response_format_compat:-1}")
fi
if [[ -n "$mounts_json" ]]; then
  harbor_args+=(--mounts "$mounts_json")
fi
if [[ -n "${VIS_BENCH_CONFIG:-}" ]]; then
  harbor_args+=(--agent-env "VIS_BENCH_CONFIG=$VIS_BENCH_CONFIG")
fi
if [[ "${VIS_BENCH_INSTALL_ONLY:-0}" == "1" ]]; then
  harbor_args+=(--install-only)
fi
harbor run "${harbor_args[@]}" > "$out/harbor.log" 2>&1


status=$?
set -e

if [[ -d artifacts/vis-traces/$task_id ]]; then
  mkdir -p "$out/vis-traces/$task_id"
  cp -R artifacts/vis-traces/$task_id/. "$out/vis-traces/$task_id/" || true
fi
latest_job=""
harbor_result_path="$(sed -n 's/^Results written to \(jobs\/.*\/result\.json\).*$/\1/p' "$out/harbor.log" | tail -1 || true)"
if [[ -n "$harbor_result_path" ]]; then
  latest_job="${harbor_result_path%/result.json}"
else
  jobs_after="$out/jobs-after.txt"
  find jobs -mindepth 1 -maxdepth 1 -type d -print 2>/dev/null | sort > "$jobs_after" || true
  latest_job="$(comm -13 "$jobs_before" "$jobs_after" | tail -1 || true)"
fi
if [[ -n "$latest_job" && -d "$latest_job" ]]; then
  python3 "$here/collect_harbor_artifacts.py" \
    --job-dir "$latest_job" \
    --run-dir "$out" \
    --task-id "$task_id" > "$out/harbor-output/collection.stdout.json" || true
fi
patch_file="$out/harbor-output/trial/verifier/agent.patch"
if [[ ! -s "$patch_file" ]]; then
  patch_file="$out/patches/$task_id.diff"
  dataset_git_root="$(git -C "$dataset_copy" rev-parse --show-toplevel 2>/dev/null || true)"
  if [[ "$dataset_git_root" == "$dataset_copy" ]]; then
    git -C "$dataset_copy" diff --no-color > "$patch_file" 2>/dev/null || : > "$patch_file"
  else
    : > "$patch_file"
  fi
fi

reward_file=""
found_reward=""
for candidate in \
  "$out/harbor-output/trial/verifier/reward_details.json" \
  "$out/harbor-output/trial/verifier/reward.json" \
  "$out/harbor-output/trial/verifier/score.json"
do
  if [[ -s "$candidate" ]]; then
    found_reward="$candidate"
    break
  fi
done
if [[ -z "$found_reward" ]]; then
  found_reward="$(find "$out" \( -name '*reward*.json' -o -name '*score*.json' \) -print | head -1 || true)"
fi
if [[ -n "$found_reward" ]]; then reward_file="--harbor-reward $found_reward"; fi
python3 "$here/metrics.py" \
  --task-id "$task_id" \
  --run-dir "$out" \
  --patch "$patch_file" \
  --trace "$out/vis-traces/$task_id/vis.trace.jsonl" \
  --dataset-copy "$dataset_copy" \
  $reward_file \
  --out "$out/summary.json" >/dev/null

echo "wrote $out (harbor exit $status)"
exit "$status"
