#!/usr/bin/env bash
set -euo pipefail

here="$(cd -P "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
export PATH="$HOME/.local/bin:$PATH"

task_id="paperless-ngx-perf-document-counts"
run_id="${RUN_ID:-paperless-python-dev-verifier-smoke-$(date -u +%Y%m%d-%H%M%S)}"
out="$here/results/$run_id"
prepare_run_id="$run_id-prepare"
prepare_out="$here/results/$prepare_run_id"
image_tag="${VIS_BENCH_PYTHON_DEV_IMAGE:-vis-senior-swe-bench/$task_id:python-dev}"

mkdir -p "$out"

if ! command -v docker >/dev/null 2>&1; then
  echo "docker command is not available. Install Docker and retry." >&2
  exit 2
fi

if ! docker info > "$out/docker-info.txt" 2>&1; then
  cat "$out/docker-info.txt" >&2
  echo "Docker preflight failed; start Docker and retry." >&2
  exit 2
fi

# Prepare a throwaway task copy with the same image-selection logic used by
# run_smoke.sh, but without starting Docker/Harbor or requiring credentials.
RUN_ID="$prepare_run_id" \
  VIS_BENCH_PREFLIGHT_ONLY=1 \
  VIS_BENCH_ALLOW_NO_CONFIG=1 \
  VIS_BENCH_PREPARE_PYTHON_DEV_IMAGE=1 \
  "$here/run_smoke.sh" > "$out/preflight-only.stdout" 2>&1

task_dir="$prepare_out/dataset/tasks/$task_id"
task_environment_dir="$task_dir/environment"
task_dockerfile="$task_environment_dir/Dockerfile"
oracle_patch="$task_dir/solution/oracle.patch"
tests_dir="$task_dir/tests"

if [[ ! -f "$oracle_patch" || ! -d "$tests_dir" ]]; then
  echo "prepared task is missing oracle patch or tests under $task_dir" >&2
  exit 2
fi

if [[ "${VIS_BENCH_REBUILD_PYTHON_DEV_IMAGE:-0}" == "1" ]] || ! docker image inspect "$image_tag" >/dev/null 2>&1; then
  if [[ ! -f "$task_dockerfile" ]]; then
    echo "prepared task is missing an environment Dockerfile: $task_dockerfile" >&2
    exit 2
  fi
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
  docker build -t "$image_tag" "$task_environment_dir" > "$out/docker-build.log" 2>&1
else
  docker image inspect "$image_tag" > "$out/docker-image.json"
fi

docker run --rm --entrypoint bash "$image_tag" -lc \
  'set -euo pipefail; python3 --version; test -f /usr/include/python3.10/Python.h; python3-config --includes; cd /repo/paperless-ngx; uv run python -c "import zxingcpp; print(\"zxingcpp-import-ok\")"' \
  > "$out/dependency-probe.log" 2>&1

docker run --rm --entrypoint bash \
  -v "$tests_dir":/tests:ro \
  -v "$oracle_patch":/tmp/oracle.patch:ro \
  -v "$out":/host-out \
  "$image_tag" -lc '
set -euo pipefail
mkdir -p /logs/verifier
trap '\''status=$?; cp -f /logs/verifier/verifier_results.json /host-out/verifier_results.json 2>/dev/null || true; cp -f /logs/verifier/runner_shell.log /host-out/runner_shell.log 2>/dev/null || true; exit $status'\'' EXIT

export REPO_NAME=paperless-ngx
export _SYS_PYTHON="$(readlink -f "$(which python3 2>/dev/null || which python 2>/dev/null)" 2>/dev/null || echo python3)"

cat > /tmp/verifier-requirements.txt <<'"'"'REQS'"'"'
pytest>=7.0,<9.0
requests>=2.28,<3.0
jinja2>=3.1,<4.0
litellm>=1.0,<2.0
pydantic>=2.0,<3.0
unidiff>=0.7,<1.0
pygments>=2.17,<3.0
ast-grep-py>=0.30,<1.0
REQS
"$_SYS_PYTHON" -m pip install --break-system-packages -r /tmp/verifier-requirements.txt -q 2>/dev/null \
  || "$_SYS_PYTHON" -m pip install -r /tmp/verifier-requirements.txt -q

cd /repo/paperless-ngx
git apply /tmp/oracle.patch
source /tests/test-setup.sh
"$_SYS_PYTHON" /tests/run_verify.py
cat /logs/verifier/verifier_results.json
' > "$out/verifier-smoke.log" 2>&1

python3 - <<'PY' "$out/verifier_results.json"
import json
import sys
from pathlib import Path

path = Path(sys.argv[1])
result = json.loads(path.read_text())
if result.get("all_pass") is not True:
    raise SystemExit(f"verifier did not pass: {result}")
PY

echo "wrote $out (paperless python-dev verifier smoke passed)"
