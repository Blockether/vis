#!/usr/bin/env bash
# Run one Dockerized Terminal-Bench task with opencode and Vis.
set -euo pipefail

TASK_ID="${TASK_ID:?TASK_ID required}"
TASKS_FILE="${TASKS_FILE:-bench/opencode/tasks.jsonl}"
OPENCODE_MODEL="${OPENCODE_MODEL:-anthropic/claude-opus-4-7}"
VIS_MODEL="${VIS_MODEL:-anthropic-coding-plan/claude-opus-4-7}"
RUN_ROOT_BASE="${RUN_ROOT_BASE:-target/vis-bench}"
BENCH_SOURCE_ROOT="${BENCH_SOURCE_ROOT:-target/bench-sources/terminal-bench}"

need() { command -v "$1" >/dev/null 2>&1 || { echo "missing command: $1" >&2; exit 2; }; }
need jq
need git
need python3
need docker
need opencode
need timeout

repo_root="$(git rev-parse --show-toplevel)"
cd "$repo_root"
[[ -x ./bin/vis ]] || { echo "missing ./bin/vis" >&2; exit 2; }

task_json="$(jq -c --arg id "$TASK_ID" 'select(.id == $id)' "$TASKS_FILE" | head -1)"
[[ -n "$task_json" ]] || { echo "No task id '$TASK_ID' in $TASKS_FILE" >&2; exit 2; }

source_repo="$(jq -r '.source_repo' <<<"$task_json")"
source_ref="$(jq -r '.source_ref' <<<"$task_json")"
source_path="$(jq -r '.source_path' <<<"$task_json")"
slug="$(jq -r '.slug' <<<"$task_json")"
timeout_seconds="${SIDE_TIMEOUT_SECONDS:-$(jq -r '.timeout_seconds // 900' <<<"$task_json") }"
timeout_seconds="${timeout_seconds//[[:space:]]/}"
test_timeout_seconds="${TEST_TIMEOUT_SECONDS:-$(jq -r '.max_test_timeout_seconds // 180' <<<"$task_json") }"
test_timeout_seconds="${test_timeout_seconds//[[:space:]]/}"

run_id="$(date +%Y%m%d-%H%M%S)-${TASK_ID}"
root="$repo_root/$RUN_ROOT_BASE/$run_id"
mkdir -p "$root/opencode" "$root/vis" "$root/eval"
printf '%s\n' "$task_json" > "$root/task.json"

fetch_source() {
  if [[ ! -d "$BENCH_SOURCE_ROOT/.git" ]]; then
    mkdir -p "$(dirname "$BENCH_SOURCE_ROOT")"
    git clone --depth=1 --filter=blob:none --sparse "$source_repo" "$BENCH_SOURCE_ROOT"
  fi
  ( cd "$BENCH_SOURCE_ROOT" && git fetch --depth=1 origin "$source_ref" && git checkout --quiet "$source_ref" && git sparse-checkout set "$source_path" )
}
fetch_source
src_task="$repo_root/$BENCH_SOURCE_ROOT/$source_path"
[[ -f "$src_task/task.yaml" ]] || { echo "missing Terminal-Bench task.yaml: $src_task" >&2; exit 2; }

python3 - "$src_task/task.yaml" > "$root/instruction.txt" <<'PY'
import pathlib, sys
p=pathlib.Path(sys.argv[1])
text=p.read_text(errors='replace').splitlines()
out=[]; in_inst=False; base_indent=None
for line in text:
    if line.startswith('instruction:'):
        in_inst=True
        continue
    if in_inst:
        if line and not line.startswith((' ', '\t')):
            break
        s=line[2:] if line.startswith('  ') else line.lstrip()
        out.append(s)
print('\n'.join(out).strip())
PY
instruction="$(cat "$root/instruction.txt")"

containers=()
images=()
cleanup() {
  for c in "${containers[@]:-}"; do docker rm -f "$c" >/dev/null 2>&1 || true; done
  if [[ "${KEEP_BENCH_IMAGES:-0}" != "1" ]]; then
    for i in "${images[@]:-}"; do docker rmi -f "$i" >/dev/null 2>&1 || true; done
  fi
}
trap cleanup EXIT

setup_side() {
  local side="$1"
  local side_dir="$root/$side"
  local task_dir="$side_dir/task"
  mkdir -p "$side_dir"
  cp -a "$src_task" "$task_dir"
  local safe_run image container
  safe_run="$(echo "$run_id-$side" | tr '[:upper:]' '[:lower:]' | tr -cs 'a-z0-9_.-' '-')"
  image="visbench-$safe_run"
  container="visbench-$safe_run"
  docker build -q -t "$image" "$task_dir" > "$side_dir/docker-build-image.txt"
  docker run -d --name "$container" -e TEST_DIR=/tests "$image" sh -c 'sleep infinity' > "$side_dir/container.id"
  containers+=("$container")
  images+=("$image")
  if [[ -d "$task_dir/tests" ]]; then docker cp "$task_dir/tests" "$container:/tests"; fi
  if [[ -f "$task_dir/run-tests.sh" ]]; then docker cp "$task_dir/run-tests.sh" "$container:/run-tests.sh"; fi
  docker exec "$container" chmod +x /run-tests.sh >/dev/null 2>&1 || true
  printf '%s\n' "$container" > "$side_dir/container.name"
}
setup_side opencode
setup_side vis
opencode_container="$(cat "$root/opencode/container.name")"
vis_container="$(cat "$root/vis/container.name")"

make_prompt() {
  local container="$1" side="$2"
  cat > "$root/$side/prompt.txt" <<EOF
You are solving a Dockerized Terminal-Bench task.

Benchmark: Terminal-Bench
Task id: $TASK_ID
Task slug: $slug
Container name: $container
Working directory inside container: /app
Test command: docker exec -w /app $container bash /run-tests.sh

Use docker exec/cp against the named container. Do not use solution.sh or inspect oracle solutions. Make the task pass the tests. When done, summarize files changed and tests run.

Task instruction:
$instruction
EOF
}
make_prompt "$opencode_container" opencode
make_prompt "$vis_container" vis

# -- run opencode -------------------------------------------------------------
set +e
(
  cd "$root/opencode"
  OPENCODE_DISABLE_UPDATE=1 /usr/bin/time -p -o "$root/opencode/time.txt" \
    timeout --kill-after=30s "${timeout_seconds}s" \
    opencode run --format json --model "$OPENCODE_MODEL" \
      --dangerously-skip-permissions --title "ar-$TASK_ID-opencode" \
      "$(cat "$root/opencode/prompt.txt")" \
    > "$root/opencode/events.jsonl" 2> "$root/opencode/stderr.txt"
)
opencode_exit=$?
set -e
printf '%s\n' "$opencode_exit" > "$root/opencode/exit_code"

# -- run Vis ------------------------------------------------------------------
rm -rf "$root/vis/db"
mkdir -p "$root/vis/db"
set +e
(
  cd "$root/vis"
  VIS_DB_PATH="$root/vis/db" VIS_CRAC=0 /usr/bin/time -p -o "$root/vis/time.txt" \
    timeout --kill-after=30s "${timeout_seconds}s" \
    "$repo_root/bin/vis" run --json --trace --model "$VIS_MODEL" --db "$root/vis/db" \
      "$(cat "$root/vis/prompt.txt")" \
    > "$root/vis/result.json" 2> "$root/vis/stderr.txt"
)
vis_exit=$?
set -e
printf '%s\n' "$vis_exit" > "$root/vis/exit_code"

# -- tests/artifacts ----------------------------------------------------------
run_tests() {
  local side="$1" container="$2"
  set +e
  timeout --kill-after=10s "${test_timeout_seconds}s" docker exec -w /app "$container" bash /run-tests.sh \
    > "$root/eval/checks.$side.txt" 2>&1
  local ec=$?
  set -e
  printf '%s\n' "$ec" > "$root/eval/checks.$side.exit_code"
  docker diff "$container" > "$root/$side/docker.diff.txt" 2>&1 || true
  docker cp "$container:/app" "$root/$side/app-after" >/dev/null 2>&1 || true
}
run_tests opencode "$opencode_container"
run_tests vis "$vis_container"

# -- parse outputs and metrics ------------------------------------------------
python3 - "$root" <<'PY'
import json, pathlib, re, sys, math
root=pathlib.Path(sys.argv[1])

def read(rel):
    p=root/rel
    return p.read_text(errors='replace') if p.exists() else ''
def code(rel):
    try: return int(read(rel).strip() or '0')
    except Exception: return 0
def wall(rel):
    m=re.search(r'^real\s+([0-9.]+)', read(rel), re.M)
    return float(m.group(1)) if m else 0.0
def walk(x):
    yield x
    if isinstance(x,dict):
        for v in x.values(): yield from walk(v)
    elif isinstance(x,list):
        for v in x: yield from walk(v)

def load_jsonl(rel):
    out=[]
    for line in read(rel).splitlines():
        try: out.append(json.loads(line))
        except Exception: pass
    return out

def opencode_text_and_tokens():
    texts=[]; vals={'input':0,'output':0,'cache_read':0,'cache_write':0,'total':0}
    names={
      'input':['input_tokens','inputTokens','prompt_tokens','promptTokens'],
      'output':['output_tokens','outputTokens','completion_tokens','completionTokens'],
      'cache_read':['cache_read_input_tokens','cacheReadInputTokens','cache_read_tokens','cacheReadTokens'],
      'cache_write':['cache_creation_input_tokens','cacheCreationInputTokens','cache_write_tokens','cacheWriteTokens'],
      'total':['total_tokens','totalTokens']}
    for obj in load_jsonl('opencode/events.jsonl'):
        for node in walk(obj):
            if isinstance(node,dict):
                for k in ('text','content','delta','message'):
                    v=node.get(k)
                    if isinstance(v,str) and 0 < len(v) < 20000 and not v.lstrip().startswith('{'):
                        texts.append(v)
                for metric, ks in names.items():
                    for k in ks:
                        v=node.get(k)
                        if isinstance(v,(int,float)): vals[metric]=max(vals[metric], int(v))
    if vals['total']==0: vals['total']=vals['input']+vals['output']+vals['cache_read']+vals['cache_write']
    return '\n'.join(dict.fromkeys(texts))[-40000:], vals

def vis_text_and_tokens():
    try: data=json.loads(read('vis/result.json'))
    except Exception: data={}
    ans=data.get('answer','') if isinstance(data,dict) else ''
    if not isinstance(ans,str): ans=json.dumps(ans)[:40000]
    toks=data.get('tokens',{}) if isinstance(data,dict) else {}
    def n(*ks):
        if not isinstance(toks,dict): return 0
        for k in ks:
            v=toks.get(k)
            if isinstance(v,(int,float)): return int(v)
        return 0
    vals={'input':n('input-tokens','input_tokens','inputTokens','prompt_tokens'),
          'output':n('output-tokens','output_tokens','outputTokens','completion_tokens'),
          'cache_read':n('cache-read-tokens','cache_read_tokens','cacheReadTokens'),
          'cache_write':n('cache-write-tokens','cache_write_tokens','cacheWriteTokens'),
          'total':n('total-tokens','total_tokens','totalTokens')}
    if vals['total']==0: vals['total']=vals['input']+vals['output']+vals['cache_read']+vals['cache_write']
    dm=0
    if isinstance(data,dict):
        x=data.get('duration-ms') or data.get('duration_ms') or data.get('durationMs') or 0
        if isinstance(x,(int,float)): dm=float(x)
    vals['duration_ms']=dm
    return ans, vals

op_text, op_tok=opencode_text_and_tokens()
vis_text, vis_tok=vis_text_and_tokens()
(root/'opencode/output.txt').write_text(op_text)
(root/'opencode/metrics.tokens.json').write_text(json.dumps(op_tok, indent=2))
(root/'vis/output.txt').write_text(vis_text)
(root/'vis/metrics.tokens.json').write_text(json.dumps(vis_tok, indent=2))

op_exit=code('opencode/exit_code'); vis_exit=code('vis/exit_code')
op_check=code('eval/checks.opencode.exit_code'); vis_check=code('eval/checks.vis.exit_code')
op_pass=1 if op_check==0 else 0; vis_pass=1 if vis_check==0 else 0
op_wall=wall('opencode/time.txt'); vis_wall=wall('vis/time.txt'); vis_task=float(vis_tok.get('duration_ms') or 0)/1000.0

# Primary loss: correctness dominates. Speed/token terms break ties.
loss=100.0
loss += 250.0*(op_pass-vis_pass)
loss += 0.05*(vis_wall-op_wall)
if op_tok.get('total') and vis_tok.get('total'):
    loss += 0.001*(vis_tok['total']-op_tok['total'])
if vis_exit != 0: loss += 25
if op_exit != 0: loss -= 10

metrics={
 'vis_loss':loss,
 'opencode_pass':op_pass,
 'vis_pass':vis_pass,
 'opencode_wall_seconds':op_wall,
 'vis_wall_seconds':vis_wall,
 'vis_task_seconds':vis_task,
 'opencode_total_tokens':op_tok.get('total',0),
 'opencode_input_tokens':op_tok.get('input',0),
 'opencode_output_tokens':op_tok.get('output',0),
 'vis_total_tokens':vis_tok.get('total',0),
 'vis_input_tokens':vis_tok.get('input',0),
 'vis_output_tokens':vis_tok.get('output',0),
 'opencode_exit':op_exit,
 'vis_exit':vis_exit,
 'opencode_checks_exit':op_check,
 'vis_checks_exit':vis_check,
}
(root/'eval/metrics.json').write_text(json.dumps(metrics, indent=2, sort_keys=True))
for k,v in metrics.items():
    if isinstance(v,(int,float)) and math.isfinite(v): print(f'METRIC {k}={v}')
print(f'ARTIFACT_ROOT {root}')
PY
