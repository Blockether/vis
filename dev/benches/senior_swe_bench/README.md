# Vis Senior SWE-Bench Adapter

This package runs Vis as a Harbor custom agent against the pinned
Senior SWE-Bench dataset without checking the benchmark into the Vis repo.

The important rule: run preflight first. A full eval needs a Linux native Vis
artifact, Docker, Harbor, Vis provider credentials inside the task container,
and verifier LLM credentials for the Senior SWE-Bench judges. Preflight checks
the artifact, selected task, image rewrite, config delivery, and mount shape
before Docker or Harbor starts.

## Setup Checklist

1. Install Harbor and make it visible to non-login shells.

   ```bash
   curl -LsSf https://astral.sh/uv/install.sh | sh
   ~/.local/bin/uv tool install harbor
   export PATH="$HOME/.local/bin:$PATH"
   harbor --version
   ```

2. Start Docker for install-only, full smoke, subset, and verifier image runs.

   ```bash
   docker info
   ```

   Preflight modes do not start Docker.

3. Provide a Linux native Vis binary and package it for Harbor.

   ```bash
   VIS_BENCH_NATIVE=target/bench/linux-aarch64/vis \
     ./dev/benches/senior_swe_bench/package_vis.sh
   ```

   The output artifact is `target/bench/vis-agent.tar.gz`. The package step
   rejects non-ELF binaries unless `VIS_BENCH_ALLOW_NON_LINUX_NATIVE=1` is set
   for local packaging tests only.

4. Confirm the adapter can see Harbor and the pinned smoke task.

   ```bash
   ./dev/benches/senior_swe_bench/run_smoke.sh --check
   ```

5. Prepare Vis runtime config for full runs.

   Preferred upload mode:

   ```bash
   VIS_BENCH_CONFIG=/tmp/vis-home/.vis/config.edn \
   VIS_BENCH_REMOTE_HOME=/root \
     ./dev/benches/senior_swe_bench/run_smoke.sh
   ```

   Mount mode, only when the container must see a home directory:

   ```bash
   VIS_BENCH_REMOTE_HOME_MOUNT=/tmp/vis-home \
     ./dev/benches/senior_swe_bench/run_smoke.sh
   ```

   Do not combine `VIS_BENCH_CONFIG` and `VIS_BENCH_REMOTE_HOME_MOUNT` unless
   the extra mount is intentional. The wrapper rejects that combination unless
   `VIS_BENCH_ALLOW_CONFIG_UPLOAD_WITH_HOME_MOUNT=1` is set.

6. Prepare verifier credentials for full judged runs.

   OpenAI-compatible verifier through OpenAI:

   ```bash
   install -m 600 /dev/null /tmp/ssb-verifier-openai.env
   $EDITOR /tmp/ssb-verifier-openai.env
   # OPENAI_API_KEY=...

   export OPENAI_JUDGE_MODEL='openai/YOUR_JUDGE_MODEL'
   export OPENAI_CLASSIFIER_MODEL='openai/YOUR_CLASSIFIER_MODEL'
   ```

   Z.AI verifier using GLM-5.2:

   ```bash
   install -m 600 /dev/null /tmp/ssb-verifier-zai.env
   $EDITOR /tmp/ssb-verifier-zai.env
   # ZAI_API_KEY=...

   VIS_BENCH_VERIFIER_PROVIDER=zai \
   VIS_BENCH_VERIFIER_ENV_FILE=/tmp/ssb-verifier-zai.env \
     ./dev/benches/senior_swe_bench/run_smoke.sh
   ```

   Z.AI mode uses the OpenAI-compatible endpoint
   `https://api.z.ai/api/coding/paas/v4/`, defaults the judge, classifier, and
   validation-agent model to `openai/glm-5.2`, and maps `ZAI_API_KEY` to
   `OPENAI_API_KEY` inside the verifier container. Z.AI documents only
   `tool_choice=auto`, so final forced single-tool emits default to its supported
   `response_format=json_object` mode and are schema-validated after parsing.
   Thinking is disabled only for that final serialization call so it cannot
   consume the output token budget before emitting JSON. Exploratory tool-use
   turns still use normal function calling with the endpoint's default thinking.
   Accounts with separate metered API credit can override the endpoint with
   `VIS_BENCH_VERIFIER_OPENAI_BASE_URL=https://api.z.ai/api/paas/v4/`.

   LM Studio verifier on the macOS host:

   ```bash
   export LM_STUDIO_MODEL_ID='YOUR_LOCAL_MODEL_ID'

   VIS_BENCH_VERIFIER_PROVIDER=lmstudio \
   VIS_BENCH_VERIFIER_OPENAI_BASE_URL=http://host.docker.internal:1234/v1 \
   VIS_BENCH_VERIFIER_MODEL="$LM_STUDIO_MODEL_ID" \
     ./dev/benches/senior_swe_bench/run_smoke.sh
   ```

   The LM Studio model id must match `/v1/models`. The wrapper adds the
   `openai/` litellm prefix when omitted, injects a dummy non-secret
   `OPENAI_API_KEY=lmstudio` if no key is present, and defaults
   `VIS_BENCH_VERIFIER_TOOL_CHOICE_COMPAT=required` so Senior SWE-Bench forced
   tool emits use the string form accepted by LM Studio. That compatibility
   mode also enables parsing plain JSON assistant text when a local model
   returns the forced emit payload without an OpenAI tool-call wrapper. For
   forced single-tool emits, the wrapper also sends LM Studio a
   `response_format=json_schema` request so final judge output is constrained
   without changing the exploratory tool-use turns. LM Studio runs also default
   `VIS_BENCH_VERIFIER_TIMEOUT_MULTIPLIER=3` because local judge calls can
   exceed Harbor's default verifier timeout.

## Comparable Provider Effort

The benchmark controls both agents with the same provider-native value:
`VIS_BENCH_REASONING_EFFORT=high|max` (default `high`). Vis receives that value
through `--reasoning-effort`. Pi receives `high` for provider `high` and
`xhigh` for provider `max`; the generated Pi model maps those levels back to
the Z.AI values `high` and `max` on the wire. The command ledger records the
provider-native effort and Pi translation separately.

Every Pi run installs a deterministic `~/.pi/agent/models.json` entry for
GLM-5.2 into the task container: Pi `0.73.1`, one-million-token context,
32,768-token output cap, Z.AI tool streaming, and only the `high`/`max` effort
rungs. Comparison reports reject missing or mismatched effort controls, a
different output cap or Pi config hash, and any Vis run whose structured
`eval.valid?` evidence is not true for every task.

Reports keep spend completeness separate from rollout telemetry completeness.
A Vis `vis.trace.jsonl` or collected pi `agent/pi.txt` is required before a
Vis-vs-pi iteration/tool-efficiency comparison is marked authoritative.

## Runner Comparison

| Mode | Command | Starts Docker | Starts Harbor | Needs Vis config | Needs verifier LLM | What it proves |
| --- | --- | --- | --- | --- | --- | --- |
| Adapter check | `run_smoke.sh --check` | No | No | No | No | Harbor CLI is present and the smoke task can be listed. |
| Preflight install | `VIS_BENCH_PREFLIGHT_ONLY=1 VIS_BENCH_INSTALL_ONLY=1 run_smoke.sh` | No | No | No | No | Artifact, task copy, image selection, and command ledger shape. |
| Preflight full | `VIS_BENCH_PREFLIGHT_ONLY=1 VIS_BENCH_CONFIG=... run_smoke.sh` | No | No | Yes | Optional | Full-run config and mount decisions before spending Docker time. |
| Install-only smoke | `VIS_BENCH_INSTALL_ONLY=1 run_smoke.sh` | Yes | Yes | No | No | Harbor can upload and install the Vis artifact in the task container. |
| Full smoke | `VIS_BENCH_CONFIG=... VIS_BENCH_VERIFIER_PROVIDER=... run_smoke.sh` | Yes | Yes | Yes | Yes | One selected task can run Vis and be judged. |
| pi.dev smoke | `ZAI_API_KEY=... run_pi_smoke.sh` | Yes | Yes | No | Yes | The same selected task runs through Harbor's pi agent. |
| Subset run | `run_subset.sh public-5` | Yes | Yes | Depends on mode | Depends on mode | Runs each task in a subset and writes an aggregate ledger. |
| Paperless verifier image smoke | `verify_paperless_python_dev_image.sh` | Yes | No | No | No | The derived paperless image can install verifier dependencies and run oracle verification. |

## Standalone Commands

Use these from the repository root.

List the smoke task:

```bash
python3 dev/benches/senior_swe_bench/list_tasks.py \
  --subset dev/benches/senior_swe_bench/subsets/smoke.json
```

Package Vis for Harbor:

```bash
VIS_BENCH_NATIVE=target/bench/linux-aarch64/vis \
  ./dev/benches/senior_swe_bench/package_vis.sh
```

Check local plumbing:

```bash
./dev/benches/senior_swe_bench/run_smoke.sh --check
```

Run install preflight without Docker or Harbor:

```bash
RUN_ID=preflight-install-$(date -u +%Y%m%d-%H%M%S) \
VIS_BENCH_PREFLIGHT_ONLY=1 \
VIS_BENCH_INSTALL_ONLY=1 \
  ./dev/benches/senior_swe_bench/run_smoke.sh
```

Run full preflight with uploaded Vis config:

```bash
RUN_ID=preflight-full-$(date -u +%Y%m%d-%H%M%S) \
VIS_BENCH_PREFLIGHT_ONLY=1 \
VIS_BENCH_CONFIG=/tmp/vis-home/.vis/config.edn \
VIS_BENCH_REMOTE_HOME=/root \
  ./dev/benches/senior_swe_bench/run_smoke.sh
```

Run non-credential install-only smoke:

```bash
RUN_ID=install-only-$(date -u +%Y%m%d-%H%M%S) \
VIS_BENCH_INSTALL_ONLY=1 \
  ./dev/benches/senior_swe_bench/run_smoke.sh
```

Run a full smoke with OpenAI verifier credentials:

```bash
RUN_ID=full-openai-$(date -u +%Y%m%d-%H%M%S) \
VIS_PROVIDER=zai-coding-plan \
VIS_MODEL=glm-5.2 \
VIS_BENCH_CONFIG=/tmp/vis-home/.vis/config.edn \
VIS_BENCH_REMOTE_HOME=/root \
VIS_BENCH_VERIFIER_PROVIDER=openai \
VIS_BENCH_VERIFIER_ENV_FILE=/tmp/ssb-verifier-openai.env \
VIS_BENCH_VERIFIER_JUDGE_MODEL="$OPENAI_JUDGE_MODEL" \
VIS_BENCH_VERIFIER_CLASSIFIER_MODEL="$OPENAI_CLASSIFIER_MODEL" \
  ./dev/benches/senior_swe_bench/run_smoke.sh
```

Run a full smoke with Z.AI verifier credentials:

```bash
RUN_ID=full-zai-$(date -u +%Y%m%d-%H%M%S) \
VIS_PROVIDER=zai-coding-plan \
VIS_MODEL=glm-5.2 \
VIS_BENCH_CONFIG=/tmp/vis-home/.vis/config.edn \
VIS_BENCH_REMOTE_HOME=/root \
VIS_BENCH_VERIFIER_PROVIDER=zai \
VIS_BENCH_VERIFIER_ENV_FILE=/tmp/ssb-verifier-zai.env \
  ./dev/benches/senior_swe_bench/run_smoke.sh
```

Run the matching pi.dev smoke with the same task, model, and verifier:

```bash
RUN_ID=pi-full-zai-$(date -u +%Y%m%d-%H%M%S) \
VIS_BENCH_AGENT_ENV_FILE=/tmp/ssb-verifier-zai.env \
VIS_BENCH_VERIFIER_PROVIDER=zai \
VIS_BENCH_VERIFIER_ENV_FILE=/tmp/ssb-verifier-zai.env \
  ./dev/benches/senior_swe_bench/run_pi_smoke.sh
```

`run_pi_smoke.sh` pins `@mariozechner/pi-coding-agent` to `0.73.1`, installs
the benchmark's explicit GLM-5.2 model definition, and records Pi's `zai` route as
`https://api.z.ai/api/coding/paas/v4`. The provider label differs from Vis's
`zai-coding-plan`, which uses z.ai's native Anthropic transport at
`https://api.z.ai/api/anthropic/v1`. Both are validated as transports for the
same `zai-coding-plan` billing route; the pay-as-you-go
`https://api.z.ai/api/paas/v4` endpoint does not satisfy that gate.

For a pi.dev subset, `VIS_BENCH_AGENT=pi` is accepted as a shorthand for the
benchmark's `PiGlm52Agent` wrapper:

```bash
SUBSET_RUN_ID=pi-overnight-$(date -u +%Y%m%d-%H%M%S) \
VIS_BENCH_AGENT=pi \
VIS_BENCH_AGENT_LABEL=pi.dev \
VIS_BENCH_AGENT_MODEL=zai/glm-5.2 \
VIS_BENCH_AGENT_ENV_FILE=/tmp/ssb-verifier-zai.env \
VIS_BENCH_VERIFIER_PROVIDER=zai \
VIS_BENCH_VERIFIER_ENV_FILE=/tmp/ssb-verifier-zai.env \
  ./dev/benches/senior_swe_bench/run_eval.sh overnight --max-tasks 10
```

Run a full smoke with an LM Studio verifier:

```bash
RUN_ID=full-lmstudio-$(date -u +%Y%m%d-%H%M%S) \
VIS_PROVIDER=zai-coding-plan \
VIS_MODEL=glm-5.2 \
VIS_BENCH_CONFIG=/tmp/vis-home/.vis/config.edn \
VIS_BENCH_REMOTE_HOME=/root \
VIS_BENCH_VERIFIER_PROVIDER=lmstudio \
VIS_BENCH_VERIFIER_OPENAI_BASE_URL=http://host.docker.internal:1234/v1 \
VIS_BENCH_VERIFIER_MODEL="$LM_STUDIO_MODEL_ID" \
  ./dev/benches/senior_swe_bench/run_smoke.sh
```

Run `public-5` in install-only mode:

```bash
SUBSET_RUN_ID=install-only-$(date -u +%Y%m%d-%H%M%S) \
VIS_BENCH_INSTALL_ONLY=1 \
  ./dev/benches/senior_swe_bench/run_subset.sh public-5
```

Run `public-5` as full judged eval:

```bash
SUBSET_RUN_ID=full-openai-$(date -u +%Y%m%d-%H%M%S) \
VIS_PROVIDER=zai-coding-plan \
VIS_MODEL=glm-5.2 \
VIS_BENCH_CONFIG=/tmp/vis-home/.vis/config.edn \
VIS_BENCH_REMOTE_HOME=/root \
VIS_BENCH_VERIFIER_PROVIDER=openai \
VIS_BENCH_VERIFIER_ENV_FILE=/tmp/ssb-verifier-openai.env \
VIS_BENCH_VERIFIER_JUDGE_MODEL="$OPENAI_JUDGE_MODEL" \
VIS_BENCH_VERIFIER_CLASSIFIER_MODEL="$OPENAI_CLASSIFIER_MODEL" \
  ./dev/benches/senior_swe_bench/run_subset.sh public-5
```

Continue a subset after individual task failures:

```bash
VIS_BENCH_CONTINUE_ON_FAILURE=1 \
  ./dev/benches/senior_swe_bench/run_subset.sh public-5
```

Verify the paperless derived python-dev image without credentials or a model:

```bash
./dev/benches/senior_swe_bench/verify_paperless_python_dev_image.sh
```

## Config Delivery

Full runs need Vis provider credentials inside the task container. The wrapper
supports two delivery modes.

Upload mode copies one config file into the container. Prefer this mode because
it avoids exposing or mutating the host home directory:

```bash
VIS_BENCH_CONFIG=/tmp/vis-home/.vis/config.edn \
VIS_BENCH_REMOTE_HOME=/root \
  ./dev/benches/senior_swe_bench/run_smoke.sh
```

Mount mode bind-mounts an eval home directory and sets both `HOME` and Java
`user.home` to the remote path:

```bash
VIS_BENCH_REMOTE_HOME_MOUNT=/tmp/vis-home \
VIS_BENCH_REMOTE_HOME=/tmp/vis-home \
VIS_BENCH_REMOTE_HOME_MOUNT_MODE=rw \
  ./dev/benches/senior_swe_bench/run_smoke.sh
```

Read-only home mounts are rejected. Vis opens `~/.vis/vis.log` during CLI
startup before provider execution, so a read-only mounted home fails even when
the provider config itself is readable.

Native artifacts built before runtime config-home resolution was fixed may have
`/root/.vis` baked in from the Linux build container. Rebuild and repackage the
native image when possible. For old artifacts, mount or upload config to `/root`.

## Python-Dev Image Handling

Some Senior SWE-Bench verifier images are missing Python development headers.
`paperless-ngx-perf-document-counts` can build `zxing-cpp` during verifier setup
and fail with `missing: Python3_INCLUDE_DIRS Development.Module`.

The runner defaults `VIS_BENCH_PREPARE_PYTHON_DEV_IMAGE=auto`. For known
affected full eval tasks, it builds a derived image before Harbor starts and
rewrites only the copied task at:

```text
dev/benches/senior_swe_bench/results/<run-id>/dataset/tasks/<task-id>/task.toml
```

The pinned dataset cache is not modified.

Useful overrides:

```bash
VIS_BENCH_PREPARE_PYTHON_DEV_IMAGE=1   # force derived image
VIS_BENCH_PREPARE_PYTHON_DEV_IMAGE=0   # disable when allowed
VIS_BENCH_TASK_IMAGE="$TASK_IMAGE"     # use a prebuilt image
VIS_BENCH_PYTHON_DEV_IMAGE="$TAG"      # choose the derived image tag
```

## Result Analysis

Each `run_smoke.sh` invocation writes:

```text
dev/benches/senior_swe_bench/results/<run-id>/
```

Important files:

| File | Meaning |
| --- | --- |
| `command.json` | Exact selected task, selected agent/model, artifact path, Vis provider/model, config delivery, mounts, task base image, and selected task image. |
| `preflight.json` | Artifact metadata, Linux ELF validation, native SHA, credential requirement result. |
| `preflight-only.txt` | Present when `VIS_BENCH_PREFLIGHT_ONLY=1` exited before Docker and Harbor. |
| `preflight-failure.json` | Structured failure for guard failures before Harbor starts. |
| `harbor.log` | Raw Harbor output for Docker/agent/verifier runs. |
| `harbor-output/collection.json` | Collected Harbor job/trial artifacts when Harbor started. |
| `summary.json` | Normalized completion, score, agent identity, token usage, and failure outcome from `metrics.py`. |
| `patches/<task-id>.diff` | Agent patch fallback when Harbor verifier artifacts do not include one. |
| `vis-traces/<task-id>/vis.trace.jsonl` | Vis trace copied from task artifacts when present. |

Inspect a preflight run:

```bash
run=dev/benches/senior_swe_bench/results/"$RUN_ID"
cat "$run/preflight-only.txt"
python3 -m json.tool "$run/command.json" | sed -n '1,120p'
python3 -m json.tool "$run/preflight.json" | sed -n '1,120p'
```

Inspect a pre-Harbor failure:

```bash
run=dev/benches/senior_swe_bench/results/"$RUN_ID"
python3 -m json.tool "$run/preflight-failure.json"
```

Inspect a Harbor run:

```bash
run=dev/benches/senior_swe_bench/results/"$RUN_ID"
tail -200 "$run/harbor.log"
python3 -m json.tool "$run/summary.json" | sed -n '1,180p'
python3 -m json.tool "$run/harbor-output/collection.json" | sed -n '1,160p'
find "$run/harbor-output" -maxdepth 4 -type f | sort
```

Interpret common outcomes:

| Symptom | Check | Likely fix |
| --- | --- | --- |
| `preflight_failed` | `preflight-failure.json` and `preflight.json` | Repackage `target/bench/vis-agent.tar.gz`, provide a Linux ELF, or fix config path. |
| `verifier_openai_api_key_missing` | `preflight-failure.json` | Put `OPENAI_API_KEY` in `VIS_BENCH_VERIFIER_ENV_FILE` or export it. |
| `verifier_lmstudio_model_missing` | `preflight-failure.json` | Set `VIS_BENCH_VERIFIER_MODEL` or both verifier model overrides. |
| `verifier_zai_api_key_missing` or z.ai `Authentication Failed` | `preflight-failure.json`, verifier `test-stdout.txt`, or direct `/models` probe | Put a valid `ZAI_API_KEY` or z.ai-compatible `OPENAI_API_KEY` in `VIS_BENCH_VERIFIER_ENV_FILE`. |
| `remote_home_mount_invalid` | `preflight-failure.json` | Use `VIS_BENCH_REMOTE_HOME_MOUNT_MODE=rw` or switch to config upload mode. |
| Docker unavailable | `docker-preflight.txt` | Start Docker before full or install-only runs. |
| Python header verifier failure | `harbor.log` | Let `auto` build the python-dev image, force it with `VIS_BENCH_PREPARE_PYTHON_DEV_IMAGE=1`, or provide `VIS_BENCH_TASK_IMAGE`. |

Subset runs write an aggregate ledger under:

```text
dev/benches/senior_swe_bench/results/subsets/<subset>-<subset-run-id>.json
```

Inspect it with:

```bash
python3 -m json.tool dev/benches/senior_swe_bench/results/subsets/"$LEDGER".json
```

The ledger's `completion.authoritative` is true only when every expected task
completed the verifier, rubric and taste judges, and required design-task
validation. `usage.complete` additionally requires input, cached-input, output,
and total tokens for every task. `comparison_ready` requires both conditions.

Token fields use one schema for both harnesses:

- `input_tokens` includes cached input.
- `cached_input_tokens` is the cached subset of input.
- `uncached_input_tokens` is `input_tokens - cached_input_tokens`.
- `output_tokens` includes reasoning when the provider counts it that way;
  `reasoning_tokens` is reported separately when available.
- `total_tokens` is input plus output.
- `reported_cost_usd` is only a provider-reported value. Missing cost is `null`,
  never zero or an estimate.

## Report Generation

Render a static HTML report for a completed Vis run:

```bash
python3 dev/benches/senior_swe_bench/render_report.py \
  --vis-run dev/benches/senior_swe_bench/results/"$VIS_RUN_ID" \
  --out dev/benches/senior_swe_bench/results/reports/vis-"$VIS_RUN_ID".html \
  --fair-provider "z.ai plan" \
  --fair-model glm-turbo
```

Render a comparison report against a Harbor-shaped pi.dev result directory:

```bash
python3 dev/benches/senior_swe_bench/render_report.py \
  --vis-run dev/benches/senior_swe_bench/results/"$VIS_RUN_ID" \
  --pi-run dev/benches/senior_swe_bench/results/"$PI_RUN_ID" \
  --out dev/benches/senior_swe_bench/results/reports/vis-vs-pidev.html \
  --fair-provider "z.ai plan" \
  --fair-model glm-turbo
```

Render a comparison report from normalized pi.dev JSON:

```bash
python3 dev/benches/senior_swe_bench/render_report.py \
  --vis-run dev/benches/senior_swe_bench/results/"$VIS_RUN_ID" \
  --pi-json dev/benches/senior_swe_bench/results/"$PI_RESULT".json \
  --out dev/benches/senior_swe_bench/results/reports/vis-vs-pidev.html \
  --fair-provider "z.ai plan" \
  --fair-model glm-turbo
```

If pi.dev data is not available, omit `--pi-run` and `--pi-json`. The report
keeps the comparison column as `pending-data` with the fair comparison params.

For multi-task or overnight runs, compare the two subset ledgers directly. This
emits JSON, Markdown, and HTML and shows every evidence gate that prevents an
authoritative claim:

```bash
python3 dev/benches/senior_swe_bench/compare_subsets.py \
  dev/benches/senior_swe_bench/results/subsets/"$VIS_LEDGER".json \
  dev/benches/senior_swe_bench/results/subsets/"$PI_LEDGER".json \
  --name vis-vs-pi-overnight
```

The comparison requires matching task ids, Harbor task checksums, task images,
dataset commit, model, provider billing route with a valid harness transport,
Harbor version, and verifier
configuration. It also requires a pinned pi version, an identified clean Vis
artifact, complete scores, complete agent token accounting, and captured
verifier overhead on both sides, plus complete agent rollout telemetry on both
sides. Every task must also carry a successful final credential-redaction scan.
The reports keep agent spend separate from
judge/classifier/validation-agent spend. Provider-reported USD cost does not
include subscription fees. A partial report is still emitted, but
`state.authoritative` remains false and the failed gates explain why.

Each run-local verifier copy records usage-only events in
`/logs/verifier/llm_usage.jsonl`; prompts, responses, and credentials are not
persisted. The adaptation also pins LiteLLM `1.92.0` and installs its direct
completion path's required FastAPI and orjson packages. Design-task
validation-agent usage is recovered from its retained
mini-swe-agent or Claude Code trajectory. Missing provider fields remain
`null` and are exposed through per-field coverage instead of being estimated.

## Environment Reference

| Variable | Default | Purpose |
| --- | --- | --- |
| `RUN_ID` | UTC timestamp | Output directory name for `run_smoke.sh`. |
| `SUBSET_FILE` | `subsets/smoke.json` | Single-task subset consumed by `run_smoke.sh`. |
| `SUBSET_RUN_ID` | UTC timestamp | Aggregate run id for `run_subset.sh`. |
| `VIS_BENCH_ARTIFACT` | `target/bench/vis-agent.tar.gz` | Harbor artifact built by `package_vis.sh`. |
| `VIS_BENCH_NATIVE` | unset | Linux native Vis binary packaged into the artifact. |
| `VIS_PROVIDER` | `zai-coding-plan` | Provider passed to Vis inside the task container. |
| `VIS_MODEL` | `glm-5.2` | Model passed to Vis inside the task container. |
| `VIS_BENCH_AGENT` | `dev.benches.senior_swe_bench.agent:VisInstalledAgent` | Harbor agent import path or built-in name. Use `pi` for pi.dev. |
| `VIS_BENCH_AGENT_LABEL` | `Vis` | Stable harness label recorded in result ledgers. |
| `VIS_BENCH_AGENT_MODEL` | `VIS_MODEL` | Harbor agent model. pi.dev uses `zai/glm-5.2`. |
| `VIS_BENCH_AGENT_ENV_FILE` | verifier env file | Credential source for non-Vis agents; pi.dev with `zai/*` requires `ZAI_API_KEY`. |
| `VIS_BENCH_AGENT_VERSION` | unset | Installed-agent version passed to Harbor. `run_pi_smoke.sh` pins `0.73.1`. |
| `VIS_BENCH_AGENT_ENDPOINT` | derived for Vis | Effective model endpoint recorded for comparison. pi.dev defaults to the z.ai coding-plan endpoint. |
| `VIS_BENCH_AGENT_ROUTE` | provider for Vis | Provider billing/service route recorded for comparison. pi.dev defaults to `zai-coding-plan`. |
| `VIS_BENCH_CONFIG` | unset | Host config file to upload into the task container. |
| `VIS_BENCH_REMOTE_HOME` | `/tmp/vis-home` | Remote home used for `HOME` and Java `user.home`. |
| `VIS_BENCH_REMOTE_HOME_MOUNT` | unset | Host directory to bind-mount as the remote home. |
| `VIS_BENCH_REMOTE_HOME_MOUNT_MODE` | `rw` | Mount mode. Read-only is rejected. |
| `VIS_BENCH_PREFLIGHT_ONLY` | `0` | Exit after host preflight and dataset/task preparation. |
| `VIS_BENCH_INSTALL_ONLY` | `0` | Ask Harbor to install the agent without invoking the model. |
| `VIS_BENCH_ALLOW_NO_CONFIG` | `0` | Allow full runs without discovered Vis config. Use only for pre-provisioned task images. |
| `VIS_BENCH_VERIFIER_PROVIDER` | unset | `openai`, `lmstudio`, or `zai` verifier mode. |
| `VIS_BENCH_VERIFIER_ENV_FILE` | unset | Env file passed to Harbor for verifier secrets. |
| `VIS_BENCH_VERIFIER_MODEL` | unset, `glm-5.2` for `zai` | Shared verifier model, useful for LM Studio and Z.AI. |
| `VIS_BENCH_VERIFIER_JUDGE_MODEL` | unset | Explicit Senior SWE-Bench judge model. |
| `VIS_BENCH_VERIFIER_CLASSIFIER_MODEL` | unset | Explicit patch-classifier model. |
| `VIS_BENCH_VERIFIER_OPENAI_BASE_URL` | env OpenAI base URL or unset | OpenAI-compatible verifier endpoint. |
| `VIS_BENCH_VERIFIER_TOOL_CHOICE_COMPAT` | `required` for LM Studio, unset otherwise | Rewrites forced OpenAI function `tool_choice` objects to a compatible form in the copied verifier and enables plain-JSON fallback for missing tool-call wrappers. Use `none` to disable. |
| `VIS_BENCH_VERIFIER_RESPONSE_FORMAT_COMPAT` | `json_schema` for LM Studio, `json_object` for z.ai | Selects the forced single-tool response-format strategy used by `verifier_adapt.py`. Use `none` to disable. |
| `VIS_BENCH_VERIFIER_TIMEOUT_MULTIPLIER` | `3` for LM Studio and z.ai, unset otherwise | Passed to Harbor as `--verifier-timeout-multiplier`; increase for slow judges. |
| `VIS_BENCH_PREPARE_PYTHON_DEV_IMAGE` | `auto` | Build/use python-dev image for known affected tasks. |
| `VIS_BENCH_TASK_IMAGE` | unset | Override the selected task image. |
| `VIS_BENCH_CONTINUE_ON_FAILURE` | `0` | Continue subset execution after a task failure. |

Results are written under `dev/benches/senior_swe_bench/results/` and are
ignored by git.
