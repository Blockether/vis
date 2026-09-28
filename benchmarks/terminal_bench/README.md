# Benchmark Vis on Terminal-Bench 4.0

Run the native Linux Vis agent in Harbor’s published Terminal-Bench 4.0 task containers, using Z.ai Coding Plan’s `glm-5.3-flash`. Harbor runs the task verifier and retains trial results, timing, and Vis’s full JSONL trace. This is a product-level run; the adapter does not modify Vis.

## Prepare the runner

You need Podman with enough CPU, memory, and disk for the task images, a Docker Compose v2-compatible provider for `podman compose`, `uv`, a Linux **amd64** Vis bundle at `artifacts/vis-agent-linux-amd64.tar.gz`, and a Coding Plan API key in your shell’s `ZAI_CODING_API_KEY`. Install the `zstd` command for the CPU queue and trace archiving. Do not put the key in a config file, job name, command argument, or tracked file. Harbor’s published task images are amd64; the macOS and Linux arm64 binaries cannot run in them. Task code runs in disposable containers, but the host still downloads images and gives the Vis process access to the API key.

From this directory:

```sh
uv sync --locked
uv run harbor download terminal-bench/terminal-bench@4.0.0 --output-dir artifacts/datasets
export PYTHONPATH="$PWD"
# On macOS, use the socket path for the Podman machine running these trials.
export DOCKER_HOST="unix://$(podman machine inspect --format '{{.ConnectionInfo.PodmanSocket.Path}}' vis-amd64)"
# Set this to your verified, executable Docker Compose v2 provider if Podman has no working default.
export PODMAN_COMPOSE_PROVIDER=/absolute/path/to/docker-compose
podman compose ls
```

A legacy `podman-compose` without `compose ls` fails Harbor’s preflight. The local macOS runner uses the official Docker Compose v5.5.1 arm64 executable, checksum-verified against its release, under ignored `artifacts/docker-compose`. `PYTHONPATH` is necessary for Harbor’s console script to import the local adapter. Keep the Podman machine running during a job.

## Run in small batches

Start with one task and inspect its results before launching more:

```sh
uv run harbor run \
  -p artifacts/datasets/terminal-bench -i interleaved-vigenere \
  -a vis_agent:VisAgent -m zai-coding-plan/glm-5.3-flash \
  -e podman -k 1 -n 1 -o jobs --job-name canary
```

For the next batch, pass `-i` once per task and use `-n 2` only after at least one initial trial contains both a Vis result and a verifier result. Give each batch a new `--job-name` to retain failures and retries. A zero Harbor process exit does **not** imply successful trials: check the summary and each trial’s `result.json` for `exception_info`, `agent_result`, and `verifier_result`.

The adapter uses `--db :memory`, a fresh per-task home, the fixed model, and `--toggles council=false,draft_backend=off`. It writes a credential-free provider selection to that home so Vis can start without an interactive first-run screen; it delivers the key through the process environment, not command arguments. A capture wrapper runs with the bundle’s Python interpreter and redacts known environment credentials from both output streams before writing `/logs/agent/vis-trace.jsonl.gz` and a separate stderr log. Harbor copies both into the trial directory. `populate_context_post_run` extracts token counts, duration, iterations, and a **metered-API price estimate** from the final result frame. `cost_usd` stays null: Coding Plan subscription billing is not a per-trial metered charge. Preserve the trace, trial `result.json`, verifier output, and job `result.json` locally for auditing; `artifacts/`, `jobs/`, and `runs/` are Git-ignored.

Some tasks require a GPU. Do not interpret an unsupported Podman run or missing verifier result as a failed solution; record it separately and use a suitable GPU runner only when available and authorized. Do not change Vis to improve these baseline results.

## Continue across CPU tasks

Once a trial has completed with a Vis result and verifier result, preview the remaining tasks and run a couple of tasks before letting the CPU queue continue:

```sh
uv run python run_suite.py --dry-run
uv run python run_suite.py --max-tasks 2
uv run python run_suite.py
```

The queue skips completed trials, currently running trials, and model attempts that exited without a final result. It retries setup-only failures and trials canceled when a peer fails; keep those attempts separate from scored results. An attempt that reaches the task's agent time limit after calling the model still counts as scored, because Harbor runs the verifier after the timeout; the queue reports it with `agent_timeout=True` and does not run it again. An attempt counts as scored only once the model has streamed output. If the provider refused every call, for example because of an invalid key or an exhausted quota, the attempt is a failed model attempt: the queue stops, and you can retry the task after fixing the cause. Without a verifier result, that timeout counts as a failed model attempt instead. If infrastructure interrupted an attempt after it called the model, inspect its trace and retry it explicitly with `--retry-task NAME` (repeat the option for multiple tasks). Preview that selection with `--dry-run` first; the option rejects tasks without a failed attempt, completed tasks, and active tasks. Selected retries run before new tasks, get new Harbor job names, and retain the original attempt and its partial trace.

The queue keeps up to two tasks running and starts the next one as soon as a slot frees. Each task gets its own Harbor job and an ignored `runs/suite-NNN.log`; interrupted runs keep their previous results. A task that needs 16 GB of memory runs alone, and two tasks share the Podman machine only when their combined memory fits within 16 GB. After validating a job, the queue archives complete gzip traces with zstd and preserves incomplete streams. Run only one queue process at a time. The queue stops rather than inventing scores when Harbor omits results, two consecutive agent errors each end within 10 minutes, or free space falls below 12 GB on the host or Podman VM; it prints why, starts no new tasks, and waits for running tasks to finish before exiting. Slower agent errors, such as reaching the output-token limit or losing the response stream mid-answer, count as scored attempts and do not stop the queue. After validating a job, the queue also removes that task's environment and verifier images from the Podman machine and trims the VM disk, so the host gets the space back; if either step fails, it prints a warning and continues. If the queue still stops for disk space, reclaim space, then rerun it. Three GPU-only tasks stay unattempted on this Podman machine.

## Review results and check the adapter

```sh
uv run python archive_traces.py  # verify SHA-256, then replace finished gzip with zstd
uv run python summarize.py       # writes runs/summary.json
uv run pytest tests
uv run ruff check vis_agent.py capture_trace.py summarize.py run_suite.py archive_traces.py tests
uv run ruff format --check vis_agent.py capture_trace.py summarize.py run_suite.py archive_traces.py tests
```

The report lists every trial attempt, including setup exceptions, and averages rewards only across completed, verified attempts. For a score comparable with other agents, it also scores each task once, by its latest verified model attempt, including agent timeouts: `solved_tasks`, `scored_tasks` and `task_pass_rate`. `task_outcomes` groups those tasks by how their latest attempt ended: `solved`, `failed_tests`, `agent_timeout`, or `vis_error:<type>` when Vis stopped with an error, such as `max-tokens-exceeded` (a response reached the output-token limit) or `http-error` (the provider connection failed). `unscored_model_tasks` lists tasks whose model attempts have no verifier result, such as an out-of-memory kill or an infrastructure interruption; retry interruptions, and count the remaining tasks as failures in a strict score. `repeated_final_errors` lists attempts whose last iterations all failed with the same error, such as `python-worker-retired` after the sandbox's Python worker exited, so every later step failed. It counts trace events, model calls, tool calls, and iteration errors by type without copying prompts or tool output. For interrupted gzip streams, it retains the available counts and marks the trace as truncated; a missing final result does not fabricate token or cost metrics. Where available, it includes verifier test counts, the Vis error type, and numeric usage from the last failed provider call. That error-path usage is not a whole-trial total or a subscription charge.

`runs/provenance.json` records the pinned source commit, image architecture, bundle checksum, dataset version, model, and runner configuration; keep it with the ignored job artifacts. New trials retain the full JSONL event stream with credential values replaced by `[REDACTED]`. The optional archive step replaces each completed, valid gzip with long-window zstd only after the decompressed SHA-256 digest matches; `agent/trace-archive.json` records the digest and byte counts. It preserves incomplete gzip traces unchanged. Early canaries have uncompressed JSONL, and the report reads all three formats. Archiving preserves existing bytes; it does not sanitize older traces.

## Keep credentials out of shared results

The capture wrapper redacts values from environment variables whose names contain `KEY`, `TOKEN`, `SECRET`, `PASSWORD`, or `CREDENTIAL`. It covers plain text, JSON/Python string escapes, URL encoding, and standard or URL-safe Base64, including values split across reads. The queue uses the same wrapper for Harbor logs. Reports and agent metadata redact matching strings and mapping keys without changing numeric metrics. Keep the relevant credential variables available when generating a report from older trials; credentials no longer present in the environment cannot be identified this way.

These scripts do not upload results. Raw jobs, run logs, and trace archives remain Git-ignored. Do not publish them automatically or force-add them to Git. Before sharing a report or artifact, scan it for every credential used by the run and review its contents. Existing artifacts and already-running trials are not rewritten by a code update. Redaction does not detect unknown secrets or every possible encoding, and traces may contain other private task data.
