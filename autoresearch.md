# Autoresearch: Make Vis beat opencode on benchmark tasks

## Objective

Optimize `vis run` until it beats `opencode run` on the same **external benchmark** tasks.

The active suite is no longer repo-native Vis backlog work. Those tasks were removed. The active suite is a curated subset of **Terminal-Bench** tasks: Dockerized terminal/coding/data/sysadmin tasks with `task.yaml`, `Dockerfile`, `run-tests.sh`, and tests.

Goal: Vis superior to opencode on benchmark pass rate first, then speed/context efficiency.

## Metrics

- **Primary**: `vis_loss` (score, lower is better)
  - Correctness dominates: Terminal-Bench test pass/fail is king.
  - Speed/token terms only break ties.
  - Negative/low score means Vis beats opencode.

Secondary metrics:

- `opencode_pass`, `vis_pass`
- `opencode_wall_seconds`, `vis_wall_seconds`, `vis_task_seconds`
- `opencode_total_tokens`, `vis_total_tokens`
- `opencode_input_tokens`, `vis_input_tokens`
- `opencode_output_tokens`, `vis_output_tokens`
- `opencode_exit`, `vis_exit`
- `opencode_checks_exit`, `vis_checks_exit`

## How to Run

Default task:

```bash
./autoresearch.sh
```

Specific task:

```bash
TASK_ID=TB-jsonl-aggregator ./autoresearch.sh
```

Longer budget:

```bash
TASK_ID=TB-large-scale-text-editing SIDE_TIMEOUT_SECONDS=1800 ./autoresearch.sh
```

The script prints `METRIC name=value` lines.

## Active Benchmark Source

Terminal-Bench:

- repo: `https://github.com/harbor-framework/terminal-bench.git`
- pinned ref: `1a6ffa9674b571da0ed040c470cb40c4d85f9b9b`
- active source root: `original-tasks/<slug>`
- local sparse clone cache: `target/bench-sources/terminal-bench`

Terminal-Bench task structure includes:

- Docker environment (`Dockerfile`, usually `docker-compose.yaml` too)
- `task.yaml` with natural-language instruction
- `run-tests.sh`
- `tests/`
- oracle `solution.sh` which agents must not inspect/use

## Curated Task Set

Task definitions live in:

```text
bench/opencode/tasks.jsonl
```

Current curated subset:

1. `TB-hello-world` — easy file operations sanity check.
2. `TB-jsonl-aggregator` — JSONL aggregation/data processing.
3. `TB-regex-log` — regex/log processing.
4. `TB-large-scale-text-editing` — large text editing/tool discipline.
5. `TB-git-multibranch` — Git branch/history manipulation.
6. `TB-cancel-async-tasks` — Python async debugging.
7. `TB-sqlite-db-truncate` — SQLite/database recovery.
8. `TB-cprofiling-python` — Python profiling/performance.
9. `TB-multi-source-data-merger` — multi-source data processing.
10. `TB-tree-directory-parser` — parser/filesystem task.

These were chosen because they are Dockerized, testable, diverse, and do not require changing Vis repo code just to define success.

## Runner Contract

`bench/opencode/run-paired-task.sh` dispatches Terminal-Bench tasks to:

```text
bench/opencode/run-terminal-bench-task.sh
```

For each side, the runner:

1. Fetches the pinned Terminal-Bench source into `target/bench-sources/terminal-bench`.
2. Copies the selected task into `target/vis-bench/<run-id>/<side>/task`.
3. Builds a side-specific Docker image.
4. Starts a side-specific container.
5. Copies tests and `run-tests.sh` into the container.
6. Prompts the agent to operate through `docker exec` against its own container.
7. Runs opencode with:
   `opencode run --format json --model "$OPENCODE_MODEL" --dangerously-skip-permissions ...`
8. Runs Vis with:
   `bin/vis run --json --trace --model "$VIS_MODEL" --db <disposable-db> ...`
9. Runs Terminal-Bench tests inside each container:
   `docker exec -w /app <container> bash /run-tests.sh`
10. Captures outputs, test logs, Docker diffs, `/app` after state, timings, and token metrics.
11. Emits `METRIC` lines.
12. Removes containers/images unless `KEEP_BENCH_IMAGES=1`.

## Models

Defaults:

- opencode: `anthropic/claude-opus-4-7`
- Vis: `anthropic-coding-plan/claude-opus-4-7`

Override:

```bash
OPENCODE_MODEL=anthropic/claude-opus-4-7 \
VIS_MODEL=anthropic-coding-plan/claude-opus-4-7 \
TASK_ID=TB-jsonl-aggregator \
./autoresearch.sh
```

## Files in Scope

Benchmark harness:

- `autoresearch.md`
- `autoresearch.sh`
- `autoresearch.checks.sh`
- `autoresearch.ideas.md`
- `bench/opencode/tasks.jsonl`
- `bench/opencode/run-paired-task.sh`
- `bench/opencode/run-terminal-bench-task.sh`

Vis optimization targets:

- `src/com/blockether/vis/**`
- `extensions/**`
- `test/**`
- `docs/src/**` when behavior changes need docs

## Off Limits

- Do not use or inspect Terminal-Bench `solution.sh` during agent runs.
- Do not train/optimize against hidden test implementation details.
- Do not weaken Vis proof/intent/audit semantics for speed.
- Do not delete user-owned `~/.vis` or opencode global state.
- Do not benchmark Pi in this session.
- Root `README.md` stays tiny.
- `Ctrl+Y` stays unbound.

## Constraints

- Docker required.
- Headless terminal safe.
- Every modified/new Clojure source namespace needs a matching real test namespace.
- SQL remains HoneySQL except allowed SQLite migration DDL.
- `./verify.sh --quick` must pass for kept Vis code changes.

## What's Been Tried

- Removed previous repo-native Vis backlog tasks from active benchmark set.
- Switched active suite to curated Terminal-Bench tasks with pinned source ref and Docker-based pass/fail checks.
- Added a Terminal-Bench runner that gives opencode and Vis separate containers for the same task.
- Baseline not run yet in this chat because user corrected task direction while working tree contains many unrelated dirty changes. Next run should start with `TB-hello-world` or `TB-jsonl-aggregator` after preserving dirty work.
