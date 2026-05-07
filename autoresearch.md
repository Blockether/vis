# Autoresearch: Make Vis beat Pi on Opus coding-agent tasks

## Objective

Make Vis better than Pi on the same Anthropic Opus 4.7 class model for real coding-agent work.

Optimize four primary dimensions:

1. **Context usage** — fewer input/total tokens for equal or better answers.
2. **Speed per task** — compare Pi task time to Vis internal task duration, not Vis CLI/JVM startup.
3. **Correctness** — exact/check/judge score from observed artifacts.
4. **Proof/attestation health** — context optimizations must not break Vis provenance, intents, gates, audit, or honest blocker reporting.

## Out of scope

CLI / TUI startup-speed tasks (`VCLI-*`, `VTUI-*`) are **NO LONGER active
optimization targets**. The earlier wins (VTUI miss-fast-path,
lazy-sqlite registrar split) are merged but we do not chase further
startup speedups. JVM cold-start is acceptable; the agent loop
speed and context usage on REAL coding work matter, the prelude does
not.

## Models

- Pi: `anthropic/claude-opus-4-7`
- Vis: `anthropic-coding-plan/claude-opus-4-7`

Do not use plain `anthropic/claude-opus-4-7` for Vis in this repo unless status proves it authenticated. Current working Vis provider is `anthropic-coding-plan`.

## Metrics

Primary metric:

- `vis_loss` — lower is better.

Important speed metrics:

- `pi_task_seconds = pi_wall_seconds - pi_startup_baseline_seconds`
- `vis_task_seconds = vis result.json ."duration-ms" / 1000`
- `vis_wall_seconds` is secondary only.
- `vis_startup_overhead_seconds = vis_wall_seconds - vis_task_seconds`

Do **not** use Vis shell wall time as the primary task-speed metric. Vis wall includes JVM/Clojure/extension/SQLite startup.

Context metrics:

- `pi_total_tokens`, `pi_input_tokens`, `pi_cache_read_tokens`, `pi_cache_write_tokens`
- `vis_total_tokens`, `vis_input_tokens`, `vis_cached_tokens`
- `context_total_ratio = vis_total_tokens / pi_total_tokens`
- `context_input_ratio = vis_input_tokens / pi_input_tokens`
- `context_total_overhead_tokens`
- `context_input_overhead_tokens`

Correctness metrics:

- `pi_correctness`
- `vis_correctness`
- `quality_floor_pass`
- `strict_task_win`
- `combined_task_win`

Tool-call reliability metrics:

- `pi_tool_call_count`, `pi_tool_result_count`, `pi_tool_success_count`, `pi_tool_error_count`, `pi_tool_event_count`
- `vis_tool_call_count`, `vis_tool_success_count`, `vis_tool_error_count`, `vis_tool_event_count`, `vis_tool_block_count`

These matter because speed/correctness can be faked by not using tools, failing tools silently, or looping through broken calls. Real tasks should prefer fewer successful calls for equal quality, but any tool error must be visible and judged.

Proof/attestation health metrics:

- `proof_floor_pass`
- `vis_provenance_refs_count`
- `vis_done_provenance_refs_count`
- `vis_running_provenance_refs_count`
- `vis_error_blocks_count`
- `vis_answer_blocks_count`

For smoke tasks, `proof_floor_pass` means Vis JSON kept a sane trace with done provenance refs and an answer block. For real coding tasks, add stricter checks/judge criteria: intents/gates/audit must still work and failures must be reported as blockers, not fake success.

## How to Run

Default task:

```bash
./autoresearch.sh
```

Specific agent-comparison task:

```bash
TASK_ID=C2-vis-duration-metric ./autoresearch.sh
```

Specific Vis CLI/TUI startup-speed task:

```bash
TASK_ID=VTUI-open-missing-conversation ./autoresearch.sh
```

Task definitions live in:

```text
bench/opus/tasks.jsonl      # Pi-vs-Vis Opus agent tasks
bench/opus/cli-tasks.jsonl  # Vis command/TUI startup speed tasks
```

Runners:

```text
bench/opus/run-task.sh      # Pi-vs-Vis agent task runner
bench/opus/run-cli-task.sh  # Vis CLI/TUI startup speed runner
```

Both runners print `METRIC name=value` lines for pi-autoresearch.

## CLI/TUI Startup Tasks (NOT ACTIVE)

The `bench/opus/cli-tasks.jsonl` suite (`VCLI-*`, `VTUI-*`) is preserved
for reproducibility of past wins but is **NO LONGER an active
optimization target**. The runner script `bench/opus/run-cli-task.sh`
stays in place; do not delete it. Past wins kept in tree:

- VTUI miss-fast-path: validates `--conversation-id` before requiring
  the heavyweight Lanterna `screen` ns. (commit `e6c59b0`)
- Lazy-sqlite registrar split: tiny manifest registrar + heavy core ns;
  persistance facade auto-loads the heavy ns on first DB op.
  (this commit batch)

Further speed-ups on these CLI/TUI tasks are explicitly out of scope.
Focus the loop on the model-agent tasks below.

## Preflight / Fix Gate

Before treating results as benchmark signal, both runners must pass the smoke task.

If Vis has an obvious substrate error, fix it before benchmark scoring:

- wrong provider/model routing;
- unauthenticated provider path;
- malformed JSON result;
- missing `."duration-ms"`;
- disposable DB path failure;
- trivial answer cannot complete.

If Vis crashes on a real task because of a Vis bug, pause scoring, reproduce, add regression coverage, fix, then rerun the same task.

If Vis completes but is slower or uses more tokens, that is valid autoresearch signal.

## Disposable DB Rule

Every Vis benchmark run deletes and recreates only its target-local DB:

```bash
rm -rf target/vis-bench/<run-id>/vis/db
```

Never delete `~/.vis` automatically.

## Files in Scope

- `bench/opus/**` — benchmark task definitions and harness.
- `src/com/blockether/vis/**` — Vis core improvements.
- `extensions/**` — provider, prompt, persistence, channel, and tooling improvements.
- `test/**` — regression tests.

## Off Limits

- Do not delete user-owned Vis DBs under `~/.vis`.
- Do not hide proof/attestation/evidence-critical context just to reduce tokens.
- Do not remove intent/gate/audit semantics for speed unless an equal or stronger proof path replaces them and tests prove it.
- Do not benchmark against unauthenticated or wrong provider paths.
- Do not claim Vis beats Pi from one task. Use suite rollup once enough tasks exist.

## Real Hard Tasks Added

The suite includes hard tasks that require isolated worktrees and Opus judging over diffs/artifacts:

- `CTX1-context-contract-compact-proof-safe` — reduce prompt/context bloat without hiding proof-critical evidence, intents, audit, tool evidence, or loaded skill bodies.
- `PRES1-presentation-render-contract` — shared presentation/render contract for Markdown/details/tool/system/provider/audit/Mermaid rendering.
- `EXTV2-extension-contract` — extension v2 slots: config, toggles, renderers, backgrounds, lifecycle hooks, custom provider descriptors.
- `CLJEXT1-clojure-test-classpath-tools` — improve the existing Vis Clojure extension with real `z/` helpers for clojure.test/lazytest, deps.edn alias/source-path discovery, Clojure CLI classpath/dependency inspection, JAR entry/source lookup, and classpath source/resource reading. This task is self-contained and carries the completed Tasks 28-34 intent/deferred-intent backbone inline: five-state intent lifecycle, source/owner/parent fields, single intent cursor, query/deferred-report APIs, suggest/accept/defer/resume/abandon APIs, extension cannot self-accept/resume, and lifecycle transitions stay provenance_event -> evidence_bundle -> attestation -> audit.
- `PYEXT1-python-structured-edit-extension` — after CLJEXT1, implement a real Vis Python language extension with working structured editing, tests, docs, and proof/intents/audit preservation.
- `GAME1-python-snake` — after PYEXT1, make Pi and Vis implement a Python Snake game in a fixture repo; Opus judges correctness, playability, tests, code quality, context, speed, and proof honesty.
- `GIT1-single-repo-checkpoints` — single-repo Git checkpoint/time-travel linked to provenance; native workspace/multirepo/submodule scope excluded.
- `BGNOTIFY1-background-process-agent-end-notify` — background process lifecycle, bounded output, patch preconditions, agent-end notifications, OSC 777 adapter.
- `PROVIDER1-provider-visibility-custom-provider` — provider trace normalization, provider-error diagnostics, custom OpenAI-compatible provider config.
- `GUIDANCE1-project-guidance-docs-ux` — compact project guidance/docs/UX cleanup; README stays tiny; Ctrl+Y stays unbound.

These tasks are marked `judge_required: true` and reference:

```text
bench/opus/judge-prompt.md
```

They are intentionally not simple exact-answer tasks. Before running them in autoresearch, use a paired-worktree runner that captures diffs, checks, logs, timings, tokens, and asks Opus judge for strict JSON scoring.

## Current First Optimization Target

Build the **paired-worktree hard-task runner** so judge-required tasks
can run end-to-end. Once the runner exists, the loop iterates through
the hard task sequence above, starting with `CTX1`.

Runner contract (`bench/opus/run-paired-task.sh`):

- For a given `TASK_ID` from `bench/opus/tasks.jsonl`:
  - Create `target/vis-bench/<run-id>/{pi,vis}/worktree/` via
    `git worktree add` from the current HEAD.
  - Drop the task prompt + constraints into each worktree.
  - Run Pi (`anthropic/claude-opus-4-7`) and Vis
    (`anthropic-coding-plan/claude-opus-4-7`) on the same prompt with
    matching tool budgets. Vis writes its disposable DB under
    `target/vis-bench/<run-id>/vis/db` (deleted before each run).
  - Capture per-side: `git diff`, `./verify.sh` exit + tail, full
    stdout/stderr, wall time, internal task time (Vis
    `.\"duration-ms\"`), tokens, tool-call counts/successes/errors,
    Vis trace + proof/intent/audit metrics.
  - Call Opus judge with `bench/opus/judge-prompt.md` over the diffs,
    requiring strict JSON output (correctness 0–100, blockers,
    proof_floor_pass, etc.).
  - Tear down: `git worktree remove --force` after metrics are written.
  - Emit `METRIC name=value` lines for every metric in the
    “Metrics” section above.

Earlier observed shape (kept for context):

- Pi Opus works.
- Vis Opus works through `anthropic-coding-plan/claude-opus-4-7`.
- Vis internal duration is available in `result.json` at `."duration-ms"`.
- Vis trivial prompt carries higher input-token overhead than Pi (real
  context-reduction work waits on `CTX1`).

First likely improvement: introduce or improve a tiny prompt path for trivial/no-tool/no-repo tasks, while preserving full coding-agent context for real work.
