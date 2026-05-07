# Autoresearch: Make Vis beat Pi on Opus coding-agent tasks

## Objective

Make Vis better than Pi on the same Anthropic Opus 4.7 class model for real coding-agent work.

Optimize four primary dimensions:

1. **Context usage** — fewer input/total tokens for equal or better answers.
2. **Speed per task** — compare Pi task time to Vis internal task duration, not Vis CLI/JVM startup.
3. **Correctness** — exact/check/judge score from observed artifacts.
4. **Proof/attestation health** — context optimizations must not break Vis provenance, intents, gates, audit, or honest blocker reporting.

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

Specific task:

```bash
TASK_ID=C2-vis-duration-metric ./autoresearch.sh
```

Task definitions live in:

```text
bench/opus/tasks.jsonl
```

Runner:

```text
bench/opus/run-task.sh
```

The runner prints `METRIC name=value` lines for pi-autoresearch.

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

## Current First Optimization Target

The smoke probe showed Vis can answer Opus correctly but uses very large context for a trivial turn.

Initial observed shape:

- Pi Opus works.
- Vis Opus works through `anthropic-coding-plan/claude-opus-4-7`.
- Vis internal duration is available in `result.json` at `."duration-ms"`.
- Vis trivial prompt currently carries high input-token overhead.

First likely improvement: introduce or improve a tiny prompt path for trivial/no-tool/no-repo tasks, while preserving full coding-agent context for real work.
