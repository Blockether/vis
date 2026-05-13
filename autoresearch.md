# Autoresearch: Vis on SWE-bench Lite (glm-5.1)

## Objective
Maximize **% resolved** when Vis solves a frozen 30-instance subset of
SWE-bench Lite, pinned to provider `zai-coding-plan` and model `glm-5.1`.
We change Vis's prompts, agent loop, SCI runtime, and bundled tools —
never the benchmark, never the model, never the subset.

## Metrics
- **Primary**: `resolved_pct` (%, higher is better)
- **Secondary**:
  - `resolved_count` — absolute solved out of 30
  - `patches_emitted` — non-empty diffs returned
  - `empty_patches` — agent gave up / produced nothing
  - `errors` (+ per-category `err_*`) — checkout / vis-exit / timeout breakdown
  - `median_seconds`, `p95_seconds` — per-instance latency
  - `tokens_total`, `cost_usd` — spend per iteration
  - `instances_run` — how far the budget guard let us go

## How to Run
`./autoresearch.sh` — ~30–60 min per iteration on N=30.

Manual full-Lite validation (do NOT use as loop primary):
`SWEBENCH_SUBSET=full ./autoresearch.sh`

## Files in Scope
- `src/com/blockether/vis/internal/prompt.clj` — system prompts, tool descriptions
- `src/com/blockether/vis/internal/loop.clj` — agent loop / journal compaction
- `src/com/blockether/vis/internal/format.clj` — tool result formatting
- `src/com/blockether/vis/internal/render.clj` — context rendering
- `extensions/languages/clojure/**` — language-specific tools
- `extensions/common/vis-foundation/**` — base tool set
- `dev/swe-bench/vis_agent.py` — prompt template (per-instance instructions)

## Off Limits
- `dev/swe-bench/instances.json` — frozen subset, **never** edit
- `dev/swe-bench/subset.py`, `run_subset.sh`, `metrics.py` — harness wiring
- `test/**` — don't move the goalposts
- `extensions/providers/vis-provider-zai/**` — model/provider held constant
- `VIS_MODEL`, `VIS_PROVIDER` env vars — pinned to `glm-5.1` / `zai-coding-plan`
- `bin/vis`, `build.clj` — change separately if absolutely needed

## Constraints
- `clojure -M:test` must pass (enforced via `autoresearch.checks.sh`)
- No new runtime deps without justification in the description
- Per-instance wallclock capped at 900s (`VIS_INSTANCE_TIMEOUT`)
- Z.ai monthly request budget: 4000. Inner-loop budget guard: 300/iter.
- Model: `glm-5.1` only. Same temperature/sampling across iterations.

## Two-tier validation
- **Inner loop (this script)**: N=30 stratified subset, ~150 requests/iter.
- **Validation (manual)**: N=300 full Lite, run before declaring victory
  on a non-trivial improvement. Burns ~1500 requests; budget it.

## What's Been Tried
_(updated as we go)_

- _baseline pending_
