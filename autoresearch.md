# Autoresearch: ctx + prompt → 4Clojure solve rate

## Objective

Improve Vis's solve rate on the 4Clojure subset benchmark. Each run uses the
new `ctx`-first architecture (just `:conversation` + `:tree` + `:defs` in the
sandbox). The model is `glm-5.1` via `zai-coding-plan`. Optimize:

- the engine system prompt (`src/com/blockether/vis/internal/prompt.clj`)
- ctx shape / content (`src/com/blockether/vis/internal/ctx.clj`)
- foundation extension prompt and runtime contribution
- iteration loop trailer rendering

…so the model solves more problems with fewer iterations and lower cost.

## Metrics

- **Primary**: `pass_pct` (higher is better) — share of subset that passed the
  Clojure test runner.
- **Secondary**:
  - `errors` — count of failing runs (provider error / eval error / wrong
    answer). Lower is better.
  - `tokens_total` — total input+output tokens across the subset. Lower is
    better.
  - `cost_usd` — total cost. Informational.
  - `median_seconds` — wall-clock median per problem. Lower is better.

## How to Run

`./autoresearch.sh` — wraps `dev/benches/4clojure/run_subset.sh` with a small
subset (8 instances) for fast iteration. Outputs `METRIC name=value` lines.

For a bigger sweep set `BENCH_INSTANCES=30`.

## Files in Scope

- `src/com/blockether/vis/internal/prompt.clj` — core system prompt body.
- `src/com/blockether/vis/internal/ctx.clj` — ctx shape, tree walker, shape
  inference, REPL trailer.
- `src/com/blockether/vis/internal/loop.clj` — iteration loop wiring, trailer
  composition, multi-fence merge.
- `extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/environment/render.clj`
  — `ctx.runtime` emission.
- `extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/core.clj`
  — foundation extension prompt aggregator.

## Off Limits

- `dev/benches/4clojure/**` — bench harness is the measurement plane, not
  the optimization target.
- `test/**`, `extensions/**/test/**` — tests stay green.
- Any DB / persistence schema.

## Constraints

- `./verify.sh --quick` must pass (format + lint) before any `keep`. The
  checks script runs it automatically.
- Output must be parseable Clojure (no breaking ctx EDN).
- Engine-owned ctx keys: `:conversation`, `:tree`, `:defs`. Anything else
  surfaces as an extension prompt comment.

## What's Been Tried

(start of session — empty)
