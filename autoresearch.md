# Autoresearch: minimize Vis iterations on 4Clojure

## Objective

For each 4Clojure problem, Vis takes one or more agent "iterations" (model
round-trips) before it produces a final `(turn-answer! …)`. We want to
**minimize the number of iterations Vis needs to correctly solve a 4Clojure
task** while still solving it.

Each autoresearch iteration:
- picks 3 fresh 4Clojure problems (deterministic rotation, no repeats),
- runs `bin/vis run --json --provider zai-coding-plan --model glm-5.1`,
- writes the answer to `solution.edn` and judges it locally,
- reports per-task iteration count, tokens, cost, context size,
- aggregates into one primary score: `iter_score` (lower is better).

No iteration-cap or test-set hardcoding is allowed. The judge runs every
problem's actual tests; we don't dial down the gate or cherry-pick problems.

## Metrics

- **Primary**: `iter_score` (count, lower is better)
  `sum over tasks of (iterations if passed else 30)`. Failed tasks are
  penalized at 30 so the optimizer can't get a "good" score by skipping work.
- **Secondary**:
  - `pass_count` (out of 3) — number of solved tasks
  - `pass_rate` (%) — pass_count / 3
  - `mean_iterations_pass` — mean iteration count over passed tasks
  - `total_cost_usd` — Z.ai cost across 3 tasks (USD, glm-5.1 pricing)
  - `total_tokens` — sum of input+output+reasoning tokens
  - `mean_prompt_chars` — mean prompt size we sent
  - `wall_seconds` — total walltime for the 3 tasks

## How to Run

```bash
./autoresearch.sh
```

Outputs `METRIC name=value` lines plus a one-line human summary per task.

Per-iteration artifacts (predictions, raw `--json` envelopes, prompt, judge
verdict) live under `dev/benches/4clojure/autoresearch/<ts>/`. These are
gitignored.

## Files in Scope

- `dev/benches/4clojure/autoresearch_runner.py` — the harness:
  picks tasks, builds prompt, runs Vis, parses envelope, judges, aggregates.
- `dev/benches/4clojure/eval_one.clj` — local 4Clojure evaluator (judge).
- `dev/benches/4clojure/autoresearch_prompt.md` — prompt template
  with `{problem}`, `{solution_path}`, `{sentinel}` placeholders.
- `src/com/blockether/vis/internal/prompt.clj` — Vis core system prompt;
  the gate language here drives how many iterations Vis insists on
  before allowing `(turn-answer! …)`.
- `src/com/blockether/vis/internal/loop.clj` — gate logic
  (`final-answer-structural-criteria-errors`, `answer-with-extension-preflight-mismatch`,
  etc.). Tuning here is allowed iff `./verify.sh --quick` keeps passing
  and existing test suites stay green.

## Off Limits

- `dev/benches/4clojure/problems.json` and `instances.json` — the workload.
- `dev/benches/4clojure/eval_one.clj`'s scoring logic (it IS the judge).
  Bug fixes are fine; relaxing what counts as "passed" is cheating.
- The rest of `src/`, `extensions/` — touch only when there's a concrete
  hypothesis that a change will reduce Vis iterations on this workload.
  Run `./verify.sh --quick` first.

## Constraints

- No hardcoded 4Clojure answers anywhere in prompts, source, or fixtures.
- No bypassing the SCI sandbox or the answer gate to fake "1 iteration".
- Workload uses real Z.ai requests; respect the request quota (see
  `bin/vis providers status zai-coding-plan`).
- Each autoresearch iteration runs in a **fresh** `bin/vis` subprocess
  (fresh JVM, fresh in-memory DB) per task — no cross-task state.
- Judge IS this Claude session; it must compare against the public
  4Clojure tests verbatim, never a hand-written shortcut.

## What's Been Tried

(Filled in as runs accumulate.)
