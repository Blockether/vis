# Autoresearch: minimize Vis iterations on a mixed Clojure workload

## Objective

For each task, Vis spends one or more agent "iterations" before producing
a final `(turn-answer! …)`. We **minimize the sum of iterations Vis needs
to correctly solve a fixed set of tasks** without sacrificing pass rate.
We also keep context (system + extension prompts and user prompt) as
small as possible.

Each autoresearch iteration runs the **fixed workload** (see
`FIXED_WORKLOAD` in `dev/benches/4clojure/autoresearch_runner.py`):

- 4Clojure #5  — Lists: conj
- 4Clojure #15 — Double Down
- filewrite fw-005 — sum a sequence

Same three problems every iteration so `iter_score` is comparable
across runs and only code changes (Vis core / prompts / extensions)
can move the metric. The anti-overfit guard rotates through other
tasks via `./autoresearch.revalidate.sh` (sets `AUTORESEARCH_ROTATE=1`).

Per task:
- runs `bin/vis run --full-trace-json-stream --provider zai-coding-plan --model glm-5.1`
  in a fresh tmp workdir,
- judges locally (`eval_one.clj` for 4Clojure, a `:pass`-or-throw verify
  form for filewrite),
- aggregates into one primary score: `iter_score` (lower is better).

No iteration-cap or test-set hardcoding is allowed. The judges run the
problems' actual tests verbatim; we never cherry-pick tasks or relax
verification.

When Vis times out before emitting an iteration count, the runner
falls back to a wall-clock estimate calibrated on this machine:
`max(1, round((wall_seconds - 5) / 7))` (5 s JVM bootstrap, ~7 s per
iter; configurable via `AUTORESEARCH_WALL_BOOTSTRAP` and
`AUTORESEARCH_WALL_PER_ITER`).

## Metrics

- **Primary**: `iter_score` (count, lower is better)
  `sum over tasks of (iterations if passed else 30)`. Failed tasks are
  penalized at 30 so the optimizer can't get a "good" score by skipping work.
- **Secondary** (we actively shrink the * starred * ones too):
  - `pass_count` (out of N tasks) — number of solved tasks. MUST stay at
    `task_count` or `iter_score` is meaningless.
  - `pass_rate` (%) — pass_count / task_count
  - `mean_iterations_pass` — mean iter count over passed tasks (sanity)
  - `max_iterations` — slowest passed task's iter count
  - `block_errors` * — number of per-form eval errors (SCI throw,
    `(spit ...)` blocked, `v/patch :search must be non-blank`, ...).
    Each one wastes an iteration. Driving this to 0 is one of the
    biggest iteration-shrink levers we have.
  - `total_cost_usd` * — Z.ai cost across tasks (USD, glm-5.1 pricing)
  - `total_tokens` * — sum of input+output+reasoning tokens
  - `mean_prompt_chars` * — mean USER-prompt size sent per task
  - `system_prompt_chars` * — Vis core system prompt size (constant per
    Vis build; cached, recomputed only when `prompt.clj` changes).
  - `foundation_prompt_chars` * — `v/` extension prompt size.
  - `z_prompt_chars` * — `z/` extension prompt size.
  - `ext_prompt_chars` * — system + foundation + z prompt sizes combined.
  - `context_score` * — `ext_prompt_chars + mean_prompt_chars`, the
    combined floor of context sent on iteration 1.
  - `z_patch_calls` / `v_patch_calls` — count of each patch surface used.
  - `z_patch_share` (%) — share of patch ops that went through `z/patch`.
    For Clojure/EDN edits `z/patch` is the right tool; higher is better.
  - `wall_seconds` — total walltime

  Keep-blockers (auto-discard if violated, even with iter_score win):
  - `pass_count` drops below `task_count`
  - `block_errors` increases by more than 1 vs. previous keep
  - `system_prompt_chars` or `ext_prompt_chars` increases by more than 5%
    vs. previous keep
  - `mean_prompt_chars` increases by more than 25% vs. previous keep
  - `total_cost_usd` increases by more than 50% vs. previous keep

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
  picks tasks, builds prompt, runs Vis, parses trace, judges, aggregates.
- `dev/benches/4clojure/eval_one.clj` — 4Clojure judge.
- `dev/benches/4clojure/autoresearch_prompt.md` — 4Clojure prompt template.
- `dev/benches/filewrite/autoresearch_prompt.md` — filewrite prompt template.
- `src/com/blockether/vis/internal/prompt.clj` — Vis core system prompt;
  the gate language here drives how many iterations Vis insists on
  before allowing `(turn-answer! …)`.
- `src/com/blockether/vis/internal/loop.clj` — gate logic
  (`final-answer-structural-criteria-errors`,
  `answer-with-extension-preflight-mismatch`, etc.). Tuning here is
  allowed iff `./verify.sh --quick` keeps passing.
- `extensions/common/vis-foundation/src/com/blockether/vis/ext/foundation/core.clj`
  and its sibling sources (`editing/core.clj`, `introspection.clj`,
  `nudges.clj`) — the `v/` extension prompt (8k+ chars today; biggest
  context shrink opportunity).
- `extensions/languages/clojure/src/com/blockether/vis/ext/lang_clojure/core.clj`
  and `patch.clj` — the `z/` extension prompt.

## Off Limits

- `dev/benches/4clojure/problems.json` and `instances.json` — 4Clojure workload.
- `dev/benches/filewrite/problems.json` — filewrite workload (the verify
  forms ARE the judge). Bug fixes to the assertions are fine; relaxing
  what counts as "passed" is cheating.
- `dev/benches/4clojure/eval_one.clj`'s scoring logic. Bug fixes are fine;
  relaxing what counts as "passed" is cheating.
- The rest of `src/`, `extensions/` — touch only when there's a concrete
  hypothesis that a change reduces Vis iterations / context. Run
  `./verify.sh --quick` first.

## Constraints

- No hardcoded 4Clojure answers anywhere in prompts, source, or fixtures.
- No bypassing the SCI sandbox or the answer gate to fake "1 iteration".
- Workload uses real Z.ai requests; respect the request quota (see
  `bin/vis providers status zai-coding-plan`).
- Each autoresearch iteration runs in a **fresh** `bin/vis` subprocess
  (fresh JVM, fresh in-memory DB) per task — no cross-task state.
- Judge IS this Claude session; it must compare against the public
  4Clojure tests verbatim, never a hand-written shortcut.

## Anti-overfitting protocol

The rotation guarantees each iteration sees a fresh 3-task slice, so a
change that only memorizes 3 problems will score badly on the next
iteration's slice. Still, individual `keep` decisions must be confirmed:

1. After every primary-metric improvement, run
   `./autoresearch.revalidate.sh` (defaults to offset+7 and offset+13).
2. Combine: total `iter_score` over the 6 confirmation tasks vs.
   what the baseline scored at *those* offsets.
3. Keep only when the confirmation slices don't regress more than 10%
   relative to baseline at the same offsets. Otherwise → discard and
   revert.
4. When a `keep` is borderline (within 1.0× noise from `log_experiment`),
   run an extra single-slice run at offset+19 before moving on.

## What's Been Tried

(Filled in as runs accumulate.)
