# e2e

End-to-end tests run the Vis CLI of this checkout on editing tasks and check
completion, file contents, errors and use of anchored `patch` edits. The runner
compiles the scenarios to a Harbor dataset and runs them as one Harbor job on
this host. See the module docstrings in `vis_harbor/` for the full contract.
Each trial starts one CLI process with `--persist` and its own temporary
database, independent of installed daemons. The trial also gets an empty global
extension directory, so extensions in `~/.vis/extensions` do not change the
measured runs. To include them, set `VIS_EXTENSIONS_DIR="$HOME/.vis/extensions"`.
These tests use real model calls and incur provider costs.

## Layout

Scenarios are under `e2e/scenarios/`. Language-neutral editing tests run
alongside Clojure (`clj-*`) and Python (`py-*`) tests.

```
e2e/
  pyproject.toml                                      the locked uv project with Harbor
  vis_harbor/                                         dataset compiler, host environment, agent, grader and runner
  scenarios/<id>/                                     foundation editing, clj-* (repair/format hook), py-* (managed REPL)

  <id>/
    scenario.json   task, fixture expectations and optional benchmark guards
    files/          input files copied to a new git repository per run
```

- **want** / **wantnot** — `{path: [substring, ...]}` checks on the resulting files.
- **want_answer** — substrings the final answer must contain (REPL / non-file tasks).
- **want_tools** — extension tools that must finish successfully, such as `patch`.
- **want_forms** — legacy substring checks on sandbox source; not execution evidence.
- **want_answer_json** — the exact final JSON value, with no extra facts or keys.
  A single JSON code fence is accepted, as is the engine’s `Goal complete: `
  wrapper when `want_goal_complete` is set. Integral float and integer JSON numbers
  compare equal; booleans, extra facts and duplicate object keys are rejected.
- **want_json_files** — `{path: [row, ...]}` exact JSONL evidence, including row order.
  Discovery fixtures write private invocation journals inside registered methods;
  the model cannot satisfy these checks just by printing a receipt.
- **want_activity_sequence** — exact operation order and count within the named
  namespaces, independently checked against terminal Activity snapshots.
- **forbid_tools** — operations that must not occur, including failed attempts.
- **want_council_recovery** — require one reply-ID error, then a successful read of its real thread.
  Set `root_content` and `reply_content` to the fixture bodies.
  The guard checks both publications, their IDs, the error, the read order and both complete bodies.
  Only this verified error and its failed Activity are expected.
  Other errors, repeated mistakes and incomplete recovery fail the scenario.
  Results retain raw failure counts and report expected failures separately.
- **max_form_output_chars** / **max_total_output_chars** — nonnegative peak and
  cumulative stdout limits. These measure characters, not tokens; traces stay intact.
- **discovery** — required `signatures` and `contracts` names, or `known: true` to
  require reuse without discovery. Duplicate unchanged lookups fail. Syntax checks
  resolve ordinary aliases and literal loops/comprehensions, not comments or quoted
  examples. Runtime Activities supply doc/apropos evidence when available; otherwise
  syntax is used. Local helpers hiding discovery are rejected. This bounded audit
  is not a Python execution tracer or a security boundary.
- **want_helper_reuse** — `true`, or the number of later forms required: a function
  defined in one sandbox form must be called by that many later forms. The audit is
  name-agnostic and counts a `def` or a name bound to a `lambda`, the two shapes the
  runtime saves. Retyping the same definition instead of calling it fails, as does a
  helper no later form uses; a definition and its call inside one form prove nothing.
- **measurement** — `true` reports the scenario's behavior check instead of gating on it.
  The run must still converge, answer correctly and stay error-free; a missed behavior
  check prints with `~` and is counted in that scenario's `BEHAVIOR` rate.
- **want_requested_route** — all provider markers and billed results must use
  the requested provider and model. Fallbacks fail the test.
- **want_folded_prefix** — exactly one direct `fold_session("-tN/iK", ...)` must target
  the immediately prior iteration and a provider call must continue after it.
- **want_cache_read** — the real provider result must report nonzero aggregate cached input tokens.
- **want_cache_metrics** — persist the run, read the session usage from the trial's
  `trajectory.json`, and independently reconcile provider totals, both percentages,
  sample counts, and (with `want_folded_prefix`) the one estimated rebuild.
  Invalid, missing or impossible token counts fail; a cold cache does not. This
  does not require a particular cache rate or report a request-level hit percentage.
- **want_context_budget** — persist the run, read the request health from the session
  usage in `trajectory.json`, and require the `auto_compress_above` and
  `model_input_limit` values the model printed from `session['utilization']` and
  answered to match it. The soft budget must be positive, at most 90% of the input
  limit, with its reminder at 75% of the budget. An object keyed by `provider/model`
  also pins both values for those routes; other routes check only these rules.
- **want_goal_complete** — require the persisted session goal in `trajectory.json`
  to be complete after the scenario, not just a model claim or a tool-call attempt.
- **want_stdout_recovery** — require an oversized raw stdout, then a later
  `read_session()` block that selects its original scope and tool-call ID. Check
  a later proof block (which may be the same block as the read) against the raw
  stdout’s UTF-8 SHA-256 digest, length and middle.
  Use `min_chars`, `head`, `middle` and `tail` to identify the original. A list
  requires distinct, correctly selected read-backs for every output. By default
  the read-back must report `GOAL_STATUS: active`; set `goal_status: null` for
  tasks without a goal. The model-facing result is clipped; the saved raw output
  is not. This is a trace guard, not a proof of arbitrary data-flow semantics.
- **files_from** — reuse a sibling scenario's input files without copying its source.
  The known-contract case reuses the original fixture with a supplied unchanged
  contract. It tests recovered-contract reuse, not cross-turn memory retention.
- **fixture_generator** — a checked-in Python script basename at the fixture root,
  run once during seeding with the workspace path as its argument. It writes
  deterministic large inputs into the task workspace when the runner compiles the
  dataset, before the fixture git commit of each trial. The script itself is not
  copied. Combine with `files_from` to share generated inputs without tracking
  bulky data.

- **workspace_filesystem** — `{id: fixture-relative directory}` registrations.
  Setup writes absolute paths and allowed ids to `vis.yml`; omit a fixture copy
  of that file.

## Large-output agent behavior

Twelve `stdout-*` and `large-*` scenarios complement `stdout-goal-cache`.
Six first print 145k–377k characters and require the agent to recover the exact
saved output before it answers: middle lookup, whole-ledger reduction, JSONL
filtering, multilingual text, two independent results, and incident correlation.
Two more reuse the lookup and ledger data but do not tell the agent how to recover
a clipped result; compare their traces for natural recovery behavior. Four start
from generated files larger than the display preview and require compact local
processing: grouped CSV, a cross-file join, malformed JSONL, and a two-ledger
reconciliation with a persisted goal and cache checks. All have exact JSON answer
oracles; the six guided recovery cases also verify the original scope, tool-call
ID, length, SHA-256 and middle data. The compact cases bound peak and total
printed characters.

Open generated files from `project_root_path / "data"`, not the Python process’s
current directory. The stdout caps include goal-completion output: printing the
full `update_goal` result can exceed them because it repeats the goal objective.
Print only the completion status if you need confirmation.

Run named cases with the command below; each model call incurs provider costs.
The generator scripts are deterministic and run when the runner compiles the
dataset. Read `results.json` and the per-case traces under `VIS_E2E_TRACES`
for failures, not just the command's exit status. Passing cases demonstrate
those tasks and routes, not universal reliability across models or prompts.

```sh
VIS_E2E_TRACES=/tmp/vis_large_stdout VIS_PROVIDER=zai-coding-plan \
VIS_MODELS=glm-5.3-flash,glm-5.3 VIS_E2E_WORKERS=2 \
uv run --project e2e python -m vis_harbor.run \
  stdout-goal-cache stdout-lookup-middle stdout-ledger-reduce stdout-jsonl-filter \
  stdout-unicode stdout-dual-source stdout-incident-join stdout-natural-lookup \
  stdout-natural-ledger large-csv-groups large-crossfile-join \
  large-jsonl-integrity large-two-ledgers
```

## Run

Start the runner with `uv` from the repository root. `uv run --project e2e` uses the
locked Harbor project in `e2e/`.

```sh
uv run --project e2e python -m vis_harbor.run                         # every scenario
uv run --project e2e python -m vis_harbor.run clj-rename py-add-func  # a subset by id
VIS_PROVIDER=zai-coding-plan VIS_MODEL=glm-5.3-flash uv run --project e2e python -m vis_harbor.run

# Run each scenario on multiple models. The command succeeds only if every
# model passes every selected scenario:
VIS_MODELS=glm-5.3-flash,glm-5.3 uv run --project e2e python -m vis_harbor.run

# Pin exact native effort and reject missing evidence or a changed route:
VIS_PROVIDER=openai-codex VIS_MODEL=gpt-6-sol VIS_REASONING_EFFORT=low uv run --project e2e python -m vis_harbor.run py-fix-body py-add-param
```

To run the same scenarios against a built native engine, set
`VIS_E2E_NATIVE_BIN` to the absolute raw native executable path, not the
`vis-agent` launcher. Each trial then runs that binary instead of the source CLI.
A missing binary or a launcher script fails the trial rather than falling back to
source. Also supply the matching Python sidecar.

```sh
VIS_E2E_NATIVE_BIN=/path/to/vis-agent-native \
VIS_PYTHON_NATIVE_PATH=/path/to/vis-agent-python/libvispython.dylib \
uv run --project e2e python -m vis_harbor.run
```

Environment variables: `VIS_MODELS` (comma-separated models, default
`VIS_MODEL`), `VIS_E2E_TIMEOUT` (agent timeout in seconds for each scenario),
`VIS_E2E_WORKERS` (trials at the same time, default 5), `VIS_E2E_REPEATS` (positive
repetitions per scenario/model, default 1) and `VIS_E2E_TRACES` (trace and
measurement directory). Multiple-model trace names include `__<model>`; repeated
runs add `__run<N>`. The Harbor dataset and jobs are in the `harbor` directory next
to `VIS_E2E_TRACES`; each trial directory keeps the agent logs and the grade. Use a
fresh trace directory for each comparison; a later invocation replaces its
`results.json`.

`VIS_REASONING_EFFORT` optionally forwards an exact provider-native effort to
`--reasoning-effort`. With it set, every run must report valid evaluation evidence
for that effort on the requested provider and model, without fallback. The run
summary includes the requested effort and route. Omit it to retain the configured
reasoning behavior.

Without `VIS_E2E_TIMEOUT`, each scenario uses its `timeout_s` or the 300-second
default. These budgets include model requests and all tool calls; they do not
change the Python execution watchdog. `extension-watchdog` uses 900 seconds to
allow for its real 310-second extension call and model response time. `find-usages`
uses 480 seconds because its answer is a single long prose reply, which flash-class
models stream slowly enough to outlast the default budget.

## Interpret measurements

`results.json` contains every run, including failures, plus per-scenario/model
summaries. Compare pass counts before efficiency: each run must converge, satisfy
all correctness guards, and have no unexpected errors or unfinished Activities.
The Council recovery guard accepts only its verified error and failed Activity.
Repeated snapshots count once, after terminal state updates.
A form's final snapshot collapses the calls it made into one row: a row that snapshot
no longer lists finished with the group, so only a row still running there counts as
unfinished work.

The report separates surfaced errors from Activity failures without a same-form
error (possible caught failures). Missing scopes remain unclassified; a shared form
error does not prove whether a particular exception was caught.

Provider input already includes cached input: `uncached = input - cached`. The
cached-input share uses summed counts, not an average of percentages. Input,
cached, uncached and output tokens, model calls, forms, wall time, peak/total stdout
and discovery counts are separate measurements. Reasoning is `unavailable` unless
the provider's result supplies it; persisted usage counters are reported separately.

`want_helper_reuse` measures a behavior rather than one edit, so read it over repeats:
a model that factors a helper in most runs can still retype the same block in one. That is
why `session-helper-reuse` sets `measurement` — the gate covers its answer and errors, and
helper reuse is reported as a rate. Raise `VIS_E2E_REPEATS`, compare the `BEHAVIOR` count
across prompt revisions, and expect small models to be less consistent.

Repeats report minimum, median and maximum, plus the number of valid token samples.
There is no fixed token/cache target: changing model, route, prompt or cache state
changes these costs. One passing run is not proof of optimal discovery or universal
cache savings. For a small repeated GLM comparison:

```sh
VIS_PROVIDER=zai-coding-plan VIS_MODEL=glm-5.3-flash VIS_E2E_REPEATS=2 VIS_E2E_WORKERS=1 uv run --project e2e python -m vis_harbor.run extension-contract-discovery extension-schema-discovery extension-known-contract
```

## Add a scenario

Create `<root>/scenarios/<id>/scenario.json` and `files/...` under the `e2e/`
directory. The runner discovers scenario folders automatically.

## Run scenarios from Vis

The project extension `.vis/extensions/evals.py` runs this runner from a Vis session:

- `evals.scenarios()` lists the scenarios and their checks.
- `evals.new_scenario(...)` creates `scenarios/<id>/scenario.json` and its `files/`.
- `evals.run_scenarios(ids, models=[...])` starts a paid run in the background, with its own `VIS_E2E_TRACES` directory.
- `evals.status()` and `evals.report()` read the run. The report lists each failed run with credentials redacted.
- `evals.compare(baseline, candidate)` compares two scenario runs case by case.

In these results, a case is one scenario on one model. `report()` gives the pass rate with its 95% interval. `detectable_change_points` is the smallest change that a second run of the same size detects. With `repeats` above 1, it also gives `pass_all_percent` (pass^k), the share of cases that passed every repeat. `pass_any_percent` (pass@k) is the share of cases that passed at least one repeat, and `unstable` names the cases that passed only some repeats. The interval allows for the similar results of repeats of one case.

To find out if a change helps, run the same scenarios on the same models before and after the change. Then compare the two runs, not their pass rates:

```python
before = evals.run_scenarios(ids, provider="zai-coding-plan", models=["glm-5.3-flash"], repeats=3)
# When this run ends, make the change and build Vis again.
after = evals.run_scenarios(ids, provider="zai-coding-plan", models=["glm-5.3-flash"], repeats=3)
evals.compare(before.run_id, after.run_id)
```

`compare()` pairs the cases that both runs measured. The verdict is `better` or `worse` only when the paired 95% interval of the change excludes zero. Each run records the Vis commit. With `native_bin`, it also records the sha256 of that executable. This digest names the code, also when the checkout has uncommitted changes. The caveats tell you when both runs used the same version, or when a version is unclear.
