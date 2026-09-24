# e2e

End-to-end tests run `vis-agent` on editing tasks and check completion, file
contents, errors and use of anchored `patch` edits. See `run.py`'s module
docstring for the full contract. Each invocation starts a gateway from the
current classpath with a temporary database, independent of installed daemons.
Every scenario passes `--persist` to use gateway session/turn handling rather than
the ephemeral CLI engine path. Sessions stay in that temporary database.
These tests use real model calls and incur provider costs.

## Layout

Scenarios are under `e2e/scenarios/`. Language-neutral editing tests run
alongside Clojure (`clj-*`) and Python (`py-*`) tests.

```
e2e/
  run.py                                              the runner
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
- **want_cache_metrics** — persist the run, read `/v1/sessions/:sid/usage` through the
  canonical gateway client, and independently reconcile provider totals, both percentages,
  sample counts, and (with `want_folded_prefix`) the one estimated rebuild.
  Invalid, missing or impossible token counts fail; a cold cache does not. This
  does not require a particular cache rate or report a request-level hit percentage.
- **want_goal_complete** — require the persisted gateway goal to be complete after
  the scenario, not just a model claim or a tool-call attempt.
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
  deterministic large inputs into the temporary workspace before the fixture
  git commit; the script itself is not copied. Combine with `files_from` to
  share generated inputs without tracking bulky data.

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
The generator scripts are deterministic and run inside temporary fixture
workspaces. Read `results.json` and the per-case traces under `VIS_E2E_TRACES`
for failures, not just the command's exit status. Passing cases demonstrate
those tasks and routes, not universal reliability across models or prompts.

```sh
VIS_E2E_TRACES=/tmp/vis_large_stdout VIS_PROVIDER=zai-coding-plan \
VIS_MODELS=glm-5.3-flash,glm-5.3 VIS_E2E_WORKERS=2 python3 e2e/run.py \
  stdout-goal-cache stdout-lookup-middle stdout-ledger-reduce stdout-jsonl-filter \
  stdout-unicode stdout-dual-source stdout-incident-join stdout-natural-lookup \
  stdout-natural-ledger large-csv-groups large-crossfile-join \
  large-jsonl-integrity large-two-ledgers
```

## Run

```sh
python3 e2e/run.py                              # every scenario across all roots
python3 e2e/run.py clj-rename py-add-func       # a subset by id
VIS_PROVIDER=zai-coding-plan VIS_MODEL=glm-5.3-flash python3 e2e/run.py

# Run each scenario on multiple models. The command succeeds only if every
# model passes every selected scenario:
VIS_MODELS=glm-5.3-flash,glm-5.3 python3 e2e/run.py

# Pin exact native effort and reject missing evidence or a changed route:
VIS_PROVIDER=github-copilot VIS_MODEL=gpt-6-astra VIS_REASONING_EFFORT=low python3 e2e/run.py py-fix-body py-add-param
```

To run the same scenarios against a built native engine, set
`VIS_E2E_NATIVE_BIN` to the absolute raw native executable path, not the
`vis-agent` launcher. Both agent turns and isolated fixture gateways then use
that binary; the JVM only runs the canonical gateway client for startup,
usage queries and cleanup. Missing binaries and launcher scripts fail the run
rather than falling back to source. Also supply the matching Python sidecar.

```sh
VIS_E2E_NATIVE_BIN=/path/to/vis-agent-native \
VIS_PYTHON_NATIVE_PATH=/path/to/vis-agent-python/libvispython.dylib \
python3 e2e/run.py
```

Environment variables: `VIS_MODELS` (comma-separated models, default
`VIS_MODEL`), `VIS_E2E_TIMEOUT` (explicit whole-scenario budget in seconds),
`VIS_E2E_WORKERS` (parallel runs, default 5), `VIS_E2E_REPEATS` (positive repetitions
per scenario/model, default 1), `VIS_E2E_TRACES` (trace and measurement directory),
and `VIS_E2E_KEEP=1` (retain temporary working directories). Multiple-model trace
names include `__<model>`; repeated runs add `__run<N>`. Use a fresh trace directory
for each comparison; a later invocation replaces its `results.json`.

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
all correctness guards, and have no surfaced errors, failed/cancelled Activities
or unfinished Activities. Repeated snapshots count once, after terminal state updates.
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
VIS_PROVIDER=zai-coding-plan VIS_MODEL=glm-5.3-flash VIS_E2E_REPEATS=2 VIS_E2E_WORKERS=1 python3 e2e/run.py extension-contract-discovery extension-schema-discovery extension-known-contract
```

## Add a scenario

Create `<root>/scenarios/<id>/scenario.json` and `files/...` under the `e2e/`
directory. The runner discovers scenario folders automatically.
