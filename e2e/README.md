# e2e

End-to-end tests run `vis-agent` on editing tasks and check completion, file
contents, errors and use of anchored `patch` edits. See `run.py`'s module
docstring for the full contract. Each invocation starts a gateway from the
current classpath with a temporary database, independent of installed daemons.
These tests use real model calls and incur provider costs.

## Layout

Scenarios are under `e2e/scenarios/`. Language-neutral editing tests run
alongside Clojure (`clj-*`) and Python (`py-*`) tests.

```
e2e/
  run.py                                              the runner
  scenarios/<id>/                                     foundation editing, clj-* (repair/format hook), py-* (managed REPL)

  <id>/
    scenario.json   {lang, prompt, want, wantnot, want_answer?, want_tools?, want_forms?,
                       want_requested_route?, want_folded_prefix?, want_cache_read?,
                       want_cache_metrics?, workspace_filesystem?, timeout_s?}
    files/          input files copied to a new git repository per run
```

- **want** / **wantnot** — `{path: [substring, ...]}` checks on the resulting files.
- **want_answer** — substrings the final answer must contain (REPL / non-file tasks).
- **want_tools** — extension tools that must run, such as `repl_eval` for a
  Python REPL test.
- **want_forms** — substrings required in a top-level sandbox form.
- **want_requested_route** — all provider markers and billed results must use
  the requested provider and model. Fallbacks fail the test.
- **want_folded_prefix** — exactly one direct `fold_session("-tN/iK", ...)` must target
  the immediately prior iteration and a provider call must continue after it.
- **want_cache_read** — the real provider result must report nonzero aggregate cached input tokens.
- **want_cache_metrics** — persist the run, read `/v1/sessions/:sid/usage` through the
  canonical gateway client, and independently reconcile provider totals, both percentages,
  sample counts, and (with `want_folded_prefix`) the one estimated rebuild.

- **workspace_filesystem** — `{id: fixture-relative directory}` registrations.
  Setup writes absolute paths and allowed ids to `vis.yml`; omit a fixture copy
  of that file.

## Run

```sh
python3 e2e/run.py                              # every scenario across all roots
python3 e2e/run.py clj-rename py-repl-compute   # a subset by id
VIS_PROVIDER=zai-coding-plan VIS_MODEL=glm-5.3-flash python3 e2e/run.py

# Run each scenario on multiple models. The command succeeds only if every
# model passes every selected scenario:
VIS_MODELS=glm-5.3-flash,glm-5.3 python3 e2e/run.py
```

Environment variables: `VIS_MODELS` (comma-separated models, default
`VIS_MODEL`), `VIS_E2E_TIMEOUT` (explicit whole-scenario budget in seconds),
`VIS_E2E_WORKERS` (parallel runs, default 5), `VIS_E2E_TRACES` (JSON trace
directory; multiple-model runs use `<id>__<model>.jsonl`), and
`VIS_E2E_KEEP=1` (retain temporary working directories).

Without `VIS_E2E_TIMEOUT`, each scenario uses its `timeout_s` or the 300-second
default. These budgets include model requests and all tool calls; they do not
change the Python execution watchdog. `extension-watchdog` uses 900 seconds to
allow for its real 310-second extension call and model response time.

## Add a scenario

Create `<root>/scenarios/<id>/scenario.json` and `files/...` under the main
`e2e/` directory or a language pack's `e2e/` directory. The runner discovers
scenario folders automatically.
