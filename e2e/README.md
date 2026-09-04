# e2e

End-to-end harness that drives the **real `vis-agent` CLI** on a battery of editing
tasks and checks each one converges, is correct, runs clean, and takes the fast
(anchored `patch`) path. Sits alongside the root `test/` dir; see the module docstring
in `run.py` for the full contract. Every invocation starts one current-classpath gateway
on an isolated temporary DB, so a healthy installed daemon cannot hide working-tree changes.

## Layout

Every scenario lives under `e2e/scenarios/`: the foundation (language-neutral
editing) set beside the per-language ones (`clj-*`, `py-*`) that exercise a
language surface.

```
e2e/
  run.py                                              the runner
  scenarios/<id>/                                     foundation editing, clj-* (repair/format hook), py-* (managed REPL)

  <id>/
    scenario.json   {lang, prompt, want, wantnot, want_answer?, want_tools?, want_forms?,
                       want_requested_route?, want_folded_prefix?, want_cache_read?,
                       want_cache_metrics?}
    files/          real files seeded into a fresh git repo per run
```

- **want** / **wantnot** — `{path: [substring, ...]}` checks on the resulting files.
- **want_answer** — substrings the final answer must contain (REPL / non-file tasks).
- **want_tools** — extension tools that MUST have fired (e.g. `repl_eval` proves
  the model actually used the Python REPL instead of computing by hand).
- **want_forms** — source substrings that MUST occur in a top-level sandbox form.
- **want_requested_route** — every provider marker and the billed result must use
  the requested provider/model; a silent fallback fails the run.
- **want_folded_prefix** — exactly one direct `fold_session("-tN/iK", ...)` must target
  the immediately prior iteration and a provider call must continue after it.
- **want_cache_read** — the real provider result must report nonzero aggregate cached input tokens.
- **want_cache_metrics** — persist the run, read `/v1/sessions/:sid/usage` through the
  canonical gateway client, and independently reconcile provider totals, both percentages,
  sample counts, and (with `want_folded_prefix`) the one estimated rebuild.

## Run

```sh
python3 e2e/run.py                              # every scenario across all roots
python3 e2e/run.py clj-rename py-repl-compute   # a subset by id
VIS_PROVIDER=zai-coding-plan VIS_MODEL=glm-5.3-flash python3 e2e/run.py

# CROSS-VALIDATION GATE — run each scenario on MULTIPLE models; a scenario passes
# only if EVERY model passes it (the gate exit code reflects that):
VIS_MODELS=glm-5.3-flash,glm-5.3 python3 e2e/run.py
```

Env: `VIS_MODELS` (comma-sep models for the cross-validation gate; default = `VIS_MODEL`),
`VIS_E2E_TIMEOUT` (per-scenario seconds, 300), `VIS_E2E_WORKERS` (parallel, 5),
`VIS_E2E_TRACES` (raw JSON trace dir; per-model `<id>__<model>.jsonl` under cross-val),
`VIS_E2E_KEEP=1` (keep temp work dirs).

## Add a scenario

Create `<root>/scenarios/<id>/scenario.json` + `files/...` under the root `e2e/`
(foundation) or the relevant pack's `e2e/` (language-specific). No `run.py` change
— it discovers every folder under each scenario root.
