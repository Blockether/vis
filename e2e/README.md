# e2e

End-to-end harness that drives the **real `vis` CLI** on a battery of editing
tasks and checks each one converges, is correct, runs clean, and takes the fast
(structural) path. Sits alongside the root `test/` dir; see the module docstring
in `run.py` for the full contract.

## Layout

Scenarios are aggregated from the root **and** each language pack's `e2e/` — the
same split as the test `--dir` list: the foundation (language-neutral
`struct_patch`) set lives here in the root; a pack owns the scenarios that
exercise ITS surface, beside its `test/` dir.

```
e2e/
  run.py                                              the runner (scans all roots)
  scenarios/<id>/                                     foundation editing, any language
extensions/languages/vis-language-clojure/e2e/scenarios/<id>/   clj editing + repair/format hook
extensions/languages/vis-language-python/e2e/scenarios/<id>/    managed Python REPL
extensions/languages/vis-language-typescript-bun/e2e/scenarios/<id>/ managed Bun/TS REPL

  <id>/
    scenario.json   {lang, prompt, want, wantnot, want_answer?, want_tools?}
    files/          real files seeded into a fresh git repo per run
```

- **want** / **wantnot** — `{path: [substring, ...]}` checks on the resulting files.
- **want_answer** — substrings the final answer must contain (REPL / non-file tasks).
- **want_tools** — tools that MUST have fired (e.g. `repl_eval` proves the model
  actually used the Python REPL instead of computing by hand).

## Run

```sh
python3 e2e/run.py                              # every scenario across all roots
python3 e2e/run.py clj-rename py-repl-compute   # a subset by id
VIS_PROVIDER=zai-coding-plan VIS_MODEL=glm-5.2 python3 e2e/run.py

# CROSS-VALIDATION GATE — run each scenario on MULTIPLE models; a scenario passes
# only if EVERY model passes it (the gate exit code reflects that):
VIS_MODELS=glm-5.2,glm-5-turbo python3 e2e/run.py
```

Env: `VIS_MODELS` (comma-sep models for the cross-validation gate; default = `VIS_MODEL`),
`VIS_E2E_TIMEOUT` (per-scenario seconds, 300), `VIS_E2E_WORKERS` (parallel, 5),
`VIS_E2E_TRACES` (raw JSON trace dir; per-model `<id>__<model>.jsonl` under cross-val),
`VIS_E2E_KEEP=1` (keep temp work dirs).

## Add a scenario

Create `<root>/scenarios/<id>/scenario.json` + `files/...` under the root `e2e/`
(foundation) or the relevant pack's `e2e/` (language-specific). No `run.py` change
— it discovers every folder under each scenario root.
