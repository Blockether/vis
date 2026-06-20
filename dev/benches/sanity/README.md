# Vis sanity harness

Fast end-to-end smoke test that the agent loop, native tool calling, the
**async-by-default** sandbox runtime (`await` / `gather` on virtual threads),
and the print-only context all work against a given model.

This is **not** a capability benchmark — see `../swe-bench` and `../4clojure`
for that. Run this after touching the loop, the system prompt, or the Python
sandbox, or when bringing up a new provider/model.

```bash
dev/benches/sanity/run.sh <provider/model>
dev/benches/sanity/run.sh zai-anthropic/glm-5-turbo
dev/benches/sanity/run.sh openai-codex/gpt-5.5 --task python-compute --repeat 3
```

- Tasks live in `tasks.json` (prompt + `expect_*` / `forbid` grading hints).
- Each task runs in its own throwaway session DB under `/tmp/vis-sanity/`.
- Grades: **PASS** (completed, expectations met), **SOFT** (completed but an
  expectation missed), **FAIL** (timeout/crash, or a forbidden phrase — e.g. a
  leaked `<unawaited async tool call>` placeholder, or the weak-model
  "sandbox is non-functional" misdiagnosis).
- On any non-PASS, the runner **dissects the session DB** and prints every
  iteration's code + the sandbox's returned `forms`, so you can see exactly what
  the model did. Exit code = number of non-PASS tasks.

Needs only `python3` (stdlib) and a configured provider. For a one-off provider
not in `~/.vis/config.edn`, drop a project-local `.vis/config.edn` overlay in
the repo root (it merges over the global config; providers are replaced).
