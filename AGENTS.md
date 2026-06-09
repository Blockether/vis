# Vis project rules

This file rides into every provider prompt verbatim. Keep it small and
project-specific. Engine contract (CTX shape, DONE pipeline, sandbox
rules) lives in the CORE system prompt — do not restate it here.

## Code conventions

- Clojure source: `cljfmt` for formatting, `clj-kondo` clean. Run
  `./verify.sh --quick` before commits.
- One namespace per file. Internal helpers `^:private`.
- Prefer `:keys` destructuring at fn signatures over `(get m :k)` in
  the body.
- No `defrecord` / `deftype` / `gen-class` in GraalPy-sandbox-facing code
  (refused by `validate-no-banned-defs!`).

## Operator workflow

`./bin/dev nrepl` boots the project nREPL on `:7888` and writes
`.nrepl-port`. Editor/LLM helpers reach it via `clj-nrepl-eval -p
$(cat .nrepl-port)`. Restart pattern:

```bash
old=$(lsof -tiTCP:7888 -sTCP:LISTEN || true); [ -n "$old" ] && kill $old
rm -f .nrepl-port
nohup ./bin/dev nrepl > .nrepl.log 2>&1 &
```

Persistent DB lives at `$HOME/.vis/vis.mdb/vis.db`. Use `--db :memory`
for throw-away sessions.
