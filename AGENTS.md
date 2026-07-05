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
- Documentation lives ONLY under `resources/vis-docs/` (plus each
  extension's own `resources/vis-docs/`). The `internal.docs` renderer
  serves those live and publishes them to the docs site. The root `docs/`
  folder is gitignored local scratch — never edit or add tracked docs
  there; change the canonical pages in `resources/vis-docs/` instead.

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

## GraalVM native-image

`clojure -T:build native` (or `bin/vis native`) builds the standalone
binary. Config travels INSIDE the image: each jar's
`META-INF/native-image/<group>/<artifact>/` is auto-discovered. Rules:

- Unified `reachability-metadata.json` only — never the legacy
  `reflect-config.json`/`resource-config.json`/etc. (deprecated).
- **Don't duplicate a library's own config.** GraalPy ships its heavy
  args (build-time init, BouncyCastleFeature, `-Xms14g`); vis only adds
  app-level reflection + its own flags.
- vis args + app-wide metadata live in main's
  `resources/META-INF/native-image/com.blockether/vis/`. Each extension
  ships its own dir for library reachability it uniquely pulls in.
- No per-namespace `--initialize-at-build-time`: graal-build-time's
  `InitClojureClasses` (enabled in main's `native-image.properties`)
  covers all Clojure classes.
- Regenerate metadata with the tracing agent, never by hand:
  `java -agentlib:native-image-agent=config-merge-dir=<artifact-dir> …`.
- Then clean the agent's Clojure-internal noise:
  `bb scripts/clean-reachability.bb`. `config-merge-dir` accumulates, and the
  agent re-captures bare `<ns>__init` classes and `…$fn__<N>` gensym anonymous
  fns every run — both redundant under `InitClojureClasses` (the gensym `<N>`
  also changes each compile). The script strips ONLY those; real
  reflection/resource/foreign entries are untouched.

