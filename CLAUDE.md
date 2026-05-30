# CLAUDE.md

Guidance for AI assistants (Claude Code and others) working **on** this
repository. For the rules that ride into the agent's *own* provider prompt at
runtime, see `AGENTS.md`; for deep architecture decisions and known issues, see
`PLAN.md`.

## What this is

**Vis** is a from-scratch Clojure coding agent inspired by
[Recursive Language Models](https://arxiv.org/abs/2512.24601): instead of
spending the context window on an ever-growing transcript, the model writes
Clojure into a sandboxed [SCI](https://github.com/babashka/sci) runtime, keeps
state in named vars and SQLite, and treats context as an external environment it
can inspect and change.

## Repository layout

Single-package monorepo: **one jar** (`com.blockether/vis`), **one namespace
tree** (`com.blockether.vis.*`). The old three-package split
(sdk / runtime / main) was collapsed because the binary, library, and SDK
always ship on the same classpath. The only real split is `extensions/` —
optional classpath plug-ins (drop the jar, drop the feature).

```
src/com/blockether/vis/
  core.clj            # Public facade — re-exports the internal API. Start here.
  internal/           # ~42 private modules (loop, env, ctx_*, registry, …)
test/                 # Aggregate test suite for the core package
extensions/           # Optional plug-ins, each its own jar + deps.edn
  channels/           #   vis-channel-tui (Lanterna), vis-channel-telegram
  providers/          #   anthropic, github-copilot, openai-codex, standard, zai
  persistance/        #   vis-persistance-sqlite (Flyway + next.jdbc + HikariCP)
  common/             #   vis-foundation-core / bridge / search / git / voice
  languages/          #   vis-language-clojure
dev/                  # REPL conveniences, benches, cli-verify (dev-only)
bin/                  # vis (CLI launcher), dev (nREPL/TUI launcher)
probes/               # Probe / verification tools
docs/                 # mdBook source -> https://blockether.github.io/vis/
deps.edn              # Classpath + aliases (source of truth for commands)
build.clj             # tools.build: jar / install / deploy
verify.sh             # Quality gate (format, lint, GraalVM, tests)
VERSION               # Single source of the published version (0.1.0)
AGENTS.md PLAN.md TAGS.md CHANGELOG.md REVAMP.md
```

Each extension declares `com.blockether/vis {:local/root "../../.."}` against
the repo root and is wired into the root `deps.edn` `:deps`.

## Common commands

Run everything from the repo root.

| Purpose | Command |
| --- | --- |
| Run the CLI | `./bin/vis <subcommand>`  (or `clojure -M:vis <args>`) |
| Smoke test (help tree, no DB) | `./bin/vis` |
| Dev nREPL on :7888 | `./bin/dev nrepl`  (or `clojure -M:dev`) |
| Dev TUI in a terminal | `clojure -M:dev tui` |
| Run a CLI entry under nREPL | `clojure -M:dev cli help` |
| **Run the whole test suite** | `clojure -M:test` |
| **Pre-commit gate (fmt + lint, ~10s)** | `./verify.sh --quick` |
| Full gate (fmt + lint + GraalVM + tests) | `./verify.sh`  (default `--full`) |
| GraalVM safety only / strict (zero warnings) | `./verify.sh --graal` / `--strict` |
| Snapshot GraalVM warning baseline | `./verify.sh --update-baseline` |
| Build all jars | `clojure -T:build jar` |
| Install to `~/.m2` / deploy to Clojars | `clojure -T:build install` / `deploy` |
| Check outdated deps | `clojure -M:antq` |

`clojure -M:test` runs lazytest across the core `test/` tree **and** every
extension's `test/` on one classpath (dirs are listed in the `:test` alias).
To run a single tree: `clojure -M:test --dir extensions/channels/vis-channel-tui/test`.

## Architecture & key concepts

Runtime vocabulary, used everywhere: **Session → Turn → Iteration → Block**.
A session is a persistent conversation; a turn is one user request + answer; an
iteration is one model call (loops until done); a block is one code fence.

Navigate the internals via these files:

- **Iteration loop** — `internal/loop.clj`, `internal/ctx_loop.clj`,
  `internal/ctx_engine.clj`, `internal/iteration.clj`: orchestrate provider
  calls, extract code blocks from the stream, evaluate them, recover from
  stream errors.
- **SCI sandbox** — `internal/env.clj`: builds the sandboxed Clojure runtime,
  exposes foundation tools and an EDN reader, and refuses banned constructs via
  `validate-no-banned-defs!`.
- **Extension system** — `internal/extension.clj`,
  `internal/extension_aggregate.clj`, `internal/registry.clj`,
  `internal/manifest.clj`: classpath discovery of plug-ins and global
  registries for channels / providers / commands.
- **Prompting & rendering** — `internal/prompt.clj`, `internal/render.clj`,
  `internal/ctx_renderer.clj`: message assembly and the answer-IR pipeline
  (markdown via CommonMark GFM).
- **Provider abstraction** — LLM routing goes through the `svar` dependency;
  concrete providers live under `extensions/providers/`.
- **Persistence** — facaded in `internal/persistance.clj`; the actual SQLite
  backend is the `vis-persistance-sqlite` extension.

## Conventions

- **Format & lint must be clean**: `cljfmt` (config `.cljfmt.edn`) and
  `clj-kondo` (config `.clj-kondo/`). Run `./verify.sh --quick` before commits.
- **One namespace per file.** Internal helpers are `^:private`.
- Prefer `:keys` destructuring at the fn signature over `(get m :k)` in the body.
- **No `defrecord` / `deftype` / `gen-class` in SCI-sandbox-facing code** —
  refused by `validate-no-banned-defs!`.
- Naming: predicates end in `-?`, dynamic vars are `*earmuffed*`, keywords are
  namespaced to the owning module (`:ext/...`, `:session/...`).
- The production binary must stay GraalVM native-image compatible: keep
  reflection / boxed-math warnings at or below the baseline
  (`.verification-baseline/`), and keep dev-only deps (e.g. `clj-reload`) in the
  `:dev` alias only.

## Testing

- Framework: **lazytest** (`io.github.noahtheduke/lazytest`).
- Style: `(defdescribe name (it "does X" (expect ...)))`, nestable with
  `(describe ...)`.
- Naming: `*_test.clj` for unit tests, `*_integration_test.clj` for
  cross-module tests. Tests mirror `src/` namespaces.
- Microbenchmarks use Criterium and are gated behind explicit `run-bench!`
  invocations, so the default suite stays fast.

## Dev workflow

`./bin/dev nrepl` boots the project nREPL on `:7888` and writes `.nrepl-port`.
Restart pattern (from `AGENTS.md`):

```bash
old=$(lsof -tiTCP:7888 -sTCP:LISTEN || true); [ -n "$old" ] && kill $old
rm -f .nrepl-port
nohup ./bin/dev nrepl > .nrepl.log 2>&1 &
```

Hot reload manually from the REPL (dev-only, not in the prod binary):
`(require '[clj-reload.core :as r]) (r/reload)`.

Persistent DB lives at `$HOME/.vis/vis.mdb/vis.db`. Use `--db :memory` for
throwaway sessions.

## Git & releases

- **Commit style**: imperative subject, no conventional-commit prefixes —
  e.g. "Remove TUI block count headers", "Recover direct answers from malformed
  fences", "Add multi-range cat reads".
- **Versioning**: `VERSION` is the single source of truth; releases are
  tag-driven (`v{MAJOR}.{MINOR}.{PATCH}`) and publish the umbrella jar plus each
  extension jar to Clojars via `.github/workflows/release.yml`.
- **CI** (`.github/workflows/ci.yml`) runs the aggregate tests, per-package
  isolation, classpath sanity, and the umbrella-jar build. ⚠️ Note: the
  `test-package` matrix and parts of `classpath-sanity` still reference an old
  `packages/vis-*` layout that no longer exists — treat `deps.edn` and
  `verify.sh` as the canonical command source, not that matrix.

## Further reading

- `AGENTS.md` — runtime rules injected into the agent's provider prompt.
- `PLAN.md` — architecture decisions, token/cache model, known issues.
- `TAGS.md` — semantic tagging system.
- `CHANGELOG.md` — Keep-a-Changelog history.
- `REVAMP.md` — TUI block/result UX redesign spec.
- Published docs: https://blockether.github.io/vis/
