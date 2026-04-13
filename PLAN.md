# PLAN.md — vis post-RLM-intake (HANDOFF)

Style: caveman-full. Machine + agent context.

## What just happened

Massive cutover. **RLM + PageIndex + git ingestion + SQLite store + bench**
moved from `../svar` into vis. svar is now a pure LLM-output + spec lib.

## New tree in vis

```
src/com/blockether/vis/
├── rlm.clj                      — public RLM facade (was svar.internal.rlm)
├── rlm/
│   ├── core.clj                 — iteration loop, code exec, env lifecycle
│   ├── sqlite.clj               — SQLite store (DDL, FTS5, triggers, CRUD)
│   ├── db.clj                   — thin facade over sqlite
│   ├── schema.clj               — specs / dynamic vars / constants
│   ├── tools.clj                — SCI sandbox + hook system
│   ├── routing.clj, sub.clj     — sub-RLM routing, depth tracking
│   ├── batch.clj, concurrency.clj — parallel fan-out, semaphore
│   ├── skills.clj               — SKILL.md discovery + ingest
│   ├── git.clj                  — JGit commit ingestion
│   ├── data.clj                 — entity/relationship ingest
│   ├── trace.clj, trajectory.clj — debug trace + JSONL export
│   ├── query.clj                — query-env! decomposition
│   ├── qa.clj, qa_manifest.clj  — query-env-qa! pipeline
│   ├── env.clj                  — env atom helpers
│   └── pageindex.clj + pageindex/{pdf,markdown,vision}.clj
│                                — document extraction pipeline
└── (existing TUI / web / agent / cli / telegram dirs untouched)

test/com/blockether/vis/rlm_test.clj       — main RLM suite
test/com/blockether/vis/rlm/*_test.clj     — per-module tests
bench/com/blockether/vis/bench/…           — 4clojure, humaneval, swebench
```

Deps added to `deps.edn`: sqlite-jdbc, next.jdbc, honeysql, pdfbox,
core.async, jgit, sci, zprint, jtokkit, edamame, yamlstar, fs, trove,
lazytest, http-client. Aliases added: `:test`, `:bench`.

## Current status

- ✅ All namespaces rewritten (`com.blockether.svar.internal.rlm.*` →
  `com.blockether.vis.rlm.*`, `com.blockether.svar.bench.*` →
  `com.blockether.vis.bench.*`).
- ✅ `clojure -M:dev -e "(require 'com.blockether.vis.rlm :reload-all)"` → OK.
- ✅ SQLite store — `:path` is now the *directory* containing `rlm.db`;
  qa-manifest.edn + trajectory EDN live as siblings. `:temp` mode wipes
  the whole dir on dispose.
- ⚠️ `clojure -M:test` → **536 tests, 38 failures.** Down from 41 after
  the dir-as-path fix.

## Failure taxonomy (tackle in this order)

1. **`SQLITE_CONSTRAINT_FOREIGNKEY` on `toc_entry` / `page` / `page_node`**
   — tests insert child rows without first inserting the parent `document`.
   The SQLite schema enforces FKs (Datalevin didn't). Fix options:
     (a) preferred: test setup stores a stub document via
         `rlm-db/db-store-pageindex-document!` before child inserts,
     (b) add a `db-store-toc-entry!` variant that upserts the parent doc,
     (c) drop the FK (NOT recommended — invariant matters).
   Start at `vis/test/com/blockether/vis/rlm_test.clj:1860`.
2. **`(contains? db-info :conn)` style assertions** — currently passing
   because I added `:conn` as alias for `:datasource`. Longer-term: update
   tests to assert `:datasource`, drop the alias.
3. **`dispose-env!` on shared-DB** — verify the borrowed datasource is NOT
   closed when `owned? = false`.
4. **`qa-manifest-write-failure-test`** — now that `:path` is a directory,
   the "target manifest path cannot be replaced" expectation needs a
   different trigger (chmod, or a path with an existing file blocking the dir).

## Next steps

### Phase A — get tests green

- [ ] Fix FK-constraint failures (see #1 above). Read the failing describes,
      write a `with-doc` helper or add `db-store-pageindex-document!` calls
      in setup.
- [ ] Iterate `clojure -M:test` until 0 failures.
- [ ] Port `verify.sh` from `../svar`: format → lint → compile-java (if any
      Java) → test → secrets check. Adapt the test-count floor.
- [ ] Add clj-kondo config. Lint the new tree.

### Phase B — wire RLM into existing vis features

vis has TUI + web + agent + telegram layers that don't yet call RLM:
- [ ] `vis.cli` — `query` subcommand that opens an env, ingests a corpus,
      runs `query-env!`, prints answer.
- [ ] `vis.web` — `/rlm/query` endpoint.
- [ ] `vis.tui.chat` — rich trace rendering (code + result + thinking).
- [ ] `vis.telegram` — `/ask-rlm` command with per-chat SQLite store.

### Phase C — deps cleanup

- [ ] Drop deps that turn out unused after a fresh compile.
- [ ] `com.blockether/svar` is `:local/root "../svar"` — good while svar
      is co-evolving. Switch to `:mvn/version` once svar stabilizes.
- [ ] Telemere vs Trove — RLM uses trove, vis uses telemere. Decide one.
      Trove already emits via telemere, so the bridge works; just noisy.

### Phase D — bench + trajectory housekeeping

- [ ] Smoke-test `clojure -M:bench -- --bench 4clojure --agent query-env
      --model gpt-4o --n 2` once tests are green.
- [ ] `bench/trajectories/` + `bench/results/` NOT copied (historical,
      large). `cp -r ../svar/bench/{trajectories,results} bench/` if
      needed, otherwise fresh runs will populate them.

### Phase E — docs

- [ ] Merge RLM-specific rules from `../svar/CLAUDE.md` into
      `vis/CLAUDE.md` (clj-paren-repair, SCI sandbox, lazytest + markdown
      doctests, caveman style, Blockether One endpoint notes, bench).
- [ ] vis/README.md — short intro; point RLM API users here.

## Non-goals

- ❌ Reimplement RLM bits in svar. Closed door.
- ❌ Break the vis → svar dep direction. svar is a leaf lib now.

## Invariants (do not violate)

- `com.blockether.vis.rlm` is the only **public** RLM namespace. All
  `com.blockether.vis.rlm.*` are internal.
- `:path` in db-info is always a *directory*. The SQLite file lives at
  `<path>/rlm.db`. qa-manifest + trajectory EDN sit next to it.
- `:temp` mode wipes the whole dir on `dispose-env!`.
- External datasources (`{:datasource ds}` / `{:conn ds}`) are never
  closed by vis — caller owns them.
- No circular dep: vis → svar. svar must never `require`
  `com.blockether.vis.*`.
