# Vis - Agent Nucleus

```text
operate = reproduce -> inspect(runtime) -> minimal_change -> test -> verify
truth   = runtime > source > docs > assumption
style   = English only; terse; expand only to prevent misread
safety  = headless terminal safe; observable > opaque; user owns data
```

## Non-negotiables

- Reproduce bugs first. No repro -> no diagnosis -> no fix.
- Prefer real integration path, then reduce to smallest seam.
- Turn repro into regression test or documented diagnostic helper.
- Runtime state beats files. Use Vis APIs before storage poking.
- Assistant-facing text must be English.
- Keep terminal/TUI paths headless-safe. Do not load graphical UI libs or add graphical TUI backends.
- Root `README.md` stays tiny: one rationale paragraph + link to `docs/src/`. Long docs go through `docs/src/SUMMARY.md`.

## Runtime / nREPL

```bash
clj-nrepl-eval --discover-ports
clj-nrepl-eval -p 7888 "(require '[com.blockether.vis.dev :as dev] :reload)"
```

- Discover ports before starting a dev JVM.
- Reuse existing Vis nREPL if usable and in this project.
- Start dev only when needed: `bin/dev` or `clojure -M:dev` (`NREPL_PORT=<port>` optional).
- `.nrepl-port` is generated; never commit it.
- Bash is for discovery/process launch. Clojure behavior checks use `clj-nrepl-eval`.
- After edits, reload with `:reload`.

Useful runtime probes:

```clojure
(require '[com.blockether.vis.core :as vis] :reload)
(require '[com.blockether.vis.ext.foundation.transcript :as tr] :reload)

(tr/transcript db conversation-id)

{:active-provider (vis/active-provider)
 :provider-ids (vis/provider-ids)
 :registered-provider-ids (mapv :provider/id (vis/registered-providers))
 :runtime-provider (vis/->svar-provider (vis/active-provider))}
```

## CLI / TUI paths

```bash
clj-nrepl-eval -p 7888 "(dev/cli! \"providers\" \"list\")"
clj-nrepl-eval -p 7888 "(dev/tui!)"
bin/dev tui
vis channels tui
```

- Use `dev/cli!` when REPL control matters.
- Use standalone `vis ...` / `bin/vis ...` for packaged entrypoint, terminal behavior, process lifecycle, or multiprocess SQLite.
- `dev/tui!` opens Terminal.app running `bin/dev terminal-tui`; nREPL and TUI share one controllable JVM.
- Do not run TUI inside nREPL/stdout tool terminal.
- Do not launch `bin/vis channels tui` from `dev/tui!`.

## Clojure edits

Use `v/patch` for all text edits, including `.clj`/`.cljc`/`.cljs`/`.edn`. The
`z/` extension was retired; `v/patch` is the canonical patch surface.

```clojure
(v/patch [{:path "src/foo.clj"
           :search "old-sym"
           :replace "new-sym"}])
```

- Prefer one vector of edit maps.
- `:search` must match exactly once in the current file; all edits validate
  before any write. Use `v/patch-check` to preflight match counts.
- Multiple same-file edits are applied in order and written once.
- Avoid `(declare ...)`; sort defs by dependency. If unavoidable, add nearby reason.
- If delimiters break, run `clj-paren-repair <files>`; do not hand-balance.
- Use only `clj-nrepl-eval` for REPL eval and `clj-paren-repair` for delimiter repair.

## Prompt system separation

- `CORE_SYSTEM_PROMPT` in `src/com/blockether/vis/internal/prompt.clj` is the
  ENGINE contract only: loop, def discipline, answer shape, banned heads,
  IR primitives. It MUST NOT name any extension symbol (`v/cat`, `v/rg`,
  `v/patch`, `v/ls`, `v/conversation-state`, …) and MUST NOT explain any
  extension-owned concept (handles, RLM tactics, tool strategies, mutation
  invalidation, …).
- Extension-facing prompt copy lives behind `:ext/prompt` on each
  extension. Foundation's `:ext/prompt` (extensions/common/vis-foundation)
  owns the `v/` tool docs and RLM tactics block.
- Engine code may reference extensions by alias only when wiring
  (`active-extensions`, `extensions-snapshot`) — never to inline
  extension prose into the core prompt.
- Tool results are PLAIN CLOJURE DATA. No protocol handles, no opaque
  records, no sandbox-private accessors. Each tool returns a map; the
  model destructures with `:keys`. Bounded preview is the
  `:journal-render-fn` / `:channel-render-fn` job, not the value shape.

## Verification

- Docs-only changes, including this file, do not require `verify.sh`.
- During code edits: `./verify.sh --quick`.
- Before commit or handoff: `./verify.sh`.

## Tests

Every Clojure source namespace needs a matching test namespace:

- `src/path/foo.clj` -> `test/path/foo_test.clj`
- `src/path/foo.cljs` -> `test/path/foo_test.cljs`
- `src/path/foo.cljc` -> `test/path/foo_test.cljc`

Rules:

1. New namespace -> create test same iteration.
2. Modify namespace lacking test -> create test before/with change.
3. Test requires namespace under test.
4. Test contains real public-API smoke/regression coverage.
5. Empty test namespace is incomplete.

## SQL / SQLite

- App SQL uses HoneySQL maps.
- Forbidden: `next.jdbc.sql` helpers and raw SQL strings in app code.
- Exception: raw `jdbc/execute!` strings only in `extensions/persistance/vis-persistance-sqlite/src/.../core.clj` for migration DDL / unsupported FTS.

```clojure
(require '[honey.sql :as sql]
         '[next.jdbc :as jdbc]
         '[next.jdbc.result-set :as rs])

(def ^:private jdbc-opts {:builder-fn rs/as-unqualified-lower-maps})

(jdbc/execute! datasource
  (sql/format {:select [:*]
               :from [:my_table]
               :where [:= :id id]})
  jdbc-opts)
```

- Persistent SQLite is multiprocess-capable.
- Do not reintroduce process-exclusive DB locks: no `vis.db.lock`, no `:vis/persistent-db-already-open`.
- Writes rely on WAL + `busy_timeout` + `transaction_mode=IMMEDIATE` + `sqlite-write-tx!` retry boundary.
- Schema install/repair is the only process-serialized path, via short-lived `vis.db.migrate.lock`.
- Do not delete/move/recreate `~/.vis/vis.mdb` or `vis.db` while any Vis process may have it open.
- Multiprocess behavior needs real process/JVM tests.
- Until told otherwise, edit `extensions/persistance/vis-persistance-sqlite/resources/db/sqlite/migration/V1__schema.sql` inline; do not add V2/V3 migrations.

## svar

- Vis calls svar through `svar/ask-code!`; old Vis-side JSON-spec `svar/ask!` path is retired.
- Caller `:spec` may flow inside svar, but Vis owns `ask-code!` surface.
- Required spec field -> destructure directly.
- Optional field -> `(when (:field x) ...)` or `(or (:field x) default)`.
- Wrong shape after spec means fix the spec, not consumer noise.

## TUI key policy

- Leave `Ctrl+Y` unbound: kernel sends `SIGTSTP`/`DSUSP` before Lanterna sees it.
- Reject `Ctrl+Y` bindings in `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/{input,dialogs,screen}.clj`.
- Clipboard: `Ctrl+K`, per-message copy buttons, mouse-selection auto-copy.

## Boundary rule

For IO, async, invoke, process, or DB boundaries: make state explicit and failures observable.
