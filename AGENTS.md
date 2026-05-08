# Vis - Agent Nucleus

```text
λ engage(nucleus).
[phi fractal euler tao mu ∃ ∀]
| [Δ λ Ω ∞/0 | ε/φ Σ/μ c/h signal/noise order/entropy truth/provability self/other]
| OODA
Human ⊗ AI ⊗ REPL

λ operate(x). reproduce -> inspect(runtime) -> change(minimal) -> test(regression) -> verify
λ style(x). English only | caveman terse > prose | clarity_exception(misread_risk)
λ truth(x). runtime > source > docs > assumption
λ fix(bug). reproduce(minimal) -> trace(cause) -> fix(structural) -> regression_test | ¬repro -> ¬diagnosis
λ sync(f). edit(f) -> reread(f) -> reload(ns) -> verify(relevant)
λ safety(x). headless_terminal_safe | observable > opaque | signal > suppress | user_data_owned_by_user
```

## S5 - Identity: non-negotiables

### Reproduce first

- Bug work starts with concrete reproduction. No repro -> no diagnosis -> no fix.
- Repro through real integration path first, then reduce to smallest seam.
- Good seams: pure fn, transcript helper, provider config/coercion, svar call, iteration-loop harness, CLI helper, persistence call, pure TUI render/state test.
- Convert repro into regression test or documented diagnostic helper before done.

### Runtime truth

- Runtime state beats files. Use Vis APIs before storage poking.
- For conversations: start with `com.blockether.vis.ext.foundation.transcript/transcript`.
- For providers: inspect `vis/active-provider`, `vis/provider-ids`, `vis/registered-providers`, `vis/provider-template`, `vis/->svar-provider`.
- Missing diagnostic view -> add Clojure helper + test. Do not bypass app.

### English + caveman

- All assistant-facing text is English: chat, commits, PRs, comments, docs, logs.
- Default response style: caveman terse. Drop filler. Keep technical exactness.
- Use fuller prose only when brevity risks destructive-action or multi-step misread.

### Headless-safe terminal path

- Do not load graphical UI libraries from terminal/TUI code paths.
- Do not add graphical TUI backends.
- Vis terminal mode must work in terminals, CI, SSH, TUI-only envs.

### README stub

Repo-root `README.md` stays tiny:

- one rationale paragraph: what Vis is + why RLM/SCI;
- link to mdBook under `docs/src/`;
- nothing else.

Long docs, install, FAQ, architecture, tables -> mdBook via `docs/src/SUMMARY.md`.

## S4 - Intelligence: how agent learns/adapts

### nREPL first, entrypoints second

```bash
clj-nrepl-eval --discover-ports
clj-nrepl-eval -p 7888 "(require '[com.blockether.vis.dev :as dev] :reload)"
```

Rules:

- Before starting dev JVM, discover nREPL ports.
- If Vis nREPL already runs, reuse it for REPL/dev checks.
- Do not start second dev nREPL unless existing one is unusable or wrong project.
- Starting intentional standalone process is allowed: `vis ...`, `bin/vis ...`, child JVM tests, packaged-entrypoint checks.
- Persistent SQLite is multiprocess-capable. Do not avoid standalone Vis solely because dev nREPL is open.
- Start dev only when needed: `bin/dev` or `clojure -M:dev` (`NREPL_PORT=<port>` optional).
- `.nrepl-port` is generated. Do not commit.
- Bash = discovery/process launch. Clojure behavior checks = nREPL.
- After edits: reload with `:reload`.

### CLI/TUI dev paths

```bash
clj-nrepl-eval -p 7888 "(dev/cli! \"providers\" \"list\")"
clj-nrepl-eval -p 7888 "(dev/tui!)"
bin/dev tui
vis channels tui
```

Rules:

- Use `dev/cli!` when REPL control matters.
- Use standalone `vis ...` / `bin/vis ...` for packaged entrypoint, terminal behavior, process lifecycle, multiprocess SQLite.
- `dev/tui!` opens separate macOS Terminal.app running `bin/dev terminal-tui`.
- In that Terminal process, nREPL + TUI run in same JVM so TUI is REPL-controllable.
- Do not launch `bin/vis channels tui` from `dev/tui!`; that defeats controllable-TUI path.
- Packaged-entrypoint TUI via `vis channels tui` / `bin/vis channels tui` is valid while dev nREPL is open.
- Do not run TUI inside nREPL/stdout tool terminal.

### Clojure edits

Use `z/patch` for `.clj` / `.cljc` / `.cljs` / `.edn` zipper edits. Same map shape as `v/patch`.

Rules:

- Prefer one vector of edit maps: `(z/patch [{:path p :search locator :replace replacement} ...])`.
- `:search` is a Clojure/EDN locator form/source snippet, not raw text.
- `:search` must match exactly once in the zipper; all edits validate before any write.
- Multiple edits to the same file are applied in vector order and written once.
- Discover locators with `(z/locators path)` or `(z/symbols path)` when unsure.
- Full rewrite-clj zipper API is available under `z/`; `z/subedit->` works in SCI for advanced transforms.
- Do not use `(declare ...)` unless strictly required by mutual recursion or an unavoidable load-order cycle. Prefer sorting defs in dependency order. If a declare is kept, add a nearby comment explaining why sorting cannot remove it.
- If delimiters break: do not manually rebalance. Run `clj-paren-repair <files>`.
- Use only `clj-nrepl-eval` for REPL eval and `clj-paren-repair` for delimiter repair.

Pattern:

```clojure
(z/patch [{:path "src/foo.clj"
           :search "old-sym"
           :replace "new-sym"}])
```

## S3 - Control: enforce policy

### Verification cadence

- Docs-only changes, including `AGENTS.md`, do not require `verify.sh`.
- During code edits: `./verify.sh --quick`.
- Before commit/commit-ready handoff: full `./verify.sh`.

### Tests for every namespace

Hard rule: every Clojure source namespace needs matching test namespace.

Mapping:

- `src/path/to/foo.clj` -> `test/path/to/foo_test.clj`
- `src/path/to/foo.cljs` -> `test/path/to/foo_test.cljs`
- `src/path/to/foo.cljc` -> `test/path/to/foo_test.cljc`

Requirements:

1. New namespace -> create test same iteration.
2. Modify namespace lacking test -> create test before/with change.
3. Test file must require namespace under test.
4. Test file must contain at least one real public-API smoke/regression test.
5. Empty test namespace fails.

Violation = work incomplete. Fix before other work.

### HoneySQL only

- Every SQL query uses `honey.sql` maps.
- Forbidden: `next.jdbc.sql` (`sql/insert!`, `sql/find-by-keys`, etc.). It creates namespaced column keys that break SQLite.
- Forbidden: raw SQL strings in app code.
- Exception: raw `jdbc/execute!` strings allowed only in `extensions/persistance/vis-persistance-sqlite/src/.../core.clj` for migration DDL + FTS HoneySQL cannot express.

Pattern:

```clojure
(require '[honey.sql :as sql]
         '[next.jdbc :as jdbc]
         '[next.jdbc.result-set :as rs])

(def ^:private jdbc-opts {:builder-fn rs/as-unqualified-lower-maps})

(jdbc/execute! datasource
  (sql/format {:select [:*]
               :from   [:my_table]
               :where  [:= :id id]})
  jdbc-opts)
```

### SQLite persistence

Persistent SQLite is multiprocess-capable.

- Do not reintroduce process-exclusive persistent DB lock: no `vis.db.lock`, no `:vis/persistent-db-already-open`.
- Normal writes rely on SQLite WAL + `busy_timeout` + `transaction_mode=IMMEDIATE` + `sqlite-write-tx!` retry boundary.
- Schema install/repair is only process-serialized bootstrap path, via short-lived `vis.db.migrate.lock` around Flyway + inline V1 repair.
- Do not delete/move/unlink/recreate `~/.vis/vis.mdb` or `vis.db` while any Vis process may have it open. Close all Vis processes first.
- Multiprocess behavior needs real process/JVM regression tests, not same-JVM-only tests.

### SQLite schema changes

Until told otherwise, schema changes are inline:

- Edit `extensions/persistance/vis-persistance-sqlite/resources/db/sqlite/migration/V1__schema.sql` directly.
- Do not add `V2__...sql`, `V3__...sql`, etc.
- Local dev DB may be deleted/recreated only after all Vis processes using it are closed.
- If user asks for real migrations/backward-compatible upgrade path, this rule suspends for that task.

### svar spec guarantees

- Vis calls svar through `svar/ask-code!`; old Vis-side JSON-spec `svar/ask!` path retired.
- Caller-supplied `:spec` may flow inside svar, but Vis owns `ask-code!` surface.
- Required spec field -> destructure directly. No blank checks. No "missing field" throws.
- Optional field -> `(when (:field x) ...)` or `(or (:field x) default)`.
- Shape wrong after spec -> spec is wrong. Fix spec, not consumer noise.

### TUI Ctrl+Y

`Ctrl+Y` sends `SIGTSTP`/`DSUSP`; kernel suspends process before Lanterna sees it.

- Leave `Ctrl+Y` unbound everywhere.
- Reject bindings in:
  `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/{input,dialogs,screen}.clj`
- Clipboard ops: `Ctrl+K` -> Copy, per-message copy buttons, mouse-selection auto-copy.

## S2 - Coordination: parts working together

```text
λ coordinate(x). REPL(control) ∥ standalone(entrypoint) ∥ SQLite(shared)
λ db(x). WAL + immediate_tx + retry | migrate_lock(short) | ¬exclusive_process_lock
λ tui(x). controllable(dev/tui!) ∨ packaged_entrypoint(vis channels tui) | choose_by_task
λ boundaries(x). io ∨ async ∨ invoke ∨ process ∨ db -> explicit_state + observable_failure
```

- nREPL is preferred control plane, not sole runtime.
- Standalone processes are valid when testing real process boundaries.
- SQLite handles cross-process runtime access; migration lock handles schema bootstrap race.
- Process/file/DB boundaries must be observable and explicit.

## S1 - Operations: concrete commands/snippets

### nREPL/runtime snippets

```bash
clj-nrepl-eval --discover-ports
clj-nrepl-eval -p 7888 "(require '[com.blockether.vis.core :as vis] :reload)"
```

```clojure
(require '[com.blockether.vis.core :as vis] :reload)
(require '[com.blockether.vis.ext.foundation.transcript :as tr] :reload)
(let [db (vis/db-info)
      cid (vis/db-resolve-conversation-id db "faf9b353")]
  (tr/transcript db cid))
```

```clojure
{:active-provider (vis/active-provider)
 :provider-ids (vis/provider-ids)
 :registered-provider-ids (mapv :provider/id (vis/registered-providers))
 :runtime-provider (vis/->svar-provider (vis/active-provider))}
```

### Common commands

```bash
./verify.sh --quick
./verify.sh
clj-paren-repair <files>
clj-nrepl-eval -p 7888 "(dev/cli! \"providers\" \"list\")"
clj-nrepl-eval -p 7888 "(dev/tui!)"
bin/dev tui
vis channels tui
```
