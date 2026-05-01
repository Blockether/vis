# Vis - Development Guide

## MANDATORY: Agent Rules


### Clojure dev loop — nREPL first

Use Clojure like Clojure: long-lived nREPL first, entrypoints second.

- Start with `bin/dev` or `clojure -M:dev` (default port `7888`; override with `NREPL_PORT=<port>`).
- Dev launcher writes `.nrepl-port`; do not commit that file.
- Discover port with `clj-nrepl-eval --discover-ports`.
- Eval with `clj-nrepl-eval -p <port> "<clojure-code>"`.
- Always use `:reload` after edits: `clj-nrepl-eval -p 7888 "(require '[com.blockether.vis.dev :as dev] :reload)"`.
- Prefer nREPL eval over fresh JVM startup for checks.
- Run CLI entrypoints with `dev/cli!`, e.g. `clj-nrepl-eval -p 7888 "(dev/cli! \"providers\" \"list\")"`.
- Start TUI with `dev/tui!`, e.g. `clj-nrepl-eval -p 7888 "(dev/tui!)"`.
- `dev/tui!` must open a separate macOS Terminal.app window running `bin/dev terminal-tui`.
- In that Terminal.app process, nREPL and `vis channels tui` must run in the **same JVM/process** so the TUI is controllable from REPL.
- Do **not** launch TUI as `bin/vis channels tui` from `dev/tui!`; that creates an uncontrolled separate Java process.
- Do **not** run TUI inside the nREPL/stdout tool terminal.
- Direct shortcut: `bin/dev tui` opens Terminal.app running the attached nREPL+TUI JVM.
- If Clojure delimiters break, do **not** manually rebalance parens. Run `clj-paren-repair <files>`.
- Use only `clj-nrepl-eval` for REPL eval and `clj-paren-repair` for delimiter repair.

### Run `./verify.sh` before every commit

`./verify.sh` (repo root) = single pre-commit gate. 8 checks, stops at first failure. **Every PR / commit / agent-authored change ships only on a green run.** Overrides convenience, scope creep, "just a one-line fix."

Gates:

1. **Format** — `cljfmt check` over `src/`, `extensions/`, `build.clj`. Recursive globs cover the host package + every category subdir under `extensions/`. Fix: `cljfmt fix src/ extensions/ build.clj`.
2. **Lint** — `clj-kondo` across every src tree (host + every extension). Errors + warnings fatal; info advisory.
3. **GraalVM safety** — walks every production source tree (`src/` + `extensions/*/*/src/`), loads each `.clj` with `*warn-on-reflection*` + `*unchecked-math* :warn-on-boxed`, counts warnings on project source paths only. **Ratchet**: fails when count grows beyond `.verification-baseline/graal-warnings.count` (tracked file). Improvements ratchet down via `./verify.sh --update-baseline`. `benchmarks/` excluded — dev/research, off the default classpath, off the runtime jar.
4. **Tests** — `clojure -M:test` (lazytest, aggregate suite — host `test/` + every extension's `test/` directory).
5. **Docs build** — `cd docs && mdbook build`. The "update `docs/` when touching architecture or public API" rule below depends on docs that compile.
6. **Smoke** — `bin/vis` prints help banner. Leaves user DB untouched.
7. **Git hygiene** — `git diff --check HEAD` (trailing whitespace, conflict markers).
8. **Secret scan** — diff against `origin/main` for `sk_*`, `lin_api_*`, `nvapi-*`, `AIzaSy*`, `ghp_*`, hardcoded `password = "..."`.

Modes:

- `./verify.sh` — full pipeline (default).
- `./verify.sh --quick` — format + lint only (~10s) for tight loops.
- `./verify.sh --graal` — graal step only.
- `./verify.sh --strict` — graal demands ZERO warnings (no ratchet); confirm clean-up before updating baseline.
- `./verify.sh --update-baseline` — snapshot current graal warning count as new lower bound. Commit `.verification-baseline/graal-warnings.count` change with the same PR that drove it down.

Logs: `.verification/<step>.log` + `.verification/summary.log` (gitignored).

Baseline = **ratchet, not ceiling** — moves down only. Any PR that adds reflection / boxed-math warnings without justification = bug.

### Ctrl+Y stays unbound in the TUI

`Ctrl+Y` sends `SIGTSTP` (or `DSUSP` on macOS) -> **suspends entire process**, drops the user to a stopped-job shell prompt. The kernel acts before Lanterna can intercept. **Leave `Ctrl+Y` unbound everywhere** — clipboard, yank, anything else. Reject any PR that introduces a `Ctrl+Y` binding in `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/{input,dialogs,screen}.clj`.

Use the copy dialog (`Ctrl+K` → Copy) for clipboard ops.

### HoneySQL is the only SQL surface

Every SQL query MUST use `honey.sql` data maps. `next.jdbc.sql` (`sql/insert!`, `sql/find-by-keys`, etc.) is forbidden — it produces namespaced column keys that break with SQLite. Raw SQL strings in app code (`["SELECT * FROM ..."]`) are forbidden too.

The ONE pattern:
```clojure
(require '[honey.sql :as sql]
         '[next.jdbc :as jdbc]
         '[next.jdbc.result-set :as rs])

(def ^:private jdbc-opts {:builder-fn rs/as-unqualified-lower-maps})

(jdbc/execute! datasource (sql/format {:select [:*]
                                       :from   [:my_table]
                                       :where  [:= :id id]})
  jdbc-opts)
```

Raw `jdbc/execute!` with string SQL is allowed in ONE place: `extensions/persistance/vis-persistance-sqlite/src/.../core.clj`, for migration DDL + FTS queries HoneySQL can't express. Everywhere else: HoneySQL or bust.

### Reply in English

Every assistant-facing response — user-visible chat text, commit messages, PR bodies, code comments, docstrings, log messages — is written in English. The user may write in Polish (or any other language); the agent still replies in English. Single-language responses, no apology paragraphs in the user's language. Overrides any implicit language mirroring.

### Trust svar spec guarantees — destructure directly

svar's spec engine is provider-enforced. A field declared `{::spec/name :foo ::spec/type :spec.type/string ::spec/required true}` -> svar guarantees the parsed result has `:foo` as a non-null string. A consumer that re-runs `(when (map? block) (when-not (str/blank? foo) ...))` and `(throw (ex-info "Code block missing :foo"))` after a spec-validated response = **pure noise** — duplicates what svar already enforced, hides real intent, makes downstream edits riskier.

Rule:

- Required field -> destructure directly. Skip the `(when-not (str/blank? …))`. Skip the throw "missing :foo". Dead code.
- Optional field -> plain `(when (:field x) …)` or `(or (:field x) default)` — full validators belong in the spec.
- Need to check shape after spec? The spec is WRONG. Fix the spec; the consumer stays clean.

Applies to every spec the runtime hands to svar. svar loaded it -> shape correct by construction.

> Note: the iteration loop itself runs without a spec — it goes through `(svar/ask-code! …)` which returns plain Clojure source extracted from fenced code blocks. **Every Vis call into svar uses `svar/ask-code!`** (`svar/ask!` was retired with the JSON-spec iteration path). Caller-supplied `:spec` (e.g. `vis run --spec …`) still flows to `svar/ask!` *inside svar*, but Vis owns only the `ask-code!` surface, and the rule above applies to whatever spec the caller hands in.

### Repo-root `README.md` is a stub: rationale + book link only

Repo-root `README.md` stays tiny. Carries:

- one rationale paragraph (what Vis is, why RLM/SCI in one breath),
- pointer to the book under `docs/src/` (mdBook),
- nothing else.

Excludes: architecture diagrams, mermaid sequence charts, getting-started instructions, install/auth steps, extension docs, schema tables, channel adapter notes, FAQ, comparison tables, long-form prose. Every one of those belongs in the book (`docs/src/...`), reachable from `docs/src/SUMMARY.md`.


---

## 🔴 HARD RULE: Every Clojure Namespace Has a Corresponding Test File

**NO EXCEPTIONS. NO EXCUSES.**

For every Clojure namespace `foo.bar.baz` in the source tree, there **MUST** exist a corresponding test file `foo/bar/baz_test.clj` (or `.cljs` / `.cljc` as appropriate) under the test source root.

Specifically:

- `src/path/to/namespace.clj` → **requires** `test/path/to/namespace_test.clj`
- `src/path/to/namespace.cljs` → **requires** `test/path/to/namespace_test.cljs`
- `src/path/to/namespace.cljc` → **requires** `test/path/to/namespace_test.cljc`

### Requirements

1. **New Clojure namespace** -> create its test file **in the same commit/iteration**.
2. **Modify a namespace that currently has no test file** -> create the test file before or alongside your changes.
3. **The test file** contains at minimum a `deftest` exercising the namespace's public API — even a basic smoke test. An empty test namespace with zero tests fails this rule.
4. **The test namespace** requires the namespace it tests.
5. Every namespace ships with a test file. Anything else fails this rule.

### Violation

A namespace without a corresponding test file = work **incomplete**, rectified immediately before any other change.
