# Vis - Development Guide

## MANDATORY: Agent Rules

### Run `./verify.sh` before every commit

`./verify.sh` (repo root) = single pre-commit gate. 8 checks, stops at first failure. **No PR / commit / agent-authored change ships without a green run.** Overrides convenience, scope creep, "just a one-line fix."

Gates:

1. **Format** — `cljfmt check` over `src/`, `extensions/`, `build.clj`. Recursive globs cover the host package + every category subdir under `extensions/`. Fix: `cljfmt fix src/ extensions/ build.clj`.
2. **Lint** — `clj-kondo` across every src tree (host + every extension). Errors + warnings fatal; info advisory.
3. **GraalVM safety** — walks every production source tree (`src/` + `extensions/*/*/src/`), loads each `.clj` with `*warn-on-reflection*` + `*unchecked-math* :warn-on-boxed`, counts warnings on project source paths only. **Ratchet**: fails if count grows beyond `.verification-baseline/graal-warnings.count` (tracked file). Improvements ratchet down via `./verify.sh --update-baseline`. `benchmarks/` excluded — dev/research, not on default classpath, not shipped as runtime jar.
4. **Tests** — `clojure -M:test` (lazytest, aggregate suite — host `test/` + every extension's `test/` directory).
5. **Docs build** — `cd docs && mdbook build`. The "update `docs/` when touching architecture or public API" rule below is meaningless if docs don't compile.
6. **Smoke** — `bin/vis` prints help banner. Doesn't touch user DB.
7. **Git hygiene** — `git diff --check HEAD` (trailing whitespace, conflict markers).
8. **Secret scan** — diff against `origin/main` for `sk_*`, `lin_api_*`, `nvapi-*`, `AIzaSy*`, `ghp_*`, hardcoded `password = "..."`.

Modes:

- `./verify.sh` — full pipeline (default).
- `./verify.sh --quick` — format + lint only (~10s) for tight loops.
- `./verify.sh --graal` — graal step only.
- `./verify.sh --strict` — graal demands ZERO warnings (no ratchet); confirm clean-up before updating baseline.
- `./verify.sh --update-baseline` — snapshot current graal warning count as new lower bound. Commit `.verification-baseline/graal-warnings.count` change with the same PR that drove it down.

Logs: `.verification/<step>.log` + `.verification/summary.log` (gitignored).

Baseline = **ratchet, not ceiling** — only moves down. Any PR adding reflection / boxed-math warnings without justification = bug.

### Never bind Ctrl+Y in the TUI

`Ctrl+Y` sends `SIGTSTP` (or `DSUSP` on macOS) -> **suspends entire process**, drops user to stopped-job shell prompt. Lanterna can't intercept before kernel acts. Do NOT bind `Ctrl+Y` to anything — clipboard, yank, anything else. Will never work. Reject any PR that re-introduces a `Ctrl+Y` binding in `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/{input,dialogs,screen}.clj`.

Use copy dialog (`Ctrl+K` → Copy) instead.

### Always use HoneySQL for SQL — no raw strings, no next.jdbc.sql

Every SQL query MUST use `honey.sql` data maps. Do NOT use `next.jdbc.sql` (`sql/insert!`, `sql/find-by-keys`, etc.) — produces namespaced column keys that break with SQLite. Do NOT write raw SQL strings in app code (`["SELECT * FROM ..."]`).

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

Raw `jdbc/execute!` with string SQL allowed ONLY inside `extensions/persistance/vis-persistance-sqlite/src/.../core.clj` for migration DDL + FTS queries HoneySQL can't express. Everywhere else: HoneySQL or bust.

### Always reply in English

Every assistant-facing response — user-visible chat text, commit messages, PR bodies, code comments, docstrings, log messages — written in English. User may write in Polish (or anything else); agent still replies English. No mixed-language responses, no apology paragraphs in user's language. Overrides any implicit language mirroring.

### Trust svar spec guarantees — do NOT defensively re-validate

svar's spec engine is provider-enforced. Field declared `{::spec/name :foo ::spec/type :spec.type/string ::spec/required true}` -> svar guarantees parsed result has `:foo` as non-null string. Writing `(when (map? block) (when-not (str/blank? foo) ...))` and `(throw (ex-info "Code block missing :foo"))` after spec-validated response = **pure noise** — duplicates what svar already enforced, hides real intent, makes downstream edits riskier.

Rule:

- Required field -> destructure directly. No `(when-not (str/blank? …))`. No throw "missing :foo". Dead code.
- Optional field -> plain `(when (:field x) …)` or `(or (:field x) default)` — NOT a full validator.
- Need to check shape after spec? Spec is WRONG. Fix the spec, not the consumer.

Applies to every spec the runtime hands to svar. svar loaded it -> shape correct by construction.

> Note: the iteration loop itself does NOT use a spec anymore — it goes through `(svar/ask-code! …)` which returns plain Clojure source extracted from fenced code blocks. Caller-supplied `:spec` (e.g. `vis run --spec …`) still flows to `svar/ask!` and the rule above applies there.

### Repo-root `README.md` is a stub: rationale + book link only

Repo-root `README.md` intentionally tiny. Carries:

- one rationale paragraph (what Vis is, why RLM/SCI in one breath),
- pointer to book under `docs/src/` (mdBook),
- nothing else.

Does NOT carry: architecture diagrams, mermaid sequence charts, getting-started instructions, install/auth steps, extension docs, schema tables, channel adapter notes, FAQ, comparison tables, long-form prose. Every one of those belongs in the book (`docs/src/...`), reachable from `docs/src/SUMMARY.md`.


---

## 🔴 HARD RULE: Every Clojure Namespace MUST Have a Corresponding Test File

**NO EXCEPTIONS. NO EXCUSES.**

For every Clojure namespace `foo.bar.baz` in the source tree, there **MUST** exist a corresponding test file `foo/bar/baz_test.clj` (or `.cljs` / `.cljc` as appropriate) under the test source root.

Specifically:

- `src/path/to/namespace.clj` → **requires** `test/path/to/namespace_test.clj`
- `src/path/to/namespace.cljs` → **requires** `test/path/to/namespace_test.cljs`
- `src/path/to/namespace.cljc` → **requires** `test/path/to/namespace_test.cljc`

### Requirements

1. **When you create a new Clojure namespace**, you MUST create its test file **in the same commit/iteration**.
2. **When you modify an existing Clojure namespace** that has no test file, you MUST create the missing test file before or alongside your changes.
3. **The test file MUST** contain at minimum a `deftest` that exercises the namespace's public API — even a basic smoke test. An empty test namespace with zero tests is **not acceptable**.
4. **The test namespace MUST** require the namespace it tests.
5. **Never** leave a namespace without a test file and move on. If you do, you have failed this rule.

### Violation

If at any point a Clojure namespace exists without a corresponding test file, the work is **incomplete** and must be rectified immediately before proceeding with any other changes.
