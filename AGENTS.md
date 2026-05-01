# Vis - Development Guide

## MANDATORY: Agent Rules


### Clojure dev loop — nREPL first

Use Clojure like Clojure: long-lived nREPL first, entrypoints second.

- Before starting a dev JVM, check for an existing nREPL with `clj-nrepl-eval --discover-ports`.
- If a Vis nREPL is already running, reuse that port; do **not** start a second nREPL/JVM unless the existing one is unusable or belongs to another project.
- Start with `bin/dev` or `clojure -M:dev` only when no reusable nREPL is running (default port `7888`; override with `NREPL_PORT=<port>`).
- Dev launcher writes `.nrepl-port`; do not commit that file.
- Discover port with `clj-nrepl-eval --discover-ports`.
- Eval with `clj-nrepl-eval -p <port> "<clojure-code>"`.
- Always use `:reload` after edits: `clj-nrepl-eval -p 7888 "(require '[com.blockether.vis.dev :as dev] :reload)"`.
- Prefer nREPL eval over fresh JVM startup for checks.
- Runtime investigation is **nREPL data-first**. Use the running process to exercise the same code paths Vis uses, inspect Clojure data, then turn the finding into a focused repro/test.
- Bash is for file discovery, process control, and launching `clj-nrepl-eval`; Clojure state and behavior checks belong in nREPL.
- Run CLI entrypoints with `dev/cli!`, e.g. `clj-nrepl-eval -p 7888 "(dev/cli! \"providers\" \"list\")"`.
- Start TUI with `dev/tui!`, e.g. `clj-nrepl-eval -p 7888 "(dev/tui!)"`.
- `dev/tui!` must open a separate macOS Terminal.app window running `bin/dev terminal-tui`.
- In that Terminal.app process, nREPL and `vis channels tui` must run in the **same JVM/process** so the TUI is controllable from REPL.
- Do **not** launch TUI as `bin/vis channels tui` from `dev/tui!`; that creates an uncontrolled separate Java process.
- Do **not** run TUI inside the nREPL/stdout tool terminal.
- Direct shortcut: `bin/dev tui` opens Terminal.app running the attached nREPL+TUI JVM.
- If Clojure delimiters break, do **not** manually rebalance parens. Run `clj-paren-repair <files>`.
- Use only `clj-nrepl-eval` for REPL eval and `clj-paren-repair` for delimiter repair.

### Runtime investigation strategy — nREPL data-first

Do this:

1. Attach to the live nREPL and reload namespaces before inspection.
2. Use Vis Clojure APIs, not storage files, to inspect runtime state.
3. For conversations, start with `com.blockether.vis.ext.foundation.transcript/transcript` and inspect the returned Clojure map.
4. For providers, inspect `vis/active-provider`, `vis/provider-ids`, `vis/registered-providers`, `vis/provider-template`, and `vis/->svar-provider`.
5. Reproduce at the smallest app seam: transcript helper, provider config/coercion, iteration-loop harness, or pure TUI render/state test.
6. If the needed diagnostic view does not exist, add a Clojure helper and a test instead of bypassing the app.

Useful snippets:

```bash
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

### Clojure source edits — use `z/` structured editing

Do this for non-trivial `.clj` / `.cljc` / `.cljs` / `.edn` edits:

1. Use `z/zedit` when changing forms, symbols, requires, map entries, vectors, or nested Clojure data.
2. Reproduce the edit on a small string in nREPL first when the zipper path is not obvious: `(z/of-string source {:track-position? true})`, find the target, inspect it, then apply the same path in `z/zedit`.
3. Treat the zipper as immutable: every movement/edit returns the next `zloc`; return the final zipper from the `z/zedit` function.
4. Find symbols by value with an explicit depth-first move: `(z/find-value zloc z/next 'some-symbol)`. The short form searches right siblings only; do not use it for whole-file scans.
5. Verify the hit before changing it: inspect `(z/sexpr hit)`, `(z/tag hit)`, and `(z/position-span hit)` when location matters. `z/zedit` creates zippers with position tracking enabled.
6. Change the current node with `(z/replace hit new-form)` or `(z/edit hit f & args)`.
7. Move structurally, not by text offsets: `z/down`, `z/up`, `z/right`, `z/left`, `z/next`, `z/prev` skip whitespace/comments and preserve them in output.
8. Use the `*` variants (`z/right*`, `z/next*`, `z/of-file*`) only when whitespace/comment nodes themselves matter. Normal navigation intentionally skips whitespace/comments while keeping them in `z/root-string`.
9. Use `z/subedit->` / `z/subedit->>` when editing inside one found form and you want the final location to remain at that form.
10. If there is no reliable structural target, first add a helper/predicate that finds the right form, then edit through that helper.

Useful patterns:

```clojure
;; Replace a symbol everywhere.
(z/zedit "src/foo.clj"
  (fn [zl]
    (loop [zloc zl]
      (if-let [hit (z/find-value zloc z/next 'old-sym)]
        (recur (z/replace hit 'new-sym))
        zloc))))

;; Replace the value to the right of a key in a map/vector-ish form.
(z/zedit "src/foo.clj"
  (fn [zl]
    (if-let [k (z/find-value zl z/next :some-key)]
      (-> k z/right (z/replace :new-value))
      zl)))

;; Add a require without hand-editing whitespace.
(z/zedit "src/foo.clj"
  (fn [zl]
    (if-let [req (z/find-value zl z/next :require)]
      (-> req z/up (z/append-child '[clojure.string :as str]))
      zl)))

;; Edit inside one found form, then return to that form's location.
(z/zedit "src/foo.clj"
  (fn [zl]
    (if-let [let-sym (z/find-value zl z/next 'let)]
      (-> let-sym
          z/up
          (z/subedit->
            (z/find-value z/next 'old-local)
            (z/replace 'new-local)))
      zl)))
```

### Verification cadence

Do this:

- Documentation-only changes, including `AGENTS.md`, do not require `verify.sh`.
- During code edits, use `./verify.sh --quick` for the loop.
- Run full `./verify.sh` only before committing or handing off a code change as commit-ready.

### Ctrl+Y stays unbound in the TUI

`Ctrl+Y` sends `SIGTSTP` (or `DSUSP` on macOS) -> **suspends entire process**, drops the user to a stopped-job shell prompt. The kernel acts before Lanterna can intercept. **Leave `Ctrl+Y` unbound everywhere** — clipboard, yank, anything else. Reject any PR that introduces a `Ctrl+Y` binding in `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/{input,dialogs,screen}.clj`.

Use the copy dialog (`Ctrl+K` → Copy) for clipboard ops.

### Avoid AWT entirely

Do **not** introduce AWT usage in Vis code. Avoid `java.awt.*`, `javax.swing.*`, desktop integrations, system tray APIs, clipboard access through AWT, and anything that requires a graphical JVM. Vis must stay safe in headless terminals, CI, SSH sessions, and TUI-only environments. If a feature appears to need AWT, design a non-AWT terminal/OS-specific adapter instead.

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
