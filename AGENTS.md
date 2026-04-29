# Vis - Development Guide

## MANDATORY: Agent Rules

### Run `./verify.sh` before every commit

`./verify.sh` (repo root) = single pre-commit gate. 8 checks, stops at first failure. **No PR / commit / agent-authored change ships without a green run.** Overrides convenience, scope creep, "just a one-line fix."

Gates:

1. **Format** — `cljfmt check` over `packages/`, `extensions/`, `build.clj`. Recursive globs cover every host package + every category subdir under `extensions/`. Fix: `cljfmt fix packages/ extensions/ build.clj`.
2. **Lint** — `clj-kondo` across every package src tree. Errors + warnings fatal; info advisory.
3. **GraalVM safety** — walks every production package's `src/`, loads each `.clj` with `*warn-on-reflection*` + `*unchecked-math* :warn-on-boxed`, counts warnings on project source paths only. **Ratchet**: fails if count grows beyond `.verification-baseline/graal-warnings.count` (tracked file). Improvements ratchet down via `./verify.sh --update-baseline`. `benchmarks/` excluded — dev/research, not on default classpath, not shipped as runtime jar.
4. **Tests** — `clojure -M:test` (lazytest, aggregate suite).
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

`Ctrl+Y` sends `SIGTSTP` (or `DSUSP` on macOS) -> **suspends entire process**, drops user to stopped-job shell prompt. Lanterna can't intercept before kernel acts. Do NOT bind `Ctrl+Y` to anything — clipboard, yank, anything else. Will never work. Reject any PR that re-introduces a `Ctrl+Y` binding in `input.clj`, `dialogs.clj`, `screen.clj`.

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

Raw `jdbc/execute!` with string SQL allowed ONLY inside `persistance/sqlite/*.clj` for migration DDL + FTS queries HoneySQL can't express. Everywhere else: HoneySQL or bust.

### Always reply in English

Every assistant-facing response — user-visible chat text, commit messages, PR bodies, code comments, docstrings, log messages — written in English. User may write in Polish (or anything else); agent still replies English. No mixed-language responses, no apology paragraphs in user's language. Overrides any implicit language mirroring.

### Trust svar spec guarantees — do NOT defensively re-validate

svar's iteration spec is provider-enforced. Field declared `{::spec/name :expr ::spec/type :spec.type/string ::spec/required true}` -> svar guarantees parsed result has `:expr` as non-null string. Writing `(when (map? block) (when-not (str/blank? expr) ...))` and `(throw (ex-info "Code block missing :time-ms"))` after spec-validated response = **pure noise** — duplicates what svar already enforced, hides real intent, makes downstream edits riskier.

Rule:

- Required field -> destructure directly. No `(when-not (str/blank? …))`. No throw "missing :time-ms". Dead code.
- Optional field -> plain `(when (:field x) …)` or `(or (:field x) default)` — NOT a full validator.
- Need to check shape after spec? Spec is WRONG. Fix the spec, not the consumer.

Applies to EVERY spec, not just iteration: code_block, next_turn, all of them. svar loaded it -> shape correct by construction.

### Repo-root `README.md` is a stub: rationale + book link only

Repo-root `README.md` intentionally tiny. Carries:

- one rationale paragraph (what Vis is, why RLM/SCI in one breath),
- pointer to book under `docs/src/` (mdBook),
- nothing else.

Does NOT carry: architecture diagrams, mermaid sequence charts, getting-started instructions, install/auth steps, extension docs, schema tables, channel adapter notes, FAQ, comparison tables, long-form prose. Every one of those belongs in the book (`docs/src/...`), reachable from `docs/src/SUMMARY.md`.

Why: book = single source of truth (see rule below). Duplicating into `README.md` -> two versions drift, `README.md` always loses. Already paid for that mistake — do not repeat. Adding a new section to `README.md`? It belongs in the book; add under `docs/src/...`, link from `SUMMARY.md`, at most add one bullet in README's docs list pointing at it.

Reject any PR that grows `README.md` past a screenful (rationale paragraph + short "Documentation" list pointing into `docs/src/`).

### Every extension declares itself in vis.edn with an inline README

Every extension under `extensions/<name>/` MUST ship exactly one classpath manifest at:

```
resources/META-INF/vis-extension/vis.edn
```

EDN map keyed by extension **id** (short symbol; same token LLM uses as SCI sandbox alias — e.g. `meta`, `vis`, `git`):

```edn
{meta
 {:nses [com.blockether.vis.ext.common-meta.core]
  :docs {"README.md"
         {:created-at  #inst "2026-04-28"
          :description "One-paragraph LLM-facing summary..."
          :content     "# Meta extension\n..."
          :links       [{:to-id vis :to-doc "README.md"
                         :context "Companion filesystem extension"}
                        {:url "https://arxiv.org/abs/2512.24601"
                         :context "RLM paper"}
                        {:file "packages/vis-extension/src/com/blockether/vis_extension/extension.clj"
                         :context "Loader implementation"}]}}}}
```

Slots:

- `:nses` — vec of namespaces loader `require`s. Each loaded ns self-registers via `register-global!` (or sibling registry calls). Plural intentional — one id can span multiple namespaces (ext registering symbols + channel + provider in three nses).
- `:docs` — map `{<doc-name> <descriptor>}`, descriptor = structured map below. Doc names = conventional Markdown filenames (`README.md`, `EXAMPLES.md`, `MIGRATION.md`, ...).

Doc descriptor fields:

- `:created-at` (mandatory) — `#inst` literal, when doc was first authored.
- `:description` (mandatory) — one-paragraph LLM-facing summary as plain Clojure string. NO YAML frontmatter inside `:content`; description is its own EDN field, not parsed out of Markdown.
- `:content` (mandatory) — full Markdown body. Pure Markdown only, no leading frontmatter block.
- `:links` (optional) — vec of author-declared outgoing references. Each link is one of:
  - cross-extension doc: `{:to-id <id> :to-doc <name> :context "..."}`
  - same-extension doc (omit `:to-id`): `{:to-doc <name> :context "..."}`
  - external URL: `{:url "https://..." :context "..."}`
  - repo file: `{:file "path/to/file.clj" :context "..."}`
- `:reflinks` — **derived**, never authored. Loader inverts every cross-ext / same-ext link into `{:from-id :from-doc :context}` on target descriptor. Hand-authoring `:reflinks` violates contract; loader overwrites.

Mandatory contents:

- Every extension's `:docs` MUST include a `"README.md"` entry. No README -> not shippable. Loader logs `:warn` and skips any doc descriptor missing `:description` or `:content`.
- Use real `#inst` literals for `:created-at`; registry preserves them as `java.util.Date` so agent can sort by freshness.

Rules:

- `vis.edn` = **single source of truth** for that extension (purpose, surface, when to use, when not). End users read via the book; LLM reads via `(meta/extension-doc '<id> "<doc-name>")` and `(meta/extension-readme '<id>)` from `:code`.
- Do NOT add a second copy of any doc anywhere in the extension tree (no `extensions/<name>/README.md`, no `docs/src/extensions/<id>.md` containing inlined content, no `extensions/<name>/src/.../README.md`). `vis.edn` is canonical.
- `:content` = Markdown with sentence-case headings + real UTF-8 (same rules as the book — see docs-style rule below). End-user-facing ext -> surface from `docs/src/SUMMARY.md` with a short manual page pointing readers at `(meta/extension-readme '<id>)` (book may briefly summarize, MUST NOT inline full body).
- The id (top-level key in `vis.edn`) MUST equal registered ext's `:ext/ns-alias :alias`. Loader doesn't enforce strictly, but reviewers MUST reject mismatches — LLM types `(meta/...)`, on-disk id is `meta`.
- Multiple jars MAY contribute under same id; their `:nses` deduped, `:docs` merged (last write wins per name). `:reflinks` recomputed across entire registry after every merge -> a later jar's links can target an earlier jar's docs.
- Editing tip: `:content` = multi-line EDN string. Supported authoring loop: keep Markdown body in sibling `.md` file in source tree, re-inline into `vis.edn` with a small `pprint`-based generator (see `dev/inline-doc.clj`-style helpers — contract is "produce valid EDN map"). Do NOT keep two on-disk copies of same content under version control.
- `vis-common-meta` exposes the LLM-facing surface: `(meta/extensions)` lists every loaded ext + docs catalog; `(meta/extension-docs ext-ref)` returns `[{:name :created-at :description :links :reflinks} …]` summaries for one ext; `(meta/extension-doc ext-ref name)` returns full descriptor including `:content`; `(meta/extension-readme ext-ref)` is the README `:content` convenience.

Why: agents that need to verify what an ext actually does — before reaching for one of its tools — must read the ext's own description as data, not guess from symbol names. Inlining docs in `vis.edn` = single classpath read at boot, eliminates path-vs-call ambiguity an external doc tree introduced (`vis/<id>` reads as a sandbox call), makes the description index trivially queryable.

Reject any PR that adds a new ext without `vis.edn`, omits the README, omits `:description` or `:content` on any declared doc, hand-authors a `:reflinks` field, or introduces a duplicate doc somewhere outside `vis.edn`.

### Always update `docs/` when touching architecture or public API

`docs/src/` (mdBook, repo root) = single source of truth for Vis architecture, extension system, public API. **Every** change to files under `loop/`, `persistance/`, `channels/`, or `core.clj` MUST be followed by an update to the relevant doc if the change affects:

- File moves, renames, deletions
- New files / namespaces
- Changes to iteration pipeline stages
- Changes to environment map shape or state atoms
- Changes to extension spec or nudge context
- Changes to public API (`create-environment`, `query!`, `register-extension!`, etc.)
- Changes to dependency graph between modules
- Changes to SQLite schema (`V1__schema.sql`)

Doc files (under repo-root `docs/`, NOT inside any package):
- `docs/src/README.md` — introduction (why RLM, why SCI, the problem, how Vis works, security model, prior art). Merged from old `rationale.md`; do NOT recreate `rationale.md`.
- `docs/src/usage.md` — getting started (install, auth, four ways to talk to the agent, browsing past conversations).
- `docs/src/architecture/` — overview, packages, iteration flow, state ownership, database schema, channels.
- `docs/src/extensions/` — overview, extension spec, symbol decorators (hooks), environment map, nudge system, vis-common-meta.

`docs/src/SUMMARY.md` = mdBook table of contents — keep in sync when adding/removing pages. Intentionally no `reference/` section or `directory-structure.md`; public API + current package layout documented inline in existing pages (and this file).

Build: `cd docs && mdbook serve --open`

Skipping this update = bug. Docs drifting from code is how we ended up with an incomprehensible god file in the first place.

### Docs style: real UTF-8, sentence-case headings, no code in titles

Three rules. Non-negotiable. Every prior violation shipped to readers as broken rendering or unreadable noise.

1. **Use real UTF-8 characters in Markdown.** Markdown is NOT JSON. `\u2014`, `\u251c`, `\u2500`, `\u2514`, `\u2026` = six-character ASCII strings, not characters. mdbook renders verbatim -> page looks like editor crash dump. Type the actual characters: `—` (em dash, U+2014), `…` (ellipsis, U+2026), `├` `─` `└` (box-drawing, U+2500 family). Editor escapes on save? Fix the editor; don't commit escapes. CI grep:

   ```bash
   grep -rn '\\u[0-9a-f]\{4\}' docs/src/ && exit 1
   ```

2. **Headings = short, descriptive, sentence-case prose. NO code identifiers in title.** Heading is a navigation aid for humans, not an API index entry.

   - Bad: `## \`:before-fn\` — entry decorator`, `### \`(meta/turn)\``, `### 1) \`conversation_soul\``, `### Embedded \`:cmd/subcommands\` vector`, `### Render caches (\`ext/channel_tui/render.clj :: fmt-cache\`)`, `## How Vis Works` (Title Case).
   - Good: `## Entry decorator`, `### Current turn snapshot`, `### Conversation soul`, `### Embedded subcommand vector`, `### Render caches`, `## How Vis works`.

   Keyword / function name / table name / file path the section documents goes in the **first line of body** as a short "Slot key: \`:before-fn\`" / "Call: \`(meta/turn)\`" / "Table: \`conversation_soul\`" / "Lives in: \`ext/channel_tui/render.clj\`" lead. Reader still gets the identifier; just isn't load-bearing on the heading.

3. **Internal links use slugified anchors of the live heading.** Renaming a heading invalidates every `#anchor` link to it. After any heading rename, grep for old anchor across `docs/src/` and repoint or delete every reference. Example: renaming `## Auto-discovery resource` to `## Auto-discovery` changes anchor from `#auto-discovery-resource` to `#auto-discovery`; doc that linked to old anchor breaks silently — mdbook does not warn.

### Triage conversations with vis-common-meta — let vis self-analyze

`~/.vis/vis.mdb/vis.db` = single source of truth for every conversation — turns, iterations, final answers, persisted SCI vars, timings, costs. Before hypothesizing about a user-reported bug that references a specific `conversation-id`, **let vis self-analyze**. The `vis-common-meta` ext (alias `meta`) exposes that data as plain Clojure maps from `:code`; agent reaches it via `bin/vis run`. Supported triage path — there is no other.

```bash
bin/vis run --json "Use the meta extension to analyze conversation \
  <conversation-uuid>. Call (meta/conversation #uuid \"<conversation-uuid>\"), \
  (meta/diagnose #uuid \"<conversation-uuid>\"), and \
  (meta/failures #uuid \"<conversation-uuid>\"). Summarize how many turns, \
  what the user asked, what failed (with classifications), and the root cause."
```

Returns structured JSON envelope with agent's answer, iteration trace, token usage, cost. Agent does failure classification, redundancy detection, cost rollup, plain-language summary — you read the `answer` field. Two iterations is the norm for a single-conversation post-mortem.

Full meta API (`(meta/turn)`, `(meta/conversation)`, `(meta/conversations)`, `(meta/diagnose)`, `(meta/failures)`, `(meta/find-attempts pattern)`, `(meta/var-history 'sym)`) documented inline in the extension manifest. Read via `(meta/extension-readme 'meta)` from `:code`; do not guess from symbol names.

Question genuinely can't be expressed through meta (schema or migration debugging — debugging the persistence layer itself, not a conversation)? Extend `vis-common-meta` with the call you wish you had instead of dropping to ad-hoc shell tools. The ext is the API; missing projection = bug in the ext, not a license to bypass it.

Only AFTER vis has self-analyzed may you form a hypothesis, propose a fix, or blame the model / UI / runtime loop. This rule exists because repeated incidents wasted a turn on plausible-sounding code-only explanations when one `bin/vis run` would have pinpointed the bug in two iterations.

## Clojure CLI

### Running

```bash
bin/vis                        # prints help tree (no implicit "default channel")
bin/vis channels tui           # TUI chat
bin/vis channels telegram      # Telegram bot
bin/vis run "prompt"           # one-shot agent query
clojure -M:test                # aggregate test runner across packages
```

`bin/vis` = checked-in wrapper that `exec`s `clojure -M:vis "$@"` from repo root. Add `bin/` to `PATH` once -> daily use is just `vis <subcommand>`.

Aliases (root `deps.edn`): `:vis` (the `vis` CLI), `:test`, `:bench`, `:build`, `:dev`, `:antq`.

### Bumping a shared dependency

vis = polylith — every package declares its OWN deps in `packages/<name>/deps.edn`. Intentionally NO `vis-common` god-bag. Bumping a coordinate that several packages share (e.g. `com.blockether/svar` lives in `vis-runtime`, `vis-channel-tui`, `vis-persistance`, `vis-benchmark`) -> use checked-in helper:

```bash
bin/bump-dependency com.blockether/svar 0.3.12   # rewrite + verify + stage
bin/bump-dependency --dry-run org.clojure/clojure 1.12.5
bin/bump-dependency --no-verify some.lib/foo 1.2.3
```

Scans root `deps.edn` + every `packages/*/deps.edn`, rewrites only `{:mvn/version "..."}` entries that follow exact coordinate (leaves `:local/root` deps alone), runs `./verify.sh --quick`, `git add`s modified files. Idempotent — re-running with same version is a no-op. Do NOT hand-edit N deps.edn files for a shared bump; script is the single supported path.

Full svar release flow (cross-repo):

```bash
cd ../svar && git tag v0.3.12 && git push origin v0.3.12   # CI deploys to Clojars
cd ../vis  && bin/bump-dependency com.blockether/svar 0.3.12
git commit -m "deps: bump svar to 0.3.12"
```

### Project Structure

Polylith-style monorepo: four host packages + sibling tree of classpath plug-ins.

- `packages/` — four host packages, layered leaf -> binary:
  - `packages/vis-persistance/` — backend-agnostic storage facade + Flyway-driven migration runner. Leaf: depends on Clojure + flyway-core only.
  - `packages/vis-extension/` — extension/channel/provider/CLI-command contracts + unified classpath discovery loader. Depends on `vis-persistance` (unified `register-global!` dispatches `:ext/persistance` slot to the persistence facade).
  - `packages/vis-runtime/` — iteration loop, SCI sandbox, conversation lifecycle, cross-channel state, public API facade (`com.blockether.vis.core`). Depends on `vis-extension` + `vis-persistance`.
  - `packages/vis-main/` — `vis` binary's CLI surface: dispatcher (`commandline.main`), built-in commands (`channels.cli`), one-shot agent helper (`channels.cli.agent`), persistence-backed Telemere `:db` handler (`logging`). Depends on `vis-runtime`.
- `extensions/` — every classpath plug-in, grouped by surface category:
  - `extensions/channels/` — `vis-channel-tui`, `vis-channel-telegram`.
  - `extensions/providers/` — `vis-provider-github-copilot`.
  - `extensions/persistance/` — `vis-persistance-sqlite`.
  - `extensions/common/` — `vis-common-meta`, `vis-common-editing` (filesystem + code-editing tools agent reaches for in nearly every task).
  - `extensions/languages/` — language-specific tooling. Currently `vis-language-clojure` (`z/zedit` rewrite-clj wrapper + the rewrite-clj.zip API, all under the single `z/` alias). Add new directory here when shipping tools that only make sense for one source language.
  Each ships a `META-INF/vis-extension/vis.edn` and self-registers at namespace load. Every extension's `deps.edn` declares `:local/root "../../../packages/vis-runtime"` (vis-runtime transitively pulls vis-extension + vis-persistance). `vis-runtime` does NOT require any extension by namespace.
- `benchmarks/` — benchmark harness (4clojure, HumanEval, SWE-bench Verified). NOT a classpath plug-in; pulled via `:bench` alias only. Working dirs (`results/`, `trajectories/`, `swebench-harness/`) created on demand under `benchmarks/`.

Each package has own `deps.edn`, publishable independently. Repo-root `deps.edn` = thin aggregator: top-level `:deps` points at `vis-cli` (transitively pulls every host package); `:vis` alias adds every classpath plug-in -> single `clojure -M:vis` (or `bin/vis` once on `PATH`) from repo root has whole product on classpath.

**Canonical package list:** `docs/src/architecture/packages.md` — single source of truth for what each package does, where it lives, dependency direction, auto-discovery resources. Update that page when adding/removing/renaming a package; do not maintain a divergent list here.

Quick mental map (use `packages.md` for details):

- `packages/vis-persistance` — storage facade contract (leaf)
- `packages/vis-extension` — extension/channel/provider/CLI contracts + unified discovery loader
- `packages/vis-runtime` — iteration loop + SCI sandbox + conversations + public API facade
- `packages/vis-main` — `vis` binary: dispatcher, built-in commands, one-shot agent, `:db` Telemere handler
- `extensions/persistance/vis-persistance-sqlite` — SQLite persistence backend
- `extensions/providers/vis-provider-github-copilot` — GitHub Copilot OAuth provider
- `extensions/channels/vis-channel-tui`, `extensions/channels/vis-channel-telegram` — channel implementations
- `extensions/common/vis-common-meta`, `extensions/common/vis-common-editing` — SCI sandbox extensions
- `extensions/languages/clojure` — Clojure structured editing (`z/zedit` + `z/` zipper API, single alias)
- `benchmarks/` — benchmark harness (`:bench` alias only; not a classpath plug-in)

ONE classpath-scan auto-discovery resource: `META-INF/vis-extension/vis.edn`. EDN map keyed by ext id; `:nses` vec loaded at startup by single loader `com.blockether.vis.core/discover-extensions!`. Each ns self-registers into whichever subsystem registry it targets (extension symbols, channels, CLI commands, providers, persistence backends) via matching `register-global!` / `register-backend!` call. Drop a jar shipping `META-INF/vis-extension/vis.edn` on classpath -> `vis-runtime` picks up at next process boot. NO per-subsystem resource files anymore; old `META-INF/vis/{extensions,channels,commandline,providers,persistance-backends}.edn` paths collapsed into unified `META-INF/vis-extension/vis.edn` and removed without backwards-compat aliases.

Docs build: `cd docs && mdbook-mermaid install . && mdbook serve --open` (mdBook source lives at repo root under `docs/`, not inside any package).

**DO NOT create or maintain a directory-structure file.** Codebase changes faster than any static tree can track. Use `find`, `ls`, `grep` to explore. Agent writes a directory-structure doc -> delete it.

## Ubiquitous Language (MANDATORY)

- Use `conversation`, never `session`, for the product concept across TUI, Telegram, CLI.
- Use `turn` for product-level ask+answer. `query` and `iteration` remain runtime internals.
- Use `tool` and `skill`. Do not use `capability` as catch-all for agent features.
- Keep `capability` / `capabilities` only where external provider/router API already requires that word.
- Use `channel` for `:tui`, `:telegram`, `:cli`. Registered channels use one keyword end-to-end — `vis-channel-tui` registers `:tui` for CLI dispatch AND stores conversations under `:tui`. The CLI agent (vis-main) is the only exception: it doesn't register a channel descriptor but stores conversations under `:cli`.
- Use `environment` in public API. `env` allowed in internal local bindings only.

### No abbreviated identifiers in source code

Do NOT abbreviate domain terms in identifiers. Spell things out. Cost of a few extra characters dwarfed by readability win + risk of LLM-or-human reader misreading half-formed shorthand. Applies to: function names, fn parameters, `let`-bindings, `def`/`defn` names, `:keys` destructuring, map keys we author, doc strings, log keys, metric names.

**Banned abbreviations — use the full word:**

| Don't write | Write           |
| ----------- | --------------- |
| `iter`      | `iteration`     |
| `expr`      | `expression`    |
| `msg`       | `message`       |
| `cnt`       | `count`         |
| `pos`       | `position`      |
| `prev`      | `previous`      |
| `cb`        | `callback`      |
| `idx`       | `index`         |
| `len`       | `length`        |
| `desc`      | `description`   |
| `cfg`       | `config` (whole word OK) |
| `tmp`       | `temp` or `temporary` |
| `init`      | `initial` or `initialize` |
| `val`       | `value`         |
| `attr`      | `attribute`     |
| `param`     | `parameter`     |
| `req`/`res` | `request`/`response` |
| `freq`      | `frequency`     |
| `dur`       | `duration`      |
| `info`      | `information` (only at end of name; `db-info` stays — domain noun) |

**Idioms that stay** (Clojure-native; do not change):

- `var`, `vars`, `defn`, `def`, `let`, `fn`, `ns`, `sym`, `ctx`, `opts`, `args`, `id`, `db`, `str` (only as `clojure.string` alias or in built-in fn names like `pr-str`, `subs`, `str/replace`).
- `env` permitted ONLY as local-scope binding name; public API uses `environment` (already enshrined above).
- `kw` (keyword) inside one-shot reader/normalizer helpers; spell out in any externally-visible name.

### SYSTEM vars are UPPERCASE and explicitly defined

Sandbox-visible system vars carrying user's current query, model's last reasoning text, prior-turn final answer = **`QUERY`, `REASONING`, `ANSWER`** — ALL CAPS, no earmuffs.

Explicitly `(def QUERY nil)` / `(def REASONING nil)` / `(def ANSWER nil)` at environment construction so symbols always resolve, even before first turn. Subsequently rebound after each iteration via iteration loop's bookkeeping.

Do NOT introduce earmuffed names (`*query*`, `*foo*`) for new system vars. The system-var registry (`SYSTEM_VAR_NAMES`) = fixed set; adding to it = deliberate API change, not free-form pattern.

Why uppercase, not earmuffs:

- Earmuffs = Clojure's idiom for *dynamic vars* (`*out*`, `*ns*`). Our SYSTEM vars are **plain SCI bindings**, not dynamic; earmuff signal misled readers into thinking they could `binding`-shadow them.
- Uppercase aligns with Clojure idiom for *constants* (`MAX_VAL`, `URL_PATTERN`). SYSTEM vars are read-only from model's POV; loop owns mutation.
- Cleaner in `<var_index>` and `<system_state>` blocks (no asterisk punctuation noise).

**Examples — prefer the right column:**

```clojure
;; BAD                                  ;; GOOD
(def MAX_ITER 30)                        (def MAX_ITERATIONS 30)
(let [iter 0 prev-msg ...] ...)          (let [iteration 0 previous-message ...] ...)
(defn run-iter [...] ...)                (defn run-iteration [...] ...)
:expr-results                            :expression-results
:prev-iteration                          :previous-iteration
(defn count-cb [cb] ...)                 (defn count-callback [callback] ...)
```

Why this matters: half the looping pathologies in CRITIQUE.md trace back to over-compressed projections (`:s :l :t :map :n 12` vs. real Clojure shape). Same compression instinct in identifiers makes call sites harder to read + diff harder to review. Spell things out. Reviewers MUST reject PRs that introduce new banned abbreviations from the table above.

## Namespace Architecture

### Namespace Layers

- `*.shared` = reusable functions for one bounded context.
- `*.core` = orchestration + use cases for one bounded context.
- `*.persistence*` = storage/schema/DB boundary for one bounded context.
- `*.presentation*` = pure rendering/view-model formatting for one bounded context or adapter.
- `channels.*` = external surface code only: TUI, Telegram, CLI.
- top-level facades like `core` should stay thin and stable.

### Channel adapters (one package each)

- `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/*` — Lanterna TUI
- `extensions/channels/vis-channel-telegram/src/com/blockether/vis/ext/channel_telegram/*` — Telegram bot
- `packages/vis-main/src/com/blockether/vis_main/commandline/main.clj` — CLI dispatcher (registry-driven)
- `packages/vis-main/src/com/blockether/vis_main/channels/cli.clj` — vis-cli's built-in commands (`run`, `auth`, `doctor`, `conversations`, `extensions list`)
- `packages/vis-main/src/com/blockether/vis_main/channels/cli/agent.clj` — one-shot agent helper used by `vis run`

Third-party channels register themselves at namespace load via `com.blockether.vis.core/register-global!` and ship a unified `META-INF/vis-extension/vis.edn` resource. CLI dispatcher discovers them; nothing in vis-runtime references a concrete channel namespace. See `docs/src/architecture/channels.md`.

### Runtime modules (vis-runtime)

All under `packages/vis-runtime/src/com/blockether/vis_runtime/`:

- `core.clj` — public API facade (`create-environment`, `dispose-environment!`, `register-extension!`, `active-extensions`, `assemble-system-prompt`, `query!`, `MAX_ITERATIONS`). Does NOT own `-main` (that's vis-cli). Extension authoring helpers (`extension`, `symbol`, `value`, `register-global!`, `render-prompt`) NOT re-exported — require `com.blockether.vis.core` directly.
- `config.clj` — config loader, db-path (`~/.vis/vis.mdb`), router builder
- `loop/core.clj` — environment lifecycle + system-prompt assembly
- `loop/mustache.clj` — Mustache templating helpers exposed in SCI sandbox
- `loop/runtime/conversation/core.clj` — conversation lifecycle (`create!`, `by-id`, `by-channel`, `for-telegram-chat!`, `set-title!`, `env-for`, `effective-system-prompt`, `send!`, `close!`, `delete!`, `db-sweep-orphaned-running-queries!`, `close-all!`)
- `loop/runtime/conversation/environment/core.clj` — SCI sandbox + var-index
- `loop/runtime/conversation/environment/query/core.clj` — query engine
- `loop/runtime/conversation/environment/query/iteration/core.clj` — iteration engine
- `channels/core.clj` — cross-channel provider mgmt + streaming progress tracker
- `channels/cancellation.clj` — in-flight query cancellation registry

### Persistence + extension contract

- `packages/vis-persistance/src/com/blockether/vis_persistance/{base,core,migration}.clj` — facade API (`db-create-connection!`, `db-dispose-connection!`, `db-list-conversations`, etc.) + migration runner. Backends self-register via unified `META-INF/vis-extension/vis.edn` resource and call `register-backend!` at namespace load time.
- `extensions/persistance/vis-persistance-sqlite/src/com/blockether/vis/ext/persistance_sqlite/core.clj` — SQLite + Flyway backend. Schema: `extensions/persistance/vis-persistance-sqlite/resources/db/sqlite/migration/V1__schema.sql`.
- `packages/vis-extension/src/com/blockether/vis_extension/extension.clj` — extension spec, symbol/value builders, hook protocol, global registry, unified classpath discovery (`META-INF/vis-extension/vis.edn`) for every extension surface (ext symbols, channels, CLI commands, providers, backends).
- `packages/vis-extension/src/com/blockether/vis_extension/channel.clj` — channel descriptor spec + global registry + classpath discovery (registered through unified `META-INF/vis-extension/vis.edn`).
- `packages/vis-extension/src/com/blockether/vis_extension/{provider,error,commandline/base}.clj` — provider descriptor spec + registry, shared error formatting, CLI command spec + registry.

Use `find`/`grep` to explore the tree — no static directory doc exists.

### Refactor Rules

- Do not introduce new `session` names in code, routes, vars, logs, UI copy.
- Do not introduce new catch-all `capability` names when real concept is `tool` or `skill`.
- `routes` or `presenter` code needing raw `conversations/env-for` or DB access? Boundary is wrong.
- Prefer in-place renames for vocabulary fixes; split files only when a namespace owns multiple contexts.
- Prefer functional ownership over historical placement: shared vs core vs persistence vs presentation vs channels, inside the correct bounded context.
- Reusable functions -> `*.shared`. Orchestration -> `*.core`. Storage -> `*.persistence*`. Rendering -> `*.presentation*`. External surfaces -> `channels.*`.
- Use `core.clj` as default application/use-case namespace in each bounded context.
- Do not turn `loop.core` into a dumping ground for unrelated behavior.
- Treat conversations as RLM subcontext, not top-level bounded context separate from RLM.
- Treat `agent.clj` as CLI-owned helper code, not a separate adapter. Lives at `packages/vis-main/src/com/blockether/vis_main/channels/cli/agent.clj` alongside `channels/cli.clj` in same package.

- Prefer extracting a deep module first, then renaming callers, then deleting stale code.
- Remove `requiring-resolve` cycles instead of spreading them further.
- Keep slices small and shippable; no big-bang folder shuffle.

### Agent Helper (agent.clj)

CLI-oriented one-shot execution layer (`channels/cli/agent.clj`).

**Programmatic usage:**
```clojure
(require '[com.blockether.vis.core :as ag])

(def reviewer
  (ag/agent {:name "reviewer"
             :system-prompt "You are a senior Clojure engineer."
             :max-iterations 30}))

(ag/run! reviewer "Review src/auth.clj for security issues")
;; => {:answer "..." :iterations 5 :duration-ms 2340 :tokens {...} :cost {...}}

(ag/result->json result)
```

**CLI usage:**
```bash
vis run "What is 2+2?"
vis run --json "Explain the auth flow"
vis run --model gpt-4o "Summarize X"
vis run --system-prompt "You are a code reviewer" "Review auth.clj"
```

### State Management (MANDATORY)

All app state lives in `ext/channel_tui/state.clj` using a re-frame dispatch pattern.

**Rules:**
- ALL app state MUST go through `state/app-db` — never create standalone atoms for app state.
- State changes MUST use `(state/dispatch [:event-name args...])` — never `swap!`/`reset!` on app-db directly.
- Event handlers registered with `reg-event-db` MUST be **pure functions**: `(fn [db event-vec] new-db)`.
- Side effects (RLM calls, disk I/O, futures) -> `reg-event-fx` + `reg-fx`.
- Local ephemeral state in dialogs (selection index, cursor position) MAY use local atoms — component-scoped + transient.

```clojure
;; Good — dispatch an event
(state/dispatch [:send-message text])

;; Good — pure event handler
(reg-event-db :update-input
  (fn [db [_ new-input]]
    (assoc db :input new-input)))

;; Good — side effects via reg-event-fx
(reg-event-fx :send-message
  (fn [db [_ text]]
    {:db (-> db (update :messages conj msg) (assoc :loading? true))
     :fx [[:rlm-query (:conversation db) text]]}))

;; Bad — direct atom mutation
(swap! some-atom assoc :key val)
```

**App-db shape:**
```clojure
{:config       nil            ;; provider config map: {:providers [{:id kw :label str :api-base str :api-key? str :model str} ...]}
 :conversation nil            ;; {:id conversation-id} — handle into the conversations cache
 :messages     []             ;; [{:role :user|:assistant :text str :timestamp #inst}]
 :msg-scroll nil              ;; row offset, nil = auto-bottom
 :input      {:lines [] :crow 0 :ccol 0}
 :loading?   false            ;; true while RLM is working
 :dialog-open? false}         ;; dialog singleton guard
```

### Conversations (`com.blockether.vis.core`)

**One module owns env lifecycle for every frontend.** TUI uses `:tui` channel, CLI agent uses `:cli`, Telegram uses `:telegram`. Conversation IDs = plain UUIDs. No name prefixes, no string lookups.

```clojure
(require '[com.blockether.vis.core :as conversations])

;; Create / lookup
(conversations/create! :tui)                    ;; new TUI conversation
(conversations/create! :cli {:title "…"})       ;; one-shot CLI agent run
(conversations/by-id conversation-id)                   ;; conversation map or nil
(conversations/by-channel :tui)                         ;; sidebar / list, recent first
(conversations/by-channel :telegram)
(conversations/for-telegram-chat! chat-id)              ;; find-or-create by chat-id

;; Mutate
(conversations/set-title! conversation-id "New title")
(conversations/env-for conversation-id)                 ;; raw env (for presenter projections / inspectors)
(conversations/effective-system-prompt conversation-id) ;; assembled system prompt for the [?] inspector

;; Turn
(conversations/send! conversation-id messages opts)     ;; locked per conversation-id; see docstring for every opt

;; Lifecycle
(conversations/close! conversation-id)                  ;; release env handle, keep DB data
(conversations/delete! conversation-id)                 ;; close + purge entity tree + sidecar row
(conversations/db-sweep-orphaned-running-queries!) ;; mark crashed queries as :error on boot
(conversations/close-all!)                      ;; process shutdown
```

### Storage

**ONE SQLite DB for everything.** Path: `~/.vis/vis.mdb` (`config/db-path`). Every frontend opens same DB; connection pool shared across environments.

Schema: soul/state model with versioned execution history. Full reference: `docs/src/architecture/database.md`.

**Entity hierarchy:**
- `conversation_soul` → `conversation_state` → `query_soul` → `query_state` → `iteration` → `expression_state`
- `expression_soul` (var/call/literal identity, branch-local)

Every `(def ...)` persisted as versioned `expression_state` row. `var-history` inspects prior versions on demand.

**Investigating DB state:**
```clojure
(require '[com.blockether.vis.core :as conversations]
         '[com.blockether.vis.core :as db])

;; List conversations
(conversations/by-channel :tui)
(conversations/by-channel :telegram)
(conversations/by-channel :cli)

;; Access environment for a conversation
(let [env     (conversations/env-for conversation-id)
      db-info (:db-info env)]
  ;; Use persistance.core functions with db-info, or drop to raw
  ;; HoneySQL via (jdbc/execute! (:datasource db-info) ...).
  )
```

**Public API (`com.blockether.vis.core` / `com.blockether.vis.core`):**
- `create-environment router {:db path :conversation selector}` — selector = `nil` | `:latest` | uuid | `[:id uuid]`. Nil creates fresh conversation; id-ref resumes existing one.
- `register-extension!` — register validated extension into environment (tools, nudges, prompt context).
- `active-extensions environment` — vec of currently-active extensions; call ONCE per query, thread vec through `assemble-system-prompt` + per-iteration nudge collectors.
- `assemble-system-prompt environment {:active-extensions vec, :system-prompt opt}` — single source of truth for system message. Required by both loop paths + TUI `[?]` inspector.
- `query! environment [(llm/user "...")] opts` — messages MUST be vec of message maps. See `conversations/send!` docstring for every opt forwarded to `query!`.
- `dispose-environment!` — releases environment handle; shared SQLite DataSource stays open for sibling envs.

Extension authoring API lives on `com.blockether.vis.core` — `extension`, `symbol`, `value`, `register-global!`, `render-prompt`, `load-extension!`, `discover-extensions!`. Channel authoring API lives on `com.blockether.vis.core`. Neither re-exported from `com.blockether.vis.core` anymore.

**Iteration lifecycle:** LLM does **not** call `(FINAL ...)` as SCI fn. svar sends spec-validated JSON response per provider capability: `ITERATION_SPEC_NON_REASONING` (includes `:thinking`) or `ITERATION_SPEC_REASONING` (no `:thinking`). Shared fields from `ITERATION_SPEC_BASE` (`:code` vec + optional `:final {:answer :confidence :language :sources}` + `:next-optimize`). `:final` set -> iteration stops, answer = RLM result. Observability: pass `{:hooks {:on-chunk (fn [{:iteration :thinking :code :final :done?}])}}` to `conversations/send!`.

**Plan slot rule (Phase 1):** reasoning continuity delivered by three structured slots, NOT a `<prior_thinking>` blob.

- `<plan>` — sticky structured TODO list (`:plan_state` ref spec). Model emits at iter 0; loop carries verbatim until re-emit. Max 20 items, exactly one `:in_progress`.
- `<breadcrumbs>` — cumulative one-liner per iteration (last K=20), authored by model in `:breadcrumb`. Tactical history at one line per iter.
- `<recent_thought>` — last iteration's `:thinking` text only, capped at 4000 chars.

SYSTEM vars (`QUERY`, `REASONING`, `ANSWER`) inlined in `<system_state>` with current values, NOT in `<var_index>`. Previous turn's bounded digest (`{:goal :counts :outcome :abandon-reason}`) lands in `<system_state>.PRIOR_TURN`.

Agent genuinely needs older reasonings? (Opt-in) `vis-common-meta` ext exposes `(meta/diagnose)`, `(meta/failures)`, `(meta/turn)`, `(meta/conversation)`, `(meta/find-attempts pattern)`, `(meta/var-history 'sym)`. Deprecated built-in `var-history` still works for backwards compat.

Do NOT reintroduce a `<prior_thinking>` blob, the lossy summarization chain it produced, or the `HANDOVER_KEEP_LAST=2` cross-query special case — deleted on purpose. Plan slot replaces all of them with bounded, structured, sticky projection.

**Frontend wiring:**
- **TUI (`vis-channel-tui`)** — registered channel id `:tui` (default channel for `vis` with no sub-command). `chat/make-conversation` creates fresh `:tui` conversation on every boot (history starts empty); disposal on exit only closes env, conversation stays in `:tui` channel so other inspectors can see it.
- **Telegram (`vis-channel-telegram`)** — registered channel id `:telegram`. `conversations/for-telegram-chat!` find-or-creates by chat-id; each incoming message becomes a `conversations/send!` with the Telegram persona system prompt.
- **CLI `agent/run!`** — one-shot. Creates fresh conversation in `:cli` channel + runs single query. Conversations persist — past runs browsable via `(conversations/by-channel :cli)`.
- **Third-party channels** — ship a jar with `META-INF/vis-extension/vis.edn` resource, namespace that calls `(channel/register-global! …)` at load, `:channel/main-fn` consuming CLI tail. Dispatcher picks them up automatically; no edits to `vis-runtime`.
