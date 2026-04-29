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

Why: book = single source of truth (see rule below). Duplicating into `README.md` -> two versions drift, `README.md` always loses. Already paid for that mistake — do not repeat. Adding a new section to `README.md`? It belongs in the book; add under `docs/src/...`, link from `SUMMARY.md`, at most add one bullet in README's docs list pointing at it.

Reject any PR that grows `README.md` past a screenful (rationale paragraph + short "Documentation" list pointing into `docs/src/`).

### Every extension declares itself in vis.edn with an inline README

Every extension under `extensions/<category>/<name>/` MUST ship exactly one classpath manifest at:

```
resources/META-INF/vis-extension/vis.edn
```

EDN map keyed by extension **id** (short symbol; same token LLM uses as SCI sandbox alias — e.g. `foundation`, `editing`, `z`):

```edn
{meta
 {:nses [com.blockether.vis.ext.common-foundation.core]
  :docs {"README.md"
         {:created-at  #inst "2026-04-28"
          :description "One-paragraph LLM-facing summary..."
          :content     "# Meta extension\n..."
          :links       [{:to-id editing :to-doc "README.md"
                         :context "Companion filesystem extension"}
                        {:url "https://arxiv.org/abs/2512.24601"
                         :context "RLM paper"}
                        {:file "src/com/blockether/vis/internal/extension.clj"
                         :context "Loader implementation"}]}}}}
```

The host package (`com.blockether.vis.core`, this repo's `src/`) is **NOT** an extension and does **NOT** ship a manifest. `vis` is the host, never an `:ext/namespace`; first-party CLI built-ins (`run`, `auth`, `doctor`, `conversations`, `extensions list`) live in `internal/main.clj` and call `registry/register-cmd!` directly. The classpath manifest is the third-party plug-in entry point only.

Slots:

- `:nses` — vec of namespaces loader `require`s. Each loaded ns self-registers via `(sdk/register-extension! …)` (where `sdk` is `com.blockether.vis.core`). Plural intentional — one id can span multiple namespaces (ext registering symbols + channel + provider in three nses).
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

- `vis.edn` = **single source of truth** for that extension (purpose, surface, when to use, when not). End users read via the book; LLM reads via `(foundation/extension-doc '<id> "<doc-name>")` and `(foundation/extension-readme '<id>)` from `:code`.
- Do NOT add a second copy of any doc anywhere in the extension tree (no `extensions/<category>/<name>/README.md`, no `docs/src/extensions/<id>.md` containing inlined content, no in-tree sibling `README.md`). `vis.edn` is canonical.
- `:content` = Markdown with sentence-case headings + real UTF-8 (same rules as the book — see docs-style rule below). End-user-facing ext -> surface from `docs/src/SUMMARY.md` with a short manual page pointing readers at `(foundation/extension-readme '<id>)` (book may briefly summarize, MUST NOT inline full body).
- The id (top-level key in `vis.edn`) MUST equal registered ext's `:ext/ns-alias :alias`. Loader doesn't enforce strictly, but reviewers MUST reject mismatches — LLM types `(meta/...)`, on-disk id is `foundation`.
- Multiple jars MAY contribute under same id; their `:nses` deduped, `:docs` merged (last write wins per name). `:reflinks` recomputed across entire registry after every merge -> a later jar's links can target an earlier jar's docs.
- Editing tip: `:content` = multi-line EDN string. Supported authoring loop: keep Markdown body in sibling `.md` file in source tree, re-inline into `vis.edn` with a small `pprint`-based generator. Do NOT keep two on-disk copies of same content under version control.
- `vis-common-foundation` exposes the LLM-facing surface: `(foundation/extensions)` lists every loaded ext + docs catalog; `(foundation/extension-docs ext-ref)` returns `[{:name :created-at :description :links :reflinks} …]` summaries for one ext; `(foundation/extension-doc ext-ref name)` returns full descriptor including `:content`; `(foundation/extension-readme ext-ref)` is the README `:content` convenience.

Why: agents that need to verify what an ext actually does — before reaching for one of its tools — must read the ext's own description as data, not guess from symbol names. Inlining docs in `vis.edn` = single classpath read at boot, eliminates path-vs-call ambiguity an external doc tree introduced (`vis/<id>` reads as a sandbox call), makes the description index trivially queryable.

Reject any PR that adds a new ext without `vis.edn`, omits the README, omits `:description` or `:content` on any declared doc, hand-authors a `:reflinks` field, or introduces a duplicate doc somewhere outside `vis.edn`.

### Always update `docs/` when touching architecture or public API

`docs/src/` (mdBook, repo root) = single source of truth for Vis architecture, extension system, public API. **Every** change to files under `src/com/blockether/vis/internal/` or `src/com/blockether/vis/core.clj` MUST be followed by an update to the relevant doc if the change affects:

- File moves, renames, deletions
- New files / namespaces
- Changes to iteration pipeline stages
- Changes to environment map shape or state atoms
- Changes to extension spec (slots, hooks, nudge context)
- Changes to public API (`create-environment`, `query!`, `register-extension!`, `send!`, `create!`, etc. — anything re-exported from `com.blockether.vis.core`)
- Changes to dependency graph between internal modules
- Changes to SQLite schema (`extensions/persistance/vis-persistance-sqlite/resources/db/sqlite/migration/V1__schema.sql`)

Doc files (under repo-root `docs/`, NOT inside any extension):
- `docs/src/README.md` — introduction (why RLM, why SCI, the problem, how Vis works, security model, prior art).
- `docs/src/usage.md` — getting started (install, auth, ways to talk to the agent, browsing past conversations).
- `docs/src/architecture/` — overview, packages, iteration flow, state ownership, database schema, channels.
- `docs/src/extensions/` — overview, spec, hooks, environment, nudges, plus per-ext sub-pages under `extensions/common/` (mirroring `extensions/common/<name>/`).

`docs/src/SUMMARY.md` = mdBook table of contents — keep in sync when adding/removing pages.

Build: `cd docs && mdbook serve --open`

Skipping this update = bug. Docs drifting from code is how we ended up with the previous god file in the first place.

### Docs style: real UTF-8, sentence-case headings, no code in titles

Three rules. Non-negotiable. Every prior violation shipped to readers as broken rendering or unreadable noise.

1. **Use real UTF-8 characters in Markdown.** Markdown is NOT JSON. `\u2014`, `\u251c`, `\u2500`, `\u2514`, `\u2026` = six-character ASCII strings, not characters. mdbook renders verbatim -> page looks like editor crash dump. Type the actual characters: `—` (em dash, U+2014), `…` (ellipsis, U+2026), `├` `─` `└` (box-drawing, U+2500 family). Editor escapes on save? Fix the editor; don't commit escapes. CI grep:

   ```bash
   grep -rn '\\u[0-9a-f]\{4\}' docs/src/ && exit 1
   ```

2. **Headings = short, descriptive, sentence-case prose. NO code identifiers in title.** Heading is a navigation aid for humans, not an API index entry.

   - Bad: `## \`:before-fn\` — entry decorator`, `### \`(foundation/turn)\``, `### 1) \`conversation_soul\``, `### Embedded \`:cmd/subcommands\` vector`, `### Render caches (\`ext/channel_tui/render.clj :: fmt-cache\`)`, `## How Vis Works` (Title Case).
   - Good: `## Entry decorator`, `### Current turn snapshot`, `### Conversation soul`, `### Embedded subcommand vector`, `### Render caches`, `## How Vis works`.

   Keyword / function name / table name / file path the section documents goes in the **first line of body** as a short "Slot key: \`:before-fn\`" / "Call: \`(foundation/turn)\`" / "Table: \`conversation_soul\`" / "Lives in: \`ext/channel_tui/render.clj\`" lead. Reader still gets the identifier; just isn't load-bearing on the heading.

3. **Internal links use slugified anchors of the live heading.** Renaming a heading invalidates every `#anchor` link to it. After any heading rename, grep for old anchor across `docs/src/` and repoint or delete every reference. Example: renaming `## Auto-discovery resource` to `## Auto-discovery` changes anchor from `#auto-discovery-resource` to `#auto-discovery`; doc that linked to old anchor breaks silently — mdbook does not warn.

### Triage conversations with vis-common-foundation — let vis self-analyze

`~/.vis/vis.mdb/vis.db` = single source of truth for every conversation — turns, iterations, final answers, persisted SCI vars, timings, costs. Before hypothesizing about a user-reported bug that references a specific `conversation-id`, **let vis self-analyze**. The `vis-common-foundation` ext (alias `foundation`) exposes that data as plain Clojure maps from `:code`; agent reaches it via `bin/vis run`. Supported triage path — there is no other.

```bash
bin/vis run --json "Use the meta extension to analyze conversation \
  <conversation-uuid>. Call (foundation/conversation #uuid \"<conversation-uuid>\"), \
  (foundation/diagnose #uuid \"<conversation-uuid>\"), and \
  (foundation/failures #uuid \"<conversation-uuid>\"). Summarize how many turns, \
  what the user asked, what failed (with classifications), and the root cause."
```

Returns structured JSON envelope with agent's answer, iteration trace, token usage, cost. Agent does failure classification, redundancy detection, cost rollup, plain-language summary — you read the `answer` field. Two iterations is the norm for a single-conversation post-mortem.

Full meta API (`(foundation/turn)`, `(foundation/conversation)`, `(foundation/conversations)`, `(foundation/diagnose)`, `(foundation/failures)`, `(foundation/find-attempts pattern)`, `(foundation/var-history 'sym)`) documented inline in the extension manifest. Read via `(foundation/extension-readme 'foundation)` from `:code`; do not guess from symbol names.

Question genuinely can't be expressed through meta (schema or migration debugging — debugging the persistence layer itself, not a conversation)? Extend `vis-common-foundation` with the call you wish you had instead of dropping to ad-hoc shell tools. The ext is the API; missing projection = bug in the ext, not a license to bypass it.

Only AFTER vis has self-analyzed may you form a hypothesis, propose a fix, or blame the model / UI / runtime loop. This rule exists because repeated incidents wasted a turn on plausible-sounding code-only explanations when one `bin/vis run` would have pinpointed the bug in two iterations.

## Clojure CLI

### Running

```bash
bin/vis                        # prints help tree
bin/vis channels tui           # TUI chat
bin/vis channels telegram      # Telegram bot
bin/vis run "prompt"           # one-shot agent query
bin/vis auth <provider>        # provider authentication
bin/vis conversations          # list persisted conversations
bin/vis doctor                 # environment + DB diagnostics
bin/vis extensions list        # registered extensions
clojure -M:test                # aggregate test runner
```

`bin/vis` = checked-in wrapper that `exec`s `clojure -M:vis "$@"` from repo root. Add `bin/` to `PATH` once -> daily use is just `vis <subcommand>`.

The dispatcher is a **pure command tree** — there is NO magical fallback for free-form prompts. `vis "do the thing"` (first token isn't a known command) prints the help tree + exits non-zero. Use `vis run "…"` explicitly.

Aliases (root `deps.edn`): `:vis` (the `vis` CLI, default `:main-opts`), `:test`, `:bench`, `:build`, `:antq`.

### Bumping a shared dependency

vis = single-package monorepo (root `deps.edn` carries the runtime) plus a sibling tree of classpath plug-ins (`extensions/<category>/<name>/deps.edn`) plus the `benchmarks/` harness. Bumping a coordinate that several jars share (e.g. `com.blockether/svar` lives in root + many extensions) -> use checked-in helper:

```bash
bin/bump-dependency com.blockether/svar 0.4.2   # rewrite + verify + stage
bin/bump-dependency --dry-run org.clojure/clojure 1.12.5
bin/bump-dependency --no-verify some.lib/foo 1.2.3
```

Scans root `deps.edn` + every `extensions/*/*/deps.edn` + `benchmarks/deps.edn`, rewrites only `{:mvn/version "..."}` entries that follow exact coordinate (leaves `:local/root` deps alone), runs `./verify.sh --quick`, `git add`s modified files. Idempotent — re-running with same version is a no-op. Do NOT hand-edit N deps.edn files for a shared bump; script is the single supported path.

Full svar release flow (cross-repo):

```bash
cd ../svar && git tag v0.4.2 && git push origin v0.4.2   # CI deploys to Clojars
cd ../vis  && bin/bump-dependency com.blockether/svar 0.4.2
git commit -m "deps: bump svar to 0.4.2"
```

### Project Structure

**Single-package monorepo** (root `deps.edn` ships the runtime as ONE jar, ONE namespace `com.blockether.vis.core`) **plus** a sibling tree of classpath plug-ins under `extensions/`. The previous three-package split (vis-sdk + vis-runtime + vis-main) collapsed because the "library / runtime / binary" boundary was not buying anything: there is only ever one runtime, and the binary, library, and SDK ship in the same classpath together for every consumer.

```
vis/
├── deps.edn                ← root: paths ["src" "resources"] + every extension as :local/root
├── src/com/blockether/vis/
│   ├── core.clj            ← public API facade (re-exports from internal/*)
│   └── internal/
│       ├── cancellation.clj  ← in-flight query cancellation registry
│       ├── commandline.clj   ← CLI dispatcher (parse-args, find-leaf, render-tree)
│       ├── config.clj        ← config loader, db-path (~/.vis/vis.mdb), router builder
│       ├── env.clj           ← SCI sandbox + var-index + SYSTEM_VAR_NAMES
│       ├── error.clj         ← shared exception → user-message formatter
│       ├── extension.clj     ← extension spec + register-extension! + slot dispatch
│       ├── format.clj        ← format-clojure, format-tokens, format-cost helpers
│       ├── loop.clj          ← iteration loop, query!, send!, create!, by-id, by-channel,
│       │                       env-for, create-environment, set-provider!,
│       │                       refresh-cached-routers!, dispose-environment!
│       ├── main.clj          ← `vis` -main + built-in commands (run/auth/doctor/
│       │                       conversations/extensions list) + agent helper +
│       │                       persistence-backed Telemere :db handler
│       ├── manifest.clj      ← META-INF/vis-extension/vis.edn classpath scanner
│       ├── persistance.clj   ← storage facade (db-store-*, db-list-*, register-backend!)
│       ├── progress.clj      ← streaming progress tracker
│       ├── prompt.clj        ← assemble-system-prompt, build-iteration-context
│       └── registry.clj      ← channel / provider / CLI command registries
├── extensions/             ← classpath plug-ins, grouped by surface category
│   ├── channels/
│   │   ├── vis-channel-tui/         ← Lanterna TUI chat
│   │   └── vis-channel-telegram/    ← Telegram long-poll bot
│   ├── providers/
│   │   ├── vis-provider-github-copilot/  ← GitHub Copilot OAuth provider
│   │   └── vis-provider-zai/             ← Z.ai (ZhipuAI) static-API-key provider
│   ├── persistance/
│   │   └── vis-persistance-sqlite/  ← SQLite + Flyway backend
│   ├── common/
│   │   ├── vis-common-foundation/         ← (foundation/turn), (foundation/conversation), (foundation/diagnose), …
│   │   ├── vis-common-environment/  ← cwd / OS / git-facts SCI helpers
│   │   └── vis-common-editing/      ← filesystem + code-editing tools (vis/cat, vis/edit, vis/rg)
│   └── languages/
│       └── clojure/                 ← (z/zedit …) rewrite-clj wrapper, alias `z`
├── benchmarks/             ← :bench alias only; NOT a classpath plug-in
└── docs/                   ← mdBook source (`cd docs && mdbook serve --open`)
```

Each extension's `deps.edn` declares `com.blockether/vis {:local/root "../../.."}`. The host runtime does NOT require any extension namespace — extensions self-register via `(sdk/register-extension! …)` at namespace load time and are discovered by classpath scan of `META-INF/vis-extension/vis.edn` (see `internal/manifest.clj`). Drop a jar shipping that resource on the classpath -> next process boot picks it up. NO per-subsystem resource files; everything goes through the unified `META-INF/vis-extension/vis.edn`.

**Canonical package list:** `docs/src/architecture/packages.md` — single source of truth. Update that page when adding/removing/renaming an extension.

Docs build: `cd docs && mdbook-mermaid install . && mdbook serve --open` (mdBook source lives at repo root under `docs/`, not inside any extension).

**DO NOT create or maintain a directory-structure file outside the snapshot above.** Codebase changes faster than any static tree can track. Use `find`, `ls`, `grep` to explore. Agent writes a separate directory-structure doc -> delete it.

## Ubiquitous Language (MANDATORY)

- Use `conversation`, never `session`, for the product concept across TUI, Telegram, CLI.
- Use `turn` for product-level ask+answer. `query` and `iteration` remain runtime internals.
- Use `tool` and `skill`. Do not use `capability` as catch-all for agent features.
- Keep `capability` / `capabilities` only where external provider/router API already requires that word.
- Use `channel` for `:tui`, `:telegram`, `:cli`. Registered channels use one keyword end-to-end — `vis-channel-tui` registers `:tui` for CLI dispatch AND stores conversations under `:tui`. The CLI agent is the only exception: it doesn't register a channel descriptor but stores conversations under `:cli` (the `vis` dispatcher itself is the surface; there is no `vis channels cli`).
- Use `environment` in public API. `env` allowed in internal local bindings only.
- Use `register-extension!` (the unified entry point). The legacy lower-level `register-channel!` / `register-cmd!` / `register-provider!` / `register-backend!` calls still exist for embedded use, but new code MUST go through `(sdk/register-extension! …)` with the appropriate `:ext/<slot>`.

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

Sandbox-visible system vars are **`USER_TURN_REQUEST`, `REASONING`, `ASSISTANT_TURN_ANSWER`, `CONVERSATION_TITLE`, `CURRENT_QUERY_ID`, `CURRENT_ITERATION_ID`, `ACTIVE_EXTENSIONS`** — ALL CAPS, no earmuffs. The fixed registry lives at `com.blockether.vis.core/SYSTEM_VAR_NAMES`; predicate is `system-var-sym?`.

Bound at environment construction so symbols always resolve, even before first turn. Subsequently rebound after each iteration via the loop's bookkeeping. `CONVERSATION_TITLE` mirrors the env's `:conversation-title-atom`, which is the in-memory cache for the conversation title (the persisted truth lives in `conversation_state.title`); the model writes through `(conversation-title "...")`, never by `def`-ing CONVERSATION_TITLE — the SYSTEM-var write guard rejects that on principle.

Do NOT introduce earmuffed names (`*query*`, `*foo*`) for new system vars. Adding to `SYSTEM_VAR_NAMES` = deliberate API change, not free-form pattern.

Why uppercase, not earmuffs:

- Earmuffs = Clojure's idiom for *dynamic vars* (`*out*`, `*ns*`). Our SYSTEM vars are **plain SCI bindings**, not dynamic; earmuff signal misled readers into thinking they could `binding`-shadow them.
- Uppercase aligns with Clojure idiom for *constants* (`MAX_VAL`, `URL_PATTERN`). SYSTEM vars are read-only from model's POV; loop owns mutation.
- Cleaner in `<var_index>` (no asterisk punctuation noise).

**Examples — prefer the right column:**

```clojure
;; BAD                                  ;; GOOD
(def MAX_ITER 30)                        (def MAX_ITERATIONS 30)
(let [iter 0 prev-msg ...] ...)          (let [iteration 0 previous-message ...] ...)
(defn run-iter [...] ...)                (defn run-iteration [...] ...)
:expr-results                            :block-results
:prev-iteration                          :previous-iteration
(defn count-cb [cb] ...)                 (defn count-callback [callback] ...)
```

Why this matters: half the looping pathologies traced back to over-compressed projections. Same compression instinct in identifiers makes call sites harder to read + diff harder to review. Spell things out. Reviewers MUST reject PRs that introduce new banned abbreviations from the table above.

## Namespace Architecture

### Namespace Layers

- `com.blockether.vis.core` = public API facade. Re-exports from `internal/*`. The names in `core.clj` are the contract; everything in `internal/` is free to be split / merged / renamed.
- `com.blockether.vis.internal.<module>` = internal module (cancellation, commandline, config, env, error, extension, format, loop, main, manifest, persistance, progress, prompt, registry).
- `com.blockether.vis.ext.<id>.<module>` = extension namespace (under `extensions/<category>/<name>/src/`). Self-registers via `(sdk/register-extension! …)`; never reaches into `com.blockether.vis.internal.*`.

### Channel adapters (one extension each)

- `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/*` — Lanterna TUI. Registers `:tui` channel + main fn.
- `extensions/channels/vis-channel-telegram/src/com/blockether/vis/ext/channel_telegram/*` — Telegram bot. Registers `:telegram` channel.
- Built-in CLI commands (`run`, `auth`, `doctor`, `conversations`, `extensions list`) live in `src/com/blockether/vis/internal/main.clj` and register directly via `registry/register-cmd!` (they predate the extension contract — first-party binary surface, not a discoverable plug-in).
- The CLI dispatcher resolves channels and commands from registries, not by namespace `require`.

Third-party channels register themselves at namespace load via `(sdk/register-extension! (sdk/extension {… :ext/channels […]}))` and ship a unified `META-INF/vis-extension/vis.edn` resource. CLI dispatcher discovers them; nothing in the host runtime references a concrete channel namespace. See `docs/src/architecture/channels.md`.

### Internal modules (host runtime)

All under `src/com/blockether/vis/internal/`:

- `loop.clj` — iteration loop, query engine, conversation lifecycle (`query!`, `send!`, `create!`, `by-id`, `by-channel`, `for-telegram-chat!`, `set-title!`, `env-for`, `effective-system-prompt`, `close!`, `delete!`, `close-all!`, `create-environment`, `dispose-environment!`, `set-provider!`, `refresh-cached-routers!`, `rebuild-router!`, `auto-forget-stale-vars!`).
- `env.clj` — SCI sandbox machinery, var-index builder, `SYSTEM_VAR_NAMES`, `system-var-sym?`.
- `prompt.clj` — `assemble-system-prompt` (single source of truth for system message), `build-iteration-context`, `CORE_SYSTEM_PROMPT`.
- `extension.clj` — extension spec, `register-extension!`, slot dispatch (`:ext/symbols`, `:ext/cli`, `:ext/channels`, `:ext/providers`, `:ext/persistance`), `discover-extensions!`, hook protocol.
- `manifest.clj` — `META-INF/vis-extension/vis.edn` classpath scanner (one resource per jar).
- `registry.clj` — channel + CLI command + provider registries (idempotent on id).
- `persistance.clj` — storage facade + backend self-registration (`register-backend!`).
- `commandline.clj` — argument parser, command tree renderer, `dispatch!`.
- `config.clj` — config loader, db-path (`~/.vis/vis.mdb`), router builder, provider presets.
- `cancellation.clj` — in-flight query cancellation tokens.
- `progress.clj` — streaming progress tracker.
- `format.clj` — `format-clojure`, `format-tokens`, `format-cost`, `format-iterations`.
- `error.clj` — shared exception → user message formatter.
- `main.clj` — `vis` binary surface: `-main`, dispatcher wiring, persistence-backed Telemere `:db` handler, built-in commands, one-shot `agent` + `run!` helpers (CLI-internal, not re-exported from `core.clj`).

### Persistence + extension contract

- `src/com/blockether/vis/internal/persistance.clj` — facade API (`db-create-connection!`, `db-dispose-connection!`, `db-list-conversations`, every `db-store-*`/`db-list-*`/`db-update-*`, `register-backend!`). Backends self-register at namespace load time.
- `extensions/persistance/vis-persistance-sqlite/src/.../core.clj` — SQLite + Flyway backend. Schema: `extensions/persistance/vis-persistance-sqlite/resources/db/sqlite/migration/V1__schema.sql`.
- `src/com/blockether/vis/internal/extension.clj` — extension spec, symbol/value builders, hook protocol, global registry, slot dispatcher. The unified classpath discovery resource is `META-INF/vis-extension/vis.edn`; one slot maps to one sub-registry call (`:ext/cli` → `register-cmd!`, `:ext/channels` → `register-channel!`, `:ext/providers` → `register-provider!`, `:ext/persistance` → `persistance/register-backend!`, `:ext/symbols` → SCI namespace + alias setup).
- `src/com/blockether/vis/internal/registry.clj` — channel descriptor spec + global registry, CLI command spec + registry, provider descriptor spec + registry. Same shape across all three: `register-X!` (idempotent on `:X/id`), `deregister-X!`, `registered-X`, `X-by-id`.

Use `find` / `grep` to explore the tree — no static directory doc beyond the snapshot above.

### Refactor Rules

- Do not introduce new `session` names in code, routes, vars, logs, UI copy.
- Do not introduce new catch-all `capability` names when real concept is `tool` or `skill`.
- Channel / TUI presentation code reaching for raw `env-for` or DB access? Boundary is wrong — go through the conversation API on `com.blockether.vis.core` (`send!`, `effective-system-prompt`, `by-channel`, …).
- Prefer in-place renames for vocabulary fixes; split files only when a namespace owns multiple contexts.
- Prefer functional ownership over historical placement: every new helper goes into the right `internal/<module>` (loop / prompt / persistance / extension / registry / etc.), not "wherever the diff is smallest".
- Do not turn `internal/loop.clj` into a dumping ground for unrelated behavior. It is already large; new responsibilities = new module + a re-export entry on `core.clj`.
- Treat conversations as a runtime sub-context, not a top-level bounded context separate from the loop.
- Treat `agent` / `run!` as CLI-owned helpers (`internal/main.clj`), NOT a separate adapter and NOT a re-export. Programmatic embedding goes through `lp/create!` + `lp/send!` (or `create-environment` + `query!`) — see `core.clj`.
- Prefer extracting a deep module first, then renaming callers, then deleting stale code.
- Remove `requiring-resolve` cycles instead of spreading them further.
- Keep slices small and shippable; no big-bang folder shuffle.

### Agent helper (one-shot CLI)

CLI-internal one-shot execution layer. Lives at `src/com/blockether/vis/internal/main.clj`. **NOT re-exported on `com.blockether.vis.core`** — the supported programmatic surface for embedded callers is `create-environment` + `query!` (or `create!` + `send!`).

**CLI usage:**
```bash
vis run "What is 2+2?"
vis run --json "Explain the auth flow"
vis run --model gpt-4o "Summarize X"
vis run --system-prompt "You are a code reviewer" "Review auth.clj"
vis run --no-persist "sensitive prompt that must not hit ~/.vis/vis.mdb"
```

`vis run` creates a fresh conversation in the `:cli` channel, runs the query, prints the answer (or JSON envelope when `--json` is set). Persistent runs are browsable via `vis conversations cli` or `(foundation/conversations :cli)`.

`--no-persist` spins up an ephemeral env on a `:memory` SQLite DB; nothing touches `~/.vis/vis.mdb` and the env is disposed at the end.

### State management (TUI, MANDATORY)

All TUI app state lives in `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/state.clj` using a re-frame-style dispatch pattern.

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

**App-db shape** (see `state.clj :: init!`):

```clojure
{:config              nil      ;; provider config map: {:providers [{:id kw :label str :api-base str :api-key? str :model str} ...]}
 :conversation        nil      ;; {:id conversation-id} — handle into the conversations cache
 :title               nil      ;; conversation title (string, syncs to DB on first turn)
 :messages            []       ;; [{:role :user|:assistant :text str :timestamp #inst}]
 :messages-scroll     nil      ;; row offset, nil = auto-bottom
 :input               {…}      ;; (input/empty-input) — lines / cursor / multiline state
 :input-history       []       ;; recently-sent prompts for ↑/↓ recall
 :input-history-index nil
 :input-history-draft nil
 :loading?            false    ;; true while RLM is working
 :cancel-token        nil      ;; cancellation token for the in-flight query
 :cancelling?         false
 :progress            nil      ;; streaming progress chunk from svar
 :settings            {…}      ;; persisted user settings (theme, show-thinking?, …)
 :dialog-open?        false    ;; dialog singleton guard
 :render-version      0        ;; dirty counter; render thread waits on this
 :shutdown?           false
 :layout              nil}     ;; messages-area layout (set by render thread)
```

The `:render-version` counter + dedicated render thread (`screen.clj :: render-loop!`, daemon `vis-channel-tui-render`) are the reason typing in long conversations stays snappy. Do NOT call `render-frame!` from the input thread — it deadlocks the draw lock.

### Conversations (`com.blockether.vis.core`)

**One module owns env lifecycle for every frontend.** TUI uses `:tui` channel, CLI agent uses `:cli`, Telegram uses `:telegram`. Conversation IDs = plain UUIDs. No name prefixes, no string lookups.

```clojure
(require '[com.blockether.vis.core :as conversations])

;; Create / lookup
(conversations/create! :tui)                            ;; new TUI conversation
(conversations/create! :cli {:title "…"})               ;; one-shot CLI agent run
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
(conversations/close! conversation-id)                       ;; release env handle, keep DB data
(conversations/delete! conversation-id)                      ;; close + purge entity tree + sidecar row
(conversations/db-sweep-orphaned-running-queries! db-info)   ;; mark crashed queries as :error on boot
(conversations/close-all!)                                   ;; process shutdown
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
(require '[com.blockether.vis.core :as sdk])

;; List conversations
(sdk/by-channel :tui)
(sdk/by-channel :telegram)
(sdk/by-channel :cli)

;; Access environment for a conversation
(let [env     (sdk/env-for conversation-id)
      db-info (:db-info env)]
  ;; Use facade fns on `sdk/db-*`, or drop to raw HoneySQL via
  ;; (jdbc/execute! (sdk/ds db-info) ...).
  )
```

**Public API (`com.blockether.vis.core`):** every conversation, environment, persistance, prompt, extension, and registry function the runtime exposes is re-exported here. The names in `core.clj` are the contract; `internal/*` is free to be reorganized.

Notable surfaces:

- `create-environment router {:db path :conversation selector}` — selector = `nil` | `:latest` | uuid | `[:id uuid]`. Nil creates fresh conversation; id-ref resumes existing one.
- `register-extension! ext` — register validated extension into the global registry (tools, channels, CLI commands, providers, backends, nudges, prompt context). Slot-dispatched.
- `active-extensions environment` — vec of currently-active extensions; call ONCE per query, thread vec through `assemble-system-prompt` + per-iteration nudge collectors.
- `assemble-system-prompt environment {:active-extensions vec, :system-prompt opt}` — single source of truth for system message. Required by both loop paths + TUI `[?]` inspector.
- `query! environment messages opts` — messages MUST be vec of message maps (`(svar/user "...")`, etc.). See `send!` docstring for every opt forwarded to `query!`.
- `dispose-environment! environment` — releases environment handle; shared SQLite DataSource stays open for sibling envs.
- Extension authoring helpers: `extension`, `symbol`, `value`, `register-extension!`, `render-prompt`, `discover-extensions!`. NO separate `register-global!` — the unified `register-extension!` handles every slot.

**Iteration lifecycle:** the loop calls `(svar/ask-code! (:router environment) {:lang "clojure" :messages …})` per iteration. svar sends `:messages` verbatim, parses the assistant's plain-text response, extracts ` ```clojure … ``` ` (and untagged) fenced source, returns the concatenated source string. The loop SCI-evaluates each top-level form; the model finishes a turn by calling `(answer "...")` from inside a fenced block — a one-arg SCI fn that `reset!`s an env-scoped atom. After every form runs, the loop reads the atom; non-nil + the answer-bearing form didn't error -> turn done. Sibling errors in OTHER forms do NOT discard the answer (Option C).

NO JSON iteration spec, NO schema validation around iteration responses, NO schema-reject retry. Reader errors on the extracted source flow as ordinary iteration errors fed back as the next user message. Observability: pass `{:hooks {:on-chunk (fn [chunk])}}` to `send!` / `query!` to receive streaming progress.

**Reasoning continuity** is delivered by the per-iteration prompt slots only:

- `<recent>` — previous iteration's executed code blocks + results, addressable as `iN.K`.
- `<var_index>` — type-aware snapshot of user-defined `(def …)` bindings.
- SCI-bound SYSTEM vars the model can read directly (`USER_TURN_REQUEST`, `ASSISTANT_TURN_ANSWER`, `REASONING`, `CURRENT_QUERY_ID`, `CURRENT_ITERATION_ID`, `ACTIVE_EXTENSIONS`).
- One optional `[system_nudge]` line when the model executes the same expression twice.

Plus an opt-in `vis-common-foundation` extension exposing `(foundation/turn)`, `(foundation/conversation)`, `(foundation/conversations)`, `(foundation/var-history 'sym)`, `(foundation/find-attempts pattern)`, `(foundation/failures)`, `(foundation/diagnose)`, `(foundation/extensions)`, `(foundation/extension-doc …)`, `(foundation/extension-readme …)` for the model to self-introspect.

**Do NOT reintroduce** any of: `<plan>` / `<breadcrumbs>` / `<recent_thought>` / `<system_state>` / `<vars_archive>` / `<prior_thinking>` slots, an iteration JSON spec, `ITERATION_SPEC_*` envelopes, plan-state validation, plan-edit-distance metrics, sticky-plan loaders, breadcrumb projection, `HANDOVER_KEEP_LAST` cross-query special cases, or per-iteration TODO list machinery. Every one of those was deleted on purpose ("Drastically simplify the agent" cull). The two slots above plus the SYSTEM vars cover the same ground without the projection drift the previous read-loop pathology produced.

**Frontend wiring:**
- **TUI (`vis-channel-tui`)** — registered channel id `:tui`, sub-command `vis channels tui`. `chat/make-conversation` creates a fresh `:tui` conversation on every boot (history starts empty); disposal on exit only closes env, conversation stays in `:tui` channel so other inspectors can see it.
- **Telegram (`vis-channel-telegram`)** — registered channel id `:telegram`, sub-command `vis channels telegram`. `conversations/for-telegram-chat!` find-or-creates by chat-id; each incoming message becomes a `conversations/send!` with the Telegram persona system prompt.
- **CLI agent (`vis run`)** — defined in `internal/main.clj`. One-shot. Creates a fresh conversation in `:cli` channel + runs single query. Conversations persist (unless `--no-persist`) — past runs browsable via `(conversations/by-channel :cli)`.
- **Third-party channels** — ship a jar with `META-INF/vis-extension/vis.edn` resource, namespace that calls `(sdk/register-extension! (sdk/extension {… :ext/channels [{…}]}))` at load. Dispatcher picks them up automatically; no edits to the host runtime.
