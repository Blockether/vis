# Vis - Development Guide

## MANDATORY: Agent Rules

### Run `./verify.sh` before every commit

`./verify.sh` (at the repo root) is the single pre-commit gate. It runs
eight checks in order and stops at the first failure. **No PR, commit,
or agent-authored change ships without a green run.** This rule
overrides convenience, scope creep, and "it's just a one-line fix."

Gates:

1. **Format** — `cljfmt check` over `packages/`, `extensions/`, `build.clj`.
   Both globs are recursive, so they cover every host package and
   every category subdir under `extensions/`. Fix with
   `cljfmt fix packages/ extensions/ build.clj`.
2. **Lint** — `clj-kondo` across every package src tree. Errors and
   warnings are fatal; info-level diagnostics are advisory.
3. **GraalVM safety** — walks every production package's `src/`, loads
   each `.clj` with `*warn-on-reflection*` + `*unchecked-math*
   :warn-on-boxed`, counts the resulting warnings on project source
   paths only. **Ratchet**: fails if the count grows beyond
   `.verification-baseline/graal-warnings.count` (tracked file).
   Improvements ratchet down via `./verify.sh --update-baseline`.
   `benchmarks/` is excluded — it's a dev/research package, not on
   the default classpath, not shipped as a runtime jar.
4. **Tests** — `clojure -M:test` (lazytest, aggregate suite).
5. **Docs build** — `cd docs && mdbook build`. The "update `docs/`
   when touching architecture or public API" rule below is meaningless
   if the docs don't compile.
6. **Smoke** — `bin/vis` prints the help banner. Doesn't touch the user DB.
7. **Git hygiene** — `git diff --check HEAD` (trailing whitespace,
   conflict markers).
8. **Secret scan** — scans the diff against `origin/main` for common
   API-key patterns (`sk_*`, `lin_api_*`, `nvapi-*`, `AIzaSy*`,
   `ghp_*`, hardcoded `password = "..."`).

Modes:

- `./verify.sh` — full pipeline (default).
- `./verify.sh --quick` — format + lint only (~10s) for tight loops.
- `./verify.sh --graal` — just the GraalVM safety check.
- `./verify.sh --strict` — graal step demands ZERO warnings
   (no ratchet); used to confirm a successful clean-up before
   updating the baseline.
- `./verify.sh --update-baseline` — snapshot the current graal
   warning count as the new lower bound. Commit the
   `.verification-baseline/graal-warnings.count` change with the same
   PR that drove the count down.

Logs land in `.verification/<step>.log` and `.verification/summary.log`
(both gitignored).

The baseline is intentionally a **ratchet, not a ceiling** — it only
moves down. Any PR that adds reflection or boxed-math warnings without
a justification is a bug, even if the rest of the diff looks fine.

### Never bind Ctrl+Y in the TUI

`Ctrl+Y` sends `SIGTSTP` (or the `DSUSP` character on macOS) which
**suspends the entire process** and drops the user to a stopped-job
shell prompt. Lanterna cannot intercept it before the kernel acts on it.
Do NOT bind `Ctrl+Y` to any action — clipboard copy, yank, or anything
else. It will never work. Any PR that re-introduces a `Ctrl+Y` binding
in `input.clj`, `dialogs.clj`, or `screen.clj` must be rejected.

Use the copy dialog (`Ctrl+K` → Copy) for clipboard operations instead.

### Always use HoneySQL for SQL — no raw strings, no next.jdbc.sql

Every SQL query in the codebase MUST use `honey.sql` data maps.
Do NOT use `next.jdbc.sql` (`sql/insert!`, `sql/find-by-keys`, etc.) —
it produces namespaced column keys that break with SQLite.
Do NOT write raw SQL strings in application code (`["SELECT * FROM ..."]`).

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

Raw `jdbc/execute!` with string SQL is allowed ONLY inside
`persistance/sqlite/*.clj` for migration DDL and FTS queries that
HoneySQL cannot express. Everywhere else: HoneySQL or bust.

### Always reply in English

Every assistant-facing response — user-visible chat text, commit messages, PR bodies, code comments, docstrings, log messages — is written in English. The user may write in Polish (or any other language); the agent still replies in English. No exceptions, no mixed-language responses, no apology paragraphs in the user's language. This rule overrides any implicit language mirroring behavior.

### Trust svar spec guarantees — do NOT defensively re-validate

svar's iteration spec is provider-enforced. When a field is declared as
`{::spec/name :expr ::spec/type :spec.type/string ::spec/required true}`,
svar guarantees the parsed result has `:expr` as a non-null string.
Writing `(when (map? block) (when-not (str/blank? expr) ...))` and
`(throw (ex-info "Code block missing :time-ms"))` after an already
spec-validated response is **pure noise** — it duplicates what svar
already enforced, hides what the code is actually trying to say, and
makes downstream edits riskier because the defensive branches obscure
the real control flow.

Rule:

- If a field is **required** in the iteration-spec, destructure it
  directly. Don't `(when-not (str/blank? …))`. Don't throw "missing
  :time-ms". That branch is dead code.
- If a field is **optional**, use plain `(when (:field x) …)` or
  `(or (:field x) default)` — NOT a full validator.
- If you ever need to check shape after spec, the spec is WRONG.
  Fix the spec, not the consumer.

This applies to EVERY spec, not just iteration spec:
code_block, next_turn, all of them. If svar loaded it, the shape is
correct by construction.

### Repo-root `README.md` is a stub: rationale + book link only

The repo-root `README.md` is intentionally tiny. It carries:

- one short rationale paragraph (what Vis is, why RLM/SCI in one breath),
- a pointer to the book under `docs/src/` (mdBook),
- nothing else.

It does NOT carry: architecture diagrams, mermaid sequence charts,
getting-started instructions, install/auth steps, extension docs,
schema tables, channel adapter notes, FAQ, comparison tables, or
long-form prose. Every one of those belongs in the book
(`docs/src/...`) and is reachable from `docs/src/SUMMARY.md`.

Why: the book is the single source of truth (see the rule below).
Duplicating it into `README.md` produces two versions that drift, and
the `README.md` always loses. We have already paid for that mistake
once — do not repeat it. If you find yourself adding a new section to
`README.md`, that section belongs in the book; add it under
`docs/src/...`, link it from `SUMMARY.md`, and at most add one bullet
in the README's documentation list pointing at it.

Any PR that grows `README.md` past a screenful (rationale paragraph +
short “Documentation” list pointing into `docs/src/`) must be rejected.

### Every extension declares itself in vis.edn with an inline README

Every extension under `extensions/<name>/` MUST ship exactly one
classpath manifest at:

```
resources/META-INF/vis-extension/vis.edn
```

The manifest is an EDN map keyed by extension **id** (a short symbol;
same token the LLM uses as the SCI sandbox alias — e.g. `foundation`,
`vis`, `git`):

```edn
{meta
 {:nses [com.blockether.vis.ext.common-foundation.core]
  :docs {"README.md"
         {:created-at #inst "2026-04-28"
          :description   "One-paragraph LLM-facing summary..."
          :content    "# Meta extension\n..."
          :links      [{:to-id vis :to-doc "README.md"
                        :context "Companion filesystem extension"}
                       {:url "https://arxiv.org/abs/2512.24601"
                        :context "RLM paper"}
                       {:file "packages/vis-extension/src/com/blockether/vis_extension/extension.clj"
                        :context "Loader implementation"}]}}}}
```

Slots:

- `:nses` — vector of namespaces the loader `require`s. Each loaded
  ns self-registers via `register-global!` (or one of the sibling
  registry calls). Plural form is intentional — one id can span
  multiple namespaces (e.g. an extension that registers symbols +
  channel + provider in three nses).
- `:docs` — map of `{<doc-name> <descriptor>}` where each
  `<descriptor>` is the structured map below. Doc names are
  conventional Markdown filenames (`README.md`, `EXAMPLES.md`,
  `MIGRATION.md`, ...).

Doc descriptor fields:

- `:created-at` (mandatory) — `#inst` literal, when the doc was
  first authored.
- `:description` (mandatory) — one-paragraph LLM-facing summary as a
  plain Clojure string. NO YAML frontmatter inside `:content`; the
  abstract is its own EDN field, not parsed out of Markdown.
- `:content` (mandatory) — full Markdown body. Pure Markdown only,
  no leading frontmatter block.
- `:links` (optional) — vector of author-declared outgoing
  references. Each link is one of:
  - cross-extension doc:
    `{:to-id <id> :to-doc <name> :context "..."}`
  - same-extension doc (omit `:to-id`):
    `{:to-doc <name> :context "..."}`
  - external URL: `{:url "https://..." :context "..."}`
  - repo file: `{:file "path/to/file.clj" :context "..."}`
- `:reflinks` — **derived**, never authored. The loader inverts
  every cross-ext / same-ext link in the registry into a
  `{:from-id :from-doc :context}` entry on the target descriptor.
  Authors who write a `:reflinks` field by hand are violating the
  contract; the loader will overwrite it.

Mandatory contents:

- Every extension's `:docs` map MUST include a `"README.md"` entry.
  No README → the extension is not shippable. The loader logs a
  `:warn` and skips any doc descriptor missing `:description` or
  `:content`.
- Use real `#inst` literals for `:created-at`; the registry
  preserves them as `java.util.Date` so the agent can sort by
  freshness.

Rules:

- `vis.edn` is the **single source of truth** for that extension
  (purpose, surface, when to use it, when not to). End users read
  the docs via the book; the LLM reads them via
  `(foundation/extension-doc '<id> "<doc-name>")` and
  `(foundation/extension-readme '<id>)` from inside `:code`.
- Do NOT add a second copy of any doc anywhere in the extension tree
  (no `extensions/<name>/README.md`, no
  `docs/src/extensions/<id>.md` containing inlined content,
  no `extensions/<name>/src/.../README.md`). `vis.edn` is the
  canonical location.
- `:content` is Markdown with sentence-case headings and real UTF-8
  characters — same rules as the book (see the docs-style rule
  above). When the extension is end-user-facing, surface it from
  `docs/src/SUMMARY.md` with a short manual page that points
  readers at `(foundation/extension-readme '<id>)` (the book is allowed to
  briefly summarize, but MUST NOT inline the full body).
- The id (top-level key in `vis.edn`) MUST equal the registered
  extension's `:ext/ns-alias :alias`. The loader does not enforce
  this strictly, but every reviewer should reject mismatches — if
  the LLM types `(meta/...)`, the on-disk id is `foundation`.
- Multiple jars MAY contribute under the same id; their `:nses` are
  deduped and their `:docs` are merged (last write wins per name).
  `:reflinks` are recomputed across the entire registry after every
  merge so a later jar's links can target an earlier jar's docs.
- Editing tip: `:content` is a multi-line EDN string. The supported
  authoring loop is to keep the Markdown body in a sibling `.md`
  file in your source tree and re-inline it into `vis.edn` with a
  small `pprint`-based generator script (see
  `dev/inline-doc.clj`-style helpers, or write your own — the
  contract is just "produce a valid EDN map"). Do NOT keep two
  on-disk copies of the same content under version control.
- The vis-common-foundation extension exposes the LLM-facing surface for this:
  `(foundation/extensions)` lists every loaded extension with its docs
  catalog; `(foundation/extension-docs ext-ref)` returns the
  `[{:name :created-at :description :links :reflinks} …]` summaries
  for one extension; `(foundation/extension-doc ext-ref name)` returns the
  full descriptor including `:content`; `(foundation/extension-readme
  ext-ref)` is the README `:content` convenience.

Why: agents that need to verify what an extension actually does —
before reaching for one of its tools — must be able to read the
extension's own description as data, not guess from symbol names.
Inlining the docs in `vis.edn` keeps it a single classpath read at
boot, eliminates the path-vs-call ambiguity an external doc tree
introduced (`vis/<id>` reads as a sandbox call), and makes the
abstract index trivially queryable.

Any PR that adds a new extension without `vis.edn`, omits the
README, omits `:description` or `:content` on any declared doc, hand-
authors a `:reflinks` field, or introduces a duplicate doc somewhere
outside `vis.edn` must be rejected.

### Always update `docs/` when touching architecture or public API

`docs/src/` (mdBook) at the repo root is the single source of truth for
Vis architecture, extension system, and public API. **Every** change to
files under `loop/`, `persistance/`, `channels/`, or `core.clj` MUST be
followed by an update to the relevant doc if the change affects:

- File moves, renames, or deletions
- New files or namespaces
- Changes to the iteration pipeline stages
- Changes to the environment map shape or state atoms
- Changes to the extension spec or nudge context
- Changes to the public API (`create-environment`, `query!`, `register-extension!`, etc.)
- Changes to the dependency graph between modules
- Changes to the SQLite schema (`V1__schema.sql`)

Doc files (all under the repo-root `docs/` tree, NOT inside any package):
- `docs/src/README.md` — introduction (why RLM, why SCI, the problem,
  how Vis works, security model, prior art). Merged from the old
  `rationale.md`; do NOT recreate `rationale.md`.
- `docs/src/usage.md` — getting started (install, auth, the four
  ways to talk to the agent, browsing past conversations).
- `docs/src/architecture/` — overview, packages, iteration flow,
  state ownership, database schema, channels.
- `docs/src/extensions/` — overview, extension spec, symbol
  decorators (hooks), environment map, nudge system, vis-common-foundation.

`docs/src/SUMMARY.md` is the mdBook table of contents — keep it in
sync when adding/removing pages. There is intentionally no
`reference/` section or `directory-structure.md` page; the public API
and current package layout are documented inline in the existing
pages (and in this file).

Build: `cd docs && mdbook serve --open`

Skipping this update is a bug. The docs drifting from the code is
how we ended up with an incomprehensible god file in the first place.

### Docs style: real UTF-8, sentence-case headings, no code in titles

Three rules. All three are non-negotiable. Every prior violation
shipped to readers as broken rendering or unreadable noise.

1. **Use real UTF-8 characters in Markdown.** Markdown is NOT JSON.
   `\u2014`, `\u251c`, `\u2500`, `\u2514`, `\u2026` are six-character
   ASCII strings, not characters. mdbook renders them verbatim and
   the page looks like an editor crash dump. Type the actual
   characters: `—` (em dash, U+2014), `…` (ellipsis, U+2026), `├`
   `─` `└` (box-drawing, U+2500 family). If your editor escapes
   them on save, fix the editor; do not commit the escapes. CI grep:

   ```bash
   grep -rn '\\u[0-9a-f]\{4\}' docs/src/ && exit 1
   ```

2. **Headings are short, descriptive, sentence-case prose. NO code
   identifiers in the title.** A heading is a navigation aid for
   humans, not an API index entry.

   - Bad: `## \`:before-fn\` — entry decorator`,
     `### \`(foundation/turn)\``,
     `### 1) \`conversation_soul\``,
     `### Embedded \`:cmd/subcommands\` vector`,
     `### Render caches (\`ext/channel_tui/render.clj :: fmt-cache\`)`,
     `## How Vis Works` (Title Case).
   - Good: `## Entry decorator`, `### Current turn snapshot`,
     `### Conversation soul`, `### Embedded subcommand vector`,
     `### Render caches`, `## How Vis works`.

   The keyword / function name / table name / file path that the
   section documents goes in the **first line of the body** as a
   short "Slot key: \`:before-fn\`" / "Call: \`(foundation/turn)\`" /
   "Table: \`conversation_soul\`" / "Lives in:
   \`ext/channel_tui/render.clj\`" lead. The reader still gets the
   identifier; it just isn't load-bearing on the heading.

3. **Internal links use slugified anchors of the live heading.**
   Renaming a heading invalidates every `#anchor` link to it. After
   any heading rename, grep for the old anchor across `docs/src/` and
   repoint or delete every reference. Example: renaming `## Auto-discovery
   resource` to `## Auto-discovery` changes the anchor from
   `#auto-discovery-resource` to `#auto-discovery`; the doc that
   linked to the old anchor breaks silently — mdbook does not warn.

### Inspect the SQLite DB before theorizing about a bug

`~/.vis/vis.mdb/vis.db` is the single source of truth for everything
that happened in every conversation — queries, iterations, final
answers, persisted SCI vars, timings, costs. Before hypothesizing
about a user-reported bug that references a specific `conversation-id`
or `query-id`, **open the DB**. The conversation API
(`com.blockether.vis.core`) covers the
common high-level reads (list by channel, env-for, send!, delete!)
and should be your first stop. Drop to raw SQL for schema/migration
checks, cross-conversation aggregates, or low-level execution-row
triage that the conversation API does not expose.

There is no longer a dedicated `dev/dev.clj` post-mortem debugger
in-tree. If you find yourself running the same ad-hoc query twice,
lift it into a real namespace (under `packages/vis-persistance` or a
new `packages/vis-debug` if it grows enough) instead of wedging it
into an editor scratch buffer.

Raw SQL triage checklist:

```bash
DB=~/.vis/vis.mdb/vis.db
CID='<conversation-soul-uuid>'

# Conversation soul + state
sqlite3 -header $DB \
  "SELECT * FROM conversation_soul WHERE id='$CID';"
sqlite3 -header $DB \
  "SELECT * FROM conversation_state WHERE conversation_soul_id='$CID';"

# All turns (queries) for a conversation state
CSID='<conversation-state-uuid>'
sqlite3 -header $DB "
  SELECT qs.id, qs.query, qs.created_at,
         qst.status, qst.llm_root_model
  FROM query_soul qs
  JOIN query_state qst ON qst.query_soul_id = qs.id
  WHERE qs.conversation_state_id='$CSID'
  ORDER BY qs.created_at;"

# Iterations for a query state
QSID='<query-state-uuid>'
sqlite3 -header $DB "
  SELECT id, position, status,
         substr(llm_thinking,1,200) AS thinking,
         llm_error, llm_full_duration_ms, metadata
  FROM iteration
  WHERE query_state_id='$QSID'
  ORDER BY position;"

# Expression states (vars + call results) for an iteration
IID='<iteration-uuid>'
sqlite3 -header $DB "
  SELECT es.id, soul.kind, soul.name,
         es.version, es.success,
         substr(es.expr,1,200) AS expr,
         es.duration_ms
  FROM expression_state es
  JOIN expression_soul soul ON soul.id = es.expression_soul_id
  WHERE es.iteration_id='$IID'
  ORDER BY es.created_at;"
```

Cross-reference with `~/.vis/vis.log` to recover the full pre-truncation `thinking`, raw LLM reasoning, input/output tokens, and `rlm-stage` transitions. Logs complement the DB; they don't replace it.

Only AFTER the DB has been inspected may you form a hypothesis, propose a fix, or blame the model, the UI, or the runtime loop. This rule exists because repeated incidents wasted a turn on plausible-sounding code-only explanations when the DB would have pinpointed the bug in one query.

## Clojure CLI

### Running

```bash
bin/vis                        # prints the help tree (no implicit "default channel")
bin/vis channels tui           # TUI chat
bin/vis channels telegram      # Telegram bot
bin/vis run "prompt"           # one-shot agent query
clojure -M:test                # aggregate test runner across packages
```

`bin/vis` is the checked-in wrapper that `exec`s `clojure -M:vis "$@"`
from the repo root. Add `bin/` to your `PATH` once and daily use is
just `vis <subcommand>`.

Available aliases (root `deps.edn`): `:vis` (the `vis` CLI), `:test`,
`:bench`, `:build`, `:dev`, `:antq`.

### Bumping a shared dependency

vis is polylith — every package declares its OWN deps in
`packages/<name>/deps.edn`. There is intentionally NO `vis-common`
god-bag. To bump a coordinate that several packages share (e.g.
`com.blockether/svar` lives in `vis-runtime`, `vis-channel-tui`,
`vis-persistance`, `vis-benchmark`) use the checked-in helper:

```bash
bin/bump-dependency com.blockether/svar 0.3.12   # rewrite + verify + stage
bin/bump-dependency --dry-run org.clojure/clojure 1.12.5
bin/bump-dependency --no-verify some.lib/foo 1.2.3
```

It scans root `deps.edn` + every `packages/*/deps.edn`, rewrites
only `{:mvn/version "..."}` entries that follow the exact
coordinate (leaves `:local/root` deps alone), runs
`./verify.sh --quick`, and `git add`s the modified files. Idempotent
— re-running with the same version is a no-op. Do NOT hand-edit N
deps.edn files for a shared bump; the script is the single supported
path.

Full svar release flow (cross-repo):

```bash
cd ../svar && git tag v0.3.12 && git push origin v0.3.12   # CI deploys to Clojars
cd ../vis  && bin/bump-dependency com.blockether/svar 0.3.12
git commit -m "deps: bump svar to 0.3.12"
```

### Project Structure

The codebase is a polylith-style monorepo split into four host
packages plus a sibling tree of classpath plug-ins:

- `packages/` — four host packages, layered from leaf to binary:
  - `packages/vis-persistance/` — backend-agnostic storage facade
    and Flyway-driven migration runner. Leaf: depends on Clojure +
    flyway-core only.
  - `packages/vis-extension/` — extension/channel/provider/CLI-command
    contracts and the unified classpath discovery loader. Depends on
    `vis-persistance` (the unified `register-global!` dispatches the
    `:ext/persistance` slot to the persistence facade).
  - `packages/vis-runtime/` — the iteration loop, the SCI sandbox, the
    conversation lifecycle, the cross-channel state, and the public
    API facade (`com.blockether.vis.core`). Depends on
    `vis-extension` and `vis-persistance`.
  - `packages/vis-main/` — the `vis` binary's CLI surface: the
    dispatcher (`commandline.main`), the built-in commands
    (`channels.cli`), the one-shot agent helper (`channels.cli.agent`),
    and the persistence-backed Telemere `:db` handler (`logging`).
    Depends on `vis-runtime`.
- `extensions/` — every classpath plug-in, grouped by surface category:
  - `extensions/channels/` — `vis-channel-tui`, `vis-channel-telegram`.
  - `extensions/providers/` — `vis-provider-github-copilot`.
  - `extensions/persistance/` — `vis-persistance-sqlite`.
  - `extensions/common/` — `vis-common-foundation`, `vis-common-editing` (filesystem +
    code-editing tools the agent reaches for in nearly every task).
  Each ships a `META-INF/vis-extension/vis.edn` and self-registers
  at namespace load. Every extension's `deps.edn` declares
  `:local/root "../../../packages/vis-runtime"` (vis-runtime transitively
  pulls vis-extension and vis-persistance). `vis-runtime` does not
  require any extension by namespace.
- `benchmarks/` — the benchmark harness (4clojure, HumanEval,
  SWE-bench Verified). NOT a classpath plug-in; pulled in via the
  `:bench` alias only. Working directories (`results/`,
  `trajectories/`, `swebench-harness/`) are created on demand under
  `benchmarks/`.

Each package has its own `deps.edn` and is publishable independently.
The repo-root `deps.edn` is a thin aggregator: its top-level `:deps`
points at `vis-cli` (which transitively pulls every host package);
its `:vis` alias adds every classpath plug-in so a single
`clojure -M:vis` (or `bin/vis` once on `PATH`) from the repo root has
the whole product on the classpath.

**Canonical package list:** `docs/src/architecture/packages.md` —
the single source of truth for what each package does, where it
lives, the dependency direction, and the auto-discovery resources.
Update that page when adding/removing/renaming a package; do not
maintain a divergent list here.

Quick mental map (use `packages.md` for details):

- `packages/vis-persistance` — storage facade contract (leaf)
- `packages/vis-extension` — extension/channel/provider/CLI contracts + unified discovery loader
- `packages/vis-runtime` — iteration loop + SCI sandbox + conversations + public API facade
- `packages/vis-main` — `vis` binary: dispatcher, built-in commands, one-shot agent, `:db` Telemere handler
- `extensions/persistance/vis-persistance-sqlite` — SQLite persistence backend
- `extensions/providers/vis-provider-github-copilot` — GitHub Copilot OAuth provider
- `extensions/channels/vis-channel-tui`, `extensions/channels/vis-channel-telegram` — channel implementations
- `extensions/common/vis-common-foundation`, `extensions/common/vis-common-editing` — SCI sandbox extensions
- `benchmarks/` — benchmark harness (`:bench` alias only; not a classpath plug-in)

ONE classpath-scan auto-discovery resource: `META-INF/vis-extension/vis.edn`. EDN
vector of namespace symbols loaded at startup by the single loader
`com.blockether.vis.core/discover-extensions!`. Each namespace
self-registers into whichever subsystem registry it targets
(extension symbols, channels, CLI commands, providers, persistence
backends) via the matching `register-global!` / `register-backend!`
call. Drop a jar that ships a `META-INF/vis-extension/vis.edn` on the classpath
and `vis-runtime` picks it up at the next process boot — no edits
required. There are NO per-subsystem resource files anymore; the
old `META-INF/vis/{extensions,channels,commandline,providers,
persistance-backends}.edn` paths were collapsed into the unified
`META-INF/vis-extension/vis.edn` and removed without backwards-compat aliases.

Docs build: `cd docs && mdbook-mermaid install . && mdbook serve --open`
(mdBook source lives at the repo root under `docs/`, not inside any
package).

**DO NOT create or maintain a directory-structure file.** The codebase
changes faster than any static tree can track. Use `find`, `ls`, `grep`
to explore. If an agent writes a directory-structure doc, delete it.

## Ubiquitous Language (MANDATORY)

- Use `conversation`, never `session`, for the product concept across TUI, Telegram, and CLI.
- Use `turn` for the product-level ask+answer. `query` and `iteration` remain runtime internals.
- Use `tool` and `skill`. Do not use `capability` as a catch-all for agent features.
- Keep `capability` / `capabilities` only where an external provider/router API already requires that word.
- Use `channel` for `:tui`, `:vis`, `:telegram`, and `:cli`. The `:vis`
  channel is the TUI's conversation namespace (TUI calls
  `(conversations/create! :vis)`); `:tui` is the registered channel
  id used by `vis-channel-tui`'s `channel/register-global!` for CLI dispatch.
- Use `environment` in public API. `env` is allowed in internal local bindings only.

### No abbreviated identifiers in source code

Do NOT abbreviate domain terms in identifiers. Spell things out. The cost
of a few extra characters is dwarfed by the readability win and by the
risk of an LLM-or-human reader misreading a half-formed shorthand. The
rule applies to: function names, fn parameters, `let`-bindings,
`def`/`defn` names, `:keys` destructuring, map keys we author, doc
strings, log keys, and metric names.

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
| `info`      | `information` (only at the end of a name; `db-info` stays — it's a domain noun) |

**Idioms that stay** (Clojure-native; do not change):

- `var`, `vars`, `defn`, `def`, `let`, `fn`, `ns`, `sym`, `ctx`, `opts`,
  `args`, `id`, `db`, `str` (only as `clojure.string` alias or in
  built-in fn names like `pr-str`, `subs`, `str/replace`).
- `env` is permitted ONLY as a local-scope binding name; public API
  uses `environment` (already enshrined above).
- `kw` (keyword) inside one-shot reader/normalizer helpers; spell out
  in any externally-visible name.

### SYSTEM vars are UPPERCASE and explicitly defined

The sandbox-visible system vars carrying the user's current query, the
model's last reasoning text, and the prior-turn final answer are named
**`USER_TURN_REQUEST`, `REASONING`, `ASSISTANT_TURN_ANSWER`** — ALL CAPS, no earmuffs.

They are explicitly `(def USER_TURN_REQUEST nil)` / `(def REASONING nil)` /
`(def ASSISTANT_TURN_ANSWER nil)` at environment construction so the symbols always
resolve, even before the first turn. They are subsequently rebound
after each iteration via the iteration loop's bookkeeping.

Do NOT introduce earmuffed names (`*query*`, `*foo*`) for new system
vars. The system-var registry (`SYSTEM_VAR_NAMES`) is a fixed set;
adding to it is a deliberate API change, not a free-form pattern.

Why uppercase, not earmuffs:

- Earmuffs are Clojure's idiom for *dynamic vars* (`*out*`, `*ns*`).
  Our SYSTEM vars are **plain SCI bindings**, not dynamic; the earmuff
  signal misled readers into thinking they could `binding`-shadow them.
- Uppercase aligns with the Clojure idiom for *constants* (`MAX_VAL`,
  `URL_PATTERN`). The SYSTEM vars are read-only from the model's POV;
  the loop owns mutation.
- Cleaner in `<var_index>` and `<system_state>` blocks (no asterisk
  punctuation noise).

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

Why this matters: half the looping pathologies in CRITIQUE.md trace back
to over-compressed projections (`:s :l :t :map :n 12` vs. real Clojure
shape). The same compression instinct in identifiers makes call sites
harder to read and diff harder to review. Spell things out. Reviewers
MUST reject PRs that introduce new banned abbreviations from the table
above.

## Namespace Architecture

### Namespace Layers

- `*.shared` means reusable functions for one bounded context.
- `*.core` means orchestration and use cases for one bounded context.
- `*.persistence*` means storage/schema/DB boundary for one bounded context.
- `*.presentation*` means pure rendering/view-model formatting for one bounded context or adapter.
- `channels.*` means external surface code only: TUI, Telegram, CLI.
- top-level facades like `core` should stay thin and stable.

### Channel adapters (one package each)

- `extensions/channels/vis-channel-tui/src/com/blockether/vis/ext/channel_tui/*` — Lanterna TUI
- `extensions/channels/vis-channel-telegram/src/com/blockether/vis/ext/channel_telegram/*` — Telegram bot
- `packages/vis-main/src/com/blockether/vis_main/commandline/main.clj` — CLI dispatcher (registry-driven)
- `packages/vis-main/src/com/blockether/vis_main/channels/cli.clj` — vis-cli's built-in commands (`run`, `auth`, `doctor`, `conversations`, `extensions list`)
- `packages/vis-main/src/com/blockether/vis_main/channels/cli/agent.clj` — one-shot agent helper used by `vis run`

Third-party channels register themselves at namespace load via
`com.blockether.vis.core/register-global!` and ship a
unified `META-INF/vis-extension/vis.edn` resource. The CLI dispatcher
discovers them; nothing in vis-runtime references a concrete channel
namespace. See `docs/src/architecture/channels.md`.

### Runtime modules (vis-runtime)

All under `packages/vis-runtime/src/com/blockether/vis_runtime/`:

- `core.clj` — public API facade (`create-environment`,
  `dispose-environment!`, `register-extension!`, `active-extensions`,
  `assemble-system-prompt`, `query!`, `MAX_ITERATIONS`). Does NOT own
  `-main` (that's vis-cli). Extension authoring helpers (`extension`,
  `symbol`, `value`, `register-global!`, `render-prompt`) are NOT
  re-exported — require `com.blockether.vis.core`
  directly.
- `config.clj` — config loader, db-path (`~/.vis/vis.mdb`), router builder
- `loop/core.clj` — environment lifecycle + system-prompt assembly
- `loop/mustache.clj` — Mustache templating helpers exposed in the SCI sandbox
- `loop/runtime/conversation/core.clj` — conversation lifecycle
  (`create!`, `by-id`, `by-channel`, `for-telegram-chat!`,
  `set-title!`, `env-for`, `effective-system-prompt`, `send!`,
  `close!`, `delete!`, `db-sweep-orphaned-running-queries!`, `close-all!`)
- `loop/runtime/conversation/environment/core.clj` — SCI sandbox + var-index
- `loop/runtime/conversation/environment/query/core.clj` — query engine
- `loop/runtime/conversation/environment/query/iteration/core.clj` — iteration engine
- `channels/core.clj` — cross-channel provider mgmt and streaming progress tracker
- `channels/cancellation.clj` — in-flight query cancellation registry

### Persistence + extension contract

- `packages/vis-persistance/src/com/blockether/vis_persistance/{base,core,migration}.clj` —
  facade API (`db-create-connection!`, `db-dispose-connection!`,
  `db-list-conversations`, etc.) + migration runner. Backends
  self-register via the unified `META-INF/vis-extension/vis.edn`
  resource and call `register-backend!` at namespace load time.
- `extensions/persistance/vis-persistance-sqlite/src/com/blockether/vis/ext/persistance_sqlite/core.clj` —
  SQLite + Flyway backend. Schema:
  `extensions/persistance/vis-persistance-sqlite/resources/db/sqlite/migration/V1__schema.sql`.
- `packages/vis-extension/src/com/blockether/vis_extension/extension.clj` —
  extension spec, symbol/value builders, hook protocol, global
  registry, unified classpath discovery
  (`META-INF/vis-extension/vis.edn`) for every extension surface
  (ext symbols, channels, CLI commands, providers, backends).
- `packages/vis-extension/src/com/blockether/vis_extension/channel.clj` —
  channel descriptor spec + global registry + classpath discovery
  (registered through the unified `META-INF/vis-extension/vis.edn`).
- `packages/vis-extension/src/com/blockether/vis_extension/{provider,error,commandline/base}.clj` —
  provider descriptor spec + registry, shared error formatting, CLI
  command spec + registry.

Use `find`/`grep` to explore the tree — no static directory doc exists.

### Refactor Rules

- Do not introduce new `session` names in code, routes, vars, logs, or UI copy.
- Do not introduce new catch-all `capability` names when the real concept is `tool` or `skill`.
- If `routes` or `presenter` code needs raw `conversations/env-for` or DB access, the boundary is wrong.
- Prefer in-place renames for vocabulary fixes; split files only when a namespace owns multiple contexts.
- Prefer functional ownership over historical placement: shared vs core vs persistence vs presentation vs channels, inside the correct bounded context.
- Put reusable functions in `*.shared`, orchestration in `*.core`, storage in `*.persistence*`, rendering in `*.presentation*`, and external surfaces in `channels.*`.
- Use `core.clj` as the default application/use-case namespace in each bounded context.
- Do not turn `loop.core` into a dumping ground for unrelated behavior.
- Treat conversations as an RLM subcontext, not as a top-level bounded context separate from RLM.
- Treat `agent.clj` as CLI-owned helper code, not as a separate adapter. It lives at `packages/vis-main/src/com/blockether/vis_main/channels/cli/agent.clj` alongside `channels/cli.clj` in the same package.

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

All application state lives in `ext/channel_tui/state.clj` using a re-frame dispatch pattern.

**Rules:**
- ALL app state must go through `state/app-db` — never create standalone atoms for app state
- State changes MUST use `(state/dispatch [:event-name args...])` — never `swap!`/`reset!` on app-db directly
- Event handlers registered with `reg-event-db` must be **pure functions**: `(fn [db event-vec] new-db)`
- Side effects (RLM calls, disk I/O, futures) go through `reg-event-fx` + `reg-fx`
- Local ephemeral state in dialogs (selection index, cursor position) may use local atoms — these are component-scoped and transient

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
{:config     nil              ;; provider config map: {:providers [{:id kw :label str :api-base str :api-key? str :model str} ...]}
 :conversation nil            ;; {:id conversation-id} — handle into the conversations cache
 :messages   []               ;; [{:role :user|:assistant :text str :timestamp #inst}]
 :msg-scroll nil              ;; row offset, nil = auto-bottom
 :input      {:lines [] :crow 0 :ccol 0}
 :loading?   false            ;; true while RLM is working
 :dialog-open? false}         ;; dialog singleton guard
```

### Conversations (`com.blockether.vis.core`)

**One module owns env lifecycle for every frontend.** TUI uses the
`:vis` channel, the CLI agent uses `:cli`, Telegram uses `:telegram`.
Conversation IDs are plain UUIDs. No name prefixes, no string lookups.

```clojure
(require '[com.blockether.vis.core :as conversations])

;; Create / lookup
(conversations/create! :vis)                    ;; new TUI conversation
(conversations/create! :cli {:title "…"})       ;; one-shot CLI agent run
(conversations/by-id conversation-id)                   ;; conversation map or nil
(conversations/by-channel :vis)                 ;; sidebar / list, recent first
(conversations/by-channel :telegram)
(conversations/for-telegram-chat! chat-id)      ;; find-or-create by chat-id

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

**ONE SQLite DB for everything.** Path: `~/.vis/vis.mdb` (`config/db-path`). Every frontend opens the same DB; the connection pool is shared across environments.

Schema: soul/state model with versioned execution history.
Full reference: `docs/src/architecture/database.md`.

**Entity hierarchy:**
- `conversation_soul` → `conversation_state` → `query_soul` → `query_state` → `iteration` → `expression_state`
- `expression_soul` (var/call/literal identity, branch-local)

Every `(def ...)` is persisted as a versioned `expression_state` row. `var-history` inspects prior versions on demand.

**Investigating DB state:**
```clojure
(require '[com.blockether.vis.core :as conversations]
         '[com.blockether.vis.core :as db])

;; List conversations
(conversations/by-channel :vis)
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
- `create-environment router {:db path :conversation selector}` — selector is `nil` | `:latest` | uuid | `[:id uuid]`. Nil creates a fresh conversation; an id-ref resumes an existing one.
- `register-extension!` — register a validated extension into the environment (tools, nudges, prompt context).
- `active-extensions environment` — vec of currently-active extensions; call ONCE per query and thread the vec through `assemble-system-prompt` + per-iteration nudge collectors.
- `assemble-system-prompt environment {:active-extensions vec, :system-prompt opt}` — single source of truth for the system message. Required by both loop paths and the TUI `[?]` inspector.
- `query! environment [(llm/user "...")] opts` — messages must be a vector of message maps. See `conversations/send!` docstring for every opt forwarded to `query!`.
- `dispose-environment!` — releases the environment handle; the shared SQLite DataSource stays open for sibling envs.

Extension authoring API lives on `com.blockether.vis.core` —
`extension`, `symbol`, `value`, `register-global!`, `render-prompt`,
`load-extension!`, `discover-extensions!`. Channel authoring API
lives on `com.blockether.vis.core`. Neither is re-exported from
`com.blockether.vis.core` anymore.

**Iteration lifecycle:** The LLM does **not** call `(FINAL ...)` as a SCI fn. svar sends a spec-validated JSON response per provider capability: `ITERATION_SPEC_NON_REASONING` (includes `:thinking`) or `ITERATION_SPEC_REASONING` (no `:thinking`). Shared fields come from `ITERATION_SPEC_BASE` (`:code` vec + optional `:final {:answer :confidence :language :sources}` + `:next-optimize`). When `:final` is set, iteration stops and the answer is the RLM result. Observability: pass `{:hooks {:on-chunk (fn [{:iteration :thinking :code :final :done?}])}}` to `conversations/send!`.

**Plan slot rule (Phase 1):** reasoning continuity is delivered by
three structured slots, not by a `<prior_thinking>` blob.

- `<plan>` — sticky structured TODO list (`:plan_state` ref spec).
  The model emits it at iter 0 and the loop carries it verbatim until
  re-emit. Max 20 items, exactly one `:in_progress`.
- `<breadcrumbs>` — cumulative one-liner per iteration (last K=20),
  authored by the model in `:breadcrumb`. Tactical history at one line
  per iter.
- `<recent_thought>` — last iteration's `:thinking` text only,
  capped at 4000 chars.

SYSTEM vars (`USER_TURN_REQUEST`, `REASONING`, `ASSISTANT_TURN_ANSWER`) appear inlined in
`<system_state>` with their current values, NOT in `<var_index>`. The
previous turn's bounded digest (`{:goal :counts :outcome
:abandon-reason}`) lands in `<system_state>.PRIOR_TURN`.

When the agent genuinely needs older reasonings, the (opt-in)
`vis-common-foundation` extension exposes `(foundation/diagnose)`, `(foundation/failures)`,
`(foundation/turn)`, `(foundation/conversation)`, `(foundation/find-attempts pattern)`,
and `(foundation/var-history 'sym)`.
The deprecated built-in `var-history` still works for backwards
compatibility.

Do NOT reintroduce a `<prior_thinking>` blob, the lossy summarization
chain it produced, or the `HANDOVER_KEEP_LAST=2` cross-query special
case — those were deleted on purpose. The plan slot replaces all of
them with a bounded, structured, sticky projection.

**Frontend wiring:**
- **TUI (`vis-channel-tui`)** — registered channel id `:tui` (default channel for `vis` with no sub-command). `chat/make-conversation` creates a fresh `:vis` conversation on every boot (history starts empty); disposal on exit only closes the env, the conversation stays in the `:vis` channel so other inspectors can see it.
- **Telegram (`vis-channel-telegram`)** — registered channel id `:telegram`. `conversations/for-telegram-chat!` find-or-creates by chat-id; each incoming message becomes a `conversations/send!` with the Telegram persona system prompt.
- **CLI `agent/run!`** — one-shot. Creates a fresh conversation in the `:cli` channel and runs a single query. Conversations persist — past runs are browsable via `(conversations/by-channel :cli)`.
- **Third-party channels** — ship a jar with a `META-INF/vis-extension/vis.edn` resource, a namespace that calls `(channel/register-global! …)` at load, and a `:channel/main-fn` that consumes the CLI tail. The dispatcher picks them up automatically; no edits to `vis-runtime`.
