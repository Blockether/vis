# Vis - Development Guide

## MANDATORY: Agent Rules

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
- `docs/src/rationale.md` — why RLM, why SCI, what we learned
- `docs/src/architecture/` — overview, iteration flow, state, database schema, channels
- `docs/src/extensions/` — extension spec, hooks, environment map, nudge system

`docs/src/SUMMARY.md` is the mdBook table of contents — keep it in
sync when adding/removing pages. There is intentionally no
`reference/` section or `directory-structure.md` page; the public API
and current package layout are documented inline in the existing
pages (and in this file).

Build: `cd docs && mdbook serve --open`

Skipping this update is a bug. The docs drifting from the code is
how we ended up with an incomprehensible god file in the first place.

### Inspect the SQLite DB before theorizing about a bug

`~/.vis/vis.mdb/rlm.db` is the single source of truth for everything
that happened in every conversation — queries, iterations, final
answers, persisted SCI vars, timings, costs. Before hypothesizing
about a user-reported bug that references a specific `conversation-id`
or `query-id`, **open the DB**. The conversation API
(`com.blockether.vis.loop.runtime.conversation.core`) covers the
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
DB=~/.vis/vis.mdb/rlm.db
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
clojure -M:run                # default channel (TUI) via channels.cli dispatcher
clojure -M:run telegram       # explicit channel sub-command
clojure -M:run run "prompt"   # one-shot agent query
clojure -M:test               # aggregate test runner across packages
```

Available aliases (root `deps.edn`): `:run`, `:test`, `:bench`,
`:build`, `:dev`, `:antq`. The `:web` alias is reserved but the
`channels.web.app` namespace is not in tree yet — do not document a
web channel as if it ships.

### Project Structure

The codebase is a polylith-style monorepo under `packages/`. Each
package has its own `deps.edn` and is publishable independently; the
root `deps.edn` aggregates them via `:local/root` so a single
`clojure -M:run` from the repo root has the whole product on the
classpath. Current packages:

- `vis-core` — public API facade, runtime loop, query/iteration
  engine, SCI sandbox, conversation lifecycle, channels.cli dispatcher
  + `cli.agent`. The only package consumers must depend on directly.
- `vis-extension` — standalone extension + channel contract
  (`com.blockether.vis.extension`, `com.blockether.vis.channel`).
  Slim deps on purpose (telemere + clojure.spec); extensions and
  third-party channels depend on this, not on `vis-core`.
- `vis-commandline` — reusable CLI primitives (command spec, arg
  parsing, dispatch, help rendering) consumed by `channels.cli`.
- `vis-persistance` — persistence FACADE: public API + spec, no JDBC.
- `vis-persistance-sqlite` — SQLite + Flyway backend; auto-registers
  via `META-INF/vis/persistance-backends.edn`.
- `vis-logging` — Telemere → SQLite log handler (opt-in).
- `vis-provider` — vendor auth (currently GitHub Copilot OAuth).
- `vis-tui` — Lanterna TUI channel (`:tui` channel id, default).
- `vis-telegram` — Telegram long-poll bot (`:telegram` channel id).
- `vis-benchmark` — benchmark harness (not a runtime package).

Docs build: `cd docs && mdbook serve --open` (mdBook source lives at
the repo root under `docs/`, not inside any package).

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
  id used by `vis-tui`'s `channel/register-global!` for CLI dispatch.
- Use `environment` in public API. `env` is allowed in internal local bindings only.

## Namespace Architecture

### Namespace Layers

- `*.shared` means reusable functions for one bounded context.
- `*.core` means orchestration and use cases for one bounded context.
- `*.persistence*` means storage/schema/DB boundary for one bounded context.
- `*.presentation*` means pure rendering/view-model formatting for one bounded context or adapter.
- `channels.*` means external surface code only: TUI, Telegram, CLI.
- top-level facades like `core` should stay thin and stable.

### Channel adapters (one package each)

- `packages/vis-tui/src/com/blockether/vis/channels/tui/*` — Lanterna TUI
- `packages/vis-telegram/src/com/blockether/vis/channels/telegram/*` — Telegram bot
- `packages/vis-core/src/com/blockether/vis/channels/cli.clj` — CLI dispatcher (registry-driven)
- `packages/vis-core/src/com/blockether/vis/channels/cli/agent.clj` — one-shot agent helper used by `vis run`

Third-party channels register themselves at namespace load via
`com.blockether.vis.channel/register-global!` and ship a
`META-INF/vis/channels.edn` resource. The CLI dispatcher discovers
them; nothing in `vis-core` references a concrete channel namespace.
See `docs/src/architecture/channels.md`.

### Runtime modules (vis-core)

All under `packages/vis-core/src/com/blockether/vis/`:

- `core.clj` — public API facade (`create-environment`,
  `dispose-environment!`, `register-extension!`, `active-extensions`,
  `assemble-system-prompt`, `query!`, `MAX_ITERATIONS`,
  `-main` dispatcher). Extension authoring helpers (`extension`,
  `symbol`, `value`, `register-global!`, `render-prompt`) are NOT
  re-exported — require `com.blockether.vis.extension` directly.
- `config.clj` — config loader, db-path (`~/.vis/vis.mdb`), router builder
- `loop/core.clj` — environment lifecycle + system-prompt assembly
- `loop/mustache.clj` — Mustache templating helpers exposed in the SCI sandbox
- `loop/runtime/conversation/core.clj` — conversation lifecycle
  (`create!`, `by-id`, `by-channel`, `for-telegram-chat!`,
  `set-title!`, `env-for`, `effective-system-prompt`, `send!`,
  `close!`, `delete!`, `sweep-orphaned-running-queries!`, `close-all!`)
- `loop/runtime/conversation/environment/core.clj` — SCI sandbox + var-index
- `loop/runtime/conversation/environment/query/core.clj` — query engine
- `loop/runtime/conversation/environment/query/iteration/core.clj` — iteration engine
- `channels/core.clj` — cross-channel provider mgmt and streaming progress tracker
- `channels/cancellation.clj` — in-flight query cancellation registry

### Persistence + extension contract

- `packages/vis-persistance/src/com/blockether/vis/persistance/{core,base,spec}.clj` —
  facade API (`create-rlm-conn`, `db-list-conversations`, etc.) + spec.
  Backends self-register via `META-INF/vis/persistance-backends.edn`.
- `packages/vis-persistance-sqlite/src/com/blockether/vis/persistance/sqlite/core.clj` —
  SQLite + Flyway backend. Schema: `resources/db/sqlite/migration/V1__schema.sql`.
- `packages/vis-extension/src/com/blockether/vis/extension.clj` —
  extension spec, symbol/value builders, hook protocol, global
  registry, classpath discovery (`META-INF/vis/extensions.edn`).
- `packages/vis-extension/src/com/blockether/vis/channel.clj` —
  channel descriptor spec + global registry + classpath discovery
  (`META-INF/vis/channels.edn`).

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
- Treat `agent.clj` as CLI-owned helper code, not as a separate adapter. It now lives at `packages/vis-core/src/com/blockether/vis/channels/cli/agent.clj` alongside `channels/cli.clj`.
- Do not introduce new top-level `channels/web/*` files. There is no web channel package today; if one is added it ships as its own `packages/vis-web` jar with its own `channel/register-global!`.
- Prefer extracting a deep module first, then renaming callers, then deleting stale code.
- Remove `requiring-resolve` cycles instead of spreading them further.
- Keep slices small and shippable; no big-bang folder shuffle.

### Agent Helper (agent.clj)

CLI-oriented one-shot execution layer (`channels/cli/agent.clj`).

**Programmatic usage:**
```clojure
(require '[com.blockether.vis.channels.cli.agent :as ag])

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

All application state lives in `channels/tui/state.clj` using a re-frame dispatch pattern.

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
     :fx [[:rlm-query (:conv db) text]]}))

;; Bad — direct atom mutation
(swap! some-atom assoc :key val)
```

**App-db shape:**
```clojure
{:config     nil              ;; provider config map: {:providers [{:id kw :label str :api-base str :api-key? str :model str} ...]}
 :conv       nil              ;; {:id conv-id} — handle into the conversations cache
 :messages   []               ;; [{:role :user|:assistant :text str :timestamp #inst}]
 :msg-scroll nil              ;; row offset, nil = auto-bottom
 :input      {:lines [] :crow 0 :ccol 0}
 :loading?   false            ;; true while RLM is working
 :dialog-open? false}         ;; dialog singleton guard
```

### Conversations (`com.blockether.vis.loop.runtime.conversation.core`)

**One module owns env lifecycle for every frontend.** TUI uses the
`:vis` channel, the CLI agent uses `:cli`, Telegram uses `:telegram`.
Conversation IDs are plain UUIDs. No name prefixes, no string lookups.

```clojure
(require '[com.blockether.vis.loop.runtime.conversation.core :as conversations])

;; Create / lookup
(conversations/create! :vis)                    ;; new TUI conversation
(conversations/create! :cli {:title "…"})       ;; one-shot CLI agent run
(conversations/by-id conv-id)                   ;; conv map or nil
(conversations/by-channel :vis)                 ;; sidebar / list, recent first
(conversations/by-channel :telegram)
(conversations/for-telegram-chat! chat-id)      ;; find-or-create by chat-id

;; Mutate
(conversations/set-title! conv-id "New title")
(conversations/env-for conv-id)                 ;; raw env (for presenter projections / inspectors)
(conversations/effective-system-prompt conv-id) ;; assembled system prompt for the [?] inspector

;; Turn
(conversations/send! conv-id msgs opts)         ;; locked per conv-id; see docstring for every opt

;; Lifecycle
(conversations/close! conv-id)                  ;; release env handle, keep DB data
(conversations/delete! conv-id)                 ;; close + purge entity tree + sidecar row
(conversations/sweep-orphaned-running-queries!) ;; mark crashed queries as :error on boot
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
(require '[com.blockether.vis.loop.runtime.conversation.core :as conversations]
         '[com.blockether.vis.persistance.core :as db])

;; List conversations
(conversations/by-channel :vis)
(conversations/by-channel :telegram)
(conversations/by-channel :cli)

;; Access environment for a conversation
(let [env     (conversations/env-for conv-id)
      db-info (:db-info env)]
  ;; Use persistance.core functions with db-info, or drop to raw
  ;; HoneySQL via (jdbc/execute! (:datasource db-info) ...).
  )
```

**Public API (`com.blockether.vis.core` / `com.blockether.vis.loop.core`):**
- `create-environment router {:db path :conversation selector}` — selector is `nil` | `:latest` | uuid | `[:id uuid]`. Nil creates a fresh conversation; an id-ref resumes an existing one.
- `register-extension!` — register a validated extension into the environment (tools, nudges, prompt context).
- `active-extensions environment` — vec of currently-active extensions; call ONCE per query and thread the vec through `assemble-system-prompt` + per-iteration nudge collectors.
- `assemble-system-prompt environment {:active-extensions vec, :system-prompt opt}` — single source of truth for the system message. Required by both loop paths and the TUI `[?]` inspector.
- `query! environment [(llm/user "...")] opts` — messages must be a vector of message maps. See `conversations/send!` docstring for every opt forwarded to `query!`.
- `dispose-environment!` — releases the environment handle; the shared SQLite DataSource stays open for sibling envs.

Extension authoring API lives on `com.blockether.vis.extension` —
`extension`, `symbol`, `value`, `register-global!`, `render-prompt`,
`load-extension!`, `discover-extensions!`. Channel authoring API
lives on `com.blockether.vis.channel`. Neither is re-exported from
`com.blockether.vis.core` anymore.

**Iteration lifecycle:** The LLM does **not** call `(FINAL ...)` as a SCI fn. svar sends a spec-validated JSON response per provider capability: `ITERATION_SPEC_NON_REASONING` (includes `:thinking`) or `ITERATION_SPEC_REASONING` (no `:thinking`). Shared fields come from `ITERATION_SPEC_BASE` (`:code` vec + optional `:final {:answer :confidence :language :sources}` + `:next-optimize`). When `:final` is set, iteration stops and the answer is the RLM result. Observability: pass `{:hooks {:on-chunk (fn [{:iteration :thinking :code :final :done?}])}}` to `conversations/send!`.

**`<prior_thinking>` rule:** the iteration loop ships **only the previous iteration's `:thinking`** in `<prior_thinking>`, plus a one-line breadcrumb. There is no spec field, no carry, no opt-in knob to request more. When the agent needs older reasonings it calls `(var-history '*reasoning*)` (or `(take-last N (var-history '*reasoning*))`) from `:code`. Do NOT reintroduce auto-shipping multiple historical reasonings — the eager-context path was deleted on purpose; the on-demand path is the only path. Cross-query handover at iter 0 (`HANDOVER_KEEP_LAST=2` from the previous turn + final answer) is a separate mechanism and stays.

**Frontend wiring:**
- **TUI (`vis-tui`)** — registered channel id `:tui` (default channel for `vis` with no sub-command). `chat/make-conversation` creates a fresh `:vis` conversation on every boot (history starts empty); disposal on exit only closes the env, the conversation stays in the `:vis` channel so other inspectors can see it.
- **Telegram (`vis-telegram`)** — registered channel id `:telegram`. `conversations/for-telegram-chat!` find-or-creates by chat-id; each incoming message becomes a `conversations/send!` with the Telegram persona system prompt.
- **CLI `agent/run!`** — one-shot. Creates a fresh conversation in the `:cli` channel and runs a single query. Conversations persist — past runs are browsable via `(conversations/by-channel :cli)`.
- **Third-party channels** — ship a jar with a `META-INF/vis/channels.edn` resource, a namespace that calls `(channel/register-global! …)` at load, and a `:channel/main-fn` that consumes the CLI tail. The dispatcher picks them up automatically; no edits to `vis-core`.
