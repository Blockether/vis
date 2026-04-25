# Vis - Development Guide

## MANDATORY: Agent Rules

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

### Always update `resources/docs/` when touching architecture or public API

`resources/docs/src/` (mdBook) is the single source of truth for Vis architecture,
extension system, and public API. **Every** change to files under `loop/`,
`persistance/`, `channels/`, or `core.clj` MUST be followed by an update
to the relevant doc if the change affects:

- File moves, renames, or deletions
- New files or namespaces
- Changes to the iteration pipeline stages
- Changes to the environment map shape or state atoms
- Changes to the extension spec or nudge context
- Changes to the public API (`create-environment`, `query!`, `register-extension!`, etc.)
- Changes to the dependency graph between modules
- Changes to the SQLite schema (`V1__schema.sql`)

Doc files:
- `resources/docs/src/rationale.md` — why RLM, why SCI, what we learned
- `resources/docs/src/architecture/` — overview, iteration flow, directory structure, state, database schema
- `resources/docs/src/extensions/` — extension spec, hooks, environment map, nudge system
- `resources/docs/src/reference/` — public API, reasoning levels, ubiquitous language

Build: `cd resources/docs && mdbook serve --open`

Skipping this update is a bug. The docs drifting from the code is
how we ended up with an incomprehensible god file in the first place.

### Always use `dev/dev.clj` FIRST when investigating conversations

`dev/dev.clj` is the unified post-mortem debugger. It wraps every
relevant DB query into an ergonomic Clojure API and reconstructs the
exact iteration context the LLM saw, the journal, var-index,
nudges, and per-turn quality metrics (repetition fires, budget
extensions, redundant-exec-ratio, slowest iteration, token counts).

Before you open `sqlite3` by hand, before you write a `SELECT ... FROM
entity`, before you theorize about why iterations look funny on the
frontend: **use the debugger**. Raw SQL triage is a fallback for cases
the debugger genuinely can't express, not a first move. If the
debugger is missing a function you need, ADD it to `dev/dev.clj` —
don't go around it. Every ad-hoc SQL query you write instead of
calling `dev/...` is a missed opportunity to make the next post-mortem
faster.

Mandatory debugger-first flow:

```clojure
(require '[dev :as d])

;; Top-level
(d/conversation    "6f832df0-…")   ;; persona, model, turn + iter count, status
(d/turns           "6f832df0-…")   ;; per-turn summary + key-vars + answer preview
(d/quality-report  "6f832df0-…")   ;; metrics: repetition fires, budget ext, redundancy

;; Drill in to one turn
(d/iterations          <query-uuid>)        ;; every iter with reconstructed executions
(d/iteration           <query-uuid> POS)    ;; single iter detail (zero-indexed)
(d/iteration-context   <query-uuid> POS)    ;; journal + var-index + nudges + approx-full

;; Cross-query perspective
(d/query-context       "6f832df0-…" TURN)   ;; cross-query handover + inherited vars
```

`dev/dev.clj` is read-only, shares the SQLite pool with the live
process, and is safe to run while the web server is up. Open a REPL
(`clojure -M:dev -r`) or one-shot `clojure -M:dev -e '(...)'` —
either works.

### Raw SQL is a fallback, not a starting point

`~/.vis/vis.mdb/rlm.db` is the single source of truth for everything that happened in every conversation — queries, iterations, final answers, persisted SCI vars, timings, costs. Before hypothesizing about user-reported bugs that reference a specific `conversation-id` or `query-id`, you MUST open the DB — preferably through `dev/dev.clj`. Only reach for raw SQL when:

- you already confirmed `dev/dev.clj` does not expose the view you
  need AND you intend to upstream that view as a new `dev` function,
- you are checking schema/migration state, not conversation data, OR
- you need cross-conversation aggregates that the debugger cannot
  express.

If those don't apply, call `dev/...` instead.

Raw SQL triage checklist for the remaining cases:

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
clojure -M:run
```

### Project Structure

See `resources/docs/src/architecture/directory-structure.md` for the full annotated tree.

Build docs: `cd resources/docs && mdbook serve --open`

## Ubiquitous Language (MANDATORY)

- Use `conversation`, never `session`, for the product concept across web, TUI, Telegram, and CLI.
- Use `turn` for the product-level ask+answer. `query` and `iteration` remain runtime internals.
- Use `tool` and `skill`. Do not use `capability` as a catch-all for agent features.
- Keep `capability` / `capabilities` only where an external provider/router API already requires that word.
- Use `channel` for `:vis`, `:telegram`, and `:cli`.
- Use `environment` in public API. `env` is allowed in internal local bindings only.

## Namespace Architecture

### Namespace Layers

- `*.shared` means reusable functions for one bounded context.
- `*.core` means orchestration and use cases for one bounded context.
- `*.persistence*` means storage/schema/DB boundary for one bounded context.
- `*.presentation*` means pure rendering/view-model formatting for one bounded context or adapter.
- `channels.*` means external surface code only: web, TUI, Telegram, and CLI.
- top-level facades like `core` should stay thin and stable.

### Adapter Target

- `src/com/blockether/vis/channels/web/*` - web adapter layer
- `src/com/blockether/vis/channels/tui/*` - TUI adapter layer
- `src/com/blockether/vis/channels/telegram/*` - Telegram adapter layer
- `src/com/blockether/vis/channels/cli.clj` - CLI adapter layer

### Web Target

- `src/com/blockether/vis/channels/web/app.clj` - process boot only; Jetty start/stop and wiring
- `src/com/blockether/vis/channels/web/routes.clj` - HTTP only; no DB/env poking, no cache mutation
- `src/com/blockether/vis/channels/web/conversations.clj` - deep web-facing module for conversation list/page/title/projection/cache
- `src/com/blockether/vis/channels/web/executor.clj` - async turn execution and live progress
- `src/com/blockether/vis/channels/web/presentation*.clj` - pure rendering only

### Runtime Target

- `src/com/blockether/vis/core.clj` - public API facade (`create-environment`, `query!`, `dispose-environment!`)
- `src/com/blockether/vis/config.clj` - config and router construction only
- `src/com/blockether/vis/loop/core.clj` - environment lifecycle + iteration loop
- `src/com/blockether/vis/loop/runtime/prompt.clj` - system prompt + nudge composers
- `src/com/blockether/vis/loop/runtime/conversation/core.clj` - conversation lifecycle (create!/send!/close!)
- `src/com/blockether/vis/loop/runtime/conversation/environment/core.clj` - SCI sandbox + var-index
- `src/com/blockether/vis/loop/runtime/conversation/environment/extension.clj` - extension spec + hooks
- `src/com/blockether/vis/loop/runtime/conversation/environment/query/core.clj` - query engine
- `src/com/blockether/vis/loop/runtime/conversation/environment/query/iteration/core.clj` - iteration engine
- `src/com/blockether/vis/persistance/core.clj` - persistence API (delegates to sqlite)

Full directory tree: `resources/docs/src/architecture/directory-structure.md`

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
- Treat `agent.clj` as CLI-owned helper code, not as a separate adapter. Prefer folding it into `channels.cli` or deleting it once callers are simplified.
- Do not keep long-term adapter code at the product root once the bounded-context APIs are ready for an `channels/*` move.
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

**One module owns env lifecycle for every frontend.** Web + TUI share the `:vis` channel; Telegram has its own `:telegram` channel. Conversation IDs are plain UUIDs. No name prefixes, no string lookups.

```clojure
(require '[com.blockether.vis.loop.runtime.conversation.core :as conversations])

;; Create / lookup
(conversations/create! :vis)                    ;; new web/TUI conversation
(conversations/create! :vis {:title "…"})
(conversations/by-id conv-id)                   ;; conv map or nil
(conversations/by-channel :vis)                 ;; sidebar / list, recent first
(conversations/for-telegram-chat! chat-id)      ;; find-or-create by chat-id

;; Mutate
(conversations/set-title! conv-id "New title")
(conversations/env-for conv-id)                 ;; raw rlm env (for presenter projections)

;; Turn
(conversations/send! conv-id msgs opts)         ;; locked per conv-id; see docstring for every opt

;; Lifecycle
(conversations/close! conv-id)                  ;; release env handle, keep DB data
(conversations/delete! conv-id)                 ;; close + purge entity tree + sidecar row
(conversations/close-all!)                      ;; process shutdown
```

### Storage

**ONE SQLite DB for everything.** Path: `~/.vis/vis.mdb` (`config/db-path`). Every frontend opens the same DB; the connection pool is shared across environments.

Schema: soul/state model with versioned execution history.
Full reference: `resources/docs/src/architecture/database.md`.

**Entity hierarchy:**
- `conversation_soul` → `conversation_state` → `query_soul` → `query_state` → `iteration` → `expression_state`
- `expression_soul` (var/call/literal identity, branch-local)

Every `(def ...)` is persisted as a versioned `expression_state` row. `var-history` inspects prior versions on demand.

**Investigating DB state:**
```clojure
(require '[com.blockether.vis.loop.runtime.conversation.core :as conversations])

;; List conversations
(conversations/by-channel :vis)
(conversations/by-channel :telegram)

;; Access environment for a conversation
(let [env     (conversations/env-for conv-id)
      db-info (:db-info env)]
  ;; Use dev/dev.clj for detailed inspection (preferred)
  ;; Or persistance.core functions with db-info
  )
```

**Public API (`com.blockether.vis.core` / `com.blockether.vis.loop.core`):**
- `create-environment router {:db path :conversation selector}` — selector is `nil` | `:latest` | uuid | `[:id uuid]`. Nil creates a fresh conversation; an id-ref resumes an existing one.
- `register-extension!` — register a validated extension into the environment (tools, nudges, prompt context).
- `query! environment [(llm/user "...")] opts` — messages must be a vector of message maps. See `conversations/send!` docstring for every opt forwarded to `query!`.
- `dispose-environment!` — releases the environment handle; the shared SQLite DataSource stays open for sibling envs.

Full API reference: `resources/docs/src/reference/api.md`.

**Iteration lifecycle:** The LLM does **not** call `(FINAL ...)` as a SCI fn. svar sends a spec-validated JSON response per provider capability: `ITERATION_SPEC_NON_REASONING` (includes `:thinking`) or `ITERATION_SPEC_REASONING` (no `:thinking`). Shared fields come from `ITERATION_SPEC_BASE` (`:code` vec + optional `:final {:answer :confidence :language :sources}` + `:next-optimize`). When `:final` is set, iteration stops and the answer is the RLM result. Observability: pass `{:hooks {:on-chunk (fn [{:iteration :thinking :code :final :done?}])}}` to `conversations/send!`.

**`<prior_thinking>` rule:** the iteration loop ships **only the previous iteration's `:thinking`** in `<prior_thinking>`, plus a one-line breadcrumb. There is no spec field, no carry, no opt-in knob to request more. When the agent needs older reasonings it calls `(var-history '*reasoning*)` (or `(take-last N (var-history '*reasoning*))`) from `:code`. Do NOT reintroduce auto-shipping multiple historical reasonings — the eager-context path was deleted on purpose; the on-demand path is the only path. Cross-query handover at iter 0 (`HANDOVER_KEEP_LAST=2` from the previous turn + final answer) is a separate mechanism and stays.

**Frontend wiring:**
- **TUI** — `chat/make-conversation` creates a fresh `:vis` conversation on every boot (history starts empty). Disposal on exit only closes the env; the conversation stays in the `:vis` channel so the web sidebar can see it.
- **Web** — the target vocabulary is conversation-first, not session-first. Move toward `create-conversation!`, `get-conversation`, `delete-conversation!`, and `conversations-list`; keep web projection/cache logic in a dedicated `web.conversations` module and keep routes/presenters away from raw env access.
- **Telegram** — `conversations/for-telegram-chat!` find-or-creates by chat-id; each incoming message becomes a `conversations/send!` with the Telegram persona system prompt.
- **CLI `agent/run!`** — one-shot. Creates a fresh conversation in the `:cli` channel and runs a single query. Conversations persist — past runs are browsable via `(conversations/by-channel :cli)`.
