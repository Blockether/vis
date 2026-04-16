# Vis - Development Guide

## Clojure CLI

### JS Interop

Always use `^js` type annotations when interoperating with JavaScript:

```clojure
;; Good - specify JS types
(defn handler [^js key]
  ...)

;; Good - convert Clojure data to JS objects
(react/createElement Component #js {:props value})

;; Bad - will cause type inference issues
(defn handler [key]
  ...)
```

### Running

```bash
clojure -M:run
```

### Project Structure

- `src/com/blockether/vis/adapters/cli.clj` - CLI entrypoint (subcommands: chat, run, help)
- `src/com/blockether/vis/agent.clj` - Agent orchestration over svar RLM (Sandcastle-inspired)
- `src/com/blockether/vis/config.clj` - Config I/O, provider presets, svar-native helpers
- `src/com/blockether/vis/logging.clj` - TTY logging setup, stream redirection
- `src/com/blockether/vis/redact.clj` - Secret redaction for logs and tool output
- `src/com/blockether/vis/trace.clj` - ANSI trace renderer for CLI + structured sections for TUI
- `src/com/blockether/vis/languages/commons/read.clj` - Base READ tool (offset/limit)
- `src/com/blockether/vis/languages/commons/write.clj` - Base WRITE tool (full overwrite)
- `src/com/blockether/vis/languages/commons/edit.clj` - Base EDIT tool (old_string → new_string)
- `src/com/blockether/vis/adapters/tui/state.clj` - App state (re-frame pattern)
- `src/com/blockether/vis/adapters/tui/screen.clj` - TUI main loop and rendering
- `src/com/blockether/vis/adapters/tui/render.clj` - Rendering components (boxes, bubbles, text wrapping)
- `src/com/blockether/vis/adapters/tui/chat.clj` - LLM integration via SVAR RLM
- `src/com/blockether/vis/adapters/tui/dialogs.clj` - Interactive dialog screens
- `src/com/blockether/vis/adapters/tui/input.clj` - Keyboard input handling
- `src/com/blockether/vis/adapters/tui/provider.clj` - Provider setup wizard (TUI dialogs)
- `src/com/blockether/vis/adapters/tui/theme.clj` - Colors and visual constants

## Ubiquitous Language (MANDATORY)

- Use `conversation`, never `session`, for the product concept across web, TUI, Telegram, and CLI.
- Use `turn` for the product-level ask+answer. `query` and `iteration` remain runtime internals.
- Use `tool` and `skill`. Do not use `capability` as a catch-all for agent features.
- Keep `capability` / `capabilities` only where an external provider/router API already requires that word.
- Use `channel` for `:vis`, `:telegram`, and `:cli`.
- Use `env` or `runtime env` only for the technical RLM object, never as a user-facing concept.

## Current Refactor Track

Canonical plan: `plans/conversation-web-refactor.md`

### Namespace Layers

- `*.shared` means reusable functions for one bounded context.
- `*.core` means orchestration and use cases for one bounded context.
- `*.persistence*` means storage/schema/DB boundary for one bounded context.
- `*.presentation*` means pure rendering/view-model formatting for one bounded context or adapter.
- `adapters.*` means external surface code only: web, TUI, Telegram, and CLI.
- top-level facades like `rlm` should stay thin and stable.

### Adapter Target

- `src/com/blockether/vis/adapters/web/*` - web adapter layer
- `src/com/blockether/vis/adapters/tui/*` - TUI adapter layer
- `src/com/blockether/vis/adapters/telegram/*` - Telegram adapter layer
- `src/com/blockether/vis/adapters/cli.clj` - CLI adapter layer

### Web Target

- `src/com/blockether/vis/adapters/web/app.clj` - process boot only; Jetty start/stop and wiring
- `src/com/blockether/vis/adapters/web/routes.clj` - HTTP only; no DB/env poking, no cache mutation
- `src/com/blockether/vis/adapters/web/conversations.clj` - deep web-facing module for conversation list/page/title/projection/cache
- `src/com/blockether/vis/adapters/web/executor.clj` - async turn execution and live progress
- `src/com/blockether/vis/adapters/web/presentation*.clj` - pure rendering only

### Runtime Target

- `src/com/blockether/vis/config.clj` - config and router construction only
- `src/com/blockether/vis/rlm.clj` - public RLM facade only
- `src/com/blockether/vis/rlm/env.clj` - runtime env construction/state only
- `src/com/blockether/vis/rlm/conversations/shared.clj` - reusable conversation functions inside RLM
- `src/com/blockether/vis/rlm/conversations/core.clj` - conversation lifecycle/send orchestration inside RLM
- `src/com/blockether/vis/rlm/conversations/persistence.clj` - conversation persistence boundary inside RLM
- `src/com/blockether/vis/rlm/shared.clj` - reusable runtime/kernel functions
- `src/com/blockether/vis/rlm/core.clj` - top-level RLM use cases/orchestration
- `src/com/blockether/vis/rlm/tools/*` - tool-surface assembly only
- `src/com/blockether/vis/rlm/skills/*` - skills subsystem only
- `src/com/blockether/vis/rlm/corpus/*` - corpus modules
- `src/com/blockether/vis/rlm/persistence/*` - persistence/contracts modules

### Refactor Rules

- Do not introduce new `session` names in code, routes, vars, logs, or UI copy.
- Do not introduce new catch-all `capability` names when the real concept is `tool` or `skill`.
- If `routes` or `presenter` code needs raw `conversations/env-for` or DB access, the boundary is wrong.
- Prefer in-place renames for vocabulary fixes; split files only when a namespace owns multiple contexts.
- Prefer functional ownership over historical placement: shared vs core vs persistence vs presentation vs adapters, inside the correct bounded context.
- Put reusable functions in `*.shared`, orchestration in `*.core`, storage in `*.persistence*`, rendering in `*.presentation*`, and external surfaces in `adapters.*`.
- Use `core.clj` as the default application/use-case namespace in each bounded context.
- Do not turn `rlm.core` or `rlm.tools` into dumping grounds for unrelated behavior.
- Treat conversations as an RLM subcontext, not as a top-level bounded context separate from RLM.
- Treat `agent.clj` as CLI-owned helper code, not as a separate adapter. Prefer folding it into `adapters.cli` or deleting it once callers are simplified.
- Do not keep long-term adapter code at the product root once the bounded-context APIs are ready for an `adapters/*` move.
- Prefer extracting a deep module first, then renaming callers, then deleting stale code.
- Remove `requiring-resolve` cycles instead of spreading them further.
- Keep slices small and shippable; no big-bang folder shuffle.

### Agent Helper (agent.clj)

Sandcastle-inspired helper layer currently used by CLI-oriented one-shot execution. It is not a separate product adapter.

**Programmatic usage:**
```clojure
(require '[com.blockether.vis.agent :as ag])

;; Define an agent with tools
(def reviewer
  (ag/agent {:name "reviewer"
             :system-prompt "You are a senior Clojure engineer. Review code for bugs, performance, and style."
             :tools [(ag/tool 'read-file slurp
                       {:doc "Read file contents"
                        :params [{:name "path" :type :string :required true}]
                        :returns {:type :string :description "File contents"}})]
             :max-iterations 30}))

;; Run a query
(ag/run! reviewer "Review src/auth.clj for security issues")
;; => {:answer "..." :iterations 5 :duration-ms 2340 :tokens {...} :cost {...}}

;; JSON output
(ag/result->json result)
```

**CLI usage:**
```bash
vis run "What is 2+2?"                          # plain text output
vis run --json "Explain the auth flow"          # JSON output
vis run --model gpt-4o "Summarize X"            # override model
vis run --system-prompt "You are a code reviewer" "Review auth.clj"
```

### State Management (MANDATORY)

All application state lives in `adapters/tui/state.clj` using a re-frame dispatch pattern.

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

### Conversations (`com.blockether.vis.rlm.conversations.core`)

**One module owns env lifecycle for every frontend.** Web + TUI share the `:vis` channel; Telegram has its own `:telegram` channel. Conversation IDs are plain UUIDs — the same `:entity/id` svar uses. No name prefixes, no string lookups.

```clojure
(require '[com.blockether.vis.rlm.conversations.core :as conversations])

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

**ONE SQLite DB for everything.** Path: `~/.vis/vis.mdb` (`config/db-path`). Every frontend opens the same DB; svar's connection pool is shared across envs, so disposing one conversation's env never breaks siblings.

Two layers:

- **rlm layer** — `:conversation` / `:query` / `:iteration` / `:iteration-var` entity tree. Owned by `com.blockether.vis.rlm.*`. `conversation_attrs` holds only `entity_id`, `system_prompt`, `model` — no name, no env_id, no caller-facing lookup keys.
- **vis sidecar** — `vis_conversation(conversation_id, channel, external_id, title, created_at)`. Keyed by the rlm conversation's entity id. `UNIQUE(channel, external_id)` is how Telegram resolves a chat-id to its conversation. Installed lazily on first access from `com.blockether.vis.rlm.conversations.persistence`.

**Entity model (unchanged):**
- `:conversation` — one per conversation; holds `:conversation/system-prompt`, `:conversation/model`.
- `:query` — one per user turn, parented to `:conversation`. `:query/text`, `:query/answer`, `:query/status`, `:query/iterations`, `:query/duration-ms`, `:query/messages` (pr-str'd).
- `:iteration` — one per LLM iteration inside a query. `:iteration/code` (pr-str'd source vec), `:iteration/results`, `:iteration/thinking`, `:iteration/answer` (present iff `:final`), `:iteration/duration-ms`.
- `:iteration-var` — one per `(def …)` inside an iteration. `:iteration.var/name`, `:iteration.var/value` (pr-str'd), `:iteration.var/code`.

Ordering within a parent is by `:entity/created-at`. `restore-var` / `restore-vars` SCI tools rebind prior `:iteration-var`s on demand.

**Investigating DB state:**
```clojure
(require '[com.blockether.vis.rlm.conversations.core :as conversations]
         '[com.blockether.vis.rlm.db :as rlm-db]
         '[com.blockether.vis.config :as config])

;; List vis/telegram conversations
(conversations/by-channel :vis)
(conversations/by-channel :telegram)

;; Raw rlm view via the conversation's env
(let [env      (conversations/env-for conv-id)
      db-info  (:db-info env)
      conv-ref (:conversation-ref env)]
  (rlm-db/db-query-history db-info conv-ref)       ;; {:text :answer-preview :iterations :key-vars}
  (rlm-db/db-list-conversation-queries db-info conv-ref)
  (rlm-db/db-latest-var-registry db-info conv-ref));; {sym → {:value :code …}}
```

**Key rlm DB functions (`com.blockether.vis.rlm.db`):**
- `create-rlm-conn` / `dispose-rlm-conn!` — SQLite pool handle open/close.
- `db-find-latest-conversation-ref` — the most-recent `:conversation` entity.
- `db-resolve-conversation-ref` — accepts `nil` / `:latest` / uuid / `[:id uuid]`.
- `db-list-conversation-queries` / `db-list-query-iterations` / `db-list-iteration-vars` — ordered children.
- `db-query-history` — compact summaries for UI rendering.
- `db-latest-var-registry` — last-write-wins var map for a conversation.
- `delete-entity-tree!` — delete an entity and every descendant via parent_id.

**rlm entrypoints vis uses (`com.blockether.vis.rlm`):**
- `create-env router {:db path :conversation selector}` — selector is `nil` | `:latest` | uuid | `[:id uuid]`. Nil creates a fresh conversation; an id-ref resumes an existing one.
- `register-env-fn!` / `register-env-def!` — wire tools/constants into the SCI sandbox.
- `query-env! env [(llm/user "...")] opts` — messages must be a vector of message maps. See `conversations/send!` docstring for every opt forwarded to `query-env!`.
- `ingest-git! env {:repo-path path :n 100}` — JGit-backed; attaches `search-commits`/`commit-history`/`file-history`/`blame`/`commit-diff`/`commit-parents`/`commits-by-ticket` to the sandbox. `:repo/name` is unique — repeated calls dedupe.
- `dispose-env!` — releases the env handle; the shared SQLite DataSource stays open for sibling envs.

**Iteration lifecycle:** The LLM does **not** call `(FINAL ...)` as a SCI fn. svar sends a spec-validated JSON response per provider capability: `ITERATION_SPEC_NON_REASONING` (includes `:thinking`) or `ITERATION_SPEC_REASONING` (no `:thinking`). Shared fields come from `ITERATION_SPEC_BASE` (`:code` vec + optional `:final {:answer :confidence :language :sources}` + `:next-optimize`). When `:final` is set, iteration stops and the answer is the RLM result. Observability: pass `{:hooks {:on-chunk (fn [{:iteration :thinking :code :final :done?}])}}` to `conversations/send!`.

**Frontend wiring:**
- **TUI** — `chat/make-conversation` creates a fresh `:vis` conversation on every boot (history starts empty). Disposal on exit only closes the env; the conversation stays in the `:vis` channel so the web sidebar can see it.
- **Web** — the target vocabulary is conversation-first, not session-first. Move toward `create-conversation!`, `get-conversation`, `delete-conversation!`, and `conversations-list`; keep web projection/cache logic in a dedicated `web.conversations` module and keep routes/presenters away from raw env access.
- **Telegram** — `conversations/for-telegram-chat!` find-or-creates by chat-id; each incoming message becomes a `conversations/send!` with the Telegram persona system prompt.
- **CLI `agent/run!`** — ephemeral one-shot. Creates a fresh rlm env (no sidecar row) and deletes the conversation entity tree in a `finally` after the query returns — CLI invocations don't accumulate in the `:vis` channel.
